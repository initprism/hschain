{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}

module Control.Distributed.P2P where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable   (Serializable)
import Control.Monad
import Control.Applicative
import Control.Concurrent                         hiding (newChan)
import Control.Concurrent.MVar
import Network.Socket                             (HostName, ServiceName)
import Network.Transport                          (EndPointAddress(..))
import Network.Transport.TCP                      (createTransport, defaultTCPParameters, defaultTCPAddr)
import Data.Functor                               ((<&>))
import Data.Binary                                (Binary(..))
import Data.Maybe                                 (isJust, isNothing)
import Data.Typeable                              (Typeable)
import qualified Data.ByteString.Char8            as BS
import qualified Data.Set                         as S

-- * Peer-to-peer API
type Peers        = S.Set ProcessId
newtype PeerState = PeerState { p2pPeers :: MVar Peers }

initPeerState :: Process PeerState
initPeerState = getSelfPid >>= liftIO . newMVar . S.singleton <&> PeerState

-- ** Initialization

-- | Make a NodeId from "host:port" string.
makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

-- | Start a controller service process and aquire connections to a swarm.
bootstrap
  :: HostName
  -> ServiceName
  -> RemoteTable
  -> [NodeId]
  -> Process ()
  -> IO ()
bootstrap host port rTable seeds prc =
  createLocalNode host port rTable
    >>= (\node -> forkProcess node (peerController seeds)
                  >> runProcess node (waitController prc))

-- | Like 'bootstrap' but use 'forkProcess' instead of 'runProcess'. Returns local node and pid of given process
bootstrapNonBlocking
  :: HostName
  -> ServiceName
  -> RemoteTable
  -> [NodeId]
  -> Process ()
  -> IO (LocalNode, ProcessId)
bootstrapNonBlocking host port rTable seeds prc =
  createLocalNode host port rTable
    >>= (\node -> forkProcess node (peerController seeds)
                  >> forkProcess node (waitController prc)
                     >>= (\pid -> return (node, pid)))

-- | Waits for controller to start, then runs given process
waitController :: Process a -> Process a
waitController prc =
  (\x -> if isNothing x then liftIO (threadDelay 100000) >> waitController prc else say "Bootstrap complete." >> prc)
      =<< whereis peerControllerService


-- | Creates tcp local node which used by 'bootstrap'
createLocalNode
  :: HostName
  -> ServiceName
  -> RemoteTable
  -> IO LocalNode
createLocalNode host port rTable =
  createTransport addr defaultTCPParameters >>= (`newLocalNode` rTable) . either (error . show) id
    where
      addr = defaultTCPAddr host port

peerControllerService :: String
peerControllerService = "P2P:Controller"

-- | A P2P controller service process.
peerController :: [NodeId] -> Process ()
peerController seeds = do
    state <- initPeerState

    getSelfPid >>= register peerControllerService

    mapM_ doDiscover seeds
    say "P2P controller started."
    forever $ receiveWait [ matchIf isPeerDiscover $ onDiscover state
                          , match $ onMonitor state
                          , match $ onPeerRequest state
                          , match $ onPeerResponse state
                          , match $ onPeerQuery state
                          , match onPeerCapable
                          ]

-- -- ** Discovery

doDiscover :: NodeId -> Process ()
doDiscover node = say ("Examining node: " ++ show node) >> whereisRemoteAsync node peerControllerService

doRegister :: PeerState -> ProcessId -> Process ()
doRegister (PeerState{..}) pid = do
    pids <- liftIO $ takeMVar p2pPeers
    if S.member pid pids
        then liftIO $ putMVar p2pPeers pids
        else do
            say $ "Registering peer:" ++ show pid
            _ <- monitor pid

            liftIO $ putMVar p2pPeers (S.insert pid pids)
            say $ "New node: " ++ show pid
            doDiscover $ processNodeId pid

doUnregister :: PeerState -> Maybe MonitorRef -> ProcessId -> Process ()
doUnregister PeerState{..} mref pid = do
    say $ "Unregistering peer: " ++ show pid
    maybe (return ()) unmonitor mref
    peers <- liftIO $ takeMVar p2pPeers
    liftIO $ putMVar p2pPeers (S.delete pid peers)

isPeerDiscover :: WhereIsReply -> Bool
isPeerDiscover (WhereIsReply service pid) =
    service == peerControllerService && isJust pid

data GiveMePeers = GiveMePeers
  deriving (Typeable)

instance Binary GiveMePeers where
  put GiveMePeers = pure ()
  get = pure GiveMePeers

onDiscover :: PeerState -> WhereIsReply -> Process ()
onDiscover _     (WhereIsReply _ Nothing) = return ()
onDiscover state (WhereIsReply _ (Just seedPid)) = do
    say $ "Peer discovered: " ++ show seedPid
    self <- getSelfPid
    send seedPid (self, GiveMePeers)

onPeerResponse :: PeerState -> (ProcessId, Peers) -> Process ()
onPeerResponse state (peer, peers) = do
    say $ "Got peers from: " ++ show peer
    known <- liftIO $ readMVar $ p2pPeers state
    mapM_ (doRegister state) (S.toList $ S.difference peers known)

onPeerRequest :: PeerState -> (ProcessId, GiveMePeers) -> Process ()
onPeerRequest PeerState{..} (peer, _) = do
    say $ "Peer exchange with " ++ show peer
    peers <- liftIO $ takeMVar p2pPeers
    if S.member peer peers
        then liftIO $ putMVar p2pPeers peers
        else do
            _ <- monitor peer
            liftIO $ putMVar p2pPeers (S.insert peer peers)

    self <- getSelfPid
    send peer (self, peers)

onPeerQuery :: PeerState -> SendPort Peers -> Process ()
onPeerQuery PeerState{..} replyTo = do
    say $ "Local peer query."
    liftIO (readMVar p2pPeers) >>= sendChan replyTo

onPeerCapable :: (String, SendPort ProcessId) -> Process ()
onPeerCapable (service, replyTo) = do
    say $ "Capability request: " ++ service
    res <- whereis service
    case res of
        Nothing -> say "I can't."
        Just pid -> say "I can!" >> sendChan replyTo pid

onMonitor :: PeerState -> ProcessMonitorNotification -> Process ()
onMonitor state (ProcessMonitorNotification mref pid reason) = do
    say $ "Monitor event: " ++ show (pid, reason)
    doUnregister state (Just mref) pid

-- ** Discovery

-- | Get a list of currently available peer nodes.
getPeers :: Process [NodeId]
getPeers = do
    say "Requesting peer list from local controller..."
    (sp, rp) <- newChan
    nsend peerControllerService (sp :: SendPort Peers)
    receiveChan rp <&> (map processNodeId . S.toList)

-- | Poll a network for a list of specific service providers.
getCapable :: String -> Process [ProcessId]
getCapable service = do
    undefined
    (sp, rp) <- newChan
    nsendPeers peerControllerService (service, sp)
    say "Waiting for capable nodes..."
    go rp []
    where go rp acc = do res <- receiveChanTimeout 100000 rp
                         case res of Just pid -> say "cap hit" >> go rp (pid:acc)
                                     Nothing -> say "cap done" >> return acc

-- ** Messaging

-- | Broadcast a message to a specific service on all peers.
nsendPeers :: Serializable a => String -> a -> Process ()
nsendPeers service msg = getPeers >>= mapM_ (\peer -> nsendRemote peer service msg)

-- | Broadcast a message to a service of on nodes currently running it.
nsendCapable :: Serializable a => String -> a -> Process ()
nsendCapable service msg = getCapable service >>= mapM_ (`send` msg)
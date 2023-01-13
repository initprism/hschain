{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Server where

import Lib

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Web.Spock
import Web.Spock.Config
import Text.PrettyPrint.GenericPretty
import GHC.Generics
import qualified Control.Distributed.P2P  as P2P
import qualified System.Log.Logger        as L
import qualified Data.Binary              as B
import qualified Data.Text as T

data Session = Session

data MainArgs = MainArgs { httpPort :: String
                         , p2pPort  :: String
                         , seedNode :: Maybe String }

data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , node            :: LocalNode
                                       , pid             :: ProcessId } deriving (Generic)

-- ADT for data that will be sent across the P2P network
data BlockUpdate = UpdateData Block | ReplaceData [Block] | RequestChain deriving (Generic)

instance B.Binary BlockUpdate

-- Type alias
type Get state return
  = forall m. (SpockState m ~ state, MonadIO m, HasSpock m) => m return


debugM :: (MonadIO m) => String -> m ()
debugM m = liftIO $ L.debugM "hschain" m

p2pServiceName :: String
p2pServiceName = "updateservice"

-- retrieve the current block chain
getBlockChain :: Get BlockChainState [Block]
getBlockChain = liftIO . readIORef . blockChainState =<< getState 

-- retrieve the most recent block in the chain
getLatestBlock :: Get BlockChainState Block
getLatestBlock = last <$> getBlockChain

-- add a block to blockchain
addBlock :: MonadIO m => IORef [Block] -> Block -> m ()
addBlock ref block = do
  chain <- liftIO $ readIORef ref
  if isValidNewBlock (last chain) block
  then do
    debugM "adding new block"
    _ <- liftIO $ atomicModifyIORef' ref $ \x -> (x <> [block], x <> [block])
    return ()
  else debugM "new block not valid, skipping"

-- given some data, create a valid vlock
mineBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => String -> m Block
mineBlock str = getLatestBlock >>= flip mineBlockFrom str

-- if this chain is valid and longer than what we have, update it
replaceChain :: MonadIO m => IORef [Block] -> [Block] -> m ()
replaceChain chainRef newChain = do
  currentChain <- liftIO $ readIORef chainRef
  if (not . isValidChain) newChain || length currentChain >= length newChain
  then 
    debugM $ "Chain is not valid for updating: " <> show newChain
  else 
    liftIO $ atomicModifyIORef' chainRef (const (newChain, newChain)) >>= debugM . show

-- ask other nodes for their chains
requestChain :: MonadIO m => LocalNode -> m ()
requestChain localNode = liftIO $ runProcess localNode $ do
  debugM "requesting chain"
  P2P.nsendPeers p2pServiceName RequestChain

-- sends the entire to all nodes in the network
-- receiving nodes should update if this chain is newer than what they have
sendChain :: MonadIO m => LocalNode -> IORef [Block] -> m ()
sendChain localNode chainRef = liftIO $ runProcess localNode $ do
  debugM "emtting chain"
  chain <- liftIO $ readIORef chainRef
  P2P.nsendPeers p2pServiceName $ ReplaceData chain

-- http endpoint
app :: SpockM () Session BlockChainState ()
app = do
  get root $ text "hschain node"

  post "block" $ do
    (BlockChainState ref localNode _) <- getState
    (blockString :: BlockArgs) <- jsonBody'
    debugM $ show blockString 

    block <- mineBlock . block $ blockString
    _ <- addBlock ref block
    chain <- getBlockChain
    debugM $ show chain
    liftIO $ runProcess  localNode $ P2P.nsendPeers p2pServiceName $ UpdateData block
    text . T.pack . pretty $ chain

  get "chain" $ do
    chain <- getBlockChain
    text . T.pack . pretty $ chain

runHsChain :: MainArgs -> IO ()
runHsChain args = do
  debugM "starting"

  (localNode, procId) <- P2P.bootstrapNonBlocking "127.0.0.1" (p2pPort args) initRemoteTable (maybeToList $ P2P.makeNodeId <$> seedNode args) (return ())
  ref <- maybe (newIORef [initialBlock]) (const $ newIORef []) (seedNode args)
  spockCfg <-  defaultSpockCfg Session PCNoDatabase (BlockChainState ref localNode procId)
  _ <- async $ runSpock (read (httpPort args) :: Int) (spock spockCfg app)

  -- wait for message to come in from the p2p network and respond to them
  runProcess localNode $ do
    getSelfPid >>= register p2pServiceName
    liftIO $ threadDelay 1000000

    if isJust $ seedNode args
    then do
      debugM "this is not the initial node, requesting a chain"
      requestChain localNode
    else debugM "this is the inital node, not requesting a chain"

    forever $ do
      message <- expect :: Process BlockUpdate
      debugM "got a message.."

      case message of
        (ReplaceData chain) -> do
          debugM $ "got some stuff to replace: " <> show chain
          replaceChain ref chain 

        (UpdateData block) -> do
          debugM $ "got some stuff to add: " <> show block
          addBlock ref block

        RequestChain       -> do
          debugM "got chain request"
          sendChain localNode ref
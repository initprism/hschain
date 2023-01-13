{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Text.Read                          (readMaybe)
import Data.ByteString.Char8              (pack)
import Control.Monad.IO.Class             (MonadIO(..))
import Crypto.Hash                        (Digest, SHA256, digestFromByteString)
import Crypto.Hash.SHA256                 (hash)
import Data.Time.Clock.POSIX              (getPOSIXTime)
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Data.Aeson
import Data.Binary

-- main data type for blockchain
data Block = Block 
  { index         :: Int
  , previousHash  :: String
  , timestamp     :: Int
  , blockData     :: String
  , nonce         :: Int
  , blockHash     :: String
  } deriving (Show, Read, Eq, Generic)

-- http params to add a block to the chain
newtype BlockArgs = BlockArgs { block :: String }
                      deriving (Show, Eq, Generic)

instance ToJSON   Block
instance FromJSON Block
instance Binary   Block
instance Out      Block
instance ToJSON   BlockArgs
instance FromJSON BlockArgs

epoch :: IO Int
epoch = round <$> getPOSIXTime

sha256 :: String -> Maybe (Digest SHA256)
sha256 = digestFromByteString . hash . pack

hashString :: String -> String
hashString = 
  maybe (error "Something went wrong generating a hash") show . sha256

calculateBlockHash :: Block -> String
calculateBlockHash (Block i p t b n _) =
    hashString $ concat [show i, p, show t, b, show n]

setBlockHash :: Block -> Block
setBlockHash block = block { blockHash = calculateBlockHash block }

setNonceAndHash :: Block -> Block
setNonceAndHash block = setBlockHash $ block { nonce = findNonce block }

difficultyTarget :: Integer
difficultyTarget = 
  0x0000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

satisfiesPow :: String -> Bool
satisfiesPow bhash =
  maybe (error $ "Something is wrong with the provided hash: " ++ bhash)
        (< difficultyTarget)
        (readMaybe ("0x" ++ bhash) :: Maybe Integer)

findNonce :: Block -> Int
findNonce block = if satisfiesPow bhash then nonce' else findNonce $ block {nonce = nonce' + 1}
  where bhash  = calculateBlockHash block
        nonce' = nonce block

initialBlock :: Block
initialBlock = setNonceAndHash block
  where block = Block 0 "0" 0 "" 0 ""

isValidNewBlock :: Block -> Block -> Bool
isValidNewBlock b1 b2
  | index b1 + 1 == index b2 &&
    blockHash b1 == previousHash b2 &&
    blockHash b2 == calculateBlockHash b2 &&
    satisfiesPow (blockHash b2) = True
  | otherwise = False


isValidChain :: [Block] -> Bool
isValidChain chain = case chain of
  []     -> True
  [x]    -> x == initialBlock
  (x:xs) -> x == initialBlock && all (uncurry isValidNewBlock) (zip chain xs)

mineBlockFrom :: MonadIO m => Block -> String -> m Block
mineBlockFrom lastblock blockdata = setNonceAndHash . initialBlock <$> liftIO epoch
  where 
    initialBlock time = Block { index        = index lastblock + 1
                              , previousHash = blockHash lastblock
                              , timestamp    = time
                              , blockData    = blockdata
                              , nonce        = 0
                              , blockHash    = "" }
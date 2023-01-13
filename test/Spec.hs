{-# LANGUAGE OverloadedStrings #-}

import Lib
import Server

import Control.Concurrent
import Control.Concurrent.Async
import Network.HTTP
import Data.Aeson                 (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, integrationTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testCase "valid chain" validChain]

integrationTests :: TestTree
integrationTests = testGroup "Integration Test" [testCase "basic chain syncing" basicSync ]

validChain :: IO ()
validChain = do
  assertBool "block eq" $ block == block
  assertBool "empty chains are valid" $ isValidChain []
  assertBool "Base chain is valid" $ isValidChain [block]
  assertBool "two init blocks are invalid" $ not $ isValidChain[block, block]
  goodBlock <- mineBlockFrom block "abcdefg"
  assertBool "actually goood chain" $ isValidChain [block, goodBlock]
  where 
    block = initialBlock 

basicSync :: IO ()
basicSync = do 
  _ <- async $ runHsChain args
  _ <- async $ runHsChain args'

  -- wait to let the servers initailze
  threadDelay 3000000
  _ <- allChainsHaveLength webPorts 1
  let blockArgs = unpack . encode $ BlockArgs "some data"

  _ <- simpleHTTP (postRequestWithBody (localhostAPI (head webPorts) "block") "application/json" blockArgs) >>=
        fmap (take 10000) . getResponseBody
  threadDelay 1000000

  _ <- allChainsHaveLength webPorts 2
  _ <- simpleHTTP (postRequestWithBody (localhostAPI (last webPorts) "block") "application/json" blockArgs)
  threadDelay 1000000

  _ <- allChainsHaveLength webPorts 3

  return ()
  where
    webPorts = ["7000", "7001"]
    p2pPorts = ["8000", "8001"]
    args     = MainArgs (head webPorts) (head p2pPorts) Nothing
    args'    = MainArgs(last webPorts) (last p2pPorts) $ Just ("127.0.0.1:" <> head p2pPorts)

localhostAPI :: String -> String -> String
localhostAPI = printf "http://127.0.0.1:%s/%s" 

allChainsHaveLength :: [String] -> Int -> IO ()
allChainsHaveLength ports len = do
  lengths <- mapM getChainsHaveLength ports
  assertBool ("all have length " <> show len) $ all (== len) lengths

getChainsHaveLength :: String -> IO Int
getChainsHaveLength serverPort = do
  body <- simpleHTTP (getRequest (localhostAPI serverPort "chain")) >>= fmap (take 10000) .getResponseBody 
  let parsed = read body :: [Block]
  print parsed
  return $ length parsed
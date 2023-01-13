{-# LANGUAGE LambdaCase #-}

module Main where

import          Server
import          System.IO                       (stdout)
import          System.Log.Formatter
import          System.Log.Handler              (setFormatter)
import          System.Log.Handler.Simple
import          System.Log.Logger
import          System.Environment              (getArgs)

main :: IO ()
main = do
  args <- getArgs >>= \case
        [h, p]    -> return $ MainArgs h p Nothing
        [h, p, s] -> return $ MainArgs h p $ Just s
        _         -> fail "Usage:\n\n$ hschain httpPort p2pPort [optional p2p address]\n\n"

  -- initialize log
  streamHandler stdout priority >>= format >>=
    \x -> fileHandler ("hschain" ++ p2pPort args ++ ".log") priority >>= format >>=
      \y -> updateGlobalLogger rootLoggerName $ setLevel priority . setHandlers [x, y]

  runHsChain args

  where priority = DEBUG
        format l = return $ setFormatter l (simpleLogFormatter "[$time : $loggername : $prio] $msg")
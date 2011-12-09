----------------------------------------------------------------------------
-- Haskell Benchmarking Client Tool for MessagePack
-- Copyright   : (c) 2011 Joerg Fritsch
--
-- License     : BSD-style
-- Maintainer  : J.Fritsch@cs.cardiff.ac.uk
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarks 1000 send/receive cycles of a string with N bytes using criterion
-- Takes the option --byte=INT to specify the number of bytes
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Network.MessagePackRpc.Client
import Control.Exception
import Control.Concurrent
import Control.Monad
import Criterion.Main
import System.Random
import System.Environment ( withArgs )
import System.Console.CmdArgs

data Strlen = Strlen {byte :: Int} deriving (Data, Typeable, Show)

strlen = cmdArgsMode $ Strlen {byte = def} &= summary "MessagePack benchmark v0.04"

ping :: RpcMethod (String -> IO String)
ping = method "ping"

sendReceive conn datastring = do
        let tryOnePing (!c, !f) i = do
            p <- ping conn datastring
            return $! case p of
              datastring -> (c+1, f)
              _          -> (c, f+1)
        (c,f) <- foldM tryOnePing (0,0) [1 .. 1000]
        return ()

main = do
  n <- cmdArgsRun strlen
  let datastring = take (byte n) $ randomRs ('a','z') (mkStdGen 3)
  putStrLn "Starting..."
  --Substitute 192.168.35.62 with the IP address of your MsgPack RPC Server
  conn <- connect "192.168.35.62" 8081
  withArgs [] $ defaultMain [bench "sendReceive" $ whnfIO (sendReceive conn datastring)]

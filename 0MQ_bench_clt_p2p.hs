----------------------------------------------------------------------------
-- Haskell Benchmarking Client Tool for 0MQ p2p messages
-- Copyright : (c) 2011 Joerg Fritsch
--
-- License : BSD-style
-- Maintainer : J.Fritsch@cs.cardiff.ac.uk
-- Stability : experimental
-- Portability : GHC
--
-- Benchmarks 1000 send/receive cycles of a string with N bytes using criterion
-- Takes the option --byte=INT to specify the number of bytes
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.ZMQ
--import Control.Monad (forM_)
--import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as B
import Criterion.Main
import Data.Foldable (foldlM)
import Text.Printf
import Control.Monad
import Control.Concurrent
import System.Random
import System.Environment ( withArgs )
import System.Console.CmdArgs


data Strlen = Strlen {byte :: Int} deriving (Data, Typeable, Show)

strlen = cmdArgsMode $ Strlen {byte = def} &= summary "0MQ Message benchmark v0.04"

sendReceive :: B.ByteString -> IO ()
sendReceive datastring = withContext 1 $ \context -> do
     withSocket context Req $ \requester -> do
        --putStrLn "Connecting ..."
        connect requester "tcp://192.168.35.69:5555"
        let tryOnePing (!c, !f) i = do
            send requester datastring []
            --putStrLn "Sent ..."
            r <- receive requester []
            --putStrLn "Received ..."
            return $ case B.unpack r of
               datastring -> (c+1, f)
               _ -> (c, f+1)
        (c,f) <- foldM tryOnePing (0,0) [1 .. 1000]
        return $! ()

main = do
     n <- cmdArgsRun strlen
     let datastring  = B.pack (take (byte n) $ randomRs ('a','z') (mkStdGen 3))
     putStrLn "Starting..."
     withArgs [] $ defaultMain [bench "sendReceive" $ whnfIO (sendReceive datastring)]

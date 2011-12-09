----------------------------------------------------------------------------
-- Haskell Benchmarking Client Tool for UDP messages (unqueued)
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

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
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

strlen = cmdArgsMode $ Strlen {byte = def} &= summary "UDP Message benchmark v0.04"

port = 1972
host = "192.168.35.69"

sendReceive :: Socket -> HostAddress -> B.ByteString -> IO ()
sendReceive s hostAddr datastring = do
        let tryOnePing (!c, !f) i = do
              sendAllTo s datastring (SockAddrInet port hostAddr)
              (r, _) <- recvFrom s 1024
              return $ case r of 
                datastring    -> (c+1, f)
                _             -> (c, f+1)
        (c,f) <- foldM tryOnePing (0,0) [1 .. 1000]
        return ()


main = withSocketsDo $ do
        n <- cmdArgsRun strlen
        let datastring = B.pack (take (byte n) $ randomRs ('a','z') (mkStdGen 3))
        putStrLn "Starting..."
        hostAddr <- inet_addr host
        s <- socket AF_INET Datagram defaultProtocol
        withArgs [] $ defaultMain [bench "sendReceive" $ whnfIO (sendReceive s hostAddr datastring)]  
        sClose s
        return()

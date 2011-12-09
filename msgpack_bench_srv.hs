----------------------------------------------------------------------------
-- Haskell Benchmarking Server Tool for MessagePack
----------------------------------------------------------------------------
-- Haskell Benchmarking Server Tool for MessagePack
-- Copyright   : (c) 2011 Joerg Fritsch
--
-- License     : BSD-style
-- Maintainer  : J.Fritsch@cs.cardiff.ac.uk
-- Stability   : experimental
-- Portability : GHC
--
-- Starts an RPC Server on tcp-8081 
-- The function ping reflects everything that it receives from the client
-----------------------------------------------------------------------------

import Network.MessagePackRpc.Server
 
ping :: String -> IO String
ping s = return $! s
 
main :: IO ()
main = do
  serve 8081 [ ("ping", fun ping) ]

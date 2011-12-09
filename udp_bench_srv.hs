----------------------------------------------------------------------------
-- Haskell Benchmarking Server Tool for UDP messages (unqueued)
-- Copyright   : (c) 2011 Joerg Fritsch
--
-- License     : BSD-style
-- Maintainer  : J.Fritsch@cs.cardiff.ac.uk
-- Stability   : experimental
-- Portability : GHC
--
-- Starts an UDP Server on tcp-1972 
-- The server reflects everything that it receives from the client
-----------------------------------------------------------------------------

import Network.Socket
import Data.Char (toUpper)

port = 1972
host = "192.168.35.69"

type Host = SockAddr

main = withSocketsDo $ do
        s <- socket AF_INET Datagram defaultProtocol
        bindAddr <- inet_addr host
        bindSocket s (SockAddrInet port bindAddr)
        let forever socket hosts = do
                (msg, host) <- receiveMessage socket
                let     maybeInsert b x xs = if b then x:xs else xs
                        hosts' = maybeInsert (host `notElem` hosts) host hosts
                sendToAll socket (map toUpper msg) hosts'
                return ()
                forever socket hosts'
        forever s []
        sClose s

receiveMessage :: Socket -> IO (String, Host)
receiveMessage socket = do
        (msg, _, hostAddr) <- recvFrom socket 1024
        return (msg, hostAddr)

sendToAll :: Socket -> String -> [Host] -> IO ()
sendToAll socket msg hosts = mapM_ (sendTo socket msg) hosts

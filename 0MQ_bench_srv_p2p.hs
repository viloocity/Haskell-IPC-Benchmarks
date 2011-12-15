----------------------------------------------------------------------------
-- Haskell Benchmarking Server Tool for 0MQ p2p messages
-- Copyright : (c) 2011 Joerg Fritsch
--
-- License : BSD-style
-- Maintainer : J.Fritsch@cs.cardiff.ac.uk
-- Stability : experimental
-- Portability : GHC
--
-- Provides a 0MQ server on port tcp-5555
-- Reflects every incoming message back to the client
-----------------------------------------------------------------------------

module Main where

import System.ZMQ
import Control.Monad (forever)

main = withContext 1 $ \context -> do  
  withSocket context Rep $ \responder -> do
    bind responder "tcp://*:5555"
    forever $ do
      message <- receive responder []
      send responder message []

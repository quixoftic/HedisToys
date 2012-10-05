{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
--
-- Module      : Main
-- Copyright   : Copyright © 2012, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- A playground for experimenting with the hedis package.
--

import Database.Redis
import System.Console.CmdArgs
import Paths_HedisTest (version)
import Data.Version (showVersion)
import Control.Monad.IO.Class
import Data.Functor
import Control.Applicative

data HedisTest = Test1 {} |
                 Test2 {}
                 deriving (Typeable, Data, Eq, Show)
                          
test1 = record Test1 {} [] += help "A simple \"hello world\" test, demonstrating how to use hedis 'get' and 'set', and the types returned by them."

test2 = record Test2 {} [] += help "Runs the 'get' part of the \"hello world\" test in a Redis transaction."

mode = cmdArgsMode_ $ modes_ [test1, test2] += help "Test the hedis package" += program "hedistest" += summary ("hedistest " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@Test1 {} -> runTest1
  opts@Test2 {} -> runTest2

runTest1 :: IO ()
runTest1 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello,world)

runTest2 :: IO ()
runTest2 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    helloworld <- multiExec $ do
      hello <- get "hello"
      world <- get "world"
      return $ (,) <$> hello <*> world
    liftIO $ print helloworld

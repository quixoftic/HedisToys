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

import qualified Data.ByteString.Char8 as BS
import Database.Redis
import System.Console.CmdArgs
import Paths_HedisTest (version)
import Data.Version (showVersion)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Control.Applicative
import Data.Maybe

data HedisTest = Test1 {} |
                 Test2 {} |
                 Test3 {} |
                 Test4 {} |
                 Test5 {} |
                 Test6 {}
                 deriving (Typeable, Data, Eq, Show)
                          
test1 = record Test1 {} [] += help "A simple \"hello world\" test, demonstrating how to use hedis 'get' and 'set', and the types returned by them."

test2 = record Test2 {} [] += help "Runs the 'get' part of the \"hello world\" test in a Redis transaction."

test3 = record Test3 {} [] += help "'get' a non-existent key."

test4 = record Test4 {} [] += help "Add a single element to a set, print the number of elements in the set. Note: not idempotent."

test5 = record Test5 {} [] += help "Same as test 4, but without 'do' notation."

test6 = record Test6 {} [] += help "Atomic test-and-set for getting/creating new keys."

mode = cmdArgsMode_ $ modes_ [test1, test2, test3, test4, test5, test6] += help "Test the hedis package" += program "hedistest" += summary ("hedistest " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@Test1 {} -> runTest1
  opts@Test2 {} -> runTest2
  opts@Test3 {} -> runTest3
  opts@Test4 {} -> runTest4 >>= print
  opts@Test5 {} -> runTest5 >>= print
  opts@Test6 {} -> runTest6 >>= print

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

runTest3 :: IO ()
runTest3 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    foobar <- get "foobar"
    liftIO $ print foobar

-- The remaining tests demonstrate how to return values wrapped in IO.
--

runTest4 :: IO (Integer)
runTest4 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right result <- sadd "set1" ["a"]
    return result

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Left"

-- Same as runTest4, but without 'do' notation. It's for the reader to
-- judge whether it's "better" this way.
runTest5 :: IO (Integer)
runTest5 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ liftM fromRight $ sadd "set1" ["a"]

valueToInteger :: BS.ByteString -> Integer
valueToInteger = fst . fromJust . BS.readInteger

integerToValue :: Integer -> BS.ByteString
integerToValue = BS.pack . show

-- Return the ID as an Integer.
runTest6 :: IO (Integer)
runTest6 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right maybeID <- get "key1"
    case maybeID of
      Just id -> return $ valueToInteger id
      Nothing -> do
        Right newID <- incr "next.id"
        Right reply <- setnx "key1" $ integerToValue newID
        if reply
          then return newID
          else do Right (Just id) <- get "key1"
                  return $ valueToInteger id

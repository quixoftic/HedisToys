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
import Paths_HedisToys (version)
import Data.Version (showVersion)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Control.Applicative
import Data.Maybe

data HedisToys = HelloWorld {} |
                 HelloWorldTx {} |
                 Get {} |
                 Sadd {} |
                 SaddPrime {} |
                 TestAndSet {}
                 deriving (Typeable, Data, Eq, Show)
                          
helloWorld = record HelloWorld {} [] += help "A simple \"hello world\" toy, demonstrating how to use hedis 'get' and 'set', and the types returned by them."

helloWorldTx = record HelloWorldTx {} [] += help "Runs the 'get' part of the \"hello world\" toy in a Redis transaction."

get_ = record Get {} [] += help "'get' a non-existent key."

sadd_ = record Sadd {} [] += help "Add a single element to a set, print the number of elements in the set. Note: not idempotent."

saddPrime = record SaddPrime {} [] += help "Same as toy 4, but without 'do' notation."

testAndSet = record TestAndSet {} [] += help "Atomic test-and-set for getting/creating new keys."

mode = cmdArgsMode_ $ modes_ [helloWorld, helloWorldTx, get_, sadd_, saddPrime, testAndSet] += help "Experiments with the hedis package" += program "hedistoys" += summary ("hedistoys " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@HelloWorld {} -> runHelloWorld
  opts@HelloWorldTx {} -> runHelloWorldTx
  opts@Get {} -> runGet
  opts@Sadd {} -> runSadd >>= print
  opts@SaddPrime {} -> runSaddPrime >>= print
  opts@TestAndSet {} -> runTestAndSet >>= print

runHelloWorld :: IO ()
runHelloWorld = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello,world)

runHelloWorldTx :: IO ()
runHelloWorldTx = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    helloworld <- multiExec $ do
      hello <- get "hello"
      world <- get "world"
      return $ (,) <$> hello <*> world
    liftIO $ print helloworld

runGet :: IO ()
runGet = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    foobar <- get "foobar"
    liftIO $ print foobar

-- The remaining toys demonstrate how to return values wrapped in IO.
--

runSadd :: IO (Integer)
runSadd = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right result <- sadd "set1" ["a"]
    return result

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Left"

-- Same as runSadd, but without 'do' notation. It's for the reader to
-- judge whether it's "better" this way.
runSaddPrime :: IO (Integer)
runSaddPrime = do
  conn <- connect defaultConnectInfo
  runRedis conn $ liftM fromRight $ sadd "set1" ["a"]

valueToInteger :: BS.ByteString -> Integer
valueToInteger = fst . fromJust . BS.readInteger

integerToValue :: Integer -> BS.ByteString
integerToValue = BS.pack . show

-- Return the ID as an Integer.
runTestAndSet :: IO (Integer)
runTestAndSet = do
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

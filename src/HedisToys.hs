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

data HedisToys = Toy1 {} |
                 Toy2 {} |
                 Toy3 {} |
                 Toy4 {} |
                 Toy5 {} |
                 Toy6 {}
                 deriving (Typeable, Data, Eq, Show)
                          
toy1 = record Toy1 {} [] += help "A simple \"hello world\" toy, demonstrating how to use hedis 'get' and 'set', and the types returned by them."

toy2 = record Toy2 {} [] += help "Runs the 'get' part of the \"hello world\" toy in a Redis transaction."

toy3 = record Toy3 {} [] += help "'get' a non-existent key."

toy4 = record Toy4 {} [] += help "Add a single element to a set, print the number of elements in the set. Note: not idempotent."

toy5 = record Toy5 {} [] += help "Same as toy 4, but without 'do' notation."

toy6 = record Toy6 {} [] += help "Atomic test-and-set for getting/creating new keys."

mode = cmdArgsMode_ $ modes_ [toy1, toy2, toy3, toy4, toy5, toy6] += help "Experiments with the hedis package" += program "hedistoys" += summary ("hedistoys " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@Toy1 {} -> runToy1
  opts@Toy2 {} -> runToy2
  opts@Toy3 {} -> runToy3
  opts@Toy4 {} -> runToy4 >>= print
  opts@Toy5 {} -> runToy5 >>= print
  opts@Toy6 {} -> runToy6 >>= print

runToy1 :: IO ()
runToy1 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello,world)

runToy2 :: IO ()
runToy2 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    helloworld <- multiExec $ do
      hello <- get "hello"
      world <- get "world"
      return $ (,) <$> hello <*> world
    liftIO $ print helloworld

runToy3 :: IO ()
runToy3 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    foobar <- get "foobar"
    liftIO $ print foobar

-- The remaining toys demonstrate how to return values wrapped in IO.
--

runToy4 :: IO (Integer)
runToy4 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right result <- sadd "set1" ["a"]
    return result

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Left"

-- Same as runToy4, but without 'do' notation. It's for the reader to
-- judge whether it's "better" this way.
runToy5 :: IO (Integer)
runToy5 = do
  conn <- connect defaultConnectInfo
  runRedis conn $ liftM fromRight $ sadd "set1" ["a"]

valueToInteger :: BS.ByteString -> Integer
valueToInteger = fst . fromJust . BS.readInteger

integerToValue :: Integer -> BS.ByteString
integerToValue = BS.pack . show

-- Return the ID as an Integer.
runToy6 :: IO (Integer)
runToy6 = do
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

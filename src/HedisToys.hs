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

data HedisToys = HelloWorld {} 
               | HelloWorldTx {}
               | Get { key :: String }
               | Set_ { key :: String, val :: String }
               | Sadd { key :: String, vals :: [String] }
               | SaddPrime { key :: String, vals :: [String] }
               | Smembers { key :: String }
               | TestAndSet { key :: String, nextIDKey :: String }
               | TestAndSetPrime { key :: String, nextIDKey :: String }
               deriving (Typeable, Data, Eq, Show)
                          
helloWorld = record HelloWorld {} [] += help "A simple \"hello world\" toy, demonstrating how to use hedis 'get' and 'set', and the types returned by them."

helloWorldTx = record HelloWorldTx {} [] += help "Runs the 'get' part of the \"hello world\" toy in a Redis transaction."

get_ = record Get { key = def } [ key := def += argPos 0 += typ "KEYNAME"
                                ] += help "'get' the value of the provided key."

set_ = record Set_ { key = def 
                   , val = def } [ key := def += argPos 0 += typ "KEYNAME"
                                 , val := def += argPos 1 += typ "VALUE"
                                 ] += help "'set' the value of the provided key."

sadd_ = record Sadd { key = def
                    , vals = def } [ key := def += argPos 0 += typ "KEYNAME"
                                   , vals := def += args += typ "VALUE ..."
                                   ] += help "Add one or more elements to a set, print the number of elements that were added to the set."

smembers_ = record Smembers { key = def } [ key := def += argPos 0 += typ "KEYNAME"
                                          ] += help "Get the members of the given set."

saddPrime = record SaddPrime { key = def
                             , vals = def } [ key := def += argPos 0 += typ "KEYNAME"
                                            , vals := def += args += typ "VALUE ..."
                                            ] += help "Same as the sadd toy, but implemented without 'do' notation."                                              

testAndSet = record TestAndSet { key = def 
                               , nextIDKey = def } [ key := def += argPos 0 += typ "KEYNAME" 
                                                     , nextIDKey := def += argPos 1 += typ "IDKEYNAME"
                                                     ] += help "Atomic test-and-set for getting/creating keys with unique, monotonically increasing integer values. The next new key ID is stored in the key with name IDKEYNAME."

testAndSetPrime = record TestAndSetPrime { key = def
                                         , nextIDKey = def } [ key := def += argPos 0 += typ "KEYNAME"
                                                             , nextIDKey := def += argPos 1 += typ "IDKEYNAME"
                                                             ] += help "Same as the testandset toy, but implemented independently of a particular RedisCtx context."

mode = cmdArgsMode_ $ modes_ [helloWorld, helloWorldTx, get_, set_, sadd_, saddPrime, smembers_, testAndSet, testAndSetPrime] += help "Experiments with the hedis package" += program "hedistoys" += summary ("hedistoys " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@HelloWorld {} -> runHelloWorld
  opts@HelloWorldTx {} -> runHelloWorldTx
  opts@Get {} -> runGet $ BS.pack $ key opts
  opts@Set_ {} -> runSet (BS.pack $ key opts) (BS.pack $ val opts)
  opts@Sadd {} -> runSadd (BS.pack $ key opts) (map BS.pack $ vals opts) >>= print
  opts@SaddPrime {} -> runSaddPrime (BS.pack $ key opts) (map BS.pack $ vals opts) >>= print
  opts@Smembers {} -> runSmembers (BS.pack $ key opts) >>= print
  opts@TestAndSet {} -> runTestAndSet (BS.pack $ key opts) (BS.pack $ nextIDKey opts) >>= print
  opts@TestAndSetPrime {} -> do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
      id <- runTestAndSetPrime (BS.pack $ key opts) (BS.pack $ nextIDKey opts)
      liftIO $ print id

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

runGet :: BS.ByteString -> IO ()
runGet key = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    val <- get key
    liftIO $ print val

runSet :: BS.ByteString -> BS.ByteString -> IO ()
runSet key val = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    status <- set key val
    liftIO $ print status

-- The remaining toys demonstrate how to return values wrapped in IO.
--

runSadd :: BS.ByteString -> [BS.ByteString] -> IO (Integer)
runSadd key vals = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right result <- sadd key vals
    return result

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Left"

-- Same as runSadd, but without 'do' notation. It's for the reader to
-- judge whether it's "better" this way.
runSaddPrime :: BS.ByteString -> [BS.ByteString] -> IO (Integer)
runSaddPrime key vals = do
  conn <- connect defaultConnectInfo
  runRedis conn $ liftM fromRight $ sadd key vals

runSmembers :: BS.ByteString -> IO ([BS.ByteString])
runSmembers key = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right vals <- smembers key
    return vals

valueToInteger :: BS.ByteString -> Integer
valueToInteger = fst . fromJust . BS.readInteger

integerToValue :: Integer -> BS.ByteString
integerToValue = BS.pack . show

-- Return the ID as an Integer.
runTestAndSet :: BS.ByteString -> BS.ByteString -> IO (Integer)
runTestAndSet key nextIDKey = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right maybeID <- get key
    case maybeID of
      Just id -> return $ valueToInteger id
      Nothing -> do
        Right newID <- incr nextIDKey
        Right reply <- setnx key $ integerToValue newID
        if reply
          then return newID
          else do Right (Just id) <- get key
                  return $ valueToInteger id

runTestAndSetPrime :: BS.ByteString -> BS.ByteString -> Redis (Integer)
runTestAndSetPrime key nextIDKey = do
  Right maybeID <- get key
  case maybeID of
    Just id -> return $ valueToInteger id
    Nothing -> do
      Right newID <- incr nextIDKey
      Right reply <- setnx key $ integerToValue newID
      if reply
        then return newID
        else do Right (Just id) <- get key
                return $ valueToInteger id

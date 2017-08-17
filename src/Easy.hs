{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-

Modification of EasyTest from:
https://github.com/unisonweb/unison/blob/3e352eac03427d02b54cab3675054a34c69bb25f/yaks/easytest/src/EasyTest.hs

MIT-licensed code; sublicensing as BSD3

Original license:

Copyright (c) 2013, Paul Chiusano and contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

-}

module Easy
  ( module Control.Monad.Managed
  , module Control.Monad
  , module Control.Applicative

  , seq, deepseq
  , Async, Sync
  , Test

  , io
  , wrap

  , fork, fork'

  , scope

  , note, note', noteScoped, notep
  , binary, hex, octal

  , run, runOnly, rerunOnly, tests

  , r
  , (<..>)
  , pick
  , list, lists
  , pair

  , ok
  , skip
  , crash
  -- , fail
  , expect, expectJust, expectRight

  , completed
  , currentScope
  )
  where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Hashable
import           Data.List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid
import           Data.Word
import           GHC.Stack
import           System.Directory
import           System.Exit
import           System.IO
import           System.Random            (Random)
import qualified System.Random            as Random

import           Control.DeepSeq

import           Control.Monad.Managed

import           Data.Char
import           Numeric

import           Simple.Internal.Pretty

data Status = Failed | Passed !Int | Skipped | Completed

combineStatus :: Status -> Status -> Status
combineStatus Skipped s             = s
combineStatus s Skipped             = s
combineStatus Failed _              = Failed
combineStatus _ Failed              = Failed
combineStatus (Passed n) (Passed m) = Passed (n + m)

data Env =
  Env { rng      :: TVar Random.StdGen
      , messages :: String
      , results  :: TBQueue (Maybe (TMVar (String, Status)))
      , noter    :: String -> IO ()
      , allow    :: String }

data Async
data Sync

newtype Test sync a = Test (ReaderT Env IO (Maybe a))

{-# INLINE io #-}
io :: IO a -> Test sync a
io = liftIO

{-# INLINE atomicLogger #-}
atomicLogger :: IO (String -> IO ())
atomicLogger = do
  lock <- newMVar ()
  pure $ \msg ->
    -- force msg before acquiring lock
    let dummy = foldl' (\_ ch -> ch == 'a') True msg
    in dummy `seq` bracket (takeMVar lock) (\_ -> putMVar lock ()) (\_ -> putStrLn msg)

{-# INLINE expect #-}
expect :: HasCallStack => Bool -> Test sync ()
expect False = crash "unexpected"
expect True  = ok

{-# INLINE expectJust #-}
expectJust :: HasCallStack => Maybe a -> Test sync a
expectJust Nothing  = crash "expected Just, got Nothing"
expectJust (Just a) = ok >> pure a

{-# INLINE expectRight #-}
expectRight :: HasCallStack => Either e a -> Test sync a
expectRight (Left _)  = crash "expected Right, got Left"
expectRight (Right a) = ok >> pure a

{-# INLINE tests #-}
tests :: [Test sync ()] -> Test sync ()
tests = msum

{-# INLINE runOnly #-}
-- | Run all tests whose scope starts with the given prefix
runOnly :: String -> Test sync a -> IO (Maybe a)
runOnly prefix t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  run' seed logger prefix t

{-# INLINE rerunOnly #-}
-- | Run all tests with the given seed and whose scope starts with the given prefix
rerunOnly :: Int -> String -> Test sync a -> IO (Maybe a)
rerunOnly seed prefix t = do
  logger <- atomicLogger
  run' seed logger prefix t

{-# INLINE run #-}
run :: Test sync a -> IO (Maybe a)
run = runOnly ""

{-# INLINE rerun #-}
rerun :: Int -> Test sync a -> IO (Maybe a)
rerun seed = rerunOnly seed []

{-# INLINE run' #-}
run' :: Int -> (String -> IO ()) -> String -> Test sync a -> IO (Maybe a)
run' seed note allow (Test t) = do
  let !rng = Random.mkStdGen seed
  resultsQ <- atomically (newTBQueue 50)
  rngVar <- newTVarIO rng
  note $ "Randomness seed for this run is " ++ show seed ++ ""
  results <- atomically $ newTVar Map.empty
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTBQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    atomically $ modifyTVar results (Map.insertWith combineStatus msgs passed)
    resultsMap <- readTVarIO results
    case Map.findWithDefault Skipped msgs resultsMap of
      Skipped -> pure ()
      Passed n -> note $ "OK " ++ (if n <= 1 then msgs else "(" ++ show n ++ ") " ++ msgs)
      Failed -> note $ "FAILED " ++ msgs
      Completed -> note $ "COMPLETED"
  let line = "------------------------------------------------------------"
  note "Raw test output to follow ... "
  note line
  e <- try (runReaderT t (Env rngVar [] resultsQ note allow))
  let finish = do
        atomically $ writeTBQueue resultsQ Nothing
        _ <- A.waitCatch rs
        resultsMap <- readTVarIO results
        let
          resultsList = Map.toList resultsMap
          succeededList = [ n | (_, Passed n) <- resultsList ]
          succeeded = length succeededList
          -- totalTestCases = foldl' (+) 0 succeededList
          failures = [ a | (a, Failed) <- resultsList ]
          failed = length failures
        case failures of
          [] -> do
            note line
            case succeeded of
              0 -> do
                note "😶  hmm ... no test results recorded"
                note "Tip: use `ok`, `expect`, or `crash` to record results"
                note "Tip: if running via `runOnly` or `rerunOnly`, check for typos"
              1 -> note $ "✅  1 test passed, no failures! 👍 🎉"
              _ -> note $ "✅  " ++ show succeeded ++ " tests passed, no failures! 👍 🎉"
          (hd:_) -> do
            note line
            note "\n"
            note $ "  " ++ show succeeded ++ (if failed == 0 then " PASSED" else " passed")
            note $ "  " ++ show (length failures) ++ (if failed == 0 then " failed" else " FAILED (failed scopes below)")
            note $ "    " ++ intercalate "\n    " (map show failures)
            note ""
            note $ "  To rerun with same random seed:\n"
            note $ "    rerun " ++ show seed
            note $ "    rerunOnly " ++ show seed ++ " " ++ "\"" ++ hd ++ "\""
            note "\n"
            note line
            note "❌"
            exitWith (ExitFailure 1)
  case e of
    Left (e :: SomeException) -> do
      note $ "Exception while running tests: " ++ show e
      finish
      return Nothing
    Right a -> do
      finish
      return a

{-# INLINE scope #-}
-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test sync a -> Test sync a
scope msg (Test t) = Test $ do
  env <- ask
  let messages' = case messages env of [] -> msg; ms -> ms ++ ('.':msg)
  case (null (allow env) || take (length (allow env)) msg `isPrefixOf` allow env) of
    False -> putResult Skipped >> pure Nothing
    True -> liftIO $ runReaderT t (env { messages = messages', allow = drop (length msg + 1) (allow env) })

{-# INLINE currentScope #-}
-- | The current scope
currentScope :: Test sync String
currentScope = asks messages

{-# INLINE noteScoped #-}
-- | Prepend the current scope to a logging message
noteScoped :: String -> Test sync ()
noteScoped msg = do
  s <- currentScope
  note (s ++ (if null s then "" else " ") ++ msg)

{-# INLINE note #-}
-- | Log a message
note :: String -> Test sync ()
note msg = do
  noter <- asks noter
  liftIO $ do
    noter msg
    yield
  pure ()

{-# INLINE note' #-}
-- | Log a showable value
note' :: Show s => s -> Test sync ()
note' = note . show

{-# INLINE notep #-}
-- | Log a pretty-able value
notep :: Pretty p => p -> Test sync ()
notep = note . pretty

{-# INLINE binary #-}
binary :: Integral i => i -> String
binary i = "0b" <> showIntAtBase 2 intToDigit (fromIntegral i) []

{-# INLINE hex #-}
hex :: Integral i => i -> String
hex i = "0x" <> (showHex @ Int) (fromIntegral i) []

{-# INLINE octal #-}
octal :: Integral i => i -> String
octal i = "0o" <> (showOct @ Int) (fromIntegral i) []

-- | Generate a random value
--
-- Use TypeApplication to monomorphize.
--
-- > count <- r @ Int
--
{-# INLINE r #-}
r :: Random a => Test sync a
r = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.random rng0
    writeTVar rng rng1
    pure a

-- | Generate a bounded random value. Inclusive on both sides.
--
-- Use TypeApplication to monomorphize.
--
-- > roll <- 1 <..> 6 @ Int
--
{-# INLINE (<..>) #-}
(<..>) :: Random a => a -> a -> Test sync a
(<..>) lower upper = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.randomR (lower,upper) rng0
    writeTVar rng rng1
    pure a
infix 1 <..>

{-# INLINE pick #-}
-- | Sample uniformly from the given list of possibilities
pick :: forall sync a. [a] -> Test sync a
pick as = let n = length as; ind = picker n as in do
  i <- 0 <..> n - 1
  Just a <- pure (ind i)
  pure a

{-# INLINE picker #-}
picker :: Int -> [a] -> (Int -> Maybe a)
picker _ [] = const Nothing
picker _ [a] = \i -> if i == 0 then Just a else Nothing
picker size as = go where
  lsize = size `div` 2
  rsize = size - lsize
  (l,r) = splitAt lsize as
  lpicker = picker lsize l
  rpicker = picker rsize r
  go i = if i < lsize then lpicker i else rpicker (i - lsize)

-- | Alias for `replicateM`
{-# INLINE list #-}
list :: Int -> Test sync a -> Test sync [a]
list i f = replicateM i f

{-# INLINE lists #-}
lists :: (Traversable f) => f Int -> (Int -> Test sync a) -> Test sync (f [a])
lists sizes gen = sizes `forM` \i -> list i (gen i)

-- | Alias for `liftA2 (,)`.
{-# INLINE pair #-}
pair :: Test sync a -> Test sync b -> Test sync (a,b)
pair = liftA2 (,)

-- | Catch all exceptions that could occur in the given `Test`
{-# INLINE wrap #-}
wrap :: Test sync a -> Test sync a
wrap (Test t) = Test $ do
  env <- ask
  lift $ runWrap env t

{-# INLINE runWrap #-}
runWrap :: Env -> ReaderT Env IO (Maybe a) -> IO (Maybe a)
runWrap env t = do
  e <- try $ runReaderT t env
  case e of
    Left e -> do
      noter env (messages env ++ " EXCEPTION: " ++ show (e :: SomeException))
      runReaderT (putResult Failed) env
      pure Nothing
    Right a -> pure a

-- | Record a successful test at the current scope
{-# INLINE ok #-}
ok :: Test sync ()
ok = Test (Just <$> putResult (Passed 1))

-- | Explicitly skip this test
{-# INLINE skip #-}
skip :: Test sync ()
skip = Test (Nothing <$ putResult Skipped)

-- | Record a failure at the current scope
{-# INLINE crash #-}
crash :: HasCallStack => String -> Test sync a
crash msg = do
  let trace = callStack
      msg' = msg ++ " " ++ prettyCallStack trace
  Test (Just <$> putResult Failed) >> note ("FAILURE " ++ msg') >> Test (pure Nothing)

{-# INLINE completed #-}
completed :: Test sync ()
completed = Test (Just <$> putResult Completed)

{-# INLINE putResult #-}
putResult :: Status -> ReaderT Env IO ()
putResult passed = do
  msgs <- asks messages
  allow <- asks (null . allow)
  r <- liftIO . atomically $ newTMVar (msgs, if allow then passed else Skipped)
  q <- asks results
  lift . atomically $ writeTBQueue q (Just r)

instance MonadReader Env (Test sync) where
  ask = Test $ do
    allow <- asks (null . allow)
    case allow of
      True  -> Just <$> ask
      False -> pure Nothing
  local f (Test t) = Test (local f t)
  reader f = Test (Just <$> reader f)

instance Monad (Test sync) where
  fail = crash
  return a = Test $ do
    allow <- asks (null . allow)
    pure $ case allow of
      True  -> Just a
      False -> Nothing
  Test a >>= f = Test $ do
    a <- a
    case a of
      Nothing -> pure Nothing
      Just a  -> let Test t = f a in t

instance Functor (Test sync) where
  fmap = liftM

instance Applicative (Test sync) where
  pure = return
  (<*>) = ap

instance MonadIO (Test sync) where
  liftIO io = do
    s <- asks (null . allow)
    case s of
      True  -> wrap $ Test (Just <$> liftIO io)
      False -> Test (pure Nothing)

instance Alternative (Test sync) where
  empty = Test (pure Nothing)
  Test t1 <|> Test t2 = Test $ do
    env <- ask
    (rng1, rng2) <- liftIO . atomically $ do
      currentRng <- readTVar (rng env)
      let (rng1, rng2) = Random.split currentRng
      (,) <$> newTVar rng1 <*> newTVar rng2
    lift $ do
      _ <- runWrap (env { rng = rng1 }) t1
      runWrap (env { rng = rng2 }) t2

instance MonadPlus (Test sync) where
  mzero = empty
  mplus = (<|>)

{-# INLINE fork #-}
-- | Run a test in a separate thread, not blocking for its result.
fork :: Test Async a -> Test Async ()
fork t = void (fork' t)

{-# INLINE fork' #-}
-- | Run a test in a separate thread, return a future which can be used
-- to block on its result.
fork' :: Test Async a -> Test Async (Test Async a)
fork' (Test t) = do
  env <- ask
  tmvar <- liftIO newEmptyTMVarIO
  liftIO . atomically $ writeTBQueue (results env) (Just tmvar)
  r <- liftIO . A.async $ runWrap env t
  waiter <- liftIO . A.async $ do
    e <- A.waitCatch r
    _ <- atomically $ tryPutTMVar tmvar (messages env, Skipped)
    case e of
      Left _  -> pure Nothing
      Right a -> pure a
  pure $ do
    a <- liftIO (A.wait waiter)
    case a of Nothing -> empty
              Just a  -> pure a

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Read a value from a file that was created
-- with the a call to `writeValue` with the given key in this
-- test scope.
{-# INLINE readValue #-}
readValue :: Read a => String -> Test Sync (Maybe a)
readValue k = do
    cs <- currentScope
    let valueFile = "trivial/tests/" <> show (abs $ hash cs + hash k) <> ".value"
    exists <- io $ doesFileExist valueFile
    if exists
        then io $ do
            h <- openFile valueFile ReadMode
            oldResults <- read <$> hGetContents h
            oldResults `seq` hClose h
            return (Just oldResults)
        else
            return Nothing

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Persist a value in a file that will only be
-- readable by calling `readValue` with the key given
-- to `writeValue` and in this benchmark scope.
{-# INLINE writeValue #-}
writeValue :: Show a => String -> a -> Test Sync ()
writeValue k v = do
    cs <- currentScope
    let dir = "trivial/tests/"
    io $ createDirectoryIfMissing True dir
    io $ writeFile (dir <> show (abs $ hash cs + hash k) <> ".value") (show v)

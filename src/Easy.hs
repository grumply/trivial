{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

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

  , note, note', noteScoped
  , binary, hex, octal

  , run, runOnly, rerunOnly

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
runOnly :: String -> Test sync a -> IO ()
runOnly prefix t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  run' seed logger prefix t

{-# INLINE rerunOnly #-}
-- | Run all tests with the given seed and whose scope starts with the given prefix
rerunOnly :: Int -> String -> Test sync a -> IO ()
rerunOnly seed prefix t = do
  logger <- atomicLogger
  run' seed logger prefix t

{-# INLINE run #-}
run :: Test sync a -> IO ()
run = runOnly ""

{-# INLINE rerun #-}
rerun :: Int -> Test sync a -> IO ()
rerun seed = rerunOnly seed []

{-# INLINE run' #-}
run' :: Int -> (String -> IO ()) -> String -> Test sync a -> IO ()
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
  e <- try (runReaderT (void t) (Env rngVar [] resultsQ note allow)) :: IO (Either SomeException ())
  case e of
    Left e -> note $ "Exception while running tests: " ++ show e
    Right () -> pure ()
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
      note $ "    EasyTest.rerun " ++ show seed
      note $ "    EasyTest.rerunOnly " ++ show seed ++ " " ++ "\"" ++ hd ++ "\""
      note "\n"
      note line
      note "❌"
      exitWith (ExitFailure 1)

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
  liftIO $ noter msg
  pure ()

{-# INLINE note' #-}
-- | Log a showable value
note' :: Show s => s -> Test sync ()
note' = note . show

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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Easy where

-- Note: to use this module, you must run your program with the -T rtsopt
-- ghc --make -O2 -rtsopts A.hs && ./A +RTS -T
import Easy

import Control.DeepSeq
import System.Directory

import Data.Hashable
import Data.Int
import Data.List
import Data.Monoid

import System.CPUTime
import System.IO

import GHC.Generics hiding (D)
import GHC.Stats
import GHC.Stack
import System.Mem

import Control.Concurrent
import Easy
import Text.Printf

import Data.Aeson

_GCStats_size = 152

{-# INLINE bench #-}
bench :: NFData a => String -> IO a -> ((BenchResult,a) -> Test Sync b) -> Test Sync b
bench nm f w =
    scope nm $ do
        noteScoped "Running benchmark"
        io yield  -- yield for printer to run
        io performGC -- cleanup when we get back
        (before, after, a) <-
            io $ do
                performGC
                !before <- getGCStats
                r <- f
                let !a = force r
                performGC
                !after <- getGCStats
                return (before, after, a)
        cs <- currentScope
        let results = calculate cs before after
        w (results,a)

-- | Given a scope, a list of BenchPreds partially applied to a base, a test to
-- benchmark, and a callback to run if a benchmark is persisted to disk
-- (either first run or all predicates succeed), run an automatic benchmark.
autobench :: NFData a => String -> [BenchResult -> BenchResult -> Bool] -> IO a -> ((BenchResult,a) -> Test Sync b) -> Test Sync b
autobench nm ps f w = do
    bench nm f $ \(new,a) -> do
        mayOld <- revealBench
        case mayOld of
          Nothing -> do
            noteScoped "Autobench writing first results to disk."
            persistBench new
          Just old -> do
            mapM_ (\f -> expect (f old new)) ps
            noteScoped "Autobench writing improved results to disk."
            persistBench new
        w (new,a)

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Read a list of bench results from a file that were created
-- with the a call to `writeBenches` with the given key.
{-# INLINE readBenches #-}
readBenches :: String -> Test Sync [BenchResult]
readBenches k = do
    cs <- currentScope
    let benchFile = "trivial/benchmarks/" <> show (abs $ hash cs + hash k) <> ".bench"
    exists <- io $ doesFileExist benchFile
    if exists
        then io $ do
            h <- openFile benchFile ReadMode
            oldResults <- read <$> hGetContents h
            oldResults `seq` hClose h
            return oldResults
        else
            return []

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Persist a list of bench results to a file that will only be
-- readable by calling `readBenches` with the key given to `writeBenches`
-- in this benchmark context.
{-# INLINE writeBenches #-}
writeBenches :: String -> [BenchResult] -> Test Sync ()
writeBenches k brs = do
    cs <- currentScope
    let dir = "trivial/benchmarks/"
    io $ createDirectoryIfMissing True dir
    io $ writeFile (dir <> show (abs $ hash cs + hash k) <> ".bench") (show brs)

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Read a bench file that was saved in the current scope with `writeBench`.
{-# INLINE revealBench #-}
revealBench :: Test Sync (Maybe BenchResult)
revealBench = do
    cs <- currentScope
    let benchFile = "trivial/benchmarks/" <> show (abs $ hash cs) <> ".bench"
    exists <- io $ doesFileExist benchFile
    if exists
        then io $ do
            h <- openFile benchFile ReadMode
            oldResult <- read <$> hGetContents h
            oldResult `seq` hClose h
            return (Just oldResult)
        else
            return Nothing

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Persist a bench file that will only be available in this scope with `readBench`.
{-# INLINE persistBench #-}
persistBench :: BenchResult -> Test Sync ()
persistBench br = do
    cs <- currentScope
    let dir = "trivial/benchmarks/"
    io $ createDirectoryIfMissing True dir
    io $ writeFile (dir <> show (abs $ hash cs)) (show br)

-- | Check if two reals are of similar magnitude given a scaling base `b`.
--
-- In general, as x grows, relative epsilon shrinks as a factor of log_b x.
--
-- `exp 1`, exported as `e`, is a good default base when developing in general. As optimization
-- becomes a focus, shrink `b` towards 1.
--
-- Note: For b < 1, similar b _ _ -> False
--
{-# INLINE similar #-}
similar :: (Real a,Real b) => a -> b -> b -> Bool
similar (realToFrac -> b) (realToFrac -> x) (realToFrac -> y) =
    -- As b ↘ 1, epsilon ↘ 0
    let epsilon = (x :: Double) / (exp 1 * logBase b (x + 1))
        (lo,hi) = (x - epsilon,x + epsilon)
    in lo <= y && y <= hi

{-# INLINE dissimilar #-}
dissimilar :: (Real a, Real b) => a -> b -> b -> Bool
dissimilar b x y = not (similar b x y)

{-# INLINE e #-}
e :: Floating a => a
e = exp 1

class Pretty a where
  pretty :: a -> String

instance Pretty [Char] where
  pretty = id

-- Store duration as microseconds, not fractional seconds!
newtype Duration = Duration { micros :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Duration where
    pretty (Duration micros)
        | micros < 1          = printf "%.2fns" (micros * 1000)
        | micros < 1000       = printf "%.2fμs" micros
        | micros < 1000000    = printf "%.2fms" (micros / 1000)
        | micros < 60000000   = printf "%.2fs" (micros / 1000000)
        | micros < 60000000^2 = printf "%dm %ds" (micros / 60000000) ((round micros :: Integer) `mod` 60000000)
        | otherwise           = printf "%dh %dm" (micros / 60000000^2) ((round micros :: Integer) `mod` 60000000^2)

dFromS :: Double -> Duration
dFromS = Duration . (1000000 *)

pattern Minutes :: Double -> Duration
pattern Minutes s <- Duration ((/60000000) -> s) where
  Minutes s = Duration (s * 60000000)

pattern Seconds :: Double -> Duration
pattern Seconds s <- Duration ((/1000000) -> s) where
  Seconds s = Duration (s * 1000000)

pattern Millis :: Double -> Duration
pattern Millis s <- Duration ((/1000) -> s) where
  Millis s = Duration (s * 1000)

pattern Micros :: Double -> Duration
pattern Micros s <- Duration s where
  Micros s = Duration s

pattern Nanos :: Double -> Duration
pattern Nanos s <- Duration ((* 1000) -> s) where
  Nanos s = Duration (s / 1000)

newtype Bytes = Bytes { getBytes :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)
instance Pretty Bytes where
    pretty (Bytes b)
        | b < 2^10  = printf "%d B/s"  b
        | b < 2^20  = printf "%d KB/s" (b `div` 2^10)
        | b < 2^30  = printf "%d MB/s" (b `div` 2^20)
        | otherwise = printf "%d GB/s" (b `div` 2^30)

newtype Throughput = Throughput { getThroughput :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Throughput where
    pretty (Throughput t)
        | t < 2^10  = printf "%d B" t
        | t < 2^20  = printf "%d KB" (round t `div` 2^10 :: Int64)
        | t < 2^30  = printf "%d MB" (round t `div` 2^20 :: Int64)
        | t < 2^40  = printf "%d GB" (round t `div` 2^30 :: Int64)
        | otherwise = printf "%d TB" (round t `div` 2^40 :: Int64)

newtype Count = Count { getCount :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)
instance Pretty Count where
  pretty (Count c) = show c

newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Percent where
  pretty (Percent p) = printf "%.2f%%" p
percent :: Double -> Percent
percent = Percent . (*100)

newtype Factor = Factor { getFactor :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Factor where
  pretty (Factor f) = printf "%.2fx" f

{-# INLINE calculate #-}
calculate :: String -> GCStats -> GCStats -> BenchResult
calculate label before after =
    let cpuElapsed = dFromS $ wallSeconds after - wallSeconds before
        cpuTime    = dFromS $ cpuSeconds after - cpuSeconds before

        cpu =
          let elapsed = cpuElapsed
              time    = cpuTime
              factor  = realToFrac time / realToFrac elapsed
          in CPU {..}

        gc =
          let elapsed = dFromS $ gcWallSeconds after - gcWallSeconds before
              time    = dFromS $ gcCpuSeconds after - gcCpuSeconds before
              factor  = realToFrac time / realToFrac elapsed
              effect  = realToFrac cpuElapsed / realToFrac elapsed
              burden  = realToFrac cpuTime / realToFrac time
              bytes   = Bytes $ bytesCopied after - bytesCopied before
              rate    = realToFrac bytes / realToFrac (elapsed / 1000000)
              work    = realToFrac bytes / realToFrac (time / 1000000)
              colls   = Count $ numGcs after - numGcs before - 1
              live    = Bytes $ currentBytesUsed after - currentBytesUsed before
          in GC {..}

        mut =
          let elapsed = dFromS $ mutatorWallSeconds after - mutatorWallSeconds before
              time    = dFromS $ mutatorCpuSeconds after - mutatorCpuSeconds before
              factor  = realToFrac time / realToFrac elapsed
              effect  = realToFrac cpuElapsed / realToFrac elapsed
              burden  = realToFrac cpuTime / realToFrac time
              bytes   = Bytes $ bytesAllocated after - bytesAllocated before - _GCStats_size - 1296
              rate    = realToFrac bytes / realToFrac (elapsed / 1000000)
              work    = realToFrac bytes / realToFrac (time / 1000000)
          in MUT {..}

    in BenchResult {..}

data Capability
  -- Sum type for the convenience of record punning
  -- record accessors are non-total
  = CPU
        { elapsed      :: {-# UNPACK #-}!Duration   -- ^ wall clock duration
        , time         :: {-# UNPACK #-}!Duration   -- ^ CPU duration
        , factor       :: {-# UNPACK #-}!Factor     -- ^ wall / cpu
        }
   | GC
        { elapsed      :: {-# UNPACK #-}!Duration   -- ^ wall clock duration
        , time         :: {-# UNPACK #-}!Duration   -- ^ CPU duration
        , factor       :: {-# UNPACK #-}!Factor     -- ^ wall / cpu
        , effect       :: {-# UNPACK #-}!Percent    -- ^ impact on wall
        , burden       :: {-# UNPACK #-}!Percent    -- ^ impact on cpu
        , bytes        :: {-# UNPACK #-}!Bytes      -- ^ bytes copied
        , rate         :: {-# UNPACK #-}!Throughput -- ^ bytes copied / wall
        , work         :: {-# UNPACK #-}!Throughput -- ^ bytes copied / cpu
        , colls        :: {-# UNPACK #-}!Count      -- ^ number of GCs
        , live         :: {-# UNPACK #-}!Bytes      -- ^ active bytes after
        }
   | MUT
        { elapsed      :: {-# UNPACK #-}!Duration   -- ^ wall clock duration
        , time         :: {-# UNPACK #-}!Duration   -- ^ CPU duration
        , factor       :: {-# UNPACK #-}!Factor     -- ^ wall / cpu
        , effect       :: {-# UNPACK #-}!Percent    -- ^ impact on wall
        , burden       :: {-# UNPACK #-}!Percent    -- ^ impact on cpu
        , bytes        :: {-# UNPACK #-}!Bytes      -- ^ bytes allocated
        , rate         :: {-# UNPACK #-}!Throughput -- ^ bytes allocated / wall
        , work         :: {-# UNPACK #-}!Throughput -- ^ bytes allocated / cpu
        }
   -- Pretend GC is not parallel for now
   -- | Par
   --      { }
   deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

data BenchResult = BenchResult
    { label      :: !String
    , cpu        :: {-# UNPACK #-}!Capability
    , mut        :: {-# UNPACK #-}!Capability
    , gc         :: {-# UNPACK #-}!Capability
    } deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

instance Pretty BenchResult where
    pretty BenchResult {..} =
        unlines
            [ ""
            , header
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "GC:"       <> p   collections   <> p "MUT:"
            , "  Bytes: " <> p ( bytes    gc ) <> "    Bytes:" <> p ( bytes mut )
            , "  Rate:  " <> p ( rate     gc ) <> "    Rate: " <> p ( rate  mut )
            , "  Work:  " <> p ( work     gc ) <> "    Work: " <> p ( work  mut )
            , "  Live:  " <> p ( live     gc )
            ]
      where
        header = "          Elapsed |"
                 <> "          Time |"
                 <> " Elapsed/Total |"
                 <> "    Time/Total |"
                 <> "       Speedup"

        collections = "(" <> p (colls gc) <> " GCs)"

        cpuTimeStats =
          "CPU:"    <> p ( elapsed cpu )
            <> "  " <> p ( time    cpu )
            <> "  " <> pad ""
            <> "  " <> pad ""
            <> "  " <> p ( factor  cpu )

        mutTimeStats =
          "MUT:"    <> p ( elapsed mut )
            <> "  " <> p ( time    mut )
            <> "  " <> p ( effect  mut )
            <> "  " <> p ( burden  mut )
            <> "  " <> p ( factor  mut )

        gcTimeStats =
          "GC: "    <> p ( elapsed gc )
            <> "  " <> p ( time    gc )
            <> "  " <> p ( effect  gc )
            <> "  " <> p ( burden  gc )
            <> "  " <> p ( factor  gc )

        p :: forall p. Pretty p => p -> String
        p = pad . pretty

        pad :: String -> String
        pad s =
          let l = length s
          in replicate (14 - l) ' ' <> s

type BenchPred a = a -> BenchResult -> BenchResult -> Bool

{-# INLINE faster #-}
faster :: Real a => BenchPred a
faster
  base
  ((elapsed . cpu) -> b1)
  ((elapsed . cpu) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE slower #-}
slower :: Real a => BenchPred a
slower base = flip (faster base)

{-# INLINE fewerAllocations #-}
fewerAllocations :: Real a => BenchPred a
fewerAllocations
  base
  ((bytes . mut) -> b1)
  ((bytes . mut) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE moreAllocations #-}
moreAllocations :: Real a => BenchPred a
moreAllocations base = flip (fewerAllocations base)

{-# INLINE fasterAllocations #-}
fasterAllocations :: Real a => BenchPred a
fasterAllocations
  base
  ((rate . mut) -> b1)
  ((rate . mut) -> b2) =
    b1 > b2 && not (similar base b1 b2)

{-# INLINE slowerAllocations #-}
slowerAllocations :: Real a => BenchPred a
slowerAllocations base = flip (fasterAllocations base)

{-# INLINE fewerGCs #-}
fewerGCs :: Real a => BenchPred a
fewerGCs
  base
  ((colls . gc) -> b1)
  ((colls . gc) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE moreGCs #-}
moreGCs :: Real a => BenchPred a
moreGCs base = flip (fewerGCs base)

{-# INLINE lessGC #-}
lessGC :: Real a => BenchPred a
lessGC
  base
  ((elapsed . gc) -> b1)
  ((elapsed . gc) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE moreGC #-}
moreGC :: Real a => BenchPred a
moreGC base = flip (lessGC base)

{-# INLINE betterUtilization #-}
betterUtilization :: Real a => BenchPred a
betterUtilization base b1 b2 =
  (work $ gc b1) < (work $ gc b2) &&
  (work $ mut b1) < (work $ mut b2) &&
  dissimilar base (work $ gc b1) (work $ gc b2) &&
  dissimilar base (work $ mut b1) (work $ mut b2)

{-# INLNE moreConservative #-}
moreConservative :: Real a => BenchPred a
moreConservative base b1 b2 =
  (bytes $ gc b1) < (bytes $ gc b2) &&
  (bytes $ mut b1) < (bytes $ mut b2) &&
  dissimilar base (bytes $ gc b1) (bytes $ gc b2) &&
  dissimilar base (bytes $ mut b1) (bytes $ mut b2)

{-# INLINE lessConservative #-}
lessConservative :: Real a => BenchPred a
lessConservative base = flip (moreConservative base)

{-# INLINE lessGCThanMutation #-}
lessGCThanMutation :: Real a => a -> BenchResult -> Bool
lessGCThanMutation base b =
  (bytes $ gc b) < (bytes $ mut b) &&
  dissimilar base (bytes $ gc b) (bytes $ mut b)

-- instance Pretty (BenchResult,BenchResult) where
--     pretty (old,new) = do
--         unlines
--             [ ""
--             , header
--             , cpuStats
--             , mutStats
--             , gcStats
--             , ""
--             , "GC:"       <> p   collections   <> p "MUT:"
--             , "  Bytes: " <> p ( bytes    gc ) <> "    Bytes:" <> p ( bytes mut )
--             , "  Rate:  " <> p ( rate     gc ) <> "    Rate: " <> p ( rate  mut )
--             , "  Work:  " <> p ( work     gc ) <> "    Work: " <> p ( work  mut )
--             , "  Live:  " <> p ( live     gc )
--             ]
--       where
--         header = "          Elapsed |"
--                   <> "          Time |"
--                   <> " Elapsed/Total |"
--                   <> "    Time/Total |"
--                   <> "       Speedup"

--         collections = "(" <> p (colls gc) <> " GCs)"

--         cpuTimeStats =
--           "CPU:"    <> p ( elapsed cpu )
--             <> "  " <> p ( time    cpu )
--             <> "  " <> pad ""
--             <> "  " <> pad ""
--             <> "  " <> p ( factor  cpu )

--         mutTimeStats =
--           "MUT:"    <> p ( elapsed mut )
--             <> "  " <> p ( time    mut )
--             <> "  " <> p ( effect  mut )
--             <> "  " <> p ( burden  mut )
--             <> "  " <> p ( factor  mut )

--         gcTimeStats =
--           "GC: "    <> p ( elapsed gc )
--             <> "  " <> p ( time    gc )
--             <> "  " <> p ( effect  gc )
--             <> "  " <> p ( burden  gc )
--             <> "  " <> p ( factor  gc )

--         p = pad . pretty

--         pad :: String -> String
--         pad s =
--           let l = length s
--           in replicate (14 - l) ' ' <> s


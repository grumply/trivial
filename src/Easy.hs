{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}
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
  ( module Control.Monad
  , module Control.Applicative

  , seq, deepseq
  , Async, Sync
  , Test

  , io
  , wrap

  , fork, fork'

  , scope

  , note, note', noteScoped, notep, noteLine
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
  , complete
  , expect, expectNothing, expectJust, expectLeft, expectRight

  , store, store'
  , retrieve, retrieve'

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
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.Monoid
import           Data.Word
import           GHC.Stack
import           System.Directory
import           System.Exit
import           System.IO
import           System.Random            (Random)
import qualified System.Random            as Random

import           Control.Exception
import           Control.DeepSeq

import           Data.Char
import           Numeric

import           Simple.Internal.Pretty

import           Text.Read hiding (lift)
import           Unsafe.Coerce

#ifdef __GHCJS__
import           Data.JSString as JSS (JSString,pack,unpack)
import           GHCJS.Types
import           GHCJS.Marshal.Pure
#endif

data Status = Failed | Passed !Int | Skipped | Completed

combineStatus :: Status -> Status -> Status
combineStatus Skipped s             = s
combineStatus s Skipped             = s
combineStatus Failed _              = Failed
combineStatus _ Failed              = Failed
combineStatus Completed s           = s
combineStatus s Completed           = s
combineStatus (Passed n) (Passed m) = Passed (n + m)

data Env =
  Env { rng      :: TVar Random.StdGen
      , messages :: String
      , results  :: TBQueue (Maybe (TMVar (String, Status)))
      , noter    :: String -> IO ()
      , allow    :: String
      }

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
  pure $ \msg -> do
    evaluate $ force msg
    withMVar lock $ \_ -> putStrLn msg

{-# INLINE expect #-}
expect :: HasCallStack => Bool -> Test sync ()
expect False = crash "unexpected"
expect True  = ok

{-# INLINE expectJust #-}
expectJust :: HasCallStack => Maybe a -> Test sync a
expectJust (Just a) = ok >> pure a
expectJust _ = crash "expected Just, got Nothing"

{-# INLINE expectNothing #-}
expectNothing :: HasCallStack => Maybe a -> Test sync ()
expectNothing Nothing  = ok
expectNothing _ = crash "expected Nothing, got Just"

{-# INLINE expectRight #-}
expectRight :: HasCallStack => Either l r -> Test sync r
expectRight (Right a) = ok >> pure a
expectRight _ = crash "expected Right, got Left"

{-# INLINE expectLeft #-}
expectLeft :: HasCallStack => Either l r -> Test sync l
expectLeft (Left l) = ok >> pure l
expectLeft _ = crash "expected Left, got Right"

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
  results <- atomically $ newTVar HM.empty
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTBQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    atomically $ modifyTVar results (HM.insertWith combineStatus msgs passed)
    resultsMap <- readTVarIO results
    case HM.lookupDefault Skipped msgs resultsMap of
      Skipped -> pure ()
      Passed n -> note $ "👌  OK " ++ (if n <= 1 then msgs else "(" ++ show n ++ ") " ++ msgs)
      Failed -> note $ "👎  FAILED " ++ msgs
      Completed -> note $ "👌  COMPLETED " ++ msgs
  note "Raw test output to follow ... "
  let noteLine = note "------------------------------------------------------------"
  noteLine
  e <- try (runReaderT t (Env rngVar [] resultsQ note allow))
  let finish = do
        atomically $ writeTBQueue resultsQ Nothing
        _ <- A.waitCatch rs
        resultsMap <- readTVarIO results
        let
          resultsList = HM.toList resultsMap
          succeededList = [ n | (_, Passed n) <- resultsList ]
          succeeded = length succeededList
          -- totalTestCases = foldl' (+) 0 succeededList
          failures = [ a | (a, Failed) <- resultsList ]
          failed = length failures
        case failures of
          [] -> do
            noteLine
            case succeeded of
              0 -> do
                note "😶  hmm ... no test results recorded"
                note "Tip: use `ok`, `expect`, or `crash` to record results"
                note "Tip: if running via `runOnly` or `rerunOnly`, check for typos"
              1 -> note $ "✅  1 test passed, no failures! 👍 🎉"
              _ -> note $ "✅  " ++ show succeeded ++ " tests passed, no failures! 👍 🎉"
          (hd:_) -> do
            noteLine
            note "\n"
            note $ "  " ++ show succeeded ++ (if failed == 0 then " PASSED" else " passed")
            note $ "  " ++ show (length failures) ++ (if failed == 0 then " failed" else " FAILED (failed scopes below)")
            note $ "    " ++ intercalate "\n    " (map show failures)
            note ""
            note $ "  To rerun with same random seed:\n"
            note $ "    rerun " ++ show seed
            note $ "    rerunOnly " ++ show seed ++ " " ++ "\"" ++ hd ++ "\""
            note "\n"
            noteLine
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

noteLine :: Test sync ()
noteLine = note "------------------------------------------------------------"

{-# INLINE scope #-}
-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test sync a -> Test sync a
scope msg (Test t) = Test $ do
  env <- ask
  let messages' = case messages env of [] -> msg; ms -> ms ++ ('.':msg)
  case (null (allow env) || take (length (allow env)) msg `isPrefixOf` allow env) of
    False -> putResult Skipped >> pure Nothing
    True ->
      let allow' = drop (length msg + 1) (allow env)
      in liftIO $ runReaderT t (env { messages = messages', allow = allow' })

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
hex i = "0x" <> showHex (fromIntegral i :: Int) []

{-# INLINE octal #-}
octal :: Integral i => i -> String
octal i = "0o" <> showOct (fromIntegral i :: Int) []

-- | Generate a random value
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
ok = Test (Just <$> putResult (Passed 1)) >> io yield

-- | Record a completed benchmark in the current scope
{-# INLINE complete #-}
complete :: Test sync ()
complete = Test (Just <$> putResult Completed) >> io yield

-- | Explicitly skip this test
{-# INLINE skip #-}
skip :: Test sync ()
skip = Test (Nothing <$ putResult Skipped) >> io yield

-- | Record a failure at the current scope
{-# INLINE crash #-}
crash :: HasCallStack => String -> Test sync a
crash msg = do
  let trace = callStack
      msg' = msg ++ " " ++ prettyCallStack trace
  Test (Just <$> putResult Failed) >> note ("FAILURE " ++ msg') >> Test (pure Nothing)

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

#ifdef __GHCJS__
foreign import javascript unsafe
  "localStorage.setItem($1,$2)" set_item_js :: JSString -> JSString -> IO ()
foreign import javascript unsafe
  "localStorage.getItem($1)" get_item_js :: JSString -> IO JSVal
foreign import javascript unsafe
  "$r = typeof $1 === 'string'" is_string_js :: JSVal -> Bool
#endif

{-# INLINE store' #-}
store' :: (Show v) => String -> String -> v -> IO ()
store' cs k v = do
  let dir = "trivial/"
#ifdef __GHCJS__
  set_item_js (JSS.pack (dir <> show (abs $ hash (cs <> k)))) (JSS.pack $ show v)
#else
  createDirectoryIfMissing True dir
  writeFile (dir <> show (abs $ hash (cs <> k))) (show v)
#endif

{-# INLINE store #-}
store :: (Show v) => String -> v -> Test sync ()
store k v = do
  cs <- currentScope
  io $ store' cs k v

{-# INLINE retrieve' #-}
retrieve' :: (Read v) => String -> String -> IO (Maybe v)
retrieve' cs k = do
  let loc = "trivial/" <> show (abs $ hash (cs <> k))
#ifdef __GHCJS__
  mv <- get_item_js (JSS.pack loc)
  if is_string_js mv then
    return $ readMaybe $ JSS.unpack $ pFromJSVal mv
  else
    return Nothing
#else
  exists <- doesFileExist loc
  if exists then do
    h <- openFile loc ReadMode
    c <- hGetContents h
    c `deepseq` hClose h
    return $ readMaybe c
  else
    return Nothing
#endif

{-# INLINE retrieve #-}
retrieve :: (Read v) => String -> Test sync (Maybe v)
retrieve k = do
  cs <- currentScope
  io $ retrieve' cs k

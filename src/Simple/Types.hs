{-# LANGUAGE RankNTypes #-}
module Simple.Types where

import Simple.Internal.Base
import Simple.Internal.Collections
import Simple.Internal.Count
import Simple.Internal.DataRate
import Simple.Internal.Improving
import Simple.Internal.Magnitude
import Simple.Internal.Pretty
import Simple.Internal.Similar
import Simple.Internal.Space
import Simple.Internal.Time

{-# INLINE e #-}
e :: Floating a => a
e = exp 1


data CPU = CPU
    { cpuElapsed      :: {-# UNPACK #-}!Elapsed
    , cputime         :: {-# UNPACK #-}!CPUTime   -- ^ CPU duration
    } deriving (Generic, Read, Show, Eq)

instance HasElapsed CPU where
  elapsed = cpuElapsed

instance HasCPUTime CPU where
  cpuTime = cputime

data GC = GC
    { gcElapsed    :: {-# UNPACK #-}!Elapsed
    , gcTime       :: {-# UNPACK #-}!CPUTime
    , copyRate     :: {-# UNPACK #-}!CopyRate
    , collections  :: {-# UNPACK #-}!Collections
    , uncollected  :: {-# UNPACK #-}!Bytes
    , copied       :: {-# UNPACK #-}!Copied
    , slop         :: {-# UNPACK #-}!Slop
    } deriving (Generic, Read, Show, Eq)

instance HasElapsed GC where
  elapsed = gcElapsed

instance HasCPUTime GC where
  cpuTime = gcTime

data MUT = MUT
    { mutElapsed      :: {-# UNPACK #-}!Elapsed
    , mutTime         :: {-# UNPACK #-}!CPUTime
    , alloc           :: {-# UNPACK #-}!Allocation
    , mutated         :: {-# UNPACK #-}!Mutated
    , peak            :: {-# UNPACK #-}!Peak
    , used            :: {-# UNPACK #-}!Used
    , cumulative      :: {-# UNPACK #-}!Cumulative
    , maxBytes        :: {-# UNPACK #-}!Max
    } deriving (Generic, Read, Show, Eq)

instance HasElapsed MUT where
  elapsed = mutElapsed

instance HasCPUTime MUT where
  cpuTime = mutTIme

data BenchResult = BenchResult
    { label      :: !String
    , cpu        :: !CPU
    , mut        :: !MUT
    , gc         :: !GC
    } deriving (Generic, Read, Show, Eq)

instance Pretty BenchResult where
    pretty BenchResult {..} =
        unlines
            [ ""
            , header
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "Collections:" <> pad 7 (pretty (colls gc))
            , "Leftover:   " <> pad 7 (pretty (live gc))
            ]
      where
        header = "            Time |"
                  <> "    Relative |"
                  <> "       Bytes |"
                  <> "    Throughput"

        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p ( time    cpu )

        mutTimeStats =
          "MUT:"    <> p ( time    mut )
            <> "  " <> p ( burden  mut )
            <> "  " <> p ( bytes   mut )
            <> "  " <> pad 14 (pretty ( work    mut ))

        gcTimeStats =
          "GC: "    <> p ( time    gc )
            <> "  " <> p ( burden  gc )
            <> "  " <> p ( bytes   gc )
            <> "  " <> pad 14 (pretty ( work    gc ))

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

prettyVerbose BenchResult {..} =
    unlines
        [ ""
        , header
        , divider
        , cpuTimeStats
        , mutTimeStats
        , gcTimeStats
        , ""
        , "GC:                      MUT:"
        , "  Bytes: " <> pad 12 (pretty (bytes gc)) <> pad 12 "Bytes:" <> pad 12 (pretty ( bytes mut ))
        , "  Rate:  " <> pad 12 (pretty (rate  gc)) <> pad 12 "Rate: " <> pad 12 (pretty ( rate  mut ))
        , "  Work:  " <> pad 12 (pretty (work  gc)) <> pad 12 "Work: " <> pad 12 (pretty ( work  mut ))
        , "  Live:  " <> pad 12 (pretty (live  gc))
        , "  GCs:   " <> pad 12 (pretty (colls gc))
        ]
  where
    header = "         (E)lapsed |"
              <> "        (T)ime |"
              <> "     (E)/Total |"
              <> "     (T)/Total |"
              <> "       Speedup"

    divider = "     -----------------------------------------------------------------------------"

    cpuTimeStats =
      "CPU:"    <> p ( elapsed cpu )
        <> "  " <> p ( time    cpu )
        <> "  " <> pad 14 ""
        <> "  " <> pad 14 ""
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
    p = pad 14 . pretty

    pad :: Int -> String -> String
    pad n s =
      let l = length s
      in replicate (n - l) ' ' <> s

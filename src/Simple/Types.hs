module Simple.Types where

{-# INLINE e #-}
e :: Floating a => a
e = exp 1

newtype Throughput = Throughput { getThroughput :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Improving Throughput -- more throughput is better
instance Pretty Throughput where
    pretty (Throughput t)
        | t < 2^10  = printf "%d B/s"  (round t :: Integer)
        | t < 2^20  = printf "%.1f KB/s" (t / 2^10)
        | t < 2^30  = printf "%.1f MB/s" (t / 2^20)
        | otherwise = printf "%.1f GB/s" (t / 2^30)
instance Similar Throughput

newtype Collections = Collections { getCollections :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show)
instance Improving Collections where
  improving c1 c2 = c1 > c2
  improvingShow _ = ">"
instance Similar Collections
instance Pretty Collections where
  pretty (Collections c) = show c

-- Not specific enough to have an Improving instance;
-- An improvement in one percentage value implies a
-- relative worsening of another.
newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Pretty Percent where
  pretty (Percent p) = printf "%.2f%%" (p * 100)

newtype Factor = Factor { getFactor :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Improving Factor -- larger factor is better
instance Pretty Factor where
  pretty (Factor f) = printf "%.2fx" f

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
        , colls        :: {-# UNPACK #-}!Collections-- ^ number of GCs
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
   deriving (Generic, Read, Show, Eq)

data BenchResult = BenchResult
    { label      :: !String
    , cpu        :: !Capability
    , mut        :: !Capability
    , gc         :: !Capability
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


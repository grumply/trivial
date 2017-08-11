{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Simple.Types (module Simple.Types, module Export) where

import Simple.Internal.Base as Export
import Simple.Internal.Count as Export
import Simple.Internal.DataRate as Export
import Simple.Internal.Improving as Export
import Simple.Internal.Magnitude as Export
import Simple.Internal.Pretty as Export
import Simple.Internal.Rate as Export
import Simple.Internal.Similar as Export
import Simple.Internal.Space as Export
import Simple.Internal.Time as Export
import Simple.Internal.Percent as Export
import Simple.Internal.Utilities as Export
import Simple.Internal.Variance as Export
import Simple.Internal as Export

{-# INLINE e #-}
e :: Floating a => a
e = exp 1

-- prettyVerbose BenchResult {..} =
--     unlines
--         [ ""
--         , header
--         , divider
--         , cpuTimeStats
--         , mutTimeStats
--         , gcTimeStats
--         , ""
--         , "GC:                      MUT:"
--         , "  Bytes: " <> pad 12 (pretty (bytes gc)) <> pad 12 "Bytes:" <> pad 12 (pretty ( bytes mut ))
--         , "  Rate:  " <> pad 12 (pretty (rate  gc)) <> pad 12 "Rate: " <> pad 12 (pretty ( rate  mut ))
--         , "  Work:  " <> pad 12 (pretty (work  gc)) <> pad 12 "Work: " <> pad 12 (pretty ( work  mut ))
--         , "  Live:  " <> pad 12 (pretty (live  gc))
--         , "  GCs:   " <> pad 12 (pretty (colls gc))
--         ]
--   where
--     header = "         (E)lapsed |"
--               <> "        (T)ime |"
--               <> "     (E)/Total |"
--               <> "     (T)/Total |"
--               <> "       Speedup"

--     divider = "     -----------------------------------------------------------------------------"

--     cpuTimeStats =
--       "CPU:"    <> p ( elapsed cpu )
--         <> "  " <> p ( time    cpu )
--         <> "  " <> pad 14 ""
--         <> "  " <> pad 14 ""
--         <> "  " <> p ( factor  cpu )

--     mutTimeStats =
--       "MUT:"    <> p ( elapsed mut )
--         <> "  " <> p ( time    mut )
--         <> "  " <> p ( effect  mut )
--         <> "  " <> p ( burden  mut )
--         <> "  " <> p ( factor  mut )

--     gcTimeStats =
--       "GC: "    <> p ( elapsed gc )
--         <> "  " <> p ( time    gc )
--         <> "  " <> p ( effect  gc )
--         <> "  " <> p ( burden  gc )
--         <> "  " <> p ( factor  gc )

--     p :: forall p. Pretty p => p -> String
--     p = pad 14 . pretty

--     pad :: Int -> String -> String
--     pad n s =
--       let l = length s
--       in replicate (n - l) ' ' <> s

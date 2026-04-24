-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SOLTest.Types
import Data.Maybe (fromMaybe)
-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
--
-- FLP: Implement this function. The following functions may (or may not) come in handy:
--      @Map.fromList@, @Map.foldlWithKey'@, @Map.empty@, @Map.lookup@, @Map.insertWith@,
--      @Map.map@, @Map.fromList@
groupByCategory :: [TestCaseDefinition] -> Map String TestCaseReport -> Map String CategoryReport
groupByCategory selected results =
  let catNames = map tcdCategory selected
      -- Předvyplnění prázdných kategorií s 0 body a prázdnými výsledky
      emptyCats = Map.fromList [(cat, CategoryReport 0 0 Map.empty) | cat <- catNames]
      
      populateCat cr tcd =
        case Map.lookup (tcdName tcd) results of
          Nothing -> cr -- Tento test neproběhl/nebyl vybrán
          Just res -> 
            let pts = tcdPoints tcd
                passedPts = if tcrResult res == Passed then pts else 0
            in cr { crTotalPoints = crTotalPoints cr + pts
                  , crPassedPoints = crPassedPoints cr + passedPts
                  , crTestResults = Map.insert (tcdName tcd) res (crTestResults cr)
                  }
                  
      folder acc tcd = Map.adjust (\cr -> populateCat cr tcd) (tcdCategory tcd) acc
  in foldl folder emptyCats selected

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
--
-- FLP: Implement this function. You'll use @computeHistogram@ here.
computeStats :: Int -> Int -> Int -> Maybe (Map String CategoryReport) -> TestStats
computeStats foundCount loadedCount selectedCount mCategoryResults =
  let cats = fromMaybe Map.empty mCategoryResults
      -- Počet úspěšných testů odvozujeme z agregovaných dat kategorií
      passed = sum [ Map.size (Map.filter (\r -> tcrResult r == Passed) (crTestResults cr)) | cr <- Map.elems cats ]
      hist = computeHistogram cats
  in TestStats foundCount loadedCount selectedCount passed hist

-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
--
-- FLP: Implement this function.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram categories =
  -- Fixní inicializace zaručuje striktní shodu s požadovanou strukturou JSON výstupu, i když některé biny zůstanou prázdné
  let emptyHist = Map.fromList [("0.0",0),("0.1",0),("0.2",0),("0.3",0),("0.4",0),("0.5",0),("0.6",0),("0.7",0),("0.8",0),("0.9",0)]
      rates = map (\cr -> 
        let total = Map.size (crTestResults cr)
            passed = Map.size (Map.filter (\r -> tcrResult r == Passed) (crTestResults cr))
        in if total == 0 then 0.0 else fromIntegral passed / fromIntegral total
        ) (Map.elems categories)
      bins = map rateToBin rates
      counts = Map.fromListWith (+) [(b, 1) | b <- bins]
  in Map.union counts emptyHist

-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac

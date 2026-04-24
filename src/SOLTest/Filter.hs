-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types
import Data.List (partition)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
--
-- FLP: Implement this function using @matchesAny@ and @matchesCriterion@.
filterTests :: FilterSpec -> [TestCaseDefinition] -> ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec tests = partition isSelected tests
  where
    isSelected test = 
      let incs = fsIncludes spec
          excs = fsExcludes spec
          -- Pokud není specifikován žádný include filtr - povolit vše, jinak test musí odpovídat alespoň jednomu include filtru. Exclude filtry mají vždy přednost.
          incMatch = null incs || matchesAny (fsUseRegex spec) incs test
          excMatch = matchesAny (fsUseRegex spec) excs test
      in incMatch && not excMatch

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
--
-- FLP: Implement this function. If you're not implementing the regex matching
-- bonus extension, you can either remove the first argument and update the usages,
-- or you can simply ignore the value.
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion _ test criterion = 
  -- trimFilterId zaručí, že bílé znaky navíc nezpůsobí selhání
  case criterion of
    ByAny val      -> let v = trimFilterId val in v == tcdName test || v == tcdCategory test || v `elem` tcdTags test
    ByCategory val -> let v = trimFilterId val in v == tcdCategory test
    ByTag val      -> let v = trimFilterId val in v `elem` tcdTags test

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace

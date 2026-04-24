-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import SOLTest.Types
import System.Directory
  ( doesFileExist,
    listDirectory,
    doesDirectoryExist
  )
import System.FilePath (replaceExtension, takeBaseName, (</>), takeExtension)
import Control.Monad (filterM)

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
--
-- FLP: Implement this function. The following functions may come in handy:
--      @doesDirectoryExist@, @takeExtension@, @forM@ or @mapM@,
--      @findCompanionFiles@ (below).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then return []
    else do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries
      
      -- Rozdělení na soubory a adresáře
      files <- filterM doesFileExist fullPaths
      dirs  <- filterM doesDirectoryExist fullPaths
      
      -- Filtrace pouze .test souborů v aktuálním adresáři
      let testFiles = filter (\p -> takeExtension p == ".test") files
      
      -- Získání deskriptorů pro všechny nalezené .test soubory
      currentTests <- mapM findCompanionFiles testFiles
      
      -- Pokud je zapnutá rekurze, prohledáme i podadresáře
      subDirTests <- if recursive
        then concat <$> mapM (discoverTests True) dirs
        else return []
        
      return (currentTests ++ subDirTests)

-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }

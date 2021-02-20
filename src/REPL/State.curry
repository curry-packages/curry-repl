------------------------------------------------------------------------------
--- The state of the REPL.
---
--- @author  Michael Hanus
--- @version February 2021
------------------------------------------------------------------------------

module REPL.State where

import Control.Monad      ( unless )
import Curry.Compiler.Distribution ( installDir )
import System.Environment ( getArgs )
import System.IO          ( Handle, hFlush, stdout )

import System.Directory   ( doesFileExist )
import System.FilePath    ( (</>), splitSearchPath )
import System.Process     ( getPID )

import REPL.Compiler

data ReplState = ReplState
  { compiler     :: CCDescription
  , rcvars       :: [(String, String)] -- content of rc file
  , verbose      :: Int        -- verbosity level:
                               -- 0 = errors and warnings
                               -- 1 = show frontend compilation status
                               -- 2 = show also kics2c compilation status
                               -- 3 = show also ghc compilation status
                               -- 4 = show analysis information
  , libPaths     :: [String]   -- directories containg the standard libraries
  , importPaths  :: [String]   -- additional directories to search for imports
  , preludeName  :: String     -- the name of the standard prelude
  , currMod      :: String     -- name of current main module
  , addMods      :: [String]   -- names of additionally added modules
  , mainExpMod   :: String     -- name of module to store main expressions
  , prompt       :: String     -- repl prompt shown in front of user input
  , showTime     :: Bool       -- show execution of main goal?
  , withShow     :: Bool       -- use class `Show` to show results
  , showBindings :: Bool       -- show free variables in main goal in output?
  , safeExec     :: Bool       -- safe execution mode without I/O actions
  , parseOpts    :: String     -- additional options for the front end
  , rtsArgs      :: String     -- run-time arguments passed to main application
  , cmpOpts      :: [(String,String)] -- compiler-specific options
  , quit         :: Bool       -- terminate the REPL?
  , exitStatus   :: Int        -- exit status (set in case of REPL errors)
  , sourceguis   :: [(String,(String,Handle))] -- handles to SourceProgGUIs
  }

--- Initial state of REPL w.r.t. a compiler description
initReplState :: CCDescription -> IO ReplState
initReplState cd = do
  pid <- getPID
  let compilerid = filter isAlphaNum (ccName cd)
  mainmod <- getUnusedMod ("Main" ++ compilerid ++ show pid)
  return $ ReplState
    { compiler     = cd
    , rcvars       = []
    , verbose      = 1
    , libPaths     = splitSearchPath (ccLibPath cd)
    , importPaths  = []
    , preludeName  = "Prelude"
    , currMod      = "Prelude"
    , addMods      = []
    , mainExpMod   = mainmod
    , prompt       = "%s> "
    , showTime     = False
    , withShow     = False
    , showBindings = False
    , safeExec     = False
    , parseOpts    = ""
    , rtsArgs      = ""
    , cmpOpts      = map (\ (CCOption _ _ tags) -> head tags) (ccOpts cd)
    , quit         = False
    , exitStatus   = 0
    , sourceguis   = []
    }
 where
  getUnusedMod f = do
    ex <- doesFileExist (f ++ ".curry")
    if ex then getUnusedMod (f ++ "X")
          else return f

mainExpFile :: ReplState -> String
mainExpFile rst = mainExpMod rst ++ ".curry"

loadPaths :: ReplState -> [String]
loadPaths rst = "." : importPaths rst ++ libPaths rst

--- Show an info message for a given verbosity level
writeVerboseInfo :: ReplState -> Int -> String -> IO ()
writeVerboseInfo rst lvl msg =
  unless (verbose rst < lvl) (putStrLn msg >> hFlush stdout)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
--- A REPL for the Curry->Go compiler.
--- The REPL can be compiled into an executable by
---
---     > cypm curry :l C2GoREPL.curry :save :q
---
--- @author  Michael Hanus
--- @version October 2021
------------------------------------------------------------------------------

module C2GoREPL where

import Data.List        ( intercalate )
import System.CurryPath ( inCurrySubdir, modNameToPath, sysLibPath )

import REPL.Compiler
import REPL.Main        ( mainREPL )

main :: IO ()
main = mainREPL c2go

--- Specification of the Curry->Go compiler:

-- Adapt this definition to the actual location of the compiler:
c2goHome :: String
c2goHome = "/usr/local/curry2go"

c2go :: CCDescription
c2go = CCDescription
  "curry2go"                     -- the compiler name
  (1,0,0)                        -- the version number
  c2goBanner                     -- the banner
  -- description of specific REPL options:
  [ ("-n|--nocypm",
     "do not invoke `cypm' to compute package load path")
  , ("--noreadline",
     "do not use input line editing via command `rlwrap'")
  ]
  c2goHome                       -- home directory of the compiler
  "info@curry-lang.org"          -- contact email
  (c2goHome ++ "/bin/curry2go-frontend") -- executable of the Curry front end
  (c2goHome ++ "/bin/curry2goc") -- compiler executable
  (c2goHome ++ "/lib")           -- base library path
  Nothing                        -- compile program with load command
  True                           -- use CURRYPATH variable
  (\s -> "-v" ++ s)              -- option to pass verbosity
  (\_ -> "")                     -- option to pass parser options
  (\s -> "--compile " ++ s)      -- option to compile only
  (\s -> "--noimports " ++ s)    -- option to create an executable
  cleanCmd                       -- command to clean module
  [stratOpt, intOpt, firstOpt, resultsOpt, errDepthtOpt]
 where
  cleanCmd m =
    "/bin/rm -f '" ++ inCurrySubdir m ++ ".*' '" ++ modNameToPath m ++ ".curry'"

c2goBanner :: String
c2goBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry2Go Interactive Environment"
  bannerLine = take (length bannerText) (repeat '-')

stratOpt :: CCOption
stratOpt = CCOption
  "fs/dfs/bfs     "
  "search strategy (fair / depth-first / breadth-first)"
  [ ConstOpt "fs"  "--fs"
  , ConstOpt "dfs" "--dfs"
  , ConstOpt "bfs" "--bfs"
  ]

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ConstOpt "-interactive" ""
  , ConstOpt "+interactive" "--interactive"
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ConstOpt "-first" ""
  , ConstOpt "+first" "--first"
  ]

resultsOpt :: CCOption
resultsOpt = CCOption
  "results <n>   "
  "set maximum number of results to be computed\n(default: 0 = infinite)"
  [ ArgOpt "results" "0" showOpt ]
 where
  showOpt s = case reads s :: [(Int,String)] of
    [(n,"")] | n >= 0 -> Just ("--results=" ++ s)
    _                 -> Nothing

errDepthtOpt :: CCOption
errDepthtOpt = CCOption
  "errdepth <n>   "
  "set print depth of expressions in error messages:\nn>0: last n nodes from error point\nn=0: do not print expressions (default)\nn<0: print complete expression"
  [ ArgOpt "errdepth" "0" showOpt ]
 where
  showOpt s = case reads s :: [(Int,String)] of
    [(_,"")] -> Just ("--errdepth=" ++ s)
    _        -> Nothing

------------------------------------------------------------------------------

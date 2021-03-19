------------------------------------------------------------------------------
--- A REPL for the Curry->Go compiler.
--- The REPL can be compiled into an executable by
---
---     > cypm curry :l C2GoREPL.curry :save :q
---
--- @author  Michael Hanus
--- @version February 2021
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
  "curry2go"                 -- the compiler name
  (1,0,0)                    -- the version number
  c2goBanner                 -- the banner
  c2goHome                   -- home directory of the compiler
  "info@curry-lang.org"      -- contact email
  "curry2go"                 -- compiler executable
  (c2goHome ++ "/lib")       -- base library path
  False                      -- parser should read untyped FlatCurry
  True                       -- use CURRYPATH variable
  (\s -> "-v" ++ s)          -- option to pass verbosity
  (\_ -> "")                 -- option to pass parser options
  (\s -> "--compile " ++ s)  -- option to compile only
  (\s -> s)                  -- option to create an executable
  cleanCmd                   -- command to clean module
  [stratOpt, intOpt, firstOpt]
 where
  cleanCmd m =
    "/bin/rm -f " ++ inCurrySubdir m ++ ".* " ++ modNameToPath m ++ ".curry"

c2goBanner :: String
c2goBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry2Go Interactive Environment"
  bannerLine = take (length bannerText) (repeat '-')

stratOpt :: CCOption
stratOpt = CCOption
  "fs/dfs/bfs     "
  "search strategy (fair / depth-first / breadth-first)"
  [ ("fs" ,"--fs")
  , ("dfs","--dfs")
  , ("bfs","--bfs")
  ]

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ("-interactive","")
  , ("+interactive","--interactive")
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ("-first","")
  , ("+first","--first")
  ]

------------------------------------------------------------------------------

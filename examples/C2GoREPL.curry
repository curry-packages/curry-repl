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
import System.CurryPath ( sysLibPath )

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
  "curry2go"
  c2goBanner
  c2goHome
  "info@curry-lang.org"
  "curry2go"
  (c2goHome ++ "/lib")  -- base library path
  False                 -- parser should read untyped FlatCurry
  True                  -- use CURRYPATH variable
  "-v%s"                -- option to pass verbosity
  ""                    -- option to pass parser options (ignored)
  "--compile %s"        -- option to compile only
  "%s"                  -- option to create an executable
  [stratOpt, intOpt, firstOpt]

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

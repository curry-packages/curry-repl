------------------------------------------------------------------------------
--- A REPL for the [Curry->Julia compiler](http://arxiv.org/abs/2008.11999)
--- based on the universal REPL for Curry compilers.
--- 
--- The REPL can be compiled into an executable by
---
---     > cypm curry :l JucsREPL.curry :save :q
---
--- @author  Michael Hanus
--- @version August 2022
------------------------------------------------------------------------------

module JucsREPL where

import Curry.Compiler.Distribution ( installDir )
import System.CurryPath ( inCurrySubdir, modNameToPath )

import REPL.Compiler
import REPL.Main         ( mainREPL )

main :: IO ()
main = mainREPL jucs

--- Specification of the Curry->Go compiler:

jucs :: CCDescription
jucs = CCDescription
  "jucs"                               -- the compiler name
  (1,0,0)                              -- the version number
  jucsBanner                           -- the banner
  [] -- description of specific REPL options
  jucsHome                             -- home directory of the compiler
  "info@curry-lang.org"                -- contact email
  (installDir ++ "/bin/pakcs-frontend") -- executable of the Curry front end
  "jucs"                               -- compiler executable
  (installDir ++ "/lib")               -- base library path
  (Just False)                         -- load command reads untyped FlatCurry
  True                                 -- use CURRYPATH variable
  (\s -> "-v" ++ s)                    -- option to pass verbosity
  (\_ -> "")                           -- option to pass parser options
  (\s -> s)                            -- option to compile only
  (\s -> "--standalone -m main " ++ s) -- option to create an executable
  cleanCmd                             -- command to clean module
  LegacyFreeMode
  [stratOpt, intOpt, firstOpt]
 where
  cleanCmd m =
    "/bin/rm -f " ++ inCurrySubdir m ++ ".* " ++ modNameToPath m ++ ".curry "
                  ++ modNameToPath m ++ ".jl"

jucsHome :: String
jucsHome = "/usr/lib/curry2julia"

jucsBanner :: String
jucsBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry->Julia Interactive Environment"
  bannerLine = take (length bannerText) (repeat '-')

stratOpt :: CCOption
stratOpt = CCOption
  "dfs/bfs     "
  "search strategy (depth-first / breadth-first)"
  [ ConstOpt "dfs" "--dfs"
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

------------------------------------------------------------------------------

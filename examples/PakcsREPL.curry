------------------------------------------------------------------------------
--- A REPL for the PAKCS compiler.
--- The REPL can be compiled into an executable by
---
---     > cypm curry :l PakcsREPL.curry :save :q
---
--- @author  Michael Hanus
--- @version June 2021
------------------------------------------------------------------------------

module PakcsREPL where

import Data.List        ( intercalate )
import System.CurryPath ( inCurrySubdir, modNameToPath, sysLibPath )

import REPL.Compiler
import REPL.Main        ( mainREPL )

main :: IO ()
main = mainREPL pakcs

-- Specification of the PAKCS compiler (based on the already
-- available PAKCS environment):

-- Adapt this definition to the actual location of the compiler:
pakcsHome :: String
pakcsHome = "/usr/lib/pakcs"

pakcs :: CCDescription
pakcs = CCDescription
  "pakcs"                                 -- the compiler name
  (3,4,0)                                 -- the version number
  pakcsBanner                             -- the banner
  pakcsHome                               -- home directory of the compiler
  "pakcs@curry-lang.org"                  -- contact email
  (pakcsHome ++ "/bin/pakcs-frontend") -- executable of the Curry front end
  (pakcsHome ++ "/bin/pakcs")             -- compiler executable
  (pakcsHome ++ "/lib")                   -- base library path
  (Just False)                            -- load command reads untyped FlatCurry
  True                                    -- use CURRYPATH variable
  (\s -> ":set v" ++ s)                   -- option to pass verbosity
  (\s -> ":set parser " ++ s)             -- option to pass parser options
  (\s -> ":compile " ++ s ++ " :quit")    -- option to compile only
  (\s -> ":load " ++ s ++ " :save :quit") -- option to create an executable
  cleanCmd                                -- command to clean module
  [intOpt, firstOpt]
 where
  cleanCmd m =
    "/bin/rm -f " ++ inCurrySubdir m ++ ".* " ++ modNameToPath m ++ ".curry"

pakcsBanner :: String
pakcsBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "PAKCS Interactive Environment"
  bannerLine = take (length bannerText) (repeat '-')

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ("-interactive",":set -interactive")
  , ("+interactive",":set +interactive")
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ("-first",":set -first")
  , ("+first",":set +first")
  ]

------------------------------------------------------------------------------

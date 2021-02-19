------------------------------------------------------------------------------
--- Definition of the parameters which must be defined to
--- implement a REPL for a given Curry compiler.
---
--- These parameters might be read at startup time (e.g., from
--- a JSON file) or statically defined in some module.
---
--- @author  Michael Hanus
--- @version February 2021
------------------------------------------------------------------------------

module REPL.Compiler where

--- The description of a Curry compiler.
--- The name should be a sequence of alphanumeric characeters and
--- is used for naming resource files, main modules etc.
--- The title is shown in the banner.
--- The directory `ccHome` should contain a template `<ccName>rc.default`
--- with the a template for a resource configuration file.
data CCDescription = CCDescription
  { ccName      :: String         -- the name of the compiler
  , ccBanner    :: String         -- the banner shown for the compiler
  , ccHome      :: String         -- home directory of the compiler
  , ccEmail     :: String         -- contact email (shown at startup)
  , ccExec      :: String         -- the executable of the compiler
  , ccLibPath   :: String         -- the path of the standard libraries
  , ccTypedFC   :: Bool           -- should the parser read typed FlatCurry?
  , ccCurryPath :: Bool           -- use CURRYPATH instead of '-i' to set
                                  -- import dirs for the compiler?
  , ccOpts      :: [CCOption]     -- list of options for the compiler
  }

--- An option implemented by a Curry compiler.
--- It consists a short and long description and a list of selections,
--- where each selection consists of a tag and the option passed
--- to the compiler if this value is selected.
--- All tags of an option are exclusive, i.e., at most one of them
--- can be set.
data CCOption = CCOption String String [(String,String)]

showCompilerOptions :: [CCOption] -> String
showCompilerOptions = unlines . map showOpt
 where
  showOpt (CCOption s1 s2 _) = s1 ++ " - " ++ s2

------------------------------------------------------------------------------

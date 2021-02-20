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
--- The name `ccName` should be a sequence of alphanumeric characters and
--- is used for naming resource files, main modules etc.
--- The banner `ccBanner` is shown at start up (if verbosity is not quiet).
--- `ccHome` is the home directory of the compiler.
--- This directory should contain a file `<ccName>rc.default`
--- with the template for a resource configuration file.
--- This is copied into the user's home directory or updated.
--- `ccExec` is the executable of the Curry compiler.
--- `ccLibPath` is the path (possibly several directies) containing
--- the base libraries of the Curry compiler. This path will be appended
--- to an existing load path (specified by the envrionment variable
--- `CURRYPATH`).
--- `ccTypedFC` should be set to `True` if the front end should read
--- Typed FlatCurry files instead of FlatCurry files.
--- If `ccCurryPath` is `True`, then the actual load path is passed
--- by the environment variable to the compiler, otherwise it is passed
--- by a sequence of `-iDIR` options.
--- `ccVerbOpt` is the option to pass the verbosity to the compiler command
--- (the substring `%s` is replaced by the actual verbosity).
--- `ccParseOpt` is the option to pass further parser options to the compiler
--- command (the substring `%s` is replaced by the actual parser options).
--- `ccCmplOpt` is the option to compile a module (the name is specified
--- by `%s` in the string) without creating an executable.
--- `ccExecOpt` is the option to compile a module (the name is specified
--- by `%s` in the string) and create an executable having the same
--- name as the module.
--- `ccOpts` is the list of additional options supported by the compiler.
data CCDescription = CCDescription
  { ccName      :: String         -- the name of the compiler
  , ccBanner    :: String         -- the banner shown for the compiler
  , ccHome      :: String         -- home directory of the compiler
  , ccEmail     :: String         -- contact email (shown at startup)
  , ccExec      :: String         -- the executable of the compiler
  , ccLibPath   :: String         -- the path of the standard libraries
  , ccTypedFC   :: Bool           -- should the parser read typed FlatCurry?
  , ccCurryPath :: Bool           -- use CURRYPATH instead of '-i' for imports
  , ccVerbOpt   :: String         -- option to pass verbosity
  , ccParseOpt  :: String         -- option to pass parser options
  , ccCmplOpt   :: String         -- option to compile only
  , ccExecOpt   :: String         -- option to create an executable
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

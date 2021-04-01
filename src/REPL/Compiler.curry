------------------------------------------------------------------------------
--- Definition of the parameters which must be defined to
--- implement a REPL for a given Curry compiler.
---
--- These parameters might be read at startup time (e.g., from
--- a JSON file) or statically defined in some module.
---
--- @author  Michael Hanus
--- @version April 2021
------------------------------------------------------------------------------

module REPL.Compiler where

--- Data type for the specification of a Curry compiler invoked by the REPL.
--- It contains the following components:
--- 
--- * The name `ccName` should be a sequence of alphanumeric characters and
---   is used for naming resource files, main modules etc.
--- * The version `ccVersion` of the compiler is used by the front end
---   and determines auxiliary directory names.
--- * The banner `ccBanner` is shown at start up (if verbosity is not quiet).
--- * `ccHome` is the home directory of the compiler.
---   This directory should contain a file `<ccName>rc.default`
---   with the template for a resource configuration file.
---   This file is copied into the user's home directory (if it does not exist)
---   or updated (when the variables in the template have been changed).
--- * `ccExec` is the executable of the Curry compiler.
--- * `ccLibPath` is the path (possibly several directies) containing
---   the base libraries of the Curry compiler. This path will be appended
---   to an existing load path (specified by the envrionment variable
---   `CURRYPATH`).
--- * `ccTypedFC` should be set to `True` if the front end should read
---   Typed FlatCurry files instead of FlatCurry files.
--- * If `ccCurryPath` is `True`, then the actual load path is passed
---   by the environment variable `CURRYPATH` to the compiler,
---   otherwise it is passed by a sequence of options of the form `-iDIR`.
--- * `ccVerbOpt` maps the actual verbosity to the compiler option to pass the
---   verbosity to the compiler command.
--- * `ccParseOpt` maps the actual parser options to the compiler option
---   to pass further parser options.
--- * `ccCmplOpt` maps the module name to the compiler option
---   to compile a module without creating an executable.
--- * `ccExecOpt` maps the module name to the compiler option
---   to compile a module and create an executable having the same
---   name as the module.
--- * `ccCleanCmd` maps the module name to the system command which
---   removes the module and all generated intermediate files.
--- * `ccOpts` is the list of additional options supported by the compiler.

data CCDescription = CCDescription
  { ccName      :: String           -- the name of the compiler
  , ccVersion   :: (Int,Int,Int)    -- major/minor/revision version
  , ccBanner    :: String           -- the banner shown for the compiler
  , ccHome      :: String           -- home directory of the compiler
  , ccEmail     :: String           -- contact email (shown at startup)
  , ccExec      :: String           -- the executable of the compiler
  , ccLibPath   :: String           -- the path of the standard libraries
  , ccTypedFC   :: Bool             -- should the parser read typed FlatCurry?
  , ccCurryPath :: Bool             -- use CURRYPATH instead of '-i' for imports
  , ccVerbOpt   :: String -> String -- option to pass verbosity
  , ccParseOpt  :: String -> String -- option to pass parser options
  , ccCmplOpt   :: String -> String -- option to compile only
  , ccExecOpt   :: String -> String -- option to create an executable
  , ccCleanCmd  :: String -> String -- command to clean module and aux. files
  , ccOpts      :: [CCOption]       -- list of options for the compiler
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

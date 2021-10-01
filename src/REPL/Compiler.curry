------------------------------------------------------------------------------
--- Definition of the parameters which must be defined to
--- implement a REPL for a given Curry compiler.
---
--- These parameters might be read at startup time (e.g., from
--- a JSON file) or statically defined in some module.
---
--- @author  Michael Hanus
--- @version July 2021
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
--- * `ccFrontend` is the executable of the Curry front end.
--- * `ccExec` is the executable of the Curry compiler.
--- * `ccLibPath` is the path (possibly several directies) containing
---   the base libraries of the Curry compiler. This path will be appended
---   to an existing load path (specified by the envrionment variable
---   `CURRYPATH`).
--- * `ccTypedFC` should be set to `Nothing` if the (re)load command should
---   compile the main program. Otherwise, the main program is only parsed
---   with the front end where `Just True` indicates that the front end
---   should produce Typed FlatCurry files instead of FlatCurry files
---   (in case of `Just False`).
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
  , ccFrontend  :: String           -- the executable of the Curry front end
  , ccExec      :: String           -- the executable of the compiler
  , ccLibPath   :: String           -- the path of the standard libraries
  , ccTypedFC   :: Maybe Bool       -- should the parser read typed FlatCurry?
  , ccCurryPath :: Bool             -- use CURRYPATH instead of '-i' for imports
  , ccVerbOpt   :: String -> String -- option to pass verbosity
  , ccParseOpt  :: String -> String -- option to pass parser options
  , ccCmplOpt   :: String -> String -- option to compile only
  , ccExecOpt   :: String -> String -- option to create an executable
  , ccCleanCmd  :: String -> String -- command to clean module and aux. files
  , ccOpts      :: [CCOption]       -- list of options for the compiler
  }

--- The specification of an option implemented by a Curry compiler.
--- It consists a short and long description (used in help messages)
--- and a list of selections, where each selection is an option
--- implementation containing a tag.
--- All tags of an option are exclusive, i.e., at most one of them
--- can be set.
data CCOption = CCOption String String [CCOptionImpl]

--- Maps a compiler option into the pair of their short and long description.
showCompilerOptionDescr :: CCOption -> (String,String)
showCompilerOptionDescr (CCOption s1 s2 _) = (s1, s2)

--- Data type to specify the implementation of a compiler option.
--- A constant option `ConstOpt` consists of a tag (shown in the REPL)
--- and the conrete option passed to the Curry compiler when the tag is set.
--- An argument option `ArgOpt` consists of a tag (shown in the REPL),
--- an argument provided when the option is set, and a mapping from the
--- argument to the concrete option passed to the Curry compiler (which
--- is `Nothing` if the argument is not valid).
data CCOptionImpl = ConstOpt String String
                  | ArgOpt   String String (String -> Maybe String)

--- The tag of a compiler option.
tagOfCompilerOption :: CCOptionImpl -> String
tagOfCompilerOption (ConstOpt t _) = t
tagOfCompilerOption (ArgOpt t _ _) = t

--- Shows a specific compiler option in user-readable form.
showCompilerOption :: CCOptionImpl -> String
showCompilerOption (ConstOpt o _) = o
showCompilerOption (ArgOpt o a _) = o ++ '=' : a

--- Maps a specific compiler option into the option passed
--- to the Curry compiler.
mapCompilerOption :: CCOptionImpl -> String
mapCompilerOption (ConstOpt _ co) = co
mapCompilerOption (ArgOpt _ a fo) = maybe "" id (fo a)

--- Replaces a compiler option by a compiler option (given as the first
--- argument) if their tags match.
replaceCompilerOption :: CCOptionImpl -> CCOptionImpl -> CCOptionImpl
replaceCompilerOption newopt oldopt =
  if tagOfCompilerOption newopt == tagOfCompilerOption oldopt
    then newopt
    else oldopt

------------------------------------------------------------------------------

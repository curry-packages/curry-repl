------------------------------------------------------------------------------
--- A universal REPL which can be used on top of a Curry compiler
---
--- @author  Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module REPL.Main where

import Control.Monad      ( when, unless )
import Curry.Compiler.Distribution ( installDir )
import Data.Char          ( toLower )
import Data.List          ( intercalate, intersperse
                          , isInfixOf, isPrefixOf, nub, sort )
import System.Environment ( getArgs, getEnv )
import System.FilePath    ( (</>) )
import System.IO          ( hClose, hFlush, hPutStrLn, isEOF, stdout )

import AbstractCurry.Types hiding (preludeName)
import AbstractCurry.Files
import AbstractCurry.Select
import System.CurryPath    ( inCurrySubdir, lookupModuleSource
                           , stripCurrySuffix )
import System.Directory    ( doesDirectoryExist, doesFileExist
                           , findFileWithSuffix, getAbsolutePath
                           , getDirectoryContents, getHomeDirectory
                           , renameFile, setCurrentDirectory )
import System.FilePath     ( searchPathSeparator, splitExtension
                           , splitFileName, splitSearchPath )
import System.FrontendExec
import System.IOExts       ( connectToCommand )
import System.Process      ( exitWith, getPID, system )

import REPL.Compiler
import REPL.RCFile
import REPL.State
import REPL.Utils          ( showMonoTypeExpr, showMonoQualTypeExpr
                           , getTimeCmd, moduleNameToPath, validModuleName
                           , notNull, removeFileIfExists, strip, writeErrorMsg )

-- ---------------------------------------------------------------------------

--- The main operation to start the REPL. It is parameterized over
--- a description of a Curry compiler.
mainREPL :: CCDescription -> IO ()
mainREPL cd = do
  rcFileDefs <- readRC cd
  args       <- getArgs
  let (nodefargs,defargs) = extractRCArgs args
      (mainargs,rtargs)   = break (=="--") nodefargs
      rcDefs              = updateRCDefs rcFileDefs defargs
      furtherRcDefs       = filter (\da -> fst da `notElem` map fst rcFileDefs)
                                   defargs
  rst   <- initReplState cd
  ipath <- defaultImportPaths rst
  let rst1 = rst { importPaths = ipath
                 , rcvars    = rcDefs
                 , rtsArgs   = if null rtargs then "" else unwords (tail rtargs)
                 }
  if null furtherRcDefs
   then processArgsAndStart
          rst1
          (map strip (words (rcValue (rcvars rst1) "defaultparams")) ++
           mainargs)
   else putStrLn $ "Error: rc property name '" ++ fst (head furtherRcDefs) ++
                   "' not found in rc file!"

processArgsAndStart :: ReplState -> [String] -> IO ()
processArgsAndStart rst []
  | quit rst  = cleanUpAndExitRepl rst
  | otherwise = do
      writeVerboseInfo rst 1 (ccBanner (compiler rst))
      writeVerboseInfo rst 1 $
        "Type \":h\" for help  (contact: " ++ ccEmail (compiler rst) ++ ")"
      repLoop rst
processArgsAndStart rst (arg:args)
  -- ignore empty arguments which can be provided by single or double quotes
  | null      arg = processArgsAndStart rst args
  -- ignore '--nocypm' or '--noreadline' (already processed by script)
  | arg == "--nocypm" || arg == "--noreadline"
  = processArgsAndStart rst args
  | arg == "-V" || arg == "--version"
  = do putStrLn (ccBanner (compiler rst))
       processArgsAndStart rst { quit = True} args
  | arg == "--compiler-name"
  = execCmp arg >> processArgsAndStart rst { quit = True} args
  | arg == "--numeric-version"
  = execCmp arg >> processArgsAndStart rst { quit = True} args
  | arg == "--base-version"
  = execCmp arg >> processArgsAndStart rst { quit = True} args
  | arg == "-h" || arg == "--help" || arg == "-?"
  = printHelp >> cleanUpAndExitRepl rst
  | isCommand arg = do
    let (cmdargs, more) = break isCommand args
    mbrst <- processCommand rst (tail (unwords (arg:cmdargs)))
    maybe printHelp (\rst' -> processArgsAndStart rst' more) mbrst
  | otherwise
  = writeErrorMsg ("unknown command: " ++ unwords (arg:args)) >> printHelp
 where
  execCmp s = system $ unwords [ccExec (compiler rst), s]

--- May a `String` be a REPL command?
isCommand :: String -> Bool
isCommand s = case s of
  ':' : _ -> True
  _       -> False

printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "Invoke interactive environment:"
  , ""
  , "    crepl <options> [ -- <run-time arguments>]"
  , ""
  , "with options:"
  , ""
  , "-h|--help|-?      : show this message and quit"
  , "-V|--version      : show version and quit"
  , "--compiler-name   : show the compiler name and quit"
  , "--numeric-version : show the compiler version number and quit"
  , "--base-version    : show the version of the base libraries and quit"
  , "--nocypm          : do not invoke `cypm' to compute package load path"
  , "--noreadline      : do not use input line editing via command `rlwrap'"
  , "-Dprop=val        : define rc property `prop' as `val'"
  , ":<cmd> <args>     : commands of the interactive environment"
  , ""
  ]

-- ---------------------------------------------------------------------------

-- The main read-eval-print loop:
repLoop :: ReplState -> IO ()
repLoop rst = do
  putStr (calcPrompt rst) >> hFlush stdout
  eof <- isEOF
  if eof
    then cleanUpAndExitRepl rst
    else do getLine >>= processInput rst . strip

calcPrompt :: ReplState -> String
calcPrompt rst =
  substS (unwords (currMod rst : addMods rst)) (prompt rst)

-- Substitute `%s` in a string with a given string (first argument).
substS :: String -> String -> String
substS replacement = sub
 where
  sub []       = []
  sub [c]      = [c]
  sub (c:d:cs) = case c of
    '%' -> case d of
             '%' -> '%' : cs
             's' -> replacement ++ sub cs
             _   -> c : d : sub cs
    _   -> c : sub (d:cs)

-- Clean resources of REPL and terminate it with exit status.
cleanUpAndExitRepl :: ReplState -> IO ()
cleanUpAndExitRepl rst = do
  terminateSourceProgGUIs rst
  exitWith (exitStatus rst)

processInput :: ReplState -> String -> IO ()
processInput rst g
  | null g      = repLoop rst
  | isCommand g = do mbrst <- processCommand rst (strip (tail g))
                     maybe (repLoop (rst { exitStatus = 1 }))
                           (\rst' -> if (quit rst') then cleanUpAndExitRepl rst'
                                                    else repLoop rst')
                           mbrst
  | otherwise   = evalExpression rst g >>= repLoop

--- Evaluate an expression w.r.t. currently loaded modules
evalExpression :: ReplState -> String -> IO ReplState
evalExpression rst expr = do
  exst <- compileMainExpression rst expr True
  return rst { exitStatus = exst }

-- Check whether the main module imports an "Unsafe" module.
importUnsafeModule :: ReplState -> IO Bool
importUnsafeModule rst =
  if containsUnsafe (addMods rst)
    then return True
    else do
      let acyMainModFile = abstractCurryFileName (currMod rst)
          frontendParams = currentFrontendParams rst (verbose rst <= 1)
      catch (do callFrontendWithParams ACY frontendParams (currMod rst)
                p <- readAbstractCurryFile acyMainModFile
                return $ containsUnsafe (imports p))
            (\_ -> return (currMod rst /= "Prelude")) -- just to be safe
 where
  containsUnsafe = any ("Unsafe" `isInfixOf`)

-- Compute the front-end parameters for the current state:
currentFrontendParams :: ReplState -> Bool -> FrontendParams
currentFrontendParams rst quiet =
    setQuiet       quiet
  $ setFullPath    (loadPaths rst)
  $ setExtended    (rcValue (rcvars rst) "curryextensions" /= "no")
  $ setOverlapWarn (rcValue (rcvars rst) "warnoverlapping" /= "no")
  $ setSpecials    (parseOpts rst)
    defaultParams


-- ---------------------------------------------------------------------------
-- Main expression file stuff
-- ---------------------------------------------------------------------------

writeSimpleMainExpFile :: ReplState -> String -> IO ()
writeSimpleMainExpFile rst exp = writeMainExpFile rst [] Nothing exp

-- write the file with the main exp where necessary imports
-- and possibly a type string is provided:
writeMainExpFile :: ReplState -> [String] -> Maybe String -> String -> IO ()
writeMainExpFile rst imports mtype exp =
  writeFile (mainExpFile rst) $ unlines $
    [noMissingSigs, "module " ++ mainExpMod rst ++ " where"] ++
    map ("import " ++) allImports ++
    maybe [] (\ts -> ["main :: " ++ ts]) mtype ++
    ["main = " ++ qualifyMain exp]
 where
  allImports = filter (/="Prelude") . nub $ currMod rst : addMods rst ++ imports

  noMissingSigs = "{-# OPTIONS_FRONTEND -W no-missing-signatures #-}"

  -- simple hack to avoid name conflict with "main":
  qualifyMain s = if strip s == "main" then currMod rst ++ ".main"
                                       else s

-- Generate, read, and delete .acy file of main expression module.
-- Return Nothing if some error occurred during parsing.
getAcyOfMainExpMod :: ReplState -> IO (Maybe CurryProg)
getAcyOfMainExpMod rst = do
  let acyMainExpFile = abstractCurryFileName (mainExpMod rst)
      frontendParams  = currentFrontendParams rst (verbose rst <= 1)
  prog <- catch (callFrontendWithParams ACY frontendParams (mainExpMod rst) >>
                 tryReadACYFile acyMainExpFile)
                (\_ -> return Nothing)
  removeFileIfExists acyMainExpFile
  return prog

getAcyOfExpr :: ReplState -> String -> IO (Maybe CurryProg)
getAcyOfExpr rst expr = do
  writeSimpleMainExpFile rst expr
  mbProg <- getAcyOfMainExpMod rst
  removeFileIfExists (mainExpFile rst)
  return mbProg

-- Show the type of an expression w.r.t. main program:
showTypeOfExp :: ReplState -> String -> IO Bool
showTypeOfExp rst exp = do
  mbProg <- getAcyOfExpr rst exp
  maybe (return False)
        (\ (CurryProg _ _ _ _ _ _ [CFunc _ _ _ qty _] _) -> do
          putStrLn $ exp ++ " :: " ++ showMonoQualTypeExpr False qty
          return True)
        mbProg

-- Get the module of a function visible in the main program:
getModuleOfFunction :: ReplState -> String -> IO String
getModuleOfFunction rst funname = do
  mbprog <- getAcyOfExpr rst $
    if isAlpha (head funname) then funname else '(' : funname ++ ")"
  return $ maybe ""
                 (\ (CurryProg _ _ _ _  _ _ [CFunc _ _ _ _ mainrules] _) ->
                    modOfMain mainrules)
                 mbprog
 where modOfMain r = case r of
        [CRule [] (CSimpleRhs (CSymbol (m, _)) [])]       -> m
        [CRule [] (CGuardedRhs [(_, CSymbol (m, _))] [])] -> m
        _                                                 -> ""

-- ---------------------------------------------------------------------------
-- Processing of REPL commands
-- ---------------------------------------------------------------------------

-- Process a command of the REPL.
-- The result is either just a new ReplState or Nothing if an error occurred.
processCommand :: ReplState -> String -> IO (Maybe ReplState)
processCommand rst cmds
  | null cmds        = skipCommand "unknown command"
  | head cmds == '!' = unsafeExec rst $ processSysCall rst (strip $ tail cmds)
  | otherwise        = case matchedCmds of
      []            -> skipCommand $ "unknown command: ':" ++ cmds ++ "'"
      [(fcmd, act)] -> if fcmd `elem` ["eval","load","quit","reload"]
                       then act rst (strip args)
                       else unsafeExec rst $ act rst (strip args)
      (_:_:_)       -> skipCommand $ "ambiguous command: ':" ++ cmds ++ "'"
 where (cmd, args) = break (==' ') cmds
       matchedCmds = filter (isPrefixOf (map toLower cmd) . fst) replCommands

unsafeExec :: ReplState -> IO (Maybe ReplState) -> IO (Maybe ReplState)
unsafeExec rst act =
  if safeExec rst
  then skipCommand "Operation not allowed in safe mode!"
  else act

-- all available REPL commands
replCommands :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replCommands =
  [ ("?"          , processHelp        )
  , ("add"        , processAdd         )
  , ("browse"     , processBrowse      )
  , ("cd"         , processCd          )
  , ("compile"    , processCompile     )
  , ("edit"       , processEdit        )
  , ("eval"       , processEval        )
  , ("fork"       , processFork        )
  , ("help"       , processHelp        )
  , ("interface"  , processInterface   )
  , ("load"       , processLoad        )
  , ("programs"   , processPrograms    )
  , ("reload"     , processReload      )
  , ("quit"       , processQuit        )
  , ("save"       , processSave        )
  , ("set"        , processSetOption   )
  , ("source"     , processSource      )
  , ("show"       , processShow        )
  , ("type"       , processType        )
  , ("usedimports", processUsedImports )
  ]

--- Skip an erroneous command with an error message
skipCommand :: String -> IO (Maybe ReplState)
skipCommand msg = writeErrorMsg msg >> return Nothing

--- Execute a call to a system command
processSysCall :: ReplState -> String -> IO (Maybe ReplState)
processSysCall rst cmd
  | null cmd  = skipCommand "missing system command"
  | otherwise = system cmd >> return (Just rst)

--- Process :add command
processAdd :: ReplState -> String -> IO (Maybe ReplState)
processAdd rst args
  | null args = skipCommand "Missing module name"
  | otherwise = Just `fmap` foldIO add rst (words args)
  where
    add rst' m = let mdl = stripCurrySuffix m in
      if validModuleName mdl
      then do
        mbf <- findFileWithSuffix (moduleNameToPath mdl) [".curry", ".lcurry"]
                                  (loadPaths rst')
        case mbf of
          Nothing -> do
            writeErrorMsg $ "Source file of module '" ++ mdl ++ "' not found"
            return rst'
          Just _  -> return rst' { addMods = insert mdl (addMods rst') }
      else do writeErrorMsg $ "Illegal module name (ignored): " ++ mdl
              return rst'

    insert m []        = [m]
    insert m ms@(n:ns)
      | m < n     = m : ms
      | m == n    = ms
      | otherwise = n : insert m ns

    foldIO _ a []      =  return a
    foldIO f a (x:xs)  =  f a x >>= \fax -> foldIO f fax xs

--- Process :browse command
processBrowse :: ReplState -> String -> IO (Maybe ReplState)
processBrowse rst args
  | notNull $ stripCurrySuffix args = skipCommand "superfluous argument"
  | otherwise                       = checkForWish $ do
      writeVerboseInfo rst 1 "Starting Curry Browser in separate window..."
      checkAndCallCpmTool "curry-browse" "currybrowse"
        (\toolexec -> execCommandWithPath rst toolexec [currMod rst])

--- Process :cd command
processCd :: ReplState -> String -> IO (Maybe ReplState)
processCd rst args = do
  dirname <- getAbsolutePath args
  exists  <- doesDirectoryExist dirname
  if exists then setCurrentDirectory dirname >> return (Just rst)
            else skipCommand $ "directory does not exist"

--- Process :compile command
processCompile :: ReplState -> String -> IO (Maybe ReplState)
processCompile rst args =
 processLoad rst args >>=
   maybe (return Nothing)
         (\rst' -> compileCurryProgram rst' (currMod rst'))

--- Process :edit command
processEdit :: ReplState -> String -> IO (Maybe ReplState)
processEdit rst args = do
  modname <- getModuleName rst args
  mbf <- findFileWithSuffix (moduleNameToPath modname) [".curry", ".lcurry"]
                            (loadPaths rst)
  editenv <- getEnv "EDITOR"
  let editcmd  = rcValue (rcvars rst) "editcommand"
      editprog = if null editcmd then editenv else editcmd
  if null editenv && null editcmd
    then skipCommand "no editor defined"
    else maybe (skipCommand "source file not found")
          (\fn -> system (editprog ++ " " ++ fn ++ "& ") >> return (Just rst))
          mbf

--- Process :eval command
processEval :: ReplState -> String -> IO (Maybe ReplState)
processEval rst args = evalExpression rst args >>= return . Just

--- Process :fork command
processFork :: ReplState -> String -> IO (Maybe ReplState)
processFork rst args
  | currMod rst == preludeName rst
  = skipCommand "no program loaded"
  | otherwise
  = do exst <- compileMainExpression rst (if null args then "main" else args)
                                     False
       if exst == 0
         then do
           pid <- getPID
           let execname = "." </> "MAINFORK" ++ show pid
           renameFile ("." </> mainExpMod rst) execname
           writeVerboseInfo rst 3 $
             "Starting executable '" ++ execname ++ "'..."
           system $ "( " ++ execname ++ " && rm -f " ++ execname ++ ") " ++
                    "> /dev/null 2> /dev/null &"
           return $ Just rst
         else return Nothing

--- Process :help command
processHelp :: ReplState -> String -> IO (Maybe ReplState)
processHelp rst _ = do
  printHelpOnCommands
  putStrLn "... or type any <expression> to evaluate\n"
  return (Just rst)

--- Process :interface command
processInterface :: ReplState -> String -> IO (Maybe ReplState)
processInterface rst args = do
  modname <- getModuleName rst args
  checkAndCallCpmTool "curry-showflat" "showflatcurry"
    (\toolexec -> execCommandWithPath rst toolexec ["-int", modname])

--- Process :load command
processLoad :: ReplState -> String -> IO (Maybe ReplState)
processLoad rst args = do
  rst' <- terminateSourceProgGUIs rst
  let dirmodname = stripCurrySuffix args
  if null dirmodname
    then skipCommand "missing module name"
    else do
      let (dirname, modname) = splitFileName dirmodname
      mbrst <- if (dirname == "./") then return (Just rst')
               else do putStrLn $ "Changing working directory to " ++ dirname
                       processCd rst' dirname
      maybe (return Nothing)
        (\rst2 ->
          lookupModuleSource (loadPaths rst2) modname >>=
          maybe
            (skipCommand $ "source file of module "++dirmodname++" not found")
            (\_ -> do parseCurryProgram rst2 modname
                      return (Just rst2 { currMod = modname, addMods = [] }))
        )
        mbrst

--- Process :reload command
processReload :: ReplState -> String -> IO (Maybe ReplState)
processReload rst args
  | currMod rst == preludeName rst
  = skipCommand "no program loaded!"
  | null (stripCurrySuffix args)
  = parseCurryProgram rst (currMod rst) >> return (Just rst)
  | otherwise
  = skipCommand "superfluous argument"

--- Process :programs command
processPrograms :: ReplState -> String -> IO (Maybe ReplState)
processPrograms rst _ = printAllLoadPathPrograms rst >> return (Just rst)

--- Process :quit command
processQuit :: ReplState -> String -> IO (Maybe ReplState)
processQuit rst _ = return (Just rst { quit = True })

processSave :: ReplState -> String -> IO (Maybe ReplState)
processSave rst args
  | currMod rst == preludeName rst
  = skipCommand "no program loaded"
  | otherwise
  = do exst <- compileMainExpression rst (if null args then "main" else args)
                                     False
       if exst == 0
         then do renameFile ("." </> mainExpMod rst) (currMod rst)
                 writeVerboseInfo rst 1 $
                   "Executable saved in '" ++ currMod rst ++ "'"
                 return $ Just rst
         else return Nothing

--- Process :show command
processShow :: ReplState -> String -> IO (Maybe ReplState)
processShow rst args = do
  modname <- getModuleName rst args
  mbf <- findFileWithSuffix (moduleNameToPath modname) [".curry", ".lcurry"]
                            (loadPaths rst)
  case mbf of
    Nothing -> skipCommand "source file not found"
    Just fn -> do
      pager <- getEnv "PAGER"
      let rcshowcmd = rcValue (rcvars rst) "showcommand"
          showprog  = if not (null rcshowcmd)
                        then rcshowcmd
                        else (if null pager then "cat" else pager)
      system $ showprog ++ ' ' : fn
      putStrLn ""
      return (Just rst)

--- Process :source command
processSource :: ReplState -> String -> IO (Maybe ReplState)
processSource rst args
  | null args   = skipCommand "missing function name"
  | null dotfun = do m <- getModuleOfFunction rst args
                     if null m
                       then skipCommand "function not found"
                       else showFunctionInModule rst m args
  | otherwise   = showFunctionInModule rst mod (tail dotfun)
  where (mod, dotfun) = break (== '.') args

--- Process :type command
processType :: ReplState -> String -> IO (Maybe ReplState)
processType rst args = do
  typeok <- showTypeOfExp rst args
  return (if typeok then Just rst else Nothing)

--- Process :usedimports command
processUsedImports :: ReplState -> String -> IO (Maybe ReplState)
processUsedImports rst args = do
  let modname  = if null args then currMod rst else stripCurrySuffix args
  checkAndCallCpmTool "curry-usedimports" "importusage"
    (\toolexec -> execCommandWithPath rst toolexec [modname])

printHelpOnCommands :: IO ()
printHelpOnCommands = putStrLn $ unlines
  [ "Commands (can be abbreviated to a prefix if unique)"
  , ":load <prog>       - load program '<prog>.[l]curry' as main module"
  , ":add  <m1> .. <mn> - add modules '<m1>' to '<mn>' to currently loaded modules"
  , ":reload            - recompile currently loaded modules"
  , ":compile <prog>    - like ':load <prog>' but also compile Haskell code"
  , ":eval <expr>       - evaluate expression <expr>"
  , ":type <expr>       - show type of expression <expr>"
  , ":programs          - show names of all Curry programs available in load path"
  , ":cd <dir>          - change current directory to <dir>"
  , ":edit              - load source of currently loaded module into editor"
  , ":edit <m>          - load source of module <m> into editor"
  , ":show              - show currently loaded source program"
  , ":show <m>          - show source of module <m>"
  , ":source <f>        - show source of (visible!) function <f>"
  , ":source <m>.<f>    - show source of function <f> in module <m>"
  , ":browse            - browse program and its imported modules"
  , ":interface         - show interface of currently loaded module"
  , ":interface <m>     - show interface of module <m>"
  , ":usedimports       - show all used imported functions/constructors"
  , ":set <option>      - set an option"
  , ":set               - see help on options and current options"
  , ":save              - save executable with main expression 'main'"
  , ":save <expr>       - save executable with main expression <expr>"
  , ":fork <expr>       - fork new process evaluating <expr>"
  , ":help              - show this message"
  , ":!<command>        - execute <command> in shell"
  , ":quit              - leave the system"
  ]

--- Print all Curry programs in current load path
printAllLoadPathPrograms :: ReplState -> IO ()
printAllLoadPathPrograms rst = mapM_ printDirPrograms (loadPaths rst)
 where
  printDirPrograms dir = do
    putStrLn $ "Curry programs in directory '" ++ dir ++ "':"
    progs <- getDirPrograms "" dir
    putStrLn $ unwords $ sort $ progs
    putStrLn ""

  getDirPrograms prefix dir = do
    exdir <- doesDirectoryExist dir
    files <- if exdir then getDirectoryContents dir else return []
    subprogs <- mapM (\d -> getDirPrograms (prefix ++ d ++ ".") (dir </> d))
                     (filter (\f -> let c = head f in c>='A' && c<='Z') files)
    return $ concat subprogs ++
      concatMap (\f -> let (base, sfx) = splitExtension f
                        in if sfx `elem` [".curry", ".lcurry"] && notNull base
                             then [prefix ++ base]
                             else [])
                files

---------------------------------------------------------------------------
-- Process setting of an option
processSetOption :: ReplState -> String -> IO (Maybe ReplState)
processSetOption rst option
  | null (strip option)
  = printOptions rst >> return (Just rst)
  | otherwise
  = case matched of
      []           -> skipCommand $ "unknown option: '" ++ option ++ "'"
      [(_,act)]    -> act rst (strip args)
      _            -> skipCommand $ "ambiguous option: ':" ++ option ++ "'"
 where
  (opt, args)  = break (==' ') option
  matched      = filter (isPrefixOf (map toLower opt) . fst)
                        (replOptions rst)

replOptions :: ReplState
            -> [(String, ReplState -> String -> IO (Maybe ReplState))]
replOptions rst =
  [ ("v0"           , \r _ -> return (Just r { verbose      = 0     }))
  , ("v1"           , \r _ -> return (Just r { verbose      = 1     }))
  , ("v2"           , \r _ -> return (Just r { verbose      = 2     }))
  , ("v3"           , \r _ -> return (Just r { verbose      = 3     }))
  , ("v4"           , \r _ -> return (Just r { verbose      = 4     }))
  , ("path"         , setOptionPath                                   )
  , ("prompt"       , setPrompt                                       )
  , ("safe"         , \r _ -> return (Just r { safeExec     = True  }))
  , ("parser"       , \r a -> return (Just r { parseOpts    = a     }))
  , ("args"         , \r a -> return (Just r { rtsArgs      = a     }))
  -- , ("prelude"      , \r a -> return (Just r { preludeName  = a     }))
  , ("+time"        , \r _ -> return (Just r { showTime     = True  }))
  , ("-time"        , \r _ -> return (Just r { showTime     = False }))
  , ("+show"        , \r _ -> return (Just r { withShow     = True  }))
  , ("-show"        , \r _ -> return (Just r { withShow     = False }))
  , ("+bindings"    , \r _ -> return (Just r { showBindings = True  }))
  , ("-bindings"    , \r _ -> return (Just r { showBindings = False }))
  ] ++
  concatMap setCmpOpt (ccOpts (compiler rst))
 where
  setCmpOpt (CCOption _ _ tags) = map (setOptTag tags) tags
  setOptTag alltags (tag,co) =
    (tag,
     \r _ -> return (Just r { cmpOpts = (tag,co) :
                                     filter (`notElem` alltags) (cmpOpts r) }))

setPrompt :: ReplState -> String -> IO (Maybe ReplState)
setPrompt rst p
  | null rawPrompt = skipCommand "no prompt specified"
  | otherwise  = case head rawPrompt of
    '"' -> case reads rawPrompt of
      [(strPrompt, [])] -> return (Just rst { prompt = strPrompt })
      _                 -> skipCommand "could not parse prompt"
    _   -> return (Just rst { prompt = rawPrompt })
 where rawPrompt = strip p

------------------------------------------------------------------------------
-- Show help on options and current settings.
printOptions :: ReplState -> IO ()
printOptions rst = putStrLn $ unlines $ filter notNull
  [ "Options for ':set' command:"
  , "v<n>            - verbosity level"
  , "                  0: quiet (errors and warnings only)"
  , "                  1: show status messages (default)"
  , "                  2: show commands"
  , "                  3: show intermediate infos"
  , "                  4: show all details"
  , "path <paths>    - set additional search paths for imported modules"
  , "prompt <prompt> - set the user prompt"
  , "safe            - safe execution mode without I/O actions"
  , "parser  <opts>  - additional options passed to parser (front end)"
  , "args    <args>  - run-time arguments passed to main program"
  -- , "prelude <name>  - name of the standard prelude"
  , "+/-time         - show compilation and execution time"
  , "+/-show         - use 'Prelude.show' to show evaluation results"
  , "+/-bindings     - show bindings of free variables in initial goal"
  , showCompilerOptions (ccOpts (compiler rst))
  , showCurrentOptions rst
  ]

showCurrentOptions :: ReplState -> String
showCurrentOptions rst = intercalate "\n" $ filter notNull
  [ "\nCurrent settings:"
  , "import paths      : " ++ intercalate [searchPathSeparator] (loadPaths rst)
  , "parser options    : " ++ parseOpts rst
  , "run-time arguments: " ++ rtsArgs rst
  , "verbosity         : " ++ show (verbose rst)
  , "prompt            : " ++ show (prompt rst) ] ++
  (if verbose rst > 2
     then [ "prelude           : " ++ preludeName rst
          , "main exp module   : " ++ mainExpMod  rst
          , "verbosity option  : " ++ ccVerbOpt  (compiler rst) "VERB"
          , "parser option     : " ++ ccParseOpt (compiler rst) "OPTS"
          , "compile option    : " ++ ccCmplOpt  (compiler rst) "MOD"
          , "executable option : " ++ ccExecOpt  (compiler rst) "MOD"
          , "clean command     : " ++ ccCleanCmd (compiler rst) "MOD"
          ]
     else []) ++
  [ "Further settings:"
  , unwords $
      [ showOnOff (showBindings rst) ++ "bindings"
      , showOnOff (withShow     rst) ++ "show"
      , showOnOff (showTime     rst) ++ "time"
      ] ++ map fst (cmpOpts rst)
  ]
 where showOnOff b = if b then "+" else "-"

---------------------------------------------------------------------------
--- The default import paths of the Curry compiler.
--- It consists of the path defined by the environment variable CURRYPATH,
--- and the "libraries" property defined in ~/.<compiler>rc
defaultImportPaths :: ReplState -> IO [String]
defaultImportPaths rst = do
  currypath <- getEnv "CURRYPATH"
  let rclibs = rcValue (rcvars rst) "libraries"
  return $ filter (/= ".") $ splitSearchPath currypath ++ splitSearchPath rclibs

defaultImportPathsWith :: ReplState -> String -> IO [String]
defaultImportPathsWith rst dirs = do
  defipath <- defaultImportPaths rst
  adirs    <- mapM getAbsolutePath (splitSearchPath dirs)
  return (adirs ++ defipath)

setOptionPath :: ReplState -> String -> IO (Maybe ReplState)
setOptionPath rst args = do
  ipath <- if null args then defaultImportPaths rst
                        else defaultImportPathsWith rst args
  return (Just rst { importPaths = ipath })

---------------------------------------------------------------------------

-- Compiles a main expression by creating a "main" module.
-- If the third argument is `True`, the generated executable is
-- invoked and then removed.
compileMainExpression :: ReplState -> String -> Bool -> IO Int
compileMainExpression rst exp runrmexec = do
  if safeExec rst
    then do -- check for imports of Unsafe
      unsafeused <- importUnsafeModule rst
      if unsafeused
        then do writeErrorMsg "Import of 'Unsafe' not allowed in safe mode!"
                return 1
        else compileProgExp
    else compileProgExp
 where
  compileProgExp = do
    ecg <- generateMainExpFile
    if ecg /= 0
      then return ecg
      else do
        when (verbose rst > 3) $ do
          putStrLn "GENERATED MAIN MODULE:"
          readFile (mainExpFile rst) >>= putStrLn
        let mainexpmod = mainExpMod rst
            compilecmd = curryCompilerCommand rst ++ " " ++
                         (ccExecOpt (compiler rst)) mainexpmod
        timecompilecmd <- getTimeCmd rst "Compilation" compilecmd
        if ccCurryPath (compiler rst)
          then execCommandWithPath rst timecompilecmd [] >> return ()
          else do writeVerboseInfo rst 2 $ "Executing: " ++ timecompilecmd
                  system timecompilecmd >> return ()
        ec <- if runrmexec
                then do
                  execcmd <- getTimeCmd rst "Execution"
                               (unwords ["./" ++ mainexpmod, rtsArgs rst])
                  writeVerboseInfo rst 2 $ "Executing: " ++ execcmd
                  ecx <- system execcmd
                  removeFileIfExists mainexpmod -- remove executable
                  return ecx
                else return 0
        cleanModule rst mainexpmod
        return ec

  generateMainExpFile = do
    removeFileIfExists $ abstractCurryFileName (mainExpMod rst)
    writeSimpleMainExpFile rst exp
    getAcyOfMainExpMod rst >>=
      maybe (return 1)
            (\cprog -> insertFreeVarsInMainExp rst cprog exp >>=
                         maybe (return 1)
                               (\ (mprog,mexp) ->
                                   makeMainExpMonomorphic rst mprog mexp >>=
                                  maybe (return 1) (const (return 0))))

-- Removes a Curry module and intermediate files.
cleanModule :: ReplState -> String -> IO ()
cleanModule rst mainmod = unless keepfiles $ do
  writeVerboseInfo rst 2 $ "Executing: " ++ cleancmd
  system cleancmd
  return ()
 where
  keepfiles = rcValue (rcvars rst) "keepfiles" == "yes"
  cleancmd  = ccCleanCmd (compiler rst) mainmod

---------------------------------------------------------------------------
-- Transforming main expression into appropriate form.

-- Insert free variables occurring in the main expressions
-- as components of the main expression so that their bindings are shown.
-- The arguments are the AbstractCurry program of the main expression
-- and the main expression as a string.
-- The result is Nothing (if some error occurred) or the transformed
-- AbstractCurry program and expression.
insertFreeVarsInMainExp :: ReplState -> CurryProg -> String
                        -> IO (Maybe (CurryProg, String))
insertFreeVarsInMainExp rst
    cprog@(CurryProg _ _ _ _ _ _ [mfunc@(CFunc _ _ _ (CQualType _ ty) _)] _)
    mainexp = do
  let freevars           = freeVarsInFuncRule mfunc
      (exp, whereclause) = breakWhereFreeClause mainexp
  if (safeExec rst) && isIOType ty
    then do writeErrorMsg "Operation not allowed in safe mode!"
            return Nothing
    else
      if null freevars
          || not (showBindings rst)
          || isIOType ty
          || (not (withShow rst) && length freevars > 10) -- due to tuple limit
          || null whereclause
        then return $ Just (cprog,mainexp)
        else do
          let freevarexp = addFreeShow exp freevars whereclause
          writeVerboseInfo rst 2 $
            "Adding printing of bindings for free variables: " ++
              intercalate "," freevars
          writeVerboseInfo rst 3 $ "New expression: " ++ freevarexp
          writeSimpleMainExpFile rst freevarexp
          getAcyOfMainExpMod rst >>=
            maybe (return Nothing)
                  (\p -> return $ Just (p,freevarexp))
 where
  addFreeShow exp freevars whereclause = unwords $
    if withShow rst
      then ["((\"{\""] ++
           intersperse ("++ \", \" ")
             (map (\v-> "++ \"" ++ v ++ " = \" ++ show " ++ v) freevars) ++
           ["++ \"} \") ++) $!! (show (", exp, "))"] ++ [whereclause]
      else ["(", exp] ++
           map (\v-> ", \"" ++ v ++ ":\", " ++ v) freevars ++
           [")"] ++ [whereclause]

  freeVarsInFuncRule f = case f of
    CFunc _ _ _ _ (CRule _ rhs : _) -> freeVarsInRhs rhs
    _ -> error "REPL.insertFreeVarsInMainGoal.freeVarsInFuncRule"

  freeVarsInRhs rhs = case rhs of
    CSimpleRhs  _ ldecls -> concatMap lvarName ldecls
    CGuardedRhs _ ldecls -> concatMap lvarName ldecls

  lvarName ldecl = case ldecl of CLocalVars vs -> map snd vs
                                 _             -> []

-- Breaks a main expression into an expression and a where...free clause.
-- If the where clause is not present, this part is empty.
breakWhereFreeClause :: String -> (String,String)
breakWhereFreeClause exp =
  let revexp = reverse exp
  in if take 4 revexp == "eerf"
       then let woWhere = findWhere (drop 4 revexp)
            in if null woWhere
                 then (exp,"")
                 else (reverse woWhere, drop (length woWhere) exp)
       else (exp,"")
 where
  findWhere [] = []
  findWhere (c:cs) | isSpace c && take 6 cs == "erehw " = drop 6 cs
                   | otherwise                          = findWhere cs


--- If the main expression is polymorphic, make it monomorphic by adding a type
--- declaration where type variables are replaced by type "()". Before,
--- type variables with a numeric constraint like "Num"/"Integral" or
--- "Fractional" are defaulted to the types "Int" or "Float", respectively.
--- The type of the main expression is only allowed to contain
--- numeric constraints.
--- If the main exp has type "IO t" where t is monomorphic, t /= (),
--- and t is not a function, then ">>= print" is added to the expression.
--- The arguments are the AbstractCurry program of the main expression
--- and the main expression as a string.
--- The result is Nothing (if some error occurred) or the transformed
--- AbstractCurry program and expression.
makeMainExpMonomorphic :: ReplState -> CurryProg -> String
                       -> IO (Maybe (CurryProg, String))
makeMainExpMonomorphic rst prog exp = case prog of
  CurryProg _ _ _ _ _ _ [CFunc _ _ _ qty _] _ -> makeMonoType qty
  _ -> error "REPL.makeMainExpMonomorphic"
 where
  makeMonoType qty@(CQualType _ ty)
    | isFunctionalType ty
    = do writeErrorMsg "expression is of functional type"
         return Nothing
    | isPolyType ty
    = case defaultQualTypeExpr qty of
        CQualType (CContext []) defTy -> do
          when (defTy /= ty) $ writeVerboseInfo rst 2 $
            "Defaulted type of main expression: " ++
            showMonoTypeExpr False defTy
          let mtype = showMonoTypeExpr True defTy
              (nwexp, whereclause) = breakWhereFreeClause exp
              mexp  = "(" ++ nwexp ++ " :: " ++ mtype ++ ") " ++ whereclause
          writeMainExpFile rst (modsOfType defTy) (Just mtype) mexp
          when (isPolyType defTy) $ writeVerboseInfo rst 2 $
            "Type of main expression \"" ++ showMonoTypeExpr False defTy
            ++ "\" made monomorphic by replacing type variables by \"()\""
          getAcyOfMainExpMod rst >>=
            maybe (return Nothing)
                  (\p -> return $ Just (p,mexp))
        _ -> do writeErrorMsg $
                  "cannot handle overloaded top-level expression of type: " ++
                  showMonoQualTypeExpr False qty
                return Nothing
    | otherwise
    = if newexp == exp
        then return $ Just (prog,exp)
        else do writeSimpleMainExpFile rst newexp
                getAcyOfMainExpMod rst >>=
                  maybe (return Nothing)
                        (\p -> return $ Just (p,newexp))
   where
    newexp = if isIOReturnType ty
               then '(' : exp ++ ") Prelude.>>= Prelude.print"
               else exp

-- Defaults type variables with a numeric constraint like "Num"/"Integral" or
-- "Fractional" to the types "Int" or "Float", respectively. Moreover,
-- existing "Data", "Eq", "Ord", "Read", and "Show" constraints for the same
-- type variable are removed.
-- Moreover, remaing type variables with "Data" constraints are
-- defaultet to "Prelude.Bool".
defaultQualTypeExpr :: CQualTypeExpr -> CQualTypeExpr
defaultQualTypeExpr (CQualType (CContext ctxt) cty) =
  defaultData (defaultTExp ctxt (CQualType (CContext []) cty))
 where
  defaultData qty@(CQualType (CContext dctxt) dcty) = case dctxt of
    [] -> qty
    (qtcons, CTVar tv) : cs | qtcons == ("Prelude","Data")
      -> defaultData (CQualType (CContext cs)
                        (substTypeVar tv (CTCons ("Prelude","Bool")) dcty))
    _ -> qty

  defaultTExp :: [CConstraint] -> CQualTypeExpr -> CQualTypeExpr
  defaultTExp []     qty                           = qty
  defaultTExp (c:cs) (CQualType (CContext cs2) ty) = case c of
    (("Prelude", ptype), CTVar tv) ->
      if ptype `elem` ["Num", "Integral", "Fractional"]
        then let defptype = if ptype == "Fractional" then "Float" else "Int"
             in defaultTExp
                  (removeConstraints tv defptype cs)
                  (CQualType (CContext (removeConstraints tv defptype cs2))
                     (substTypeVar tv (CTCons ("Prelude", defptype)) ty))
        else defaultTExp cs (CQualType (CContext (cs2 ++ [c])) ty)
    _ -> defaultTExp cs (CQualType (CContext (cs2 ++ [c])) ty)

  removeConstraints _  _        []       = []
  removeConstraints tv dflttype (c3:cs3) = case c3 of
    (("Prelude", cls), CTVar tv2)
      | tv == tv2 && cls `elem` ["Data", "Eq", "Ord", "Read", "Show"]
      -> removeConstraints tv dflttype cs3
      | tv == tv2 && dflttype == "Int" && cls == "Enum"
      -> removeConstraints tv dflttype cs3
    _ -> c3 : removeConstraints tv dflttype cs3

-- Replaces a type variable with a type expression.
substTypeVar :: CTVarIName -> CTypeExpr -> CTypeExpr -> CTypeExpr
substTypeVar tv def te@(CTVar      tv2) = if tv == tv2 then def else te
substTypeVar _  _   te@(CTCons       _) = te
substTypeVar tv def (CFuncType te1 te2) =
  CFuncType (substTypeVar tv def te1) (substTypeVar tv def te2)
substTypeVar tv def (CTApply   te1 te2) =
  CTApply (substTypeVar tv def te1) (substTypeVar tv def te2)

---------------------------------------------------------------------------

-- Parse a Curry program to detect errors (for load/reload command):
parseCurryProgram :: ReplState -> String -> IO Int
parseCurryProgram rst curryprog = do
  let frontendparams = currentFrontendParams rst (verbose rst == 0)
      target         = if ccTypedFC (compiler rst) then TFCY else FCY
  catch (callFrontendWithParams target frontendparams curryprog >> return 0)
        (\_ -> return 1)

-- Compile a Curry program with the Curry compiler:
compileCurryProgram :: ReplState -> String -> IO (Maybe ReplState)
compileCurryProgram rst curryprog = do
  let compilecmd = curryCompilerCommand rst ++ " " ++
                   (ccCmplOpt (compiler rst)) curryprog
  timecompilecmd <- getTimeCmd rst "Compilation" compilecmd
  if ccCurryPath (compiler rst)
    then execCommandWithPath rst timecompilecmd []
    else do writeVerboseInfo rst 2 $ "Executing: " ++ timecompilecmd
            system timecompilecmd
            return $ Just rst

-- Generate the base command to call the Curry compiler:
curryCompilerCommand :: ReplState -> String
curryCompilerCommand rst = unwords [ccExec (compiler rst), cmpopts]
 where
  cmpopts = unwords $
    [ -- pass current value of "bindingoptimization" property to compiler:
      -- "-Dbindingoptimization=" ++ rcValue (rcvars rst) "bindingoptimization"
      (ccVerbOpt (compiler rst)) (show (transVerbose (verbose rst)))
    ] ++
    (if ccCurryPath (compiler rst)
       then []
       else map ("-i" ++) (loadPaths rst)) ++
    filter notNull (map snd (cmpOpts rst)) ++
    (if null (parseOpts rst)
      then []
      else [(ccParseOpt (compiler rst)) (parseOpts rst)])

  transVerbose n | n == 0    = 0
                 | otherwise = n - 1

--- Extract a module name, possibly prefixed by a path, from an argument,
--- or return the current module name if the argument is the empty string.
getModuleName :: ReplState -> String -> IO String
getModuleName rst args =
  if null args
  then return (currMod rst)
  else let (dirname, mname) = splitFileName (stripCurrySuffix args)
        in if dirname == "./"
           then return mname
           else getAbsolutePath (stripCurrySuffix args)

---------------------------------------------------------------------------
-- Operations to call auxiliary tools.

-- Showing source code of functions via SourcProgGUI tool.
-- If necessary, open a new connection and remember it in the repl state.
showFunctionInModule :: ReplState -> String -> String -> IO (Maybe ReplState)
showFunctionInModule rst mod fun =
  checkForWish $
  checkAndCallCpmTool "curry-showsource" "sourceproggui" $ \spgui -> do
    writeVerboseInfo rst 1 $
      "Showing source code of function '" ++ mod ++ "." ++ fun ++
      "' in separate window..."
    let spguicmd = "CURRYPATH=" ++
                   intercalate [searchPathSeparator] (loadPaths rst) ++
                   " && export CURRYPATH && " ++ spgui ++ " " ++ mod
    writeVerboseInfo rst 2 $ "Executing: " ++ spguicmd
    (rst',h') <- maybe (do h <- connectToCommand spguicmd
                           return (rst {sourceguis = (mod,(fun,h))
                                                     : sourceguis rst }, h))
                       (\ (f,h) -> do
                           hPutStrLn h ('-':f)
                           hFlush h
                           return (rst {sourceguis = updateFun (sourceguis rst)
                                                               mod fun }, h))
                       (lookup mod (sourceguis rst))
    hPutStrLn h' ('+':fun)
    hFlush h'
    return (Just rst')
 where
  updateFun []                _  _  = []
  updateFun ((m,(f,h)):sguis) md fn =
    if m==md then (m,(fn,h)):sguis
             else (m,(f,h)) : updateFun sguis md fn

-- Check whether some CPM tool is available, i.e., either in the current
-- path or in the CPM bin directory. If it is not available,
-- skip the command with an error message how to install the tool from
-- the package (specified in the second argument). Otherwise, continue with
-- the last argument by passing the name of the CPM tool.
checkAndCallCpmTool :: String -> String -> (String -> IO (Maybe ReplState))
                    -> IO (Maybe ReplState)
checkAndCallCpmTool tool package continue = do
  excmd <- system $ "which " ++ tool ++ " > /dev/null"
  if excmd == 0
    then continue tool
    else do homedir <- getHomeDirectory
            let cpmtoolfile = homedir </> ".cpm" </> "bin" </> tool
            excpm <- doesFileExist cpmtoolfile
            if excpm
              then continue cpmtoolfile
              else skipCommand errmsg
 where
  errmsg = "'" ++ tool ++ "' not found. Install it by: 'cypm install " ++
           package ++ "'!"

-- Execute some command (first argument) with some arguments (second argument).
-- The current load path is exported to the command via the environment
-- variable CURRYPATH.
execCommandWithPath :: ReplState -> String -> [String]
                    -> IO (Maybe ReplState)
execCommandWithPath rst cmd args = do
  let setpath = "CURRYPATH=" ++
                intercalate [searchPathSeparator] (loadPaths rst) ++
                " && export CURRYPATH && "
      syscmd = setpath ++ cmd ++ ' ' : unwords args
  writeVerboseInfo rst 2 $ "Executing: " ++ syscmd
  system syscmd >> return (Just rst)

-- Check whether some system command is available. If it is not available,
-- skip the command with the given error message, otherwise continue with
-- the last argument.
checkForCommand :: String -> String -> IO (Maybe ReplState)
                -> IO (Maybe ReplState)
checkForCommand cmd errmsg continue = do
  excmd <- system $ "which " ++ cmd ++ " > /dev/null"
  if (excmd>0) then skipCommand errmsg
               else continue

-- Check whether the windowing shell "wish" is available.
checkForWish :: IO (Maybe ReplState) -> IO (Maybe ReplState)
checkForWish =
  checkForCommand "wish"
    "Windowing shell `wish' not found. Please install package `tk'!"

-- Terminate all open SourceProgGUIs
terminateSourceProgGUIs :: ReplState -> IO ReplState
terminateSourceProgGUIs rst
  | null sguis = return rst
  | otherwise  = do
      writeVerboseInfo rst 1 "Terminating source program GUIs..."
      catch (mapM_ (\ (_,(_,h)) -> hPutStrLn h "q" >> hFlush h >> hClose h)
                   sguis)
            (\_ -> return ())
      return rst { sourceguis = [] }
 where sguis = sourceguis rst

---------------------------------------------------------------------------

------------------------------------------------------------------------------
--- Some operations to handle the REPL resource configuration file
--- that is stored in $HOME/.<compiler-name>rc
---
--- @author  Michael Hanus
--- @version April 2021
------------------------------------------------------------------------------

module REPL.RCFile
  ( readRC, rcValue, setRCProperty, extractRCArgs, updateRCDefs )
 where

import Control.Monad     ( unless )
import Data.Char         ( toLower, isSpace )
import Data.List         ( isPrefixOf, partition, sort )

import Data.PropertyFile
import System.FilePath   ( FilePath, (</>), (<.>) )
import System.Directory  ( getHomeDirectory, doesFileExist, copyFile
                         , renameFile )
import REPL.Compiler
import REPL.PkgConfig    ( packagePath )
import REPL.Utils        ( strip )

--- Returns the location of the default rc template file.
--- If the Curry compiler has its own, return this one, otherwise
--- return the template from this package.
getDefaultRC :: CCDescription -> IO FilePath
getDefaultRC cd = do
  let cmprc = ccHome cd </> ccName cd ++ "rc.default"
  excmprc <- doesFileExist cmprc
  if excmprc then return cmprc
             else return (packagePath </> "curryrc.default")

--- Location of the rc file of a user.
--- After bootstrapping, one can also use Distribution.rcFileName
--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: CCDescription -> IO FilePath
rcFileName cd = (</> "." ++ ccName cd ++ "rc") `fmap` getHomeDirectory

--- Reads the rc file. If it is not present, the standard file
--- from the distribution will be copied.
readRC :: CCDescription -> IO [(String, String)]
readRC cd = do
  rcname <- rcFileName cd
  rcdefname <- getDefaultRC cd
  rcexists  <- doesFileExist rcname
  catch (if rcexists
           then updateRC cd rcdefname
           else do putStrLn $ "Installing '" ++ rcname ++ "'..."
                   copyFile rcdefname rcname)
        (const $ return ())
  readPropertyFile rcname

rcKeys :: [(String, String)] -> [String]
rcKeys = sort . map fst

--- Reads the rc file (which must be present) and compares the definitions
--- with the distribution rc file. If the set of variables is different,
--- update the rc file with the distribution but keep the user's definitions.
updateRC :: CCDescription -> String -> IO ()
updateRC cd defaultrc = do
  rcname    <- rcFileName cd
  userprops <- readPropertyFile rcname
  distprops <- readPropertyFile defaultrc
  unless (rcKeys userprops == rcKeys distprops) $ do
    putStrLn $ "Updating '" ++ rcname ++ "'..."
    renameFile rcname $ rcname <.> "bak"
    copyFile defaultrc rcname
    mapM_ (\ (n, v) -> maybe (return ())
             (\uv -> unless (uv == v) $ updatePropertyFile rcname n uv)
             (lookup n userprops))
          distprops

--- Sets a property in the rc file.
setRCProperty :: CCDescription -> String -> String -> IO ()
setRCProperty cd pname pval = do
  readRC cd -- just be to sure that rc file exists and is up-to-date
  rcname <- rcFileName cd
  updatePropertyFile rcname pname pval

--- Look up a configuration variable in the list of variables from the rc file.
--- Uppercase/lowercase is ignored for the variable names and the empty
--- string is returned for an undefined variable.
rcValue :: [(String, String)] -> String -> String
rcValue rcdefs var = strip $ maybe "" id $
  lookup (map toLower var) (map (first (map toLower)) rcdefs)
 where
  first f (x, y) = (f x, y)

--- Extract from a list of command-line arguments rc properties
--- of the from "-Dprop=val" and return the remaining arguments
--- and the extracted properties.
extractRCArgs :: [String] -> ([String],[(String,String)])
extractRCArgs args =
  let (dargs,otherargs) = partition ("-D" `isPrefixOf`) args
  in (otherargs, map splitDefs (map (drop 2) dargs))
 where
  splitDefs darg = case break (== '=') darg of
    (var,_:val) -> (var,val)
    _           -> (darg,"")

--- Update list of rc properties w.r.t. a list new properties.
updateRCDefs :: [(String,String)] -> [(String,String)] -> [(String,String)]
updateRCDefs orgdefs newdefs =
  map (\ (name,val) -> (name, maybe val id (lookup name newdefs))) orgdefs

------------------------------------------------------------------------------


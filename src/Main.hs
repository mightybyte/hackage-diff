
{-# LANGUAGE   LambdaCase
             , ScopedTypeVariables
             , OverloadedStrings
             , RecordWildCards #-}

module Main (main) where

import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Except
import Text.Printf
import HackageDiff

main :: IO ()
main = do
    -- Process command line arguments
    (pkgName, argVerA, argVerB, flags) <-
        runExcept <$> (getCmdOpt <$> getProgName <*> getArgs) >>= either die return
    mode <- case foldr (\f r -> case f of FlagMode m -> m; _ -> r) "downloaddb" flags of
                "downloaddb" -> return ModeDownloadDB
                "builddb"    -> return ModeBuildDB
                "parsehs"    -> return ModeParseHS
                m            -> die $ printf "'%s' is not a valid mode" m
    let disableColor = FlagDisableColor `elem` flags
        silentFlag   = FlagSilent       `elem` flags
    ediff <- packageDiff (DiffConfig mode silentFlag) (PackageToDiff pkgName argVerA argVerB)
    case ediff of
      Left e -> die e
      Right d -> outputDiff d disableColor silentFlag

data CmdFlag = FlagDisableColor | FlagSilent | FlagMode String
               deriving (Eq)

getCmdOpt :: String -> [String] -> Except String (String, String, String, [CmdFlag])
getCmdOpt prgName args =
    case getOpt RequireOrder opt args of
        (flags, (pkgName:verA:verB:[]), []) -> return (pkgName, verA, verB, flags)
        (_, _, [])                          -> throwError usage
        (_, _, err)                         -> throwError (concat err ++ "\n" ++ usage)
  where
    header =
      "hackage-diff | Compare the public API of different versions of a Hackage library\n" ++
      "github.com/blitzcode/hackage-diff | www.blitzcode.net | (C) 2016 Tim C. Schroeder\n\n" ++
      "Usage: " ++ prgName ++ " [options] <package-name> <old-version|path> <new-version|path>"
    footer =
      "\nExamples:\n" ++
      "  " ++ prgName ++ " mtl 2.1 2.2.1\n" ++
      "  " ++ prgName ++ " --mode=builddb JuicyPixels 3.1.4.1 3.1.5.2\n" ++
      "  " ++ prgName ++ " conduit 1.1.5 ~/tmp/conduit-1.1.6/dist/doc/html/conduit/conduit.txt\n" ++
      "  " ++ prgName ++ " --mode=parsehs QuickCheck 2.6 2.7.6\n" ++
      "  " ++ prgName ++ " --mode=parsehs -s Cabal ~/tmp/Cabal-1.18.0/ 1.20.0.0\n"
    usage  = usageInfo header opt ++ footer
    opt    = [ Option []
                      ["mode"]
                      (ReqArg FlagMode "[downloaddb|builddb|parsehs]")
                      ( "what to download / read, how to compare\n" ++
                        "  downloaddb - download Hoogle DBs and diff (Default)\n" ++
                        "  builddb    - download packages, build Hoogle DBs and diff\n" ++
                        "  parsehs    - download packages, directly diff .hs exports"
                      )
             , Option ['c']
                      ["disable-color"]
                      (NoArg FlagDisableColor)
                      "disable color output"
             , Option ['s']
                      ["silent"]
                      (NoArg FlagSilent)
                      "disable progress output"
             ]

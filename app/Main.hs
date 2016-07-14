--Copyright (c) 2015 Siddharth Bhat

--Permission is hereby granted, free of charge, to any person obtaining
--a copy of this software and associated documentation files (the "Software")
--to deal in the Software without restriction, including without limitation the
--rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
--sell copies of the Software, and to permit persons to whom the Software is
--furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall
-- be included in all copies or substantial portions of the Software.

--THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
--OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
--OTHER DEALINGS IN THE SOFTWARE.


#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Monad
import Data.Traversable
import Data.Maybe
import Data.List

import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding

import Options.Applicative
import Filesystem.Path.CurrentOS as Path

import qualified Turtle
--import qualified Data.Configurator as Config

import qualified Data.Aeson as JSON
import Data.Aeson ((.=), (.:))

import qualified Data.ByteString.Lazy as B

import qualified System.Console.ANSI as ANSI


import Debug.Trace

-- options passed to 'tp list'
data ListOptions = ListOptions deriving (Show)

-- options pased to 'tp add'
data AddOptions = AddOptions {
    addname :: String,
    folderPath :: FilePath
} deriving (Show)

-- options passed to 'tp remove'
data RemoveOptions = RemoveOptions {
    removename :: String
} deriving (Show)

-- options parrsed to 'tp goto'
data GotoOptions = GotoOptions {
    gotoname :: String
} deriving(Show)
-- the combined datatype representing all tp commands
data Command = CommandList ListOptions |
               CommandAdd AddOptions |
               CommandRemove RemoveOptions |
               CommandGoto GotoOptions
    deriving (Show)

-- an abstract entity representing a point to which we can tp to
data TpPoint = TpPoint {
    name :: String,
    absFolderPath :: String
} deriving (Show)


-- the main data that is loaded from JSON 
data TpData = TpData {
    tpPoints :: [TpPoint]
} deriving (Show)

defaultTpData :: TpData
defaultTpData = TpData {
    tpPoints = []
}

-- flip function for ease of chaining computations
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


filePathToString :: FilePath -> String
filePathToString = Path.encodeString

tpProgDesc :: String
tpProgDesc = "use teleport to quickly setup teleport points and move to these " ++
               "when needed"

tpHeader :: String
tpHeader = "Teleport: move around your filesystem"



-- | A version of 'execParser' which shows full help on error.                
--                                                                            
-- The regular 'execParser' only prints usage on error, which doesn't         
-- include the options, subcommands, or mention of the help switch            
-- @--help@.                                                                  
showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO ()
main = do 
    command <- showHelpOnErrorExecParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc tpProgDesc <>
                        header tpHeader))
    run command

-- Data Loading
-- """"""""""""

-- parse tpPoint
instance JSON.FromJSON TpPoint where
     parseJSON (JSON.Object v) =
        TpPoint <$> v .: "name"
                  <*> v .: "absFolderPath"

instance JSON.ToJSON TpPoint where
    toJSON (TpPoint {..}) = 
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]

-- parse tpData
instance JSON.FromJSON TpData where
    parseJSON (JSON.Object v) =
        TpData <$> v .: "tpPoints"

instance JSON.ToJSON TpData where
    toJSON(TpData{..}) = 
        JSON.object ["tpPoints" .= tpPoints]

dieJSONParseError :: FilePath -> String -> IO TpData
dieJSONParseError jsonFilePath err = 
    ("parse error in: " ++ (show jsonFilePath) ++
    "\nerror:------\n" ++ err) |>
    T.pack |>
    Turtle.die

decodeTpData :: FilePath -> IO TpData
decodeTpData jsonFilePath = do
    rawInput <- B.readFile (filePathToString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput  

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json

createTpDataFile :: FilePath -> IO ()
createTpDataFile jsonFilePath = saveTpData jsonFilePath defaultTpData

loadTpData :: FilePath -> IO TpData
loadTpData jsonFilePath = do
    exists <- (Turtle.testfile jsonFilePath)
    if exists then
        decodeTpData jsonFilePath
    else
       do
           createTpDataFile jsonFilePath
           return defaultTpData

saveTpData :: FilePath -> TpData -> IO ()
saveTpData jsonFilePath tpData = do
    let dataBytestring = JSON.encode tpData
    Turtle.touch jsonFilePath
    B.writeFile (filePathToString jsonFilePath) dataBytestring


getTpDataPath :: IO FilePath
getTpDataPath = do
    homeFolder <- Turtle.home
    return $ homeFolder </> ".tpdata"
-- Common parsers
-- """"""""""""""
readFolderPath :: String -> ReadM FilePath
readFolderPath s = T.pack s |> 
                 Path.fromText |> 
                 (\path -> if Path.valid path
                     then return path
                     else readerError ("invalid path: " ++ (show path)))


tpnameParser :: Parser String
tpnameParser = argument str
                  (metavar "NAME" <>
                  help "name of the teleport point for usage")


-- Command parsers
-- """""""""""""""

parseAddCommand :: Parser Command
parseAddCommand =  
    CommandAdd <$> (AddOptions <$>  tpnameParser <*> folderParser) where
        folderParser = argument
                     (str >>= readFolderPath)
                     (value "./"  <>
                      metavar "FOLDERPATH" <>
                      help "path of the teleport folder to teleport to. By default, taken as current working directory")

parseListCommand :: Parser Command
parseListCommand = pure (CommandList ListOptions)

parseRemoveCommand :: Parser Command
parseRemoveCommand = CommandRemove <$> (RemoveOptions <$> tpnameParser)

parseGotoCommand :: Parser Command
parseGotoCommand = CommandGoto <$> (GotoOptions <$> tpnameParser)

parseCommand :: Parser Command
parseCommand = subparser
    -- add command
    ((command "add" (info (helper <*> parseAddCommand) (fullDesc <> progDesc "add a teleport point"))) <>
    -- list command
    (command "list"
        (info (helper <*> parseListCommand) (fullDesc <> progDesc "list all teleport points"))) <>
    -- remove command
    (command "remove"
        (info (helper <*> parseRemoveCommand) (fullDesc <>progDesc "remove a teleport point"))) <>
    -- goto command
    (command "goto"
        (info (helper <*> parseGotoCommand) (fullDesc <> progDesc "go to a created teleport point"))))

-- Stream Helpers
-- """"""""""""""

setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]    


tpPointPrint :: TpPoint -> IO ()
tpPointPrint tpPoint = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]    
    putStr (name tpPoint)
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr (absFolderPath tpPoint)
    putStr "\n"

-- Add command runner
-- """"""""""""""""""

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path =
    setErrorColor >> 
    ("unable to find folder: " ++ (show path)) |>
    T.pack |>
    Turtle.die

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = 
    setErrorColor >>
    ("expected folder, not file: " ++ (show path)) |>
    T.pack |>
    Turtle.die

dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = 
    do
        folderExists <- Turtle.testdir path
        fileExists <- Turtle.testfile path
        -- error checking
        when fileExists (needFolderNotFileError path)
        unless folderExists (folderNotFoundError path)
       -- we know the folder exists

dieTpPointExists :: TpPoint -> IO ()
dieTpPointExists tpPoint  =  do
    setErrorColor
    putStrLn ("teleport point " ++ (name tpPoint) ++ " already exists:\n")
    tpPointPrint tpPoint
    Turtle.die ""

runAdd :: AddOptions -> IO ()
runAdd AddOptions{..} = do
    dieIfFolderNotFound folderPath
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath
    absFolderPath <- Turtle.realpath folderPath
    
    let existingTpPoint = find (\tp -> name tp == addname) (tpPoints tpData)
    case existingTpPoint of
        Just tpPoint -> dieTpPointExists tpPoint
        Nothing -> do
                        let newTpPoint = TpPoint {
                            name = addname,
                            absFolderPath = filePathToString absFolderPath
                        }
                      
                        putStrLn "creating teleport point: \n"
                        tpPointPrint newTpPoint

                        let newTpData = TpData {
                             tpPoints =  newTpPoint:(tpPoints tpData)   
                        }
                        

                        saveTpData tpDataPath newTpData
    
-- List Command
-- """"""""""""


runList :: ListOptions -> IO ()
runList ListOptions = do
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath
    let num_points = length $ tpPoints tpData
    putStr "teleport points: "

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr $ "(total " <> (show num_points) <>  ")\n"
    forM_ (tpPoints tpData) tpPointPrint
    

-- Remove Command
-- """""""""""""""

dieTpPointNotFound :: String ->IO ()
dieTpPointNotFound name = 
    setErrorColor >>
    (name ++ " tp point not found") |>
    T.pack |>
    Turtle.die

runRemove :: RemoveOptions -> IO ()
runRemove RemoveOptions{..} = do
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath

    let wantedTpPoint = find (\tp -> name tp == removename) (tpPoints tpData)
    case wantedTpPoint of
        Nothing -> dieTpPointNotFound removename
        Just _ ->  do
                    let newTpPoints = filter (\tp -> name tp /= removename)
                                               (tpPoints tpData)
                    let newTpData = tpData {
                        tpPoints = newTpPoints
                    }

                    saveTpData tpDataPath newTpData
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]    
                    putStr "removed teleport point ["
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
                    putStr removename
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]    
                    putStr "]"
                    

runGoto :: GotoOptions -> IO ()
runGoto GotoOptions{..} = do
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath
    
    let wantedTpPoint = find (\tp -> name tp == gotoname) (tpPoints tpData)
    case wantedTpPoint of
        Nothing -> dieTpPointNotFound gotoname
        Just tpPoint -> do
                             Turtle.echo (T.pack (absFolderPath tpPoint))
                             Turtle.exit (Turtle.ExitFailure 2) 
      
run :: Command -> IO ()
run command = 
    case command of
        CommandAdd addOpt -> runAdd addOpt
        CommandList listOpt -> runList listOpt
        CommandRemove removeOpt -> runRemove removeOpt
        CommandGoto gotoOpt -> runGoto gotoOpt
        other @ _ -> print other

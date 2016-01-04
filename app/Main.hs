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

-- options passed to 'warp list'
data ListOptions = ListOptions deriving (Show)

-- options pased to 'warp add'
data AddOptions = AddOptions {
    folderPath :: FilePath,
    addname :: String
} deriving (Show)

-- options passed to 'warp remove'
data RemoveOptions = RemoveOptions {
    removename :: String
} deriving (Show)

-- options parrsed to 'warp goto'
data GotoOptions = GotoOptions {
    gotoname :: String
} deriving(Show)
-- the combined datatype representing all warp commands
data Command = CommandList ListOptions |
               CommandAdd AddOptions |
               CommandRemove RemoveOptions |
               CommandGoto GotoOptions
    deriving (Show)

-- an abstract entity representing a point to which we can warp to
data WarpPoint = WarpPoint {
    name :: String,
    absFolderPath :: String
} deriving (Show)


-- the main data that is loaded from JSON 
data WarpData = WarpData {
    warpPoints :: [WarpPoint]
} deriving (Show)

defaultWarpData :: WarpData
defaultWarpData = WarpData {
    warpPoints = []
}

-- flip function for ease of chaining computations
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


filePathToString :: FilePath -> String
filePathToString = Path.encodeString

warpProgDesc :: String
warpProgDesc = "use warp to quickly setup warp points and move to these " ++
               "when needed"

warpHeader :: String
warpHeader = "Warp: move around your filesystem"


main :: IO ()
main = do 
    command <- execParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc warpProgDesc <>
                        header warpHeader))
    run command

-- Data Loading
-- """"""""""""

-- parse warpPoint
instance JSON.FromJSON WarpPoint where
     parseJSON (JSON.Object v) =
        WarpPoint <$> v .: "name"
                  <*> v .: "absFolderPath"

instance JSON.ToJSON WarpPoint where
    toJSON (WarpPoint {..}) = 
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]

-- parse warpData
instance JSON.FromJSON WarpData where
    parseJSON (JSON.Object v) =
        WarpData <$> v .: "warpPoints"

instance JSON.ToJSON WarpData where
    toJSON(WarpData{..}) = 
        JSON.object ["warpPoints" .= warpPoints]

dieJSONParseError :: FilePath -> String -> IO WarpData
dieJSONParseError jsonFilePath err = 
    ("parse error in: " ++ (show jsonFilePath) ++
    "\nerror:------\n" ++ err) |>
    T.pack |>
    Turtle.die

decodeWarpData :: FilePath -> IO WarpData
decodeWarpData jsonFilePath = do
    rawInput <- B.readFile (filePathToString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput  

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json

createWarpDataFile :: FilePath -> IO ()
createWarpDataFile jsonFilePath = saveWarpData jsonFilePath defaultWarpData

loadWarpData :: FilePath -> IO WarpData
loadWarpData jsonFilePath = do
    exists <- (Turtle.testfile jsonFilePath)
    if exists then
        decodeWarpData jsonFilePath
    else
       do
           createWarpDataFile jsonFilePath
           return defaultWarpData

saveWarpData :: FilePath -> WarpData -> IO ()
saveWarpData jsonFilePath warpData = do
    let dataBytestring = JSON.encode warpData
    Turtle.touch jsonFilePath
    B.writeFile (filePathToString jsonFilePath) dataBytestring


getWarpDataPath :: IO FilePath
getWarpDataPath = do
    homeFolder <- Turtle.home
    return $ homeFolder </> ".warpdata"
-- Common parsers
-- """"""""""""""
readFolderPath :: String -> ReadM FilePath
readFolderPath s = T.pack s |> 
                 Path.fromText |> 
                 (\path -> if Path.valid path
                     then return path
                     else readerError ("invalid path: " ++ (show path)))


warpnameParser :: Parser String
warpnameParser = argument str
                  (metavar "NAME" <>
                  help "name of the warp point for usage")


-- Command parsers
-- """""""""""""""

parseAddCommand :: Parser Command
parseAddCommand =  
    CommandAdd <$> (AddOptions <$> folderParser <*> warpnameParser) where
        folderParser = option
                     (str >>= readFolderPath)
                     --(long "path" <>
                      --short 'p' <>
                      (value "./"  <>
                      metavar "FOLDERPATH" <>
                      help "path of the warp folder to warp to")

parseListCommand :: Parser Command
parseListCommand = pure (CommandList ListOptions)

parseRemoveCommand :: Parser Command
parseRemoveCommand = CommandRemove <$> (RemoveOptions <$> warpnameParser)

parseGotoCommand :: Parser Command
parseGotoCommand = CommandGoto <$> (GotoOptions <$> warpnameParser)

parseCommand :: Parser Command
parseCommand = subparser 
    -- add command
    ((command "add" (info parseAddCommand (progDesc "add a warp point"))) <>
    -- list command
    (command "list"
        (info parseListCommand (progDesc "list all warp points"))) <>
    -- remove command
    (command "remove"
        (info parseRemoveCommand (progDesc "remove a warp point"))) <>
    -- goto command
    (command "goto"
        (info parseGotoCommand (progDesc "go to a created warp point"))))

-- Stream Helpers
-- """"""""""""""

setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]    


warpPointPrint :: WarpPoint -> IO ()
warpPointPrint warpPoint = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]    
    putStr (name warpPoint)
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr (absFolderPath warpPoint)
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

dieWarpPointExists :: WarpPoint -> IO ()
dieWarpPointExists warpPoint  =  do
    setErrorColor
    putStrLn ("warp point " ++ (name warpPoint) ++ " already exists:\n")
    warpPointPrint warpPoint
    Turtle.die ""

runAdd :: AddOptions -> IO ()
runAdd AddOptions{..} = do
    dieIfFolderNotFound folderPath
    print "yay, folder exists"
    print "loding warp data"
    
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath
    absFolderPath <- Turtle.realpath folderPath
    
    let existingWarpPoint = find (\warp -> name warp == addname) (warpPoints warpData)
    case existingWarpPoint of
        Just warpPoint -> dieWarpPointExists warpPoint
        Nothing -> do
                        let newWarpPoint = WarpPoint {
                            name = addname,
                            absFolderPath = filePathToString absFolderPath
                        }
                      
                        putStrLn "creating warp point: \n"
                        warpPointPrint newWarpPoint

                        let newWarpData = WarpData {
                             warpPoints =  newWarpPoint:(warpPoints warpData)   
                        }
                        

                        saveWarpData warpDataPath newWarpData
    
-- List Command
-- """"""""""""


runList :: ListOptions -> IO ()
runList ListOptions = do
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath

    forM_ (warpPoints warpData) warpPointPrint
    

-- Remove Command
-- """""""""""""""

dieWarpPointNotFound :: String ->IO ()
dieWarpPointNotFound name = 
    setErrorColor >>
    (name ++ " warp point not found") |>
    T.pack |>
    Turtle.die

runRemove :: RemoveOptions -> IO ()
runRemove RemoveOptions{..} = do
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath


    let wantedWarpPoint = find (\warp -> name warp == removename) (warpPoints warpData)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound removename
        Just _ ->  do
                    let newWarpPoints = filter (\warp -> name warp /= removename)
                                               (warpPoints warpData)
                    let newWarpData = warpData {
                        warpPoints = newWarpPoints
                    }

                    saveWarpData warpDataPath newWarpData

runGoto :: GotoOptions -> IO ()
runGoto GotoOptions{..} = do
    warpDataPath <- getWarpDataPath
    warpData <- loadWarpData warpDataPath
    
    let wantedWarpPoint = find (\warp -> name warp == gotoname) (warpPoints warpData)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound gotoname
        Just warpPoint -> do
                             Turtle.echo (T.pack (absFolderPath warpPoint))
                             Turtle.exit (Turtle.ExitFailure 2) 
      
run :: Command -> IO ()
run command = 
    case command of
        CommandAdd addOpt -> runAdd addOpt
        CommandList listOpt -> runList listOpt
        CommandRemove removeOpt -> runRemove removeOpt
        CommandGoto gotoOpt -> runGoto gotoOpt
        other @ _ -> print other

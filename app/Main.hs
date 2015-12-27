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
import Prelude hiding (FilePath)
import qualified Data.Text as T

import Options.Applicative
import Filesystem.Path.CurrentOS as Path

import qualified Turtle
--import qualified Data.Configurator as Config

import qualified Data.Aeson as JSON
import Data.Aeson ((.=), (.:))

import qualified Data.ByteString.Lazy as B


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

-- the combined datatype representing all warp commands
data Command = CommandList ListOptions |
               CommandAdd AddOptions |
               CommandRemove RemoveOptions
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

-- flip function for ease of chaining computations
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

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

dataFilePath :: String
dataFilePath = "~/.warpdata"

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

loadData :: FilePath -> IO WarpData
loadData jsonFilePath = do

    rawInput <- B.readFile (Path.encodeString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput  

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json

-- Common parsers
-- """"""""""""""
readFolderPath :: String -> ReadM FilePath
readFolderPath s = T.pack s |> 
                 Path.fromText |> 
                 (\path -> if Path.valid path
                     then return path
                     else readerError ("invalid path: " ++ (show path)))


warpnameParser :: Parser String
warpnameParser = strOption
                 (long "name" <>
                  short 'n' <>
                  metavar "NAME" <>
                  help "name of the warp point for usage")


-- Command parsers
-- """""""""""""""

parseAddCommand :: Parser Command
parseAddCommand =  
    CommandAdd <$> (AddOptions <$> folderParser <*> warpnameParser) where
        folderParser = option
                     (str >>= readFolderPath)
                     (long "path" <>
                      short 'p' <>
                      value "./"  <>
                      metavar "FOLDERPATH" <>
                      help "path of the warp folder to warp to")

parseListCommand :: Parser Command
parseListCommand = pure (CommandList ListOptions)

parseRemoveCommand :: Parser Command
parseRemoveCommand = (CommandRemove <$> (RemoveOptions <$> warpnameParser))

parseCommand :: Parser Command
parseCommand = subparser 
    -- add command
    ((command "add" (info parseAddCommand (progDesc "add a warp point"))) <>
    -- list command
    (command "list"
        (info parseListCommand (progDesc "list all warp points"))) <>
    -- remove command
    (command "remove"
        (info parseRemoveCommand (progDesc "remove a warp point"))))

-- Add command runner
-- """"""""""""""""""

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = 
    ("unable to find folder: " ++ (show path)) |>
    T.pack |>
    Turtle.die

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = 
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

run :: Command -> IO ()
run (CommandAdd AddOptions{..}) = do
    dieIfFolderNotFound folderPath
    print "yay, folder exists"


run command = print command

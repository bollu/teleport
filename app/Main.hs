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


--import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as T

import Options.Applicative
import Filesystem.Path.CurrentOS as Path


data ListOptions = ListOptions deriving (Show)


data AddOptions = AddOptions {
    folderPath :: FilePath,
    addname :: String
} deriving (Show)

data RemoveOptions = RemoveOptions {
    removename :: String
} deriving (Show)


data Command = CommandList ListOptions |
               CommandAdd AddOptions |
               CommandRemove RemoveOptions
    deriving (Show)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

warpProgDesc :: String
warpProgDesc = "use warp to quickly setup warp points and move to these " ++
               "when needed"

warpHeader :: String
warpHeader = "Warp: move around your filesystem"

-- helper takes a parser and adds a help option to it

main :: IO ()
main = do 
    command <- execParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc warpProgDesc <>
                        header warpHeader))
    run command


readFolderPath :: String -> ReadM FilePath
readFolderPath s = T.pack s |> 
                 Path.fromText |> 
                 (\path -> if Path.valid path
                     then return path
                     else readerError ("invalid path: " ++ (show path)))


-- Common parsers
-- """"""""""""""
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

run :: Command -> IO ()
run (CommandAdd AddOptions{..}) = putStrLn  addname
run command = print command

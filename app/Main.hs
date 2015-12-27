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

import Turtle
import Options.Applicative

data ListOptions = ListOptions deriving (Show)
data AddOptions = AddOptions deriving (Show)
data RemoveOptions = RemoveOptions deriving (Show)


data Command = CommandList ListOptions |
               CommandAdd AddOptions |
               CommandRemove RemoveOptions
    deriving (Show)


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


parseAddOptions :: Parser Command
parseAddOptions = pure (CommandAdd AddOptions)

parseCommand :: Parser Command
parseCommand = subparser 
    (command "add" (info parseAddOptions (progDesc "add a warp point")))

run :: Command -> IO ()
run command = print command

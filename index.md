–Copyright (c) 2015 Siddharth Bhat

–Permission is hereby granted, free of charge, to any person obtaining
–a copy of this software and associated documentation files (the
“Software”) –to deal in the Software without restriction, including
without limitation the –rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or –sell copies of the Software, and to
permit persons to whom the Software is –furnished to do so, subject to
the following conditions:

– The above copyright notice and this permission notice shall – be
included in all copies or substantial portions of the Software.

–THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
–OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, –FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE –AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER –LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING –FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR –OTHER DEALINGS IN THE SOFTWARE.

!/usr/bin/env stack
===================

{-\# LANGUAGE OverloadedStrings \#-} {-\# LANGUAGE RecordWildCards \#-}

import Control.Monad import Data.Traversable import Data.Maybe import
Data.List

import Prelude hiding (FilePath) import qualified Data.Text as T import
qualified Data.Text.Encoding as T.Encoding

import Options.Applicative import Filesystem.Path.CurrentOS as Path

import qualified Turtle –import qualified Data.Configurator as Config

import qualified Data.Aeson as JSON import Data.Aeson ((.=), (.:))

import qualified Data.ByteString.Lazy as B

import qualified System.Console.ANSI as ANSI

import Debug.Trace

– options passed to ‘tp list’ data ListOptions = ListOptions deriving
(Show)

– options pased to ‘tp add’ data AddOptions = AddOptions { addname ::
String, folderPath :: FilePath } deriving (Show)

– options passed to ‘tp remove’ data RemoveOptions = RemoveOptions {
removename :: String } deriving (Show)

– options parrsed to ‘tp goto’ data GotoOptions = GotoOptions { gotoname
:: String } deriving(Show) – the combined datatype representing all tp
commands data Command = CommandList ListOptions | CommandAdd AddOptions
| CommandRemove RemoveOptions | CommandGoto GotoOptions deriving (Show)

– an abstract entity representing a point to which we can tp to data
TpPoint = TpPoint { name :: String, absFolderPath :: String } deriving
(Show)

– the main data that is loaded from JSON data TpData = TpData { tpPoints
:: \[TpPoint\] } deriving (Show)

defaultTpData :: TpData defaultTpData = TpData { tpPoints = \[\] }

– flip function for ease of chaining computations (|&gt;) :: a -&gt; (a
-&gt; b) -&gt; b (|&gt;) = flip (\$)

filePathToString :: FilePath -&gt; String filePathToString =
Path.encodeString

tpProgDesc :: String tpProgDesc = “use teleport to quickly setup
teleport points and move to these” ++ “when needed”

tpHeader :: String tpHeader = “Teleport: move around your filesystem”

– | A version of ‘execParser’ which shows full help on error.
-------------------------------------------------------------

– The regular ‘execParser’ only prints usage on error, which doesn’t\
– include the options, subcommands, or mention of the help switch\
– @--help@.\
showHelpOnErrorExecParser :: ParserInfo a -&gt; IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO () main = do command &lt;- showHelpOnErrorExecParser (info
(helper &lt;\*&gt; parseCommand) (fullDesc &lt;&gt; progDesc tpProgDesc
&lt;&gt; header tpHeader)) run command

– Data Loading – “”“”“”“”“”“”

– parse tpPoint instance JSON.FromJSON TpPoint where parseJSON
(JSON.Object v) = TpPoint &lt;\$&gt; v .: “name” &lt;\*&gt; v .:
“absFolderPath”

instance JSON.ToJSON TpPoint where toJSON (TpPoint {..}) = JSON.object
\[ “name” .= name ,“absFolderPath” .= absFolderPath\]

– parse tpData instance JSON.FromJSON TpData where parseJSON
(JSON.Object v) = TpData &lt;\$&gt; v .: “tpPoints”

instance JSON.ToJSON TpData where toJSON(TpData{..}) = JSON.object
\[“tpPoints” .= tpPoints\]

dieJSONParseError :: FilePath -&gt; String -&gt; IO TpData
dieJSONParseError jsonFilePath err = (“parse error in:” ++ (show
jsonFilePath) ++ “\nerror:——\n” ++ err) |&gt; T.pack |&gt; Turtle.die

decodeTpData :: FilePath -&gt; IO TpData decodeTpData jsonFilePath = do
rawInput &lt;- B.readFile (filePathToString jsonFilePath) let jsonResult
= JSON.eitherDecode’ rawInput

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json

createTpDataFile :: FilePath -&gt; IO () createTpDataFile jsonFilePath =
saveTpData jsonFilePath defaultTpData

loadTpData :: FilePath -&gt; IO TpData loadTpData jsonFilePath = do
exists &lt;- (Turtle.testfile jsonFilePath) if exists then decodeTpData
jsonFilePath else do createTpDataFile jsonFilePath return defaultTpData

saveTpData :: FilePath -&gt; TpData -&gt; IO () saveTpData jsonFilePath
tpData = do let dataBytestring = JSON.encode tpData Turtle.touch
jsonFilePath B.writeFile (filePathToString jsonFilePath) dataBytestring

getTpDataPath :: IO FilePath getTpDataPath = do homeFolder &lt;-
Turtle.home return \$ homeFolder &lt;/&gt; “.tpdata” – Common parsers –
“”“”“”“”“”“”“” readFolderPath :: String -&gt; ReadM FilePath
readFolderPath s = T.pack s |&gt; Path.fromText |&gt; (\path -&gt; if
Path.valid path then return path else readerError (“invalid path:” ++
(show path)))

tpnameParser :: Parser String tpnameParser = argument str (metavar
“NAME” &lt;&gt; help “name of the teleport point for usage”)

– Command parsers – “”“”“”“”“”“”“”"

parseAddCommand :: Parser Command parseAddCommand =\
CommandAdd &lt;$> (AddOptions <$&gt; tpnameParser &lt;\*&gt;
folderParser) where folderParser = argument (str &gt;&gt;=
readFolderPath) (value “./” &lt;&gt; metavar “FOLDERPATH” &lt;&gt; help
“path of the teleport folder to teleport to. By default, taken as
current working directory”)

parseListCommand :: Parser Command parseListCommand = pure (CommandList
ListOptions)

parseRemoveCommand :: Parser Command parseRemoveCommand = CommandRemove
&lt;$> (RemoveOptions <$&gt; tpnameParser)

parseGotoCommand :: Parser Command parseGotoCommand = CommandGoto
&lt;$> (GotoOptions <$&gt; tpnameParser)

parseCommand :: Parser Command parseCommand = subparser – add command
((command “add” (info (helper &lt;*&gt; parseAddCommand) (fullDesc
&lt;&gt; progDesc “add a teleport point”))) &lt;&gt; – list command
(command “list” (info (helper &lt;*&gt; parseListCommand) (fullDesc
&lt;&gt; progDesc “list all teleport points”))) &lt;&gt; – remove
command (command “remove” (info (helper &lt;*&gt; parseRemoveCommand)
(fullDesc &lt;&gt;progDesc “remove a teleport point”))) &lt;&gt; – goto
command (command “goto” (info (helper &lt;*&gt; parseGotoCommand)
(fullDesc &lt;&gt; progDesc “go to a created teleport point”))))

– Stream Helpers – “”“”“”“”“”“”“”

setErrorColor :: IO () setErrorColor = ANSI.setSGR \[ANSI.SetColor
ANSI.Foreground ANSI.Vivid ANSI.Red\]

tpPointPrint :: TpPoint -&gt; IO () tpPointPrint tpPoint = do
ANSI.setSGR \[ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White\]\
putStr (name tpPoint) ANSI.setSGR \[ANSI.SetColor ANSI.Foreground
ANSI.Vivid ANSI.Blue\]\
putStr “\t” putStr (absFolderPath tpPoint) putStr “\n”

– Add command runner – “”“”“”“”“”“”“”“”“”

folderNotFoundError :: FilePath -&gt; IO () folderNotFoundError path =
setErrorColor &gt;&gt; (“unable to find folder:” ++ (show path)) |&gt;
T.pack |&gt; Turtle.die

needFolderNotFileError :: FilePath -&gt; IO () needFolderNotFileError
path = setErrorColor &gt;&gt; (“expected folder, not file:” ++ (show
path)) |&gt; T.pack |&gt; Turtle.die

dieIfFolderNotFound :: FilePath -&gt; IO () dieIfFolderNotFound path =
do folderExists &lt;- Turtle.testdir path fileExists &lt;-
Turtle.testfile path – error checking when fileExists
(needFolderNotFileError path) unless folderExists (folderNotFoundError
path) – we know the folder exists

dieTpPointExists :: TpPoint -&gt; IO () dieTpPointExists tpPoint = do
setErrorColor putStrLn (“teleport point” ++ (name tpPoint) ++ " already
exists:\n“) tpPointPrint tpPoint Turtle.die”"

runAdd :: AddOptions -&gt; IO () runAdd AddOptions{..} = do
dieIfFolderNotFound folderPath tpDataPath &lt;- getTpDataPath tpData
&lt;- loadTpData tpDataPath absFolderPath &lt;- Turtle.realpath
folderPath

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

– List Command – “”“”“”“”“”“”

runList :: ListOptions -&gt; IO () runList ListOptions = do tpDataPath
&lt;- getTpDataPath tpData &lt;- loadTpData tpDataPath let num\_points =
length \$ tpPoints tpData putStr “teleport points:”

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr $ "(total " <> (show num_points) <>  ")\n"
    forM_ (tpPoints tpData) tpPointPrint

– Remove Command – “”“”“”“”“”“”“”"

dieTpPointNotFound :: String -&gt;IO () dieTpPointNotFound name =
setErrorColor &gt;&gt; (name ++ " tp point not found“) |&gt; T.pack
|&gt; Turtle.die

runRemove :: RemoveOptions -&gt; IO () runRemove RemoveOptions{..} = do
tpDataPath &lt;- getTpDataPath tpData &lt;- loadTpData tpDataPath

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
                    

runGoto :: GotoOptions -&gt; IO () runGoto GotoOptions{..} = do
tpDataPath &lt;- getTpDataPath tpData &lt;- loadTpData tpDataPath

    let wantedTpPoint = find (\tp -> name tp == gotoname) (tpPoints tpData)
    case wantedTpPoint of
        Nothing -> dieTpPointNotFound gotoname
        Just tpPoint -> do
                             Turtle.echo (T.pack (absFolderPath tpPoint))
                             Turtle.exit (Turtle.ExitFailure 2) 
      

run :: Command -&gt; IO () run command = case command of CommandAdd
addOpt -&gt; runAdd addOpt CommandList listOpt -&gt; runList listOpt
CommandRemove removeOpt -&gt; runRemove removeOpt CommandGoto gotoOpt
-&gt; runGoto gotoOpt other @ \_ -&gt; print other

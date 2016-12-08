

<h1> Teleport - A haskell tutorial on Turtle, JSON and having fun</h1>


<h2> Goal </h2>


We're going to build a command line application called `teleport`,
It allows people to add "warp points" to navigate the file system. These
can be added, deleted, listed, and goto'd.

<h3> Commands </h3>

<h4> tp add  &lt;warpname&gt; [warppath] </h4>

add a "warp point" that allows us to come back to the folder.
By default, the current working directory is pointed by the name. An 
alternate path can be supplied.

<h5> Example Usage </h5>

```
teleport-haskell [master●] tp add teleport-hs
creating teleport point:

teleport-hs	/Users/bollu/play/teleport-haskell/
```

<h4> tp list </h4>

list all warp points

<h5> Example Usage </h5>

```
teleport-haskell [master●] tp list
teleport points: (total 3)
se	/Users/bollu/play/se/
sf	/Users/bollu/play/software-foundations/
tp	/Users/bollu/prog/teleport-haskell/
```

<h4> tp goto &lt;warp point&gt; </h4>

go to the warp point. This is complicated, since we are not allowed to change
the working directory of the shell. So, we will write a simple shell
script wrapper around teleport. 

The shell script is called `teleport.sh`


<h4> tp remove &lt;warp point&gt; </h4>

Remove an existing warp point.

<h5> Example Usage </h5>
```
teleport-haskell [master●] tp remove teleport-hs
removed teleport point [teleport-hs]
```


<h2> Code </h2>

Let's start reading the code, and learn about the libraries as we go along
First thing's first, let us get the MIT license out of the way.


<hr/>
\begin{code}
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
\end{code}

Haskell Extensions
------------------

<hr/>
\begin{code}
#!/usr/bin/env stack
\end{code}

`OverloadedStrings` allows us to freely write code in
\" and have it be treated as String or Data.Text depending on context. It's
a handy extension to have around.

`RecordWildCards` is more interesting, and I'll describe it in more detail when we
get to it
<hr/>
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
\end{code}


<hr/>
\begin{code}
import qualified Turtle
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
\end{code}
`Turtle` is the haskell library we use to interact with the OS. It has
a nice set of abstractions for dealing with OS specific stuff.


We choose to hide `FilePath` since `turtle` (the library for interfacing
with the OS) has its own version of `FilePath`.

<hr/>
\begin{code}
import qualified Data.Aeson as JSON
import Data.Aeson ((.=), (.:))
\end{code}

We use `Aeson` for reading and writing JSON files. We use JSON to store
our settings


<hr/>
\begin{code}
import Options.Applicative
import Control.Monad
import Data.Traversable
import Data.Maybe
import Data.List
\end{code}
These are our default imports of standard library stuff.



<hr/>
\begin{code}
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.ByteString.Lazy as B
\end{code}


We choose `Text` over `String` since the libraries that we use play along
nicer with `Text`. `String` is just `[Char]` in haskell, which is quite
inefficient since its _literally_ a linked list.
`Text` uses a more efficient representation of text.
Text is used internally everywhere in the application to manipulate text.

We need `ByteString` to read and write JSON files onto the filesystem. 

<hr/>
\begin{code}
import qualified System.Console.ANSI as ANSI
\end{code}

the `ANSI` library is used for coloring our outputs.

<hr/>
\begin{code}
tpProgDesc :: String
tpProgDesc = "use teleport to quickly setup teleport points and move to these " ++
               "when needed"

tpHeader :: String
tpHeader = "Teleport: move around your filesystem"
\end{code}

Strings that are used in our library for descriptions. I prefer to keep these
as constants rather than hard-code them.

<hr/>
\begin{code}
-- the combined datatype representing all tp commands
data Command = CommandList |
               CommandAdd AddOptions |
               CommandRemove RemoveOptions |
               CommandGoto GotoOptions
    deriving (Show)
\end{code}

The `Command` sum type represents the commands we can call on `teleport`, and 
we create options datatypes to store the options.

* `AddOptions` needs the name of the warp point to add, and the path to the folder
* `RemoveOptions` needs the name of the warp point to remove
* `GotoOptions` needs the name of the warp point to go to
* `Command` is the data type that allows us to combine all of this
   information.

our parser will return a `Command` that tells us what to do.

<hr/>
\begin{code}
-- options pased to 'tp add'
data AddOptions = AddOptions {
    addname :: String,
    folderPath :: FilePath
} deriving (Show)
\end{code}

`tp add` needs the name of the warp point to add, and the path of the folder
where it should get added to.

<hr/>
\begin{code}
-- options passed to 'tp remove'
data RemoveOptions = RemoveOptions {
    removename :: String
} deriving (Show)

-- options parrsed to 'tp goto'
data GotoOptions = GotoOptions {
    gotoname :: String
} deriving(Show)
\end{code}


<hr/>
\begin{code}
-- | A version of 'execParser' which shows full help on error.                
--                                                                            
-- The regular 'execParser' only prints usage on error, which doesn't         
-- include the options, subcommands, or mention of the help switch            
-- @--help@.                                                                  
showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO ()
main = do 
    -- command :: Command
    command <- showHelpOnErrorExecParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc tpProgDesc <>
                        header tpHeader))
    -- run :: IO ()
    run command
\end{code}
Let's unpack the types in `main`.

<h5 class="codeheader">
```haskell
parseCommand :: Parser Command
```
</h5>
this is our core `Parser` which we run using
`showHelpOnErrorExecParser` which executes the parser,
and shows an error in case the parser fails to execute. If the parse
succeeds, it calls `run` which runs `command :: Command`

<h5 class="codeheader">
```haskell
helper :: Parser (a -> a)
```
</h5>
`helper` takes any parser, and adds "help" as an option to it. We apply
it to all parsers so `--help` works.

<h5 class="codeheader">
```haskell
info :: Parser a -> InfoMod a -> ParserInfo a
```
</h5>
`info` takes a parser and allows us to attach a `InfoMod` which adds help and
display information to the parser


<h5 class="codeheader">
```haskell
fullDesc :: InfoMod a
progDesc :: String -> InfoMod a
header :: String -> InfoMod a
```
</h5>
all of these allow us to attach `InfoMod` to a `Parser`, which changes the
information that is printed with a `Parser`.

They have a `Monoid` instance, and the `<>` is the `mappend` operator that
allows us to "smash together" two modifiers into one single modifier. One
can think of `<>` as `++` for lists: it lets us collect two lists into one.


<h5 class="codeheader">
```haskell
showHelpOnErrorExecParser
```
</h5>
As explained above, it takes a parser and allows it to show help information
when the parse fails. It executed the parser passed to it (`parseCommand`)

<hr/>
\begin{code}
parseCommand :: Parser Command
parseCommand = subparser
    -- add command
    ((command 
        "add" -- command name
        (info -- attach help information to the parser 
            (helper <*> parseAddCommand) -- core parser with the --help option
            (fullDesc <> progDesc "add a teleport point") -- description of command (for info)
        )
    ) 
    <> -- combine with the next command

    -- list command
    (command "list"
        (info (helper <*> parseListCommand) (fullDesc <> progDesc "list all teleport points"))) <>
    -- remove command
    (command "remove"
        (info (helper <*> parseRemoveCommand) (fullDesc <>progDesc "remove a teleport point"))) <>
    -- goto command
    (command "goto"
        (info (helper <*> parseGotoCommand) (fullDesc <> progDesc "go to a created teleport point"))))

\end{code}
the `subparser` is a function that lets us create a `Parser` out of of a
`command`. We smash the `command`s together with their monoid instance (`<>`).

The same use of `info`, `fullDesc`, `progDesc`, and `helper` is made as in
`main` to attach information and help to the parser.


<hr/>
\begin{code}
-- Command parsers
-- """""""""""""""

-- List
-- ----
-- $ tp list
parseListCommand :: Parser Command
parseListCommand = pure (CommandList)
\end{code}


the parser needs no options (the `list` command takes no options),
so we use
```haskell
pure :: a -> f a
```
to convert
```haskell
CommandList :: Command
```
to
```haskell
pure CommandList :: Parser Command
```

<hr/>
\begin{code}
parseAddCommand :: Parser Command
parseAddCommand = fmap -- :: (AddOptions -> Command) -> Parser AddOptions -> Parser Command
                   CommandAdd -- :: AddOptions -> Command
                   (liftA2 -- :: (String -> FilePath -> AddOptions) ->
                           --       Parser String -> Parser FilePath -> Parser AddOptions
                        AddOptions -- :: String -> FilePath -> AddOptions
                        tpnameParser -- :: Parser String
                        folderParser -- :: Parser FilePath
                   )
\end{code}

we use
```haskell
liftA2 AddOptions :: Parser String -> Parser FilePath -> Parser AddOptions
```

and we pass it two parser `tpNameParser` and `folderParser` (which will be defined below)
to create a `Parser AddOptions`.

we then convert `Parser AddOptions` to `Parser Command` by using

```haskell
fmap CommandAdd :: Parser AddOptions -> Parser Command
```

<hr/>
\begin{code}
-- Warp Name parser
-- """"""""""""""""
tpnameParser :: Parser String
tpnameParser = argument  -- :: ReadM String -> Mod ArgumentFields String -> Parser String
                  str -- :: ReadM String
                  (metavar "NAME" <>
                  help "name of the teleport point for usage") -- Mod ArgumentFields
\end{code}

Till now, we were creating "command" parsers that parse things like
```
$ tp add
```
or
```
$ tp list
```

Now, we need to learn how to parser _options_. Options such as
```
$ tp add <warp point name> ...
```
to do this, the __general function that is used is called `argument`__.
```haskell
argument :: ReadM a -> -- in general, "can be read".
            Mod ArgumentFields a -> -- modifiers to a parser
            Parser a
```
Breaking this down as usual,

<h5 class="codeheader">
```haskell
ReadM a
```
</h5>
I won't explain `ReadM` here, it's mostly a way to "read something in". We will mostly start with
the `ReadM` instance
```haskell
str :: ReadM String
```
and use the `Functor` and `Monad` instance on `str` create new `ReadM` instances. [For more on
`ReadM`, click here](https://hackage.haskell.org/package/optparse-applicative-0.13.0.0/docs/Options-Applicative-Builder.html#t:ReadM)

<h5 class="codeheader">
```haskell
Mod ArgumentFields a
```
</h5>

This lets us "Modify" a `Parser` by providing it with modifiers. The modifiers have a `Monoid` instance, so
we use `<>` (`mappend`)
```haskell
<> :: Monoid m -> m -> m -> m
mappend :: Monoid m -> m -> m -> m
mappend = <>
```
to "combine" two options together.

<h5> full picture </h5>
Now, reading through the code we have, we start with a `str :: ReadM String`, use the
`metavar` option to give it a name, and the `help` option to give it a help string.

```
$ tp add --help
Usage: teleport-exe add NAME ...
 ...
Available options:
  ...
  NAME                     name of the teleport point for usage
  ...
```
the `NAME` comes from the `metavar` option, and the help string comes from the `help` option

<hr/>
\begin{code}
-- Folder Parser
-- """"""""""""""
folderParser :: Parser FilePath
folderParser = argument
              (str -- :: Parser String
                >>=
               readFolderPath)
              (value "./"  <>
              metavar "FOLDERPATH" <>
              help "path of the teleport folder to teleport to. By default, taken as current working directory")

readFolderPath :: String -> ReadM FilePath
readFolderPath s = T.pack s |>
                 Path.fromText |>
                 (\path -> if Path.valid path
                     then return path
                     else readerError ("invalid path: " ++ (show path)))

\end{code}


<hr/>
\begin{code}
parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap (CommandRemove . RemoveOptions) tpnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = fmap (CommandGoto . GotoOptions) tpnameParser

\end{code}




This is our program representation. The `TpPoint` class stores the
information of a warp point.


We will implement the `FromJSON` and `ToJSON` typeclasses for both
to allow us to save these as JSON files

<hr/>
\begin{code}
-- an abstract entity representing a point to which we can tp to
data TpPoint = TpPoint {
    name :: String,
    absFolderPath :: String
} deriving (Show)


instance JSON.FromJSON TpPoint where
     parseJSON (JSON.Object v) =
        TpPoint <$> v .: "name"
                  <*> v .: "absFolderPath"

instance JSON.ToJSON TpPoint where
    toJSON (TpPoint {..}) = 
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]
\end{code}

The `TpData` class stores all the warp points together.

<hr/>
\begin{code}
-- the main data that is loaded from JSON 
data TpData = TpData {
    tpPoints :: [TpPoint]
} deriving (Show)

instance JSON.FromJSON TpData where
    parseJSON (JSON.Object v) =
        TpData <$> v .: "tpPoints"

instance JSON.ToJSON TpData where
    toJSON(TpData{..}) = 
        JSON.object ["tpPoints" .= tpPoints]
\end{code}

the `defaultTpData` represents the default `TpData` we will use if no
warp data is found on execution.

<hr/>
\begin{code}
defaultTpData :: TpData
defaultTpData = TpData {
    tpPoints = []
}

\end{code}


<hr/>
\begin{code}


-- flip function for ease of chaining computations
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


filePathToString :: FilePath -> String
filePathToString = Path.encodeString



-- Data Loading
-- """"""""""""

-- parse tpPoint

-- parse tpData

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


runList :: IO ()
runList = do
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
        CommandList -> runList
        CommandRemove removeOpt -> runRemove removeOpt
        CommandGoto gotoOpt -> runGoto gotoOpt
        other @ _ -> print other
\end{code}

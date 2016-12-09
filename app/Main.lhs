

<h1> Teleport - A haskell tutorial on Turtle, JSON and having fun</h1>


<h2> Goal </h2>


We're going to build a command line application called `teleport`,
It allows people to add "warp points" to navigate the file system. These
can be added, deleted, listed, and goto'd.

<h3> Commands </h3>

<h4> tp add  &lt;warpname&gt; [warppath] </h4>

Add a "warp point" that allows us to come back to the folder.
By default, the current working directory is pointed by the name. An 
alternate path can be supplied.

<h5> Example Usage </h5>

```
teleport-haskell [master●] tp add teleport-hs
creating teleport point:

teleport-hs	/Users/bollu/play/teleport-haskell/
```

<h4> tp list </h4>

List all warp points.

<h5> Example Usage </h5>

```
teleport-haskell [master●] tp list
teleport points: (total 3)
se	/Users/bollu/play/se/
sf	/Users/bollu/play/software-foundations/
tp	/Users/bollu/prog/teleport-haskell/
```

<h4> tp goto &lt;warp point&gt; </h4>

Go to the warp point. There's a complication here: we are not allowed to change
the working directory of the shell. To deal with this, we write a simple shell
script wrapper around teleport.

The shell script is called: `teleport.sh`


<h4> tp remove &lt;warp point&gt; </h4>

Remove an existing warp point.

<h5> Example Usage </h5>
```
teleport-haskell [master●] tp remove teleport-hs
removed teleport point [teleport-hs]
```


<h2> Code </h2>

Let's start reading the code, and learn about the libraries as we go along.
First things first, let us get the MIT license out of the way.


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
get to it.
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
our settings.


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
inefficient since it's _literally_ a linked list.
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

Our parser will return a `Command` that tells us what to do.

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
This is our core `Parser` which we run using
`showHelpOnErrorExecParser`. This executes the parser,
and shows an error in case the parser fails to execute. If it
succeeds, it calls `run` which runs `command :: Command`.

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
display information to the parser.


<h5 class="codeheader">
```haskell
fullDesc :: InfoMod a
progDesc :: String -> InfoMod a
header :: String -> InfoMod a
```
</h5>
All of these allow us to attach `InfoMod` to a `Parser`, which changes the
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
The `subparser` is a function that lets us create a `Parser` out of of a
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


The parser needs no options (the `list` command takes no options),
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

We use
```haskell
liftA2 AddOptions :: Parser String -> Parser FilePath -> Parser AddOptions
```

and we pass it two parser `tpNameParser` and `folderParser` (which will be defined below)
to create a `Parser AddOptions`.

We then convert `Parser AddOptions` to `Parser Command` by using

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
                  (metavar -- :: String -> Mod ArgumentFields String
                    "NAME" <>
                  help -- :: String -> Mod ArgumentFields String
                    "name of the teleport point for usage") -- Mod ArgumentFields String
\end{code}

Till now, we were creating "command" parsers that parse things like
```
$ tp add
```
or
```
$ tp list
```

Now, we need to learn how to parse _options_. Eg.:
```
$ tp add <warp point name> ...
```
To do this, the __general function that is used is called `argument`__.
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
I won't explain `ReadM` here: it's mostly a way to "read something in". We will mostly start with
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

This lets us modify a `Parser` by providing it with modifiers. The modifiers have a `Monoid` instance, so
we use `<>` (`mappend`)
```haskell
<> :: Monoid m -> m -> m -> m
mappend :: Monoid m -> m -> m -> m
mappend = <>
```
to "combine" two options together.

<h5 class="codeheader"> full picture </h5>
Now, reading through the code we have, we start with a `str :: ReadM String`, use the
`metavar` option to give it a name, and the `help` option to give it a help string.

```
metavar :: ReadM a => String -> Mod ArgumentFields String
help ::  String -> Mod f a
```

<h6> use of `metavar` and `help` </h6>
```
$ tp add --help
Usage: teleport-exe add NAME ...
 ...
Available options:
  ...
  NAME                     name of the teleport point for usage
  ...
```
The `NAME` comes from the `metavar` option, and the help string comes from the `help` option.

<hr/>
\begin{code}
-- Folder Parser
-- """"""""""""""
folderParser :: Parser FilePath
folderParser = argument
              (str -- :: ReadM String
                >>=
               readFolderPath) -- :: String -> ReadM FilePath
              (value "./"  <>
              metavar "FOLDERPATH" <>
              help "path of the teleport folder to teleport to. By default, taken as current working directory")

readFolderPath :: String -> ReadM FilePath
readFolderPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
      then return path
      else readerError ("invalid path: " ++ (show path))

\end{code}

Here, we look at how to build a more complex argument parser from the simple `str` argument.
We compose `str :: ReadM String` with `readFolderPath ::  String -> ReadM FilePath` using their
`Monad instance`

<h5 class="codeheader">
```haskell
readFolderPath :: String -> ReadM FilePath
```
</h5>
There is some fluff with converting from `String` to `Text` using `T.pack`. The main part of the code
is in using
```haskell
readerError String -> ReadM FilePath
```
to report an error when the path given is invalid.

<h5 class="codeheader">
```haskell
value :: HasValue f a => a -> Mod f a
```
</h5>

We use the `value` modifier to assign a default value to the option. We use ".",
which is the current folder, as a default option.

<hr/>
\begin{code}
parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap (CommandRemove . RemoveOptions) tpnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = fmap (CommandGoto . GotoOptions) tpnameParser

\end{code}

We reuse our `tpnameParser :: Parser String` to parse names. We convert them to
`Command` by first converting them to the correct `{Remove, Goto}Options` type,
and then using the `Command{Remove, Goto}` constructors.

<hr/>
\begin{code}
-- an abstract entity representing a point to which we can tp to
data TpPoint = TpPoint {
    name :: String,
    absFolderPath :: String
} deriving (Show)


instance JSON.FromJSON TpPoint where
     parseJSON (JSON.Object json) =
        liftA2 TpPoint (json .: "name")
                  (json .: "absFolderPath")

instance JSON.ToJSON TpPoint where
    toJSON (TpPoint {..}) =
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]
\end{code}

This is our program representation. The `TpPoint` class stores the
information of a warp point.


We implement the `FromJSON` and `ToJSON` typeclasses for both
to allow us to save these as JSON files

<h5 class="codeheader"> `FromJSON` </h5>
We are given an `(Object json) :: Value`, and we need to produce a `Parser`

```haskell
(.:) :: FromJSON a => Object -> Text -> Parser a
```
We use `(.:)`, which when given a JSON `Object` and the name of a key, gives us a `Parser a`.
This is used to extract the `name` and the `absFolderPath` from the JSON object.

The constructor for `TpPoint`  is lifted into the `Parser` using `liftA2`.
 
<h5 class="codeheader"> `ToJSON` </h5>
We need to implement
```haskell
toJSON :: a -> Value
```

To create a `Value`, we use
```haskell
JSON.object = object :: [Pair] -> Value
```

This lets us give it an array of `Pair` objects and it creates a Value.
We make `Pair` objects using


```haskell
(.=) :: ToJSON v => Text -> v -> (kv ~ Pair)
```
The `.=` creates any `KeyValue`. We use it to create a `Pair` from a tag name
and a value (which has a `ToJSON` instance).

<hr/>
\begin{code}
-- the main data that is loaded from JSON
data TpData = TpData {
    tpPoints :: [TpPoint]
} deriving (Show)

instance JSON.FromJSON TpData where
    parseJSON (JSON.Object v) =
        fmap TpData (v .: "tpPoints")

instance JSON.ToJSON TpData where
    toJSON(TpData{..}) = 
        JSON.object ["tpPoints" .= tpPoints]
\end{code}

The `TpData` class stores all the warp points together.

<hr/>
\begin{code}
defaultTpData :: TpData
defaultTpData = TpData {
    tpPoints = []
}
\end{code}

The `defaultTpData` represents the default `TpData` we will use if no
warp data is found on execution.
<hr/>
\begin{code}
filePathToString :: FilePath -> String
filePathToString = Path.encodeString

-- Data Loading
-- """"""""""""

dieJSONParseError :: FilePath -> String -> IO a
dieJSONParseError jsonFilePath err = do
    let errorstr = ("parse error in: " ++ (show jsonFilePath) ++
                    "\nerror:------\n" ++ err)
    Turtle.die (T.pack errorstr)
\end{code}
We write a quick function that errors out if the parse failed. To do this,
we use `Turtle.die` that takes an error string and returns an `IO a` for failure.

<hr/>
\begin{code}
decodeTpData :: FilePath -> IO TpData
decodeTpData jsonFilePath = do
    rawInput <- B.readFile (filePathToString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json
\end{code}

We use

```haskell
JSON.eitherDecode' ::  FromJSON a => ByteString -> Either String a
```

which takes a file path and returns an `Either String a` with the error
in `Left`

<hr/>
\begin{code}
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
\end{code}

We try to load a file. If the file does not exist, we use
```haskell
defaultTpData :: TpData
```
We save this in the `createTpDataFile`, and then just retrn the default value.
If we do get a value, then we return the parsed object.

<hr/>
\begin{code}
saveTpData :: FilePath -> TpData -> IO ()
saveTpData jsonFilePath tpData = do
    let dataBytestring = JSON.encode tpData
    Turtle.touch jsonFilePath
    B.writeFile (filePathToString jsonFilePath) dataBytestring


getTpDataPath :: IO FilePath
getTpDataPath = do
    homeFolder <- Turtle.home
    return $ homeFolder </> ".tpdata"
\end{code}

Note the use of `Turtle` for finding the home folder (`Turtle.home`) and
to touch files (`Turtle.touch`). We concatenate `FilePath`s using

```haskell
</> :: FilePath -> FilePath -> FilePath
``` 

<hr/>
\begin{code}
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

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
    setErrorColor  
    let errorstr = T.pack ("unable to find folder: " ++ (show path)) 
    Turtle.die errorstr

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = do
    setErrorColor
    let errorstr = T.pack ("expected folder, not file: " ++ (show path)) 
    Turtle.die errorstr

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
\end{code}

We write functions to error out nicely with colors, since everybody likes colors `:)`

Again, we use turtle for
```haskell
Turtle.testdir :: MonadIO io => FilePath -> io Bool
Turtle.testfile :: MonadIO io => FilePath -> io Bool
```
to check if the file and folder we care about exists.

We also use the `ANSI` library for coloring the output.
<h5 class="codeheader">
```haskell
setSGR :: [SGR] -> IO ()
```
</h5>

It takes an array of `SGR` (Select Graphic Rendition) objects, and applies them.

The ones we use in `Teleport`:
```haskell
SetColor :: ConsoleLayer ColorIntensity Color -> SGR

ConsoleLayer = Foreground | Background
ColorIntensity = Dull | Vivid
Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
```
(to add colors to our output).
<hr/>
\begin{code}
-- Add command runner
-- """"""""""""""""""

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
\end{code}

We check if a similar teleport point exists. If it does, we quit using
`dieIfPointExists`.

If not, we:

* create a new teleport point `newTpPoint`,
* add it to the list in `newTpData`, and
* save the new data with `saveTpData`.

<hr/>
\begin{code}
    
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

\end{code}
<h5 class="codeheader">
```haskell
forM_ :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ()
```
</h5>
the `Foldable` constraint means that the type `t` allows us
to run "side-effectful" (Monadic `m`) code over the object.
`[]` is `Foldable`, so we use `forM_` to print all values

We use `forM_` to:

* loop over the list,
* with monadic effect (IO), hence `M`,
* ignore the return result (we don't care about the return value), hence `_`

<hr/>
\begin{code}

-- Remove Command
-- """""""""""""""

dieTpPointNotFound :: String ->IO ()
dieTpPointNotFound name = do
    setErrorColor
    let errorname = T.pack (name ++ " tp point not found")
    Turtle.die errorname

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
\end{code}

If there is no teleport point with the given name, we fail.
If such a teleport point exists, we remove the point by filtering it out
and then save the JSON file
<hr/>
\begin{code}
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

\end{code}

Since a process is not allowed to change another process' working directory, `tp goto` cannot
change the shell's working directory. So, we use a simple shell script (`teleport.sh`)
<h5 class="codeheader">
`teleport.sh`
</h5>
```bash
 #!/bin/bash
 # teleport.sh
function tp() {
    OUTPUT=`teleport-exe $@`
    # return code 2 is used to indicate that the shell script
    # should use the output to warp to
    if [ $? -eq 2 ]
        then cd "$OUTPUT"
        else echo "$OUTPUT"
    fi
}
```
When `tp goto` succeeds, we print out the path to the output stream in Haskell
and returns a return code of `2`. The shell script sees that the return code is `2`, so
it runs a `cd` to the correct path

If `tp` returns any code other than `2`, the shell script echoes all the output to the screen.
<hr/>
\begin{code}
run :: Command -> IO ()
run command = 
    case command of
        CommandAdd addOpt -> runAdd addOpt
        CommandList -> runList
        CommandRemove removeOpt -> runRemove removeOpt
        CommandGoto gotoOpt -> runGoto gotoOpt
        other @ _ -> print other
\end{code}
We simply pattern match on the command and then call the correct `run*` function.

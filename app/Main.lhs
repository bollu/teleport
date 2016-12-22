<!-- Place this tag in your head or just before your close body tag. -->
<script async defer src="https://buttons.github.io/buttons.js"></script>

<h1> Teleport - How to write a small, useful command line application in Haskell</h1>

<a class="github-button" href="https://github.com/bollu/teleport" data-icon="octicon-star" data-style="mega" data-count-href="/bollu/teleport/stargazers" data-count-api="/repos/bollu/teleport#stargazers_count" data-count-aria-label="# stargazers on GitHub" aria-label="Star bollu/teleport on GitHub">Star</a>

<a class="github-button" href="https://github.com/bollu/teleport/fork" data-icon="octicon-repo-forked" data-style="mega" data-count-href="/bollu/teleport/network" data-count-api="/repos/bollu/teleport#forks_count" data-count-aria-label="# forks on GitHub" aria-label="Fork bollu/teleport on GitHub">Fork</a>

We're going to build a command line application called `teleport`,
It allows people to add "warp points" to navigate the file system. These
can be added, deleted, listed, and goto'd.

We shall use the libraries:

* `optparse-applicative`: parsing command line arguments
* `Aeson`: reading/writing `JSON`
* `Turtle`: writing "shell"-y code for files and directories
* `ANSI`: emit colors in the console
* `Text` and `Bytestring`: forced to use these because of `Aeson`, `Filepath`

<h3> Demo </h3>
<script type="text/javascript"
         src="https://asciinema.org/a/a0rzkn428t6mrnvzquc5fqoyr.js" 
        id="asciicast-a0rzkn428t6mrnvzquc5fqoyr" async></script>

<h3> Intended audience </h3>

The indented audience are those who are comfortable with

* Functor, Applicative, Monad and `do` notation
* `IO` (no other monads required)
* general haskell patterns

You'll see Haskell libraries in action, and put them together to build something
tangible.

<h3> Getting the code </h3>

[The code is available at the repository here (link)](https://github.com/bollu/teleport).

To use the tutorial, a handy way of downloading and building `teleport`:

```bash
$ git clone https://github.com/bollu/teleport.git && cd teleport && cabal build && cabal install teleport
```

To use the `teleport` wrapper you will need, run 

```bash
$ echo source `pwd`/teleport.sh >> ~/.bashrc
```

change `~/.bashrc` to the correct shell needed

<h3> Teleport's commands </h3>

<h4>`tp add  <warpname> [warppath]` </h4>

add a "warp point" that allows us to come back to the folder.

the default "warp path" is the current folder.

<h6> Example Usage </h5>

```
 # by default, current working directory is used
~/play/teleport-haskell$ tp add teleport-hs
creating teleport point:

teleport-hs	/Users/bollu/play/teleport-haskell/

 # on providing the path to a teleport point, that path is used
~/play/teleport-haskell$ tp add sf ~/play/software-foundations
creating teleport point:

sf	/Users/bollu/play/software-foundations

```

<h4> `tp list` </h4>

list all warp points

<h6> Example Usage </h6>

```
~/play/teleport-haskell$ tp list
teleport points: (total 3)
se	/Users/bollu/play/se/
sf	/Users/bollu/play/software-foundations/
tp	/Users/bollu/prog/teleport-haskell/
```

<h4> `tp goto <warp point>` </h4>

Go to the warp point. This is impossible within our application.
The reason it is impossible is because one process (our application, `teleport`)
cannot change the working directory of another application (the shell).


So, we have written a simple shell
script wrapper around teleport. The wrapper runs inside the shell,
so a `cd` is able to edit the shell's current working directory

The shell script is called `teleport.sh`

<h6> Example Usage </h6>
```
~$ tp goto tp
~/p/teleport-haskell$
```
our current working directory changed and became the `teleport-haskell`
folder

<h4> `tp remove <warp point>` </h4>

Remove an existing warp point.

<h6> Example Usage </h6>
```
~/play/teleport-haskell$ tp remove teleport-hs
removed teleport point [teleport-hs]
```


<h2> Reading the Code </h2>

Let's start reading the code, and learn about the libraries as we go along
First thing's first, let us get the MIT license out of the way.



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

The interesting code starts from here.


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
\end{code}


`OverloadedStrings` allows us to freely write code in
\" and have it be treated as String or Data.Text depending on context. It's
a handy extension to have around.

`RecordWildCards` is more interesting, and I'll describe it in more detail when we
get to it

\begin{code}
import qualified Turtle
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
\end{code}
`Turtle` is the haskell library we use to interact with the OS. It has
a nice set of abstractions for dealing with OS specific stuff.


We choose to hide `FilePath` since `turtle` (the library for interfacing
with the OS) has its own version of `FilePath`.


\begin{code}
import qualified Data.Aeson as JSON
import Data.Aeson ((.=), (.:))
\end{code}

We use `Aeson` for reading and writing JSON files. We use JSON to store
our settings



\begin{code}
import Options.Applicative
import Control.Monad
import Data.Traversable
import Data.Maybe
import Data.List
\end{code}
These are our default imports of standard library stuff.




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


\begin{code}
import qualified System.Console.ANSI as ANSI
\end{code}

the `ANSI` library is used for coloring our outputs.


\begin{code}
tpProgDesc :: String
tpProgDesc = "use teleport to quickly setup teleport points and move to these " ++
               "when needed"

tpHeader :: String
tpHeader = "Teleport: move around your filesystem"
\end{code}

Strings that are used in our library for descriptions. I prefer to keep these
as constants rather than hard-code them.


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

our parser returns a `Command` that tells us what to do.


\begin{code}
-- options pased to 'tp add'
data AddOptions = AddOptions {
    addname :: String,
    folderPath :: FilePath
} deriving (Show)
\end{code}

`tp add` needs the name of the warp point to add, and the path of the folder
where it should get added to.


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


\begin{code}
parseCommand :: Parser Command
parseCommand = subparser $
    -- add command
    (command
        "add" -- command name
        (info -- attach help information to the parser
            (helper <*> parseAddCommand) -- core parser with the --help option
            (fullDesc <> progDesc "add a teleport point") -- description of command (for info)
        )
    )
    <> -- combine with the next command

    -- list command
    (command "list"
        (info (helper <*> parseListCommand)
        (fullDesc <> progDesc "list all teleport points"))
    ) <>
    -- remove command
    (command "remove"
        (info (helper <*> parseRemoveCommand)
        (fullDesc <>progDesc "remove a teleport point"))
    ) <>
    -- goto command
    (command "goto"
        (info (helper <*> parseGotoCommand)
        (fullDesc <> progDesc "go to a created teleport point"))
    )

\end{code}
the `subparser` is a function that lets us create a `Parser` out of of a
`command`. We smash the `command`s together with their monoid instance (`<>`).

The same use of `info`, `fullDesc`, `progDesc`, and `helper` is made as in
`main` to attach information and help to the parser.



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
so we use `(pure :: a -> f a)`{.haskell} to convert `(CommandList :: Command)`{.haskell}
to `(pure CommandList :: Parser Command)`{.haskell}


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
`(liftA2 AddOptions :: Parser String -> Parser FilePath -> Parser AddOptions)`{.haskell}
and we pass it two parsers `tpNameParser` and `folderParser` (which is defined below)
to create a `Parser AddOptions`{.haskell}.

we then convert `(Parser AddOptions)`{.haskell} to `(Parser Command)`{.haskell}
by using `(fmap CommandAdd :: Parser AddOptions -> Parser Command)`{.haskell}



Till now, we were creating "command" parsers that parse things like
```bash
$ tp add
$ tp list
```

Now, we need to learn how to parse __options__, such as:
```bash
$ tp add <warp point name> ...
```
to do this, the __general function that is used is called `argument`{.haskell}__.
```haskell
argument :: ReadM a -> -- in general, "can be read".
            Mod ArgumentFields a -> -- modifiers to a parser
            Parser a
```

Let's read the code and then come back to the explanation with context:
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

<h5> Types </h5>

* `ReadM a`{.haskell} is a way to "read something in". Let's start with
the `ReadM` instance
`(str :: ReadM String)`{.haskell}
and use the `Functor` and `Monad` instance on `str` create new `ReadM` instances. [For more on
`ReadM`, click here](https://hackage.haskell.org/package/optparse-applicative-0.13.0.0/docs/Options-Applicative-Builder.html#t:ReadM)

* `Mod ArgumentFields a`{.haskell} allows us to modify a `Parser` by
providing it with modifiers. The modifiers have a `Monoid` instance,
which allows us to smash them together with `mappend`

<h5> Code </h5>

* start with a `str :: ReadM String`{.haskell}
* use the `metavar` option to give it a name
* use the `help` option to give it a help string.

<h5> Use of `metavar` & `help` </h5>
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


\begin{code}
-- take a string, parse it into a folder path.
-- if path does not exist, return an error
readFolderPath :: String -> ReadM FilePath
readFolderPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
      then return path
      else readerError ("invalid path: " ++ (show path))

\end{code}

We convert a `String` to a `ReadM FilePath`{.haskell}. 
Since `ReadM`{.haskell} is a monad, it allows us to do
error handling within it.

We return `ReadM FilePath`{.haskell} and not a `FilePath`{.haskell} to
have the ability to return an error.

The `(readerError :: String -> ReadM a)`{.haskell} function
allows to return an error string.





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
              help ("path of the teleport folder to teleport to." ++ 
                   "By default, taken as current working directory"))

\end{code}

Here, we look at how to build a more complex argument parser from
the simple `str` argument.

* The composition of `(str :: ReadM String)`{.haskell}
with `(readFolderPath ::  String -> ReadM FilePath)`{.haskell}
using `(>>=)`{.haskell}
gives us a function that takes a raw string, tries to parse it to a folder
and fails if the parse fails.

* The `(value :: HasValue f a => a -> Mod f a)`{.haskell} lets us
define a default value to the "folder" option. We set the default to "."
(the current folder)


\begin{code}
parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap (CommandRemove . RemoveOptions) tpnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = fmap (CommandGoto . GotoOptions) tpnameParser

\end{code}

* `tpnameParser :: Parser String`{.haskell} is used to parse names. 
* `(CommandRemove . RemoveOptions :: String -> Command)`{.haskell} converts
`String =RemoveOptions=> RemoveOptions =CommandRemove=> Command`{.latex}

Similary, we created a `(CommandGoto :: Command)`{.haskell} with the same pipeline


We have created data types to store the data for our app.

* `TpPoint`{.haskell} stores the information of a warp point.
* `FromJSON`{.haskell} and `ToJSON`{.haskell} typeclasses for
   `TpPoint` to allow it to store and retreive `JSON`

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

\end{code}


* `FromJSON`{.haskell} is to convert a `JSON` object to a `TpPoint`.
* `(Object json) :: Value`{.haskell} is our parameter, and we need to creae a `TpPoint`.

* We use the 
`( (.:) :: FromJSON a => Object -> Text -> Parser a)`{.haskell} operator,
which when given a `JSON` `Object` and a key, gives us a `Parser a`{.haskell}

* the `Parser` has an applicative instance, so we lift our `TpPoint`{.haskell} 
to the `Parser`{.haskell} type with `liftA2`


* Here, we also see `RecordWildCards` (the extension) at play. It automatically
  "unpacks" the `TpPoint` for us, and we can directly access `name` and `absFoldeerPath`

* The syntax of `{..}`{.haskell} is used to denote that this declaration must 
  be unpacked

\begin{code}
instance JSON.ToJSON TpPoint where
    toJSON (TpPoint {..}) =
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]
\end{code}


* `(toJSON :: a -> Value)`{.haskell} is used to create a JSON Value from an object
`a`. For us, the `(a ~ TpPoint)`{.haskell}.

* To create a `Value`, we use
`(JSON.object :: object :: [Pair] -> Value)`{.haskell}. We give it
an array of `Pair` objects and it creates a `Value`(JSON Value).

* We use
`( (.=) :: ToJSON v => Text -> v -> (kv ~ Pair) )`{.haskell} to pair up a key
with a `Value`. the `.=`{.haskell} creates any `KeyValue`.
We use it to create a `Pair`.



We'll write a `TpData` class which stores all the warp points together in a
list.

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



\begin{code}
defaultTpData :: TpData
defaultTpData = TpData {
    tpPoints = []
}
\end{code}

the `defaultTpData` represents the default `TpData` that is used
if no previously saved data is found (esentially, a fresh start)

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


\begin{code}
decodeTpData :: FilePath -> IO TpData
decodeTpData jsonFilePath = do
    rawInput <- B.readFile (filePathToString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json
\end{code}

We use `JSON.eitherDecode' ::  FromJSON a => ByteString -> Either String a`{.haskell}
which takes a file path and returns an `Either String a`{.haskell} with the error
in `Left`{.haskell}


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

We try to load a file. If the file does not exist, we use `defaultTpData :: TpData`{.haskell}
We save this in the `createTpDataFile`, and then just return the default value.
If we do get a value, then we return the parsed object.


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

Note the use of `Turtle`{.haskell} for finding the home folder (`Turtle.home`) and
to touch files (`Turtle.touch`{.haskell}).
We concatenate `FilePath`s using `(</> :: FilePath -> FilePath -> FilePath)`{.haskell}


We're now  writing functions to error out nicely with colors,
since everybody likes colors `:)`

\begin{code}
-- Stream Helpers
-- """"""""""""""

-- set terminal to output error color
setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [ANSI.SetColor -- color to set
                             ANSI.Foreground -- wherther foreground / background should be affected
                            ANSI.Vivid -- use the "vivid" color versus the muted color
                            ANSI.Red -- use red
                            ]    
\end{code}

* `setSGR :: [SGR] -> IO ()`{.haskell} lets us color the output. 
    It takes an array of `SGR` (Select Graphic Rendition) objects, and applies them.

* The `SGR` instance we use in `Teleport` are `SetColor :: ConsoleLayer ColorIntensity Color -> SGR`{.haskell}
to add colors to our output


\begin{code}
-- print a teleport point to stdout
tpPointPrint :: TpPoint -> IO ()
tpPointPrint tpPoint = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    putStr (name tpPoint)
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr (absFolderPath tpPoint)
    putStr "\n"

-- error out that the given folder is not found
folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
    setErrorColor  
    let errorstr = T.pack ("unable to find folder: " ++ (show path)) 
    Turtle.die errorstr

-- error out that folder is required, but path points
-- to a file
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

-- error out that the teleport point already exists
dieTpPointExists :: TpPoint -> IO ()
dieTpPointExists tpPoint  =  do
    setErrorColor
    putStrLn ("teleport point " ++ (name tpPoint) ++ " already exists:\n")
    tpPointPrint tpPoint
    Turtle.die ""
\end{code}


* `Turtle.testdir :: MonadIO io => FilePath -> io Bool`{.haskell} allows us to check
    if the directory exists
* `Turtle.testfile :: MonadIO io => FilePath -> io Bool`{.haskell} lets us check if
    the file exists

to check if the file and folder we care about exists.



Now, we're writing the `run` functions that tie everything up. `runAdd`:

* Checks that the teleport point is valid.
* Checks that there is no other point of the same name.

if both these conditions hold true, it proceeds to create the point
and save the data.
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




We just iterate over all the teleport points, printing them
one-by-one. Since we need an "effect" to happen for each `tpPoint`
(it needs to be printed), we use `(forM_ :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ())`{.haskell}
to achieve that.
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



To remove a teleport point:

* check if a teleport point with the name exists
* If it does, filter it out and save the rest of the points
* Otherwise, print an error

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

The proces of going to a teleport point is slightly different, since  our command (`teleport`)
cannot change the working directory of another process (the shell).

So, we:

* run `teleport`(the executable) within a shell script (`teleport.sh`)
* return a special value (`2`) to the person who runs `teleport` (which is `teleport.sh`)
* have `teleport.sh` execute a `cd` when it detects a return value of `2`.

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
when `tp goto` succeeds, we print out the path to the output stream in Haskell
and returns a return code of `2`. The shell script sees that the return code is `2`, so
it runs a `cd` to the correct path

If `tp` returns any code other than `2`, the shell script echoes all the output to the screen.


Now, we see all of it together in our `run` function which was called by `main`
We simply pattern match on the command and then call the correct `run*` function
\begin{code}
run :: Command -> IO ()
run command = 
    case command of
        CommandAdd addOpt -> runAdd addOpt
        CommandList -> runList
        CommandRemove removeOpt -> runRemove removeOpt
        CommandGoto gotoOpt -> runGoto gotoOpt
\end{code}


<h2> Finale and Conclusion </h2>

Hopefully, this gave you a decent overview on how to combine libraries and use
all of them in Haskell. If there are any bugs/comments, please do report them
at [the github repository](https://github.com/bollu/teleport)



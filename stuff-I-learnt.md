* turtle canonicalize path is weird
* no awesome library for config files
* there are awesome libraries for JSON
* applicative style parsing
* Haskell string types are a pain
* Haskell newtypes in documentation make it hard to see what is actually happening
* debugging in haskell with trace
* when haskell works, it works beautifully

* design decision, make a variable of type `IO a`, that creates accessing it a
side effect but simplifies life, or add an extra  `a -> ...` parameter
everywhere

`getWarpDataPath :: IO FilePath`

now, functions like

`loadData` that are responsible to load data can either be

1. `loadData :: IO String` and call `getWarpDataPath` internally, or the can be

2. `loadData :: FilePath -> IO String` and ask for the `warpDataPath` from the user


I think (2)


* Haskell needs a good library for box-drawing

* Having non-named positional arguments is weird in optparse-applicative

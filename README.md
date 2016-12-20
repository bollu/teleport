# Teleport
### A lightning-fast tool to quickly switch between repositories.
#### [View Literate Haskell tutorial for the project here](http://bollu.github.io/teleport)

[![Build Status](https://travis-ci.org/bollu/teleport.svg?branch=master)](https://travis-ci.org/bollu/teleport)


This tool is written as a basic introduction on writing CLIs in haskell, and a
practical one at that. I'll be showing

- Parsing command line arguments using `optparse-applicative`
- Persisting data needed for tools using JSON using `aeson`
- actually doing something useful using `turtle`
- uploading to hackage


# Getting started

## Install script

To quickly download, install, and build, run

```bash
$ git clone https://github.com/bollu/teleport.git && cd teleport && cabal build && cabal install teleport
```

To use the `teleport` wrapper you will need, run 

```bash
$ echo source `pwd`/teleport.sh >> ~/.bashrc
```

change `~/.bashrc` to the correct shell needed

# Contributing

Pull requests and bug reports welcome! This is my first haskell "tutorial", so there are quite possibly a lot of stupid mistakes

# License

Copyright (c) 2015 Siddharth Bhat

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.




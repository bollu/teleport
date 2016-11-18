#!/usr/bin/env bash

pandoc -t html -f markdown+lhs app/Main.lhs > index.html && open index.html

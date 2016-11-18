#!/usr/bin/env bash

# pandoc -c github-markdown.css -t html -f markdown+lhs app/Main.lhs > index.html
pandoc -c pandoc.css  -t html -f markdown+lhs app/Main.lhs > index.html

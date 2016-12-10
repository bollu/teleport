#!/usr/bin/env bash

# pandoc -c github-markdown.css -t html -f markdown+lhs app/Main.lhs > index.html
pandoc --smart -c pandoc.css  -t html -f markdown+lhs app/Main.lhs > index-notoc.html
pandoc -c pandoc.css index-notoc.html --standalone --toc --toc-depth=3 --self-contained > index.html
rm index-notoc.html

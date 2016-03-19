#!/usr/bin/env bash

stack build
for i in 5 8 16 24 120 600; do
    stack exec vefc-parser cell$i data/${i}cell_vefc.txt > ~/Development/elm-fourspace/src/elm/Four/Geometry/Cell$i.elm;
done

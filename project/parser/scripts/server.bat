@echo off

cd ..
nodemon --exec "stack build && stack exec surge expose" --ext ".hs" --watch .
cd scripts

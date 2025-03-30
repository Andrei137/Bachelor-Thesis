@echo off

set install="%1"
cd ..
echo Building the project...
stack build
if %install%=="-i" (
    stack install
)
cd scripts

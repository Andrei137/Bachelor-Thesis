@echo off
setlocal enabledelayedexpansion

set build=False
set clear=False
set file="%1"
set interpret=
set prettify=
set ast=

shift
:parse_flags
if "%1"=="" goto end_parse
set flag=%1
if "%flag:~0,1%"=="-" (
    set flag=%flag:~1%
) else (
    goto help_message
)
for /l %%i in (0,1,2) do (
    set "char=!flag!"
    set "char=!char:~%%i,1!"
    if "!char!"=="" goto end_parse
    if "!char!"=="b" (
        set build=True
    ) else if "!char!"=="c" (
        set clear=True
    ) else if "!char!"=="p" (
        set prettify=prettify
    ) else if "!char!"=="a" (
        set ast=-ast
    ) else if "!char!"=="i" (
        set interpret=run
    ) else (
        goto help_message
    )
)
shift
goto parse_flags
:end_parse

if %build%==True (
    if %clear%==True (
        cls && call build.bat && cls && goto run
    ) else (
        call build.bat && goto run
    )
) else (
    if %clear%==True (
        cls
    )
    goto run
)
:end
endlocal
exit /b 0

:run
cd ..
stack exec surge -- %file% %interpret% %prettify% %ast%
cd scripts
goto end

:help_message
echo Usage: %0 [OPTIONS]
echo Param:
echo  -h     Display this help message
echo  -b     Build the project
echo  -c     Clear the console
echo  -i     Interpret the code
echo  -a     Get the code AST
echo  -p     Prettify the code
exit /b

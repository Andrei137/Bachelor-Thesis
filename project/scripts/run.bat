@echo off
setlocal enabledelayedexpansion

set build=False
set clear=False
set pretty=Off

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
        set pretty=On
    ) else (
        goto help_message
    )
)
shift
goto parse_flags
:end_parse

echo Running the project...

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
stack exec main %pretty%
cd scripts
goto end

:help_message
echo Usage: %0 [OPTIONS]
echo Param:
echo  -h     Display this help message
echo  -b     Build the project
echo  -c     Clear the console
echo  -p     Prettify the output
exit /b

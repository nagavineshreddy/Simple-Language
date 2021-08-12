@echo off
if [%1]==[] goto usage
if "%~x1" NEQ ".sim" (
    echo File containing source code should have a .sim extension
    goto end
)
set filename='%1'
set currentdir="%cd%"
@echo on
swipl -g catch(simple(%filename%),Exception,write(Exception)) -t halt %currentdir%/src/parser.pl
@echo off
goto end
:usage
@echo Usage: %0 ^<FilePath^>
:end
@echo off
echo makeit.bat will compile "new.bin" from "lungris2.bin" and data files.
stortime
echo generating op/ed...
openfilt
rem clean scripts
sort16
rem dual tile count
conv16-1
if errorlevel 1 goto halted
rem convert regular eucs
conv16-2
if errorlevel 1 goto halted
rem convert lookup table eucs
conv16-l
if errorlevel 1 goto halted
rem convert 8x8
conv8x8
rem continue with script
conv16-3
if errorlevel 1 goto halted
import.exe
echo.
echo Process completed successfully!
calctime
echo.
goto ende
:halted
echo.
echo.
echo Warning, process aborted! Please rerun "makeit"
calctime
echo.
:ende
del timerset.dat


REM  Find Visual Studio [Express] in one of the usual places.
REM  Expects something like "x86", "amd64", or "x86_amd64" as an argument.

set VCMODE=%1

REM For 2022 and later, look in "Program Files"
set Applications=%ProgramFiles%

set VCVARBAT=%Applications%\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat

REM For 2019 and earlier, look in "Program Files (x86)"
set Applications=%ProgramFiles(x86)%
if "%Applications%" == "" set Applications=%ProgramFiles%

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2019\Enterprise\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 14.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 13.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 12.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 11.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 10.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 9.0\vc\vcvarsall.bat

"%VCVARBAT%" %VCMODE%

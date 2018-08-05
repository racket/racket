
REM  Find Visual Studio [Express] in one of the usual places.
REM  Expects something like "x86", "amd64", or "x86_amd64" as an argument.

set VCMODE=%1

set Applications=%ProgramFiles(x86)%
if "%Applications%" == "" set Applications=%ProgramFiles%

set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 14.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 13.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 12.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 11.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 10.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=%Applications%\Microsoft Visual Studio 9.0\vc\vcvarsall.bat

"%VCVARBAT%" %VCMODE%

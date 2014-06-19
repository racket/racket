
REM  Find Visual Studio [Express] in one of the usual places.
REM  Expects something like "x86", "amd64", or "x86_amd64" as an argument.

set VCMODE=%1

set VCVARBAT=C:\Program Files\Microsoft Visual Studio 12.0\vc\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files (x86)\Microsoft Visual Studio 12.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files\Microsoft Visual Studio 11.0\vc\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files (x86)\Microsoft Visual Studio 11.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files\Microsoft Visual Studio 10.0\vc\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files (x86)\Microsoft Visual Studio 10.0\vc\vcvarsall.bat

if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files\Microsoft Visual Studio 9.0\vc\vcvarsall.bat
if not exist "%VCVARBAT%" set VCVARBAT=C:\Program Files (x86)\Microsoft Visual Studio 9.0\vc\vcvarsall.bat

"%VCVARBAT%" %VCMODE%

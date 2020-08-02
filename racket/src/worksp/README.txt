This directory contains scripts, solution files, and project files for
building Racket using Visual Studio. Some parts or variants can be
built using projects, but most use Visual Studio command-line tools.

The Racket BC implementation also compiles with MinGW; see
"...\README.txt".


========================================================================
 Building from the Command Line via Visual Studio
========================================================================

If `cl.exe` and `msbuild.exe` are not already in your path --- along
with many associated environment variables --- then you can run

    msvcprep.bat

from any directory to configure your environment, assuming that Visual
Studio is in one of its usual locations. The "msvcprep.bat" script
tries to find Visual Studio and run its "vcvarsall.bat". Provide an
argument such as "x86" (32-bit mode) or "x86_amd64" (64-bit mode) to
select the build mode.

When using PowerShell, run the "msvcprep.ps1" script instead of
"msvcprep.bat".  Running the latter from PowerShell will appear to
work, but will not actually change any environment variables.

After you have Visual Studio command-line tools set up, then you can
build either the Racket BC or Racket CS implementations (or both).

Racket CS
---------

Build the Racket CS implementation using

   build-cs.bat

which builds "..\..\RacketBC.exe" to bootstrap the build, because the
main build script is implemented in Racket.

To instead build using an existing Racket installation (version 7.1 or
later), use

   racket.exe csbuild.rkt

The result is "..\..\Racket.exe", DLLs and "GRacket.exe" in
"..\..\lib", and other files in "..\..\lib", "..\..\etc", etc.
Many intermediate files will be put in "../build".

To add a "CS" suffix to the generated executables, call "csbuild.rkt"
with `--racketcs-suffix "CS"`.

See also "Completing the Build" below.

Racket BC
---------

Build the Racket BC implementation using

   build.bat

The result is "..\..\RacketBC.exe", DLLs and "GRacketBC.exe" in
"..\..\lib", and other files in "..\..\lib", "..\..\etc", etc.

A "..\..\RacketCGC.exe" executable and associated DLLs are built in
the process of building "..\..\RacketBC.exe".

To avoid the "BC" suffix, set the `UNDERSCORE_BC_SUFFIX` environment
variable to `_` before running "build.bat".

See also "Completing the Build" below.

Both Racket BC and Racket CS
----------------------------

A Racket BC build with a "BC" suffix is also configured to read and
bytecode in a "bc" subdirectory of "compiled". COnsequently, a
"RacketBC.exe" build can coexist with a CS build as "Racket.exe". So,
the sequence

   build.bat
   ..\..\RacketBC.exe csbuild.rkt

builds both.

Completing the Build
--------------------

The "build.bat" and "csbuild.rkt" scripts do not install support DLLs
for encoding conversion, extflonums, and OpenSLL. To install those
libraries, finish with

   ..\..\raco pkg install racket-lib

If you are building from a source distribution (as opposed to a Git
repository checkout), then most likely "racket-lib" is already
included and installed as part of the the distribution, but without
Windows-specific dependencies of "racket-lib". In that case, instead
of the above command, use

   ..\..\raco pkg update --auto racket-lib

Only if you are starting completely from scratch, see also
"..\native-lib\README.txt".


========================================================================
 Building via Visual Studio Projects/Solutions
========================================================================

The Racket BC implementation's CGC variant can be built and debugged
using visual Studio solutions and project. (See "..\racket\README.txt"
for information on Racket BC variants.) Further steps using the
command line can then build the 3m variant and related executables.

The CGC implementation is split into several projects that are grouped
into a few solutions. To build the `X' solution with Visual Studio,
open the file racket\src\worksp\X\X.sln, but add `9' before ".sln" if
you're using Visual Studio 2008 (i.e., version 9.0). The solution
files without a number are for Visual Studio 2010, but they should
upgrade automatically for later versions.

 [The .vcproj files are used by the ...9.sln solutions, while the
  .vcxproj files are used by the other .sln solutions. The latter are
  compatible with Visual Studio 2010. For Visual Studio versions later
  than 2010, "build.bat" script auto-upgrades projects in a copy whose
  file name ends with a literal "X" to match the current tool version,
  but you can also just upgrade them within your version of Visual
  Studio.]

To build RacketCGC, build the Racket solution in

  racket\src\worksp\racket - makes racket\RacketCGC.exe

  [When you open the solution, switch to a "Release" configuration
   before building.]

To build GRacketCGC, build the GRacket solution:

  racket\src\worksp\gracket - makes racket\GRacketCGC.exe

  [Again, switch to a "Release" configuration if necessary.]

The build processes for RacketCGC and GRacketCGC automatically builds

  sgc        - makes racket\lib\libmzgcxxxxxxx.dll and
               racket\src\worksp\sgc\Release\libmzgcxxxxxxx.lib

  libracket  - makes racket\lib\libracketxxxxxxx.dll and
               racket\src\worksp\mzsrc\Release\mzsrcxxxxxxx.lib

In addition, building RacketCGC executes

    ..\racket\dynsrc\mkmzdyn.bat

which copies ".exp", ".obj", and ".lib" files into "..\..\lib".

Building Racket3m and GRacket3m
-------------------------------

After "RacketCGC.exe" is built, you can can build 3m executables:

  1. Ensure that the Visual Studio command-line tools are in your path
     as described above in "Building from the Command Line via Visual
     Studio".

  2. Change directories to "gc2" and run

       ..\..\..\racketcgc.exe -c make.rkt

The resulting "..\..\Racket.exe" will appear with the DLL
"libracket3mxxxxxxx.dll" in "..\..\\lib". (Unlike the CGC build, there
is no corresponding "libmzgc3mxxxxxxx.dll". Instead, it is merged with
"libracket3mxxxxxxx.dll".)

Building Collections and Other Executables
------------------------------------------

If you're building from scratch, you'll also want the starter programs
used by the launcher collection to create "raco.exe". Build the
following solutions:

  racket\src\worksp\mzstart - makes racket\collects\launcher\mzstart.exe
  racket\src\worksp\mrstart - makes racket\collects\launcher\mrstart.exe

  [The "mzstart" and "mrstart" programs have no CGC versus 3m
   distinction.]

Then, set up all the other executables (besides GRacket[CGC].exe and
Racket[CGC].exe) by running

    racket.exe -l- setup

Building MzCOM
--------------

Building "MzCOMCGC.exe" is similar to building "RacketCGC.exe".
Building the 3m variant "MzCOM.exe" is a little different.

To build MzCOMCGC, make the MzCOM solution in

    racket\src\worksp\mzcom - makes racket\MzCOMCGC.exe

Use the "Release" configuration. The result is
"..\..\lib\MzCOMCGC.exe".

After building MzCOMCGC, you can build the 3m variant by

  1. Change directories to "mzcom" and run

       ..\..\..\racketcgc.exe -c xform.rkt

  2. Switch to the "3m" configuration in the MzCOM solution (in Visual
     Studio).

  3. Build (in Visual Studio).

The result is "..\..\lib\MzCOM.exe".

========================================================================
 Versioning
========================================================================

The "xxxxxxx" in DLL names is a placeholder for a version number.
Embedding a version number in a DLL name appears to be the simplest
and surest way to avoid version confusion.

For local testing, you can use the "xxxxxxx" libraries directly. For
any executables that will be distributed, however, the placeholder
should be replaced with a specific version.

To replace the "xxxxxxx" with a specific version, run

    racket -l setup/winvers

The `setup/winvers` module will have to make a temporary copy of
"Racket.exe" and the "lib" sub-directory (into the temporary
directory), and it will re-launch Racket a couple of times. Every
".exe", ".dll", ".lib", ".def", ".exp", and ".pdb" file within the
"racket" tree is updated to replace "xxxxxxxx" with a specific version
number.


========================================================================
 Finding DLLs
========================================================================

Since the "libracket3mxxxxxxx.dll" or "libracketcsxxxxxxx.dll" (or
"libmzgcxxxxxxx.dll" and "libracketxxxxxxx.dll") is installed in a
"lib" subdirectory next to the executable, the normal search path for
DLLs would not find them when running "Racket.exe". To find the DLLs,
the executables are "delayload" linked with the DLLs, and the
executables explicitly load the DLLs from "lib" on start-up.

The relative DLL path is embedded in each executable, and it can be
replaced with a path of up to 512 characters. The path is stored in
the executable in wide-character format, and it is stored immediately
after the wide-character tag "dLl dIRECTORy:" with a wide NUL
terminator. The path can be either absolute or relative; in the latter
case, the relative path is resolved with respect to the executable.
Replacing the first character of the path with "<" disables the
explicit DLL load, so that the DLLs must appear in the normal DLL
search path.

See also "..\start\README.txt" for information on the embedded
"collects" path in executables.


========================================================================
 Embedding
========================================================================

The Racket BC implementation's DLLs can be used within an embedding
application.

The libraries

    racket\lib\win32\msvc\libracket3mxxxxxxx.lib
    racket\lib\win32\msvc\libracketxxxxxxx.lib
    racket\lib\win32\msvc\libmzgcxxxxxxx.lib

provide linking information for using the "libracket3mxxxxxxx.dll",
"libracketxxxxxxx.dll", and "libmzgcxxxxxxx.dll" DLLs. The versioning
script adjusts the names, as described above.

See the "Inside Racket" manual for more information about using these
libraries to embed Racket in an application.

If you need Racket to link to a DLL-based C library (instead of
statically linking to the C library within the Racket DLL), then
compile Racket with the /MD flag.

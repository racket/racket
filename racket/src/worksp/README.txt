This directory contains scripts, resources, and other Windows-specific
content for a Racket build.

========================================================================
 Building from a Source Distribution
========================================================================

If you are building from a source distribution (as opposed to a Git
repository checkout), then beware that a regular/full Racket
distribution will not build correctly. A regular source distribution
is intended for Unix platforms, and it does not include native
libraries that are needed on Windows. You should start with a source
distribution that is labelled "Minimal Racket", instead.

When building from a minimal Racket source distribution, then most
likely "racket-lib" is already included and installed as part of the
the distribution, but without Windows-specific dependencies of
"racket-lib". After following steps below to build and install,
complete the build (in the "src" parent directory of "worksp") with

   ..\raco pkg update --auto racket-lib

If your goal is to arrive at the same content as a regular Racket
distribution, then after building and installing minimal Racket,
finish with

   ..\raco pkg install -i main-distribution


========================================================================
 Building from the Command Line via Visual Studio
========================================================================

If `cl.exe` and `msbuild.exe` are not already in your path --- along
with many associated environment variables --- then you can run

    msvcprep.bat

from any directory to configure your environment, assuming that Visual
Studio is in one of its usual locations. The "msvcprep.bat" script
tries to find Visual Studio and run its "vcvarsall.bat". Provide an
argument such as "x86" (32-bit Intel mode), "x86_amd64" (64-bit Intel
mode), or "x64_arm64" (64-bit Arm mode) to select the build mode.

When using PowerShell, run the "msvcprep.ps1" script instead of
"msvcprep.bat".  Running the latter from PowerShell will appear to
work, but will not actually change any environment variables.

After you have Visual Studio command-line tools set up, then you can
build either the Racket BC or Racket CS implementations (or both).

Racket CS
---------

Build the Racket CS implementation from the "src" parent directory of
"worksp" using

   winfig.bat
   nmake
   nmake install

The result is "..\Racket.exe", DLLs and "GRacket.exe" in "..\lib", and
other files in "..\lib", "..\etc", etc.

To add a "CS" suffix to the generated executables, call "winfig.bat"
with `/suffix CS`.

To disable compression of embedded boot files, set the
`PLT_BOOTFILE_NO_COMPRESS` environment variable (to anything). To
increase compression of compiled code (at the expense of load times),
set the `PLT_CS_MAKE_COMPRESSED_DATA`. To instead disable the
compression of compiled code, set `PLT_CS_MAKE_NO_COMPRESSED`.

See also "Completing the Build" below.

Racket BC
---------

Build the Racket BC implementation from the "src" parent directory of
"worksp" using

   winfig.bat /bconly /suffix BC
   nmake
   nmake install

The result is "..\RacketBC.exe", DLLs and "GRacketBC.exe" in "..\lib",
and other files in "..\lib", "..\etc", etc.

To avoid the "BC" suffix, omit `/suffix BC`.

See also "Completing the Build" below.

Both Racket BC and Racket CS
----------------------------

A Racket BC build with a "BC" suffix is also configured to read and
bytecode in a "bc" subdirectory of "compiled". Consequently, a
"RacketBC.exe" build can coexist with a CS build as "Racket.exe". So,
the sequence

   winfig.bat /both

configures a build for both, but `nmake bc` and `nmake bc-install`
will be needed to build and install BC.

Completing the Build
--------------------

The build scripts for Racket do not install support DLLs for encoding
conversion, extflonums (in BC), and OpenSLL. To install those
libraries, finish with

   ..\raco pkg install racket-lib

If you are building from a source distribution (as opposed to a Git
repository checkout), see "Building from a Source Distribution" above.

Only if you are starting completely from scratch, see also
"..\..\native-lib\README.txt".


========================================================================
 Building from the Command Line via MinGW
========================================================================

You can build Racket on Windows using MinGW and a Unix-like shell such
as MSYS2. follow the instructions for a Unix build as described in
"..\README.txt" --- but see also "Completing the Build" in the Visual
Studio section above, because that also applies to a MinGW-based
build.


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

The Racket implementation's DLLs can be used within an embedding
application.

The definition files

    racket\lib\libracketcsxxxxxxx.def
    racket\lib\libracket3mxxxxxxx.def
    racket\lib\libracketxxxxxxx.def
    racket\lib\libmzgcxxxxxxx.def

provide linking information for using the "libracketcsxxxxxxx.dll",
"libracket3mxxxxxxx.dll", "libracketxxxxxxx.dll", and
"libmzgcxxxxxxx.dll" DLLs. The versioning script adjusts the names, as
described above.

See the "Inside Racket" manual for more information about using these
libraries to embed Racket in an application.

If you need Racket to link to a DLL-based C library (instead of
statically linking to the C library within the Racket DLL), then
compile Racket with the /MD flag.

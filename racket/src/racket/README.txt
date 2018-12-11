This directory contains most of the source code to the traditional
implementation of Racket. See "../README.txt" for general information
on building.


========================================================================
 CGC versus 3m
========================================================================

Traditional Racket and GRacket have two variants: CGC and 3m. The CGC
variant is older, and it cooperates more easily with extensions
written in C. The 3m variant is the default: it is more robust and
usually provides better overall performance.

The default build mode creates 3m executables only (except for a CGC
executable that is used to build the 3m executable). To create CGC
executables in addition to 3m executables, use `make cgc` in addition
to `make`, or run `make both`. To install both variants, use `make
install-both` instead of just `make install`. Alternatively, use just
`make cgc` and `make install-cgc` to build and install just the CGC
variants.

CGC variants are installed with a "cgc" suffix.  To swap the default
build and install mode, supply `--enable-cgcdefault` to `configure`.  In
that case, CGC variants are built by default, `make 3m` creates 3m
executables, and `make install-both` installs CGC variants without a suffix
and 3m variants with a "3m" suffix.


========================================================================
 Additional Compilation Notes
========================================================================

CGC Build Options
-----------------

As noted above in "CGC versus 3m", Racket builds a CGC variant in the
process of creating the normal 3m variant. Within the CGC variant, two
implementations are possible.

By default, Racket CGC is implemented with SenoraGC (in the "sgc"
directory), which is relativey portable. Provide `--disable-sgc` to
instead use the Boehm GC (in the "gc" directory), which should perform
better and was the default for Racket CGC through version 6.1.

The variant of the Boehm GC that is included with Racket has been
modified slightly from Boehm's standard distribution; mostly, the
changes modify the way that object finalization is handled.

Floating point, x87, SSE, Extflonums, and the JIT
-------------------------------------------------

Pre-processor tests in "sconfig.h" and "scheme.h" attempt to determine
when the x87 floating-point processor needs to be configured for
double-precision mode, when JIT can use SSE2 instructions, and when
extflonums can be supported because both the JIT and C code use SSE2
for double-precision floating-point while `long double` is available
for extflonums.

In particular, "scheme.h" looks for __SSE2_MATH__ to indicate that gcc
is compiling floating-point operations as SSE2, so be sure to include
flags like "-mfpmath=sse" or "-mfpmath=387" in CPPFLAGS, and not just
CFLAGS. See related configuration options below.

The Windows build using MSVC enables extflonum support through a
MinGW-compiled "longdouble.dll", since MSVC does not support `long
double` as extended-precision floating point.

Configuration Options
---------------------

Although `configure` flags control most options, some configurations
options can be modified by setting flags in "racket/sconfig.h".

Some CPP flags control default settings in "racket/sconfig.h":

 * MZ_{USE_,NO_}JIT_SSE - {en,dis}ables use of SSE2 floating point

 * MZ_USE_DETERMINSTIC_FUEL - disables use of itimer or pthread for
   Racket thread scheduling.

 * C_COMPILER_USES_SSE - declares that the C compiler is using SSE2
   instructions to implement `double` floating-point operations.


========================================================================
 Porting to New Platforms
========================================================================

At a minimum, to port Racket to a new platform, edit "racket/sconfig.h"
and "rktio/rktio_platform.h" to provide a platform-specific compilation
information.


========================================================================
 Modifying Racket
========================================================================

If you modify Racket and change any primitive syntax or the collection
of built-in identifiers, be sure to update the version number in
"racket/src/schvers.h", so that various tools know to rebuild
bytecode. If you add or remove primitives, you'll also need to adjust
the counter in "racket/src/schminc.h" .

Some general guidelines for modying this code:

 * Use `scheme_...` for a definition that is globally visible. In
   particular, any non-`static` function name should start with
   `scheme_`.

 * If a globally visible function is meant for exporting to embedding
   applications, extensions, or through the FFI, put the prototype in
   "src/schemef.h" (and re-generate files with `make exports`).
   Functions available only to embedding applications can go in
   "scheme.h". Otherwise, put global function prototypes in
   "schpriv.h".

 * Don't use "//" comments, because we still use pre-C99 compilers
   that don't support them.

 * Declare all variables at the beginning of a block (again, for
   pre-C99 compilers).

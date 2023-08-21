This directory contains most of the source code to the Racket BC
implementation. See "../README.txt" for general information on
building.

You may need to use `zuo` instead of `make` for working at the level
of this directory. See "../zuo".


========================================================================
 CGC versus 3m
========================================================================

Racket BC and GRacket BC have two variants: CGC and 3m. The CGC
variant is older, and it cooperates more easily with extensions
written in C. The 3m variant is the default: it is more robust and
usually provides better overall performance.

The default build mode creates 3m executables only (except for a CGC
executable that is used to build the 3m executable). To create CGC
executables in addition to 3m executables, use `zuo . cgc` in addition
to `zuo`, or run `zuo . both`. To install both variants, use `zuo .
install-both` instead of just `zuo . install`. Alternatively, use just
`zuo . cgc` and `zuo . install-cgc` to build and install just the CGC
variants.

CGC variants are installed with a "cgc" suffix. To swap the default
build and install mode, supply `--enable-cgcdefault` to `configure`.
In that case, CGC variants are built by default, `zuo . 3m` creates 3m
executables, and `zuo . install-both` installs CGC variants without a
suffix and 3m variants with a "3m" suffix.


========================================================================
 Additional Compilation Notes
========================================================================

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

If you modify Racket in a way that changes compiled code, including
changing the set of primitives, be sure to update the version number
in "../version/racket_version.h", so that various tools know to
rebuild bytecode. If you add or remove primitives, you'll also need to
adjust the counter in "src/schminc.h" .

Some general guidelines for modying this code:

 * Use `scheme_...` for a definition that is globally visible. In
   particular, any non-`static` function name should start with
   `scheme_`.

 * If a globally visible function is meant for exporting to embedding
   applications, extensions, or through the FFI, put the prototype in
   "src/schemef.h" (and re-generate files with `zuo . exports`).
   Functions available only to embedding applications can go in
   "scheme.h". Otherwise, put global function prototypes in
   "schpriv.h".

 * Don't use "//" comments, because we still use pre-C99 compilers
   that don't support them.

 * Declare all variables at the beginning of a block (again, for
   pre-C99 compilers).

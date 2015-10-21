#lang scribble/doc
@(require "utils.rkt")

@(define cgc-v-3m "CGC versus 3m")

@title[#:tag "embedding"]{Embedding into a Program}

@section-index["embedding Racket"]

The Racket run-time system can be embedded into a larger program.  The
embedding process for Racket CGC or Racket 3m (see @secref[cgc-v-3m])
is essentially the same, but the process for Racket 3m is most easily
understood as a variant of the process for Racket CGC.

@section{CGC Embedding}

To embed Racket CGC in a program, follow these steps:

@itemize[

 @item{Locate or build the Racket CGC libraries. Since the
  standard distribution provides 3m libraries, only, you will most
  likely have to build from source.

  On Unix, the libraries are @as-index{@filepath{libracket.a}}
  and @as-index{@filepath{libmzgc.a}} (or
  @as-index{@filepath{libracket.so}} and
  @as-index{@filepath{libmzgc.so}} for a dynamic-library build, with
  @as-index{@filepath{libracket.la}} and
  @as-index{@filepath{libmzgc.la}} files for use with
  @exec{libtool}). Building from source and installing places the
  libraries into the installation's @filepath{lib} directory. Be sure
  to build the CGC variant, since the default is 3m.

  On Windows, stub libraries for use with Microsoft tools are
  @filepath{libracket@italic{x}.lib} and
  @filepath{libmzgc@italic{x}.lib} (where @italic{x} represents the
  version number) are in a compiler-specific directory in
  @filepath{racket\lib}. These libraries identify the bindings that are
  provided by @filepath{libracket@italic{x}.dll} and
  @filepath{libmzgc@italic{x}.dll} --- which are typically installed
  in @filepath{racket\lib}. When linking with Cygwin, link to
  @filepath{libracket@italic{x}.dll} and
  @filepath{libmzgc@italic{x}.dll} directly.  At run time, either
  @filepath{libracket@italic{x}.dll} and
  @filepath{libmzgc@italic{x}.dll} must be moved to a location in the
  standard DLL search path, or your embedding application must
  ``delayload'' link the DLLs and explicitly load them before
  use. (@filepath{Racket.exe} and @filepath{GRacket.exe} use the latter
  strategy.)

  On Mac OS X, dynamic libraries are provided by the
  @filepath{Racket} framework, which is typically installed in
  @filepath{lib} sub-directory of the installation. Supply
  @exec{-framework Racket} to @exec{gcc} when linking, along
  with @exec{-F} and a path to the @filepath{lib} directory. Beware
  that CGC and 3m libraries are installed as different versions within
  a single framework, and installation marks one version or the other
  as the default (by setting symbolic links); install only CGC to
  simplify accessing the CGC version within the framework.  At run
  time, either @filepath{Racket.framework} must be moved to a
  location in the standard framework search path, or your embedding
  executable must provide a specific path to the framework (possibly
  an executable-relative path using the Mach-O @tt["@executable_path"]
  prefix).}

 @item{For each C/C++ file that uses Racket library functions,
  @cpp{#include} the file @as-index{@filepath{scheme.h}}.

  The C preprocessor symbol @cppi{SCHEME_DIRECT_EMBEDDED} is defined
  as @cpp{1} when @filepath{scheme.h} is @cpp{#include}d, or as
  @cpp{0} when @filepath{escheme.h} is @cpp{#include}d.

  The @filepath{scheme.h} file is distributed with the Racket software in
  the installation's @filepath{include} directory. Building and
  installing from source also places this file in the installation's
  @filepath{include} directory.}

 @item{Start your main program through the @cpp{scheme_main_setup} (or
  @cpp{scheme_main_stack_setup}) trampoline, and put all uses of
  Racket functions inside the function passed to
  @cpp{scheme_main_setup}. The @cpp{scheme_main_setup} function
  registers the current C stack location with the memory manager. It
  also creates the initial namespace @cpp{Scheme_Env*} by calling
  @cppi{scheme_basic_env} and passing the result to the function
  provided to @cpp{scheme_main_setup}. (The
  @cpp{scheme_main_stack_setup} trampoline registers the C stack with
  the memory manager without creating a namespace.)

  On Windows, when support for parallelism is enabled in the Racket
  build (as is the default), then before calling
  @cpp{scheme_main_setup}, your embedding application must first call
  @cppi{scheme_register_tls_space}:

  @verbatim[#:indent 2]{
   scheme_register_tls_space(&tls_space, 0);
  }

  where @cpp{tls_space} is declared as a thread-local pointer variable
  in the main executable (i.e., not in a dynamically linked DLL):

  @verbatim[#:indent 2]{
   static __declspec(thread) void *tls_space;
  }

  @history[#:changed "6.3" @elem{Calling @cpp{scheme_register_tls_space} is
                                 required on all Windows variants, although the call
                                 may be a no-op, depending on how Racket is
                                 built.}]}

 @item{Configure the namespace by adding module declarations. The
  initial namespace contains declarations only for a few primitive
  modules, such as @racket['#%kernel], and no bindings are imported
  into the top-level environment.

  To embed a module like @racketmodname[racket/base] (along with all
  its dependencies), use @exec{raco ctool --c-mods}, which generates a C file
  that contains modules in bytecode form as encapsulated in a static
  array. The generated C file defines a @cppi{declare_modules}
  function that takes a @cpp{Scheme_Env*}, installs the modules into
  the environment, and adjusts the module name resolver to access the
  embedded declarations.

  Alternately, use @cpp{scheme_set_collects_path} and
  @cpp{scheme_init_collection_paths} to configure and install a path
  for finding modules at run time.}

 @item{Access Racket through @cppi{scheme_dynamic_require},
  @cppi{scheme_load}, @cppi{scheme_eval}, and/or other functions
  described in this manual.

  If the embedding program configures built-in parameters in a way
  that should be considered part of the default configuration, then
  call @cpp{scheme_seal_parameters} afterward. The snapshot of
  parameter values taken by @cpp{scheme_seal_parameters} is used for
  certain privileged operations, such as installing a @|PLaneT|
  package.}

 @item{Compile the program and link it with the Racket libraries.}

]

@index['("allocation")]{With} Racket CGC, Racket values are
garbage collected using a conservative garbage collector, so pointers
to Racket objects can be kept in registers, stack variables, or
structures allocated with @cppi{scheme_malloc}. In an embedding
application on some platforms, static variables are also automatically
registered as roots for garbage collection (but see notes below
specific to Mac OS X and Windows).

For example, the following is a simple embedding program which
evaluates all expressions provided on the command line and displays
the results, then runs a @racket[read]-@racket[eval]-@racket[print]
loop. Run

@commandline{raco ctool --c-mods base.c ++lib racket/base}

to generate @filepath{base.c}, which encapsulates @racket[racket/base]
and all of its transitive imports (so that they need not be found
separately a run time).

@verbatim[#:indent 2]{
#include "scheme.h"

#include "base.c"

static int run(Scheme_Env *e, int argc, char *argv[])
{
  Scheme_Object *curout;
  int i;
  mz_jmp_buf * volatile save, fresh;

  /* Declare embedded modules in "base.c": */
  declare_modules(e);

  scheme_namespace_require(scheme_intern_symbol("racket/base"));

  curout = scheme_get_param(scheme_current_config(), 
                            MZCONFIG_OUTPUT_PORT);

  for (i = 1; i < argc; i++) {
    save = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &fresh;
    if (scheme_setjmp(scheme_error_buf)) {
      scheme_current_thread->error_buf = save;
      return -1; /* There was an error */
    } else {
      Scheme_Object *v, *a[2];
      v = scheme_eval_string(argv[i], e);
      scheme_display(v, curout);
      scheme_display(scheme_make_char('\n'), curout);
      /* read-eval-print loop, uses initial Scheme_Env: */
      a[0] = scheme_intern_symbol("racket/base");
      a[1] = scheme_intern_symbol("read-eval-print-loop");
      scheme_apply(scheme_dynamic_require(2, a), 0, NULL);
      scheme_current_thread->error_buf = save;
    }
  }
  return 0;
}

int main(int argc, char *argv[])
{
  return scheme_main_setup(1, run, argc, argv);
}
}

If modules embedded in the executable need to access runtime files
(via @racketmodname[racket/runtime-path] forms), supply the
@DFlag{runtime} flag to @exec{raco ctool}, specifying a directory
where the runtime files are to be gathered. The modules in the
generated @filepath{.c} file will then refer to the files in that
directory; the directory is normally specified relative to the
executable, but the embedding application must call
@cppi{scheme_set_exec_cmd} to set the executable path (typically
@cpp{argv[0]}) before declaring modules.

On Mac OS X, or on Windows when Racket is compiled to a DLL
using Cygwin, the garbage collector cannot find static variables
automatically. In that case, @cppi{scheme_main_setup} must be called with a
non-zero first argument.

On Windows (for any other build mode), the garbage collector finds
static variables in an embedding program by examining all memory
pages. This strategy fails if a program contains multiple Windows
threads; a page may get unmapped by a thread while the collector is
examining the page, causing the collector to crash. To avoid this
problem, call @cpp{scheme_main_setup} with a non-zero first argument.

When an embedding application calls @cpp{scheme_main_setup} with a non-zero
first argument, it must register each of its static variables with
@cppi{MZ_REGISTER_STATIC} if the variable can contain a GCable
pointer. For example, if @cpp{curout} above is made @cpp{static}, then
@cpp{MZ_REGISTER_STATIC(curout)} should be inserted before the call to
@cpp{scheme_get_param}.

When building an embedded Racket CGC to use SenoraGC (SGC) instead of
the default collector, @cpp{scheme_main_setup} must be called with a
non-zero first argument.  See @secref["im:memoryalloc"] for more
information.


@section{3m Embedding}

Racket 3m can be embedded mostly the same as Racket, as long as the
embedding program cooperates with the precise garbage collector as
described in @secref["im:3m"].

In either your source in the in compiler command line, @cpp{#define}
@cpp{MZ_PRECISE_GC} before including @filepath{scheme.h}. When using
@|mzc| with the @DFlag{cc} and @DFlag{3m} flags, @cpp{MZ_PRECISE_GC}
is automatically defined.

In addition, some library details are different:

@itemize[

 @item{On Unix, the library is just
  @as-index{@filepath{libracket3m.a}} (or
  @as-index{@filepath{libracket3m.so}} for a dynamic-library build,
  with @as-index{@filepath{libracket3m.la}} for use with
  @exec{libtool}). There is no separate library for 3m analogous to
  CGC's @filepath{libmzgc.a}.}

 @item{On Windows, the stub library for use with Microsoft tools is
  @filepath{libracket3m@italic{x}.lib} (where @italic{x} represents the
  version number). This library identifies the bindings that are
  provided by @filepath{libracket3m@italic{x}.dll}.  There is no
  separate library for 3m analogous to CGC's
  @filepath{libmzgc@italic{x}.lib}.}

  @item{On Mac OS X, 3m dynamic libraries are provided by the
  @filepath{Racket} framework, just as for CGC, but as a version
  suffixed with @filepath{_3m}.}

]

For Racket 3m, an embedding application must call @cpp{scheme_main_setup}
with a non-zero first argument.

The simple embedding program from the previous section can be
processed by @exec{raco ctool --xform}, then compiled and linked with
Racket 3m.  Alternately, the source code can be extended to work with
either CGC or 3m depending on whether @cpp{MZ_PRECISE_GC} is defined
on the compiler's command line:

@verbatim[#:indent 2]{
#include "scheme.h"

#include "base.c"

static int run(Scheme_Env *e, int argc, char *argv[])
{
  Scheme_Object *curout = NULL, *v = NULL, *a[2] = {NULL, NULL};
  Scheme_Config *config = NULL;
  int i;
  mz_jmp_buf * volatile save = NULL, fresh;

  MZ_GC_DECL_REG(8);
  MZ_GC_VAR_IN_REG(0, e);
  MZ_GC_VAR_IN_REG(1, curout);
  MZ_GC_VAR_IN_REG(2, save);
  MZ_GC_VAR_IN_REG(3, config);
  MZ_GC_VAR_IN_REG(4, v);
  MZ_GC_ARRAY_VAR_IN_REG(5, a, 2);

  MZ_GC_REG();

  declare_modules(e);

  v = scheme_intern_symbol("racket/base");
  scheme_namespace_require(v);

  config = scheme_current_config();
  curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);

  for (i = 1; i < argc; i++) {
    save = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &fresh;
    if (scheme_setjmp(scheme_error_buf)) {
      scheme_current_thread->error_buf = save;
      return -1; /* There was an error */
    } else {
      v = scheme_eval_string(argv[i], e);
      scheme_display(v, curout);
      v = scheme_make_char('\n');
      scheme_display(v, curout);
      /* read-eval-print loop, uses initial Scheme_Env: */
      a[0] = scheme_intern_symbol("racket/base");
      a[1] = scheme_intern_symbol("read-eval-print-loop");
      v = scheme_dynamic_require(2, a);
      scheme_apply(v, 0, NULL);
      scheme_current_thread->error_buf = save;
    }
  }

  MZ_GC_UNREG();

  return 0;
}

int main(int argc, char *argv[])
{
  return scheme_main_setup(1, run, argc, argv);
}
}

Strictly speaking, the @cpp{config} and @cpp{v} variables above need
not be registered with the garbage collector, since their values are
not needed across function calls that allocate. The code is much
easier to maintain, however, when all local variables are registered
and when all temporary values are put into variables.

@; ----------------------------------------
@include-section["hooks.scrbl"]

#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "overview"]{Overview}

@section{``Scheme'' versus ``Racket''}

The old name for Racket was ``PLT Scheme,'' and the core compiler and
run-time system used to be called ``MzScheme.'' The old names are
entrenched in Racket internals, to the point that most C bindings
defined in this manual start with @cpp{scheme_}. They all should be
renamed to start @cpp{racket_}.

@; ----------------------------------------------------------------------

@section{Building Racket from Source}

The normal Racket distribution includes @filepath{.rkt} sources for
collection-based libraries. After modifying library files, run
@exec{raco setup} (see @secref[#:doc '(lib
"scribblings/raco/raco.scrbl") "setup"]) to rebuild installed
libraries.

The normal Racket distribution does not include the C sources for
Racket's run-time system. To build Racket from scratch, download a
source distribution, or get the latest from the @tt{git} repository at
@url{https://github.com/plt/racket}. Detailed build instructions are
in the @filepath{README} file in the top-level @filepath{src}
directory.

@; ----------------------------------------------------------------------

@section{CGC versus 3m}

Before mixing any C code with Racket, first decide whether to use the
@bold{3m} variant of Racket, the @bold{CGC} variant of Racket, or
both:

@itemize[

@item{@bold{@as-index{3m}} : the main variant of Racket, which
  uses @defterm{precise} garbage collection instead of conservative
  garbage collection, and it may move objects in memory during a
  collection.}

@item{@bold{@as-index{CGC}} : the original variant of Racket, where
  memory management depends on a @defterm{conservative} garbage
  collector. The conservative garbage collector can automatically find
  references to managed values from C local variables and (on some
  platforms) static variables.}

]

At the C level, working with CGC can be much easier than working with
3m, but overall system performance is typically better with 3m.

@; ----------------------------------------------------------------------

@section{Writing Racket Extensions}

@section-index["extending Racket"]

The process of creating an extension for 3m or CGC is essentially the
same, but the process for 3m is most easily understood as a variant of
the process for CGC.

@subsection{CGC Extensions}


To write a C/C++-based extension for Racket CGC, follow these
steps:

@itemize[

 @item{@index['("header files")]{For} each C/C++ file that uses
 Racket library functions, @cpp{#include} the file
 @as-index{@filepath{escheme.h}}.

 This file is distributed with the Racket software in an
 @filepath{include} directory, but if @|mzc| is used to
 compile, this path is found automatically.}


 @item{Define the C function @cppi{scheme_initialize}, which takes a
 @cpp{Scheme_Env*} namespace (see @secref["im:env"]) and returns a
 @cpp{Scheme_Object*} Racket value.
 
 This initialization function can install new global primitive
 procedures or other values into the namespace, or it can simply
 return a Racket value. The initialization function is called when the
 extension is loaded with @racket[load-extension] the first time in a
 given @|tech-place|; the return value from @cpp{scheme_initialize} is used
 as the return value for @racket[load-extension]. The namespace
 provided to @cpp{scheme_initialize} is the current namespace when
 @racket[load-extension] is called.}


 @item{Define the C function @cppi{scheme_reload}, which has the same
 arguments and return type as @cpp{scheme_initialize}.

 This function is called if @racket[load-extension] is called a second
 time (or more times) for an extension in a given @|tech-place|. Like
 @cpp{scheme_initialize}, the return value from this function is the
 return value for @racket[load-extension].}


 @item{Define the C function @cppi{scheme_module_name}, which takes
 no arguments and returns a @cpp{Scheme_Object*} value, either a
 symbol or @cpp{scheme_false}.

 The function should return a symbol when the effect of calling
 @cpp{scheme_initialize} and @cpp{scheme_reload} is only to declare
 a module with the returned name. This function is called when the
 extension is loaded to satisfy a @racket[require] declaration.

 The @cpp{scheme_module_name} function may be called before
 @cpp{scheme_initialize} and @cpp{scheme_reload}, after those
 functions, or both before and after, depending on how the extension
 is loaded and re-loaded.}


 @item{Compile the extension C/C++ files to create platform-specific
 object files.

 The @as-index{@|mzc|} compiler, which is distributed with Racket,
 compiles plain C files when the @as-index{@DFlag{cc}} flag is
 specified. More precisely, @|mzc| does not compile the files itself,
 but it locates a C compiler on the system and launches it with the
 appropriate compilation flags.  If the platform is a relatively
 standard Unix system, a Windows system with either Microsoft's C
 compiler or @exec{gcc} in the path, or a Mac OS X system with Apple's
 developer tools installed, then using @|mzc| is typically easier than
 working with the C compiler directly. Use the @as-index{@DFlag{cgc}}
 flag to indicate that the build is for use with Racket CGC.}


 @item{Link the extension C/C++ files with
 @as-index{@filepath{mzdyn.o}} (Unix, Mac OS X) or
 @as-index{@filepath{mzdyn.obj}} (Windows) to create a shared object. The
 resulting shared object should use the extension @filepath{.so} (Unix),
 @filepath{.dll} (Windows), or @filepath{.dylib} (Mac OS X).

 The @filepath{mzdyn} object file is distributed in the installation's
 @filepath{lib} directory. For Windows, the object file is in a
 compiler-specific sub-directory of @filepath{racket\lib}.

 The @|mzc| compiler links object files into an extension when the
 @as-index{@DFlag{ld}} flag is specified, automatically locating
 @filepath{mzdyn}. Again, use the @DFlag{cgc} flag with @|mzc|.}

 @item{Load the shared object within Racket using
 @racket[(load-extension _path)], where @racket[_path] is the name of
 the extension file generated in the previous step.

 Alternately, if the extension defines a module (i.e.,
 @cpp{scheme_module_name} returns a symbol), then place the shared
 object in a special directory with a special name, so that it is
 detected by the module loader when @racket[require] is used. The
 special directory is a platform-specific path that can be obtained by
 evaluating @racket[(build-path "compiled" "native"
 (system-library-subpath))]; see @racket[load/use-compiled] for more
 information.  For example, if the shared object's name is
 @filepath{example_rkt.dll}, then @racket[(require "example.rkt")] will
 be redirected to @filepath{example_rkt.dll} if the latter is placed in
 the sub-directory @racket[(build-path "compiled" "native"
 (system-library-subpath))] and if @filepath{example.rkt} does not
 exist or has an earlier timestamp.

 Note that @racket[(load-extension _path)] within a @racket[module]
 does @italic{not} introduce the extension's definitions into the
 module, because @racket[load-extension] is a run-time operation. To
 introduce an extension's bindings into a module, make sure that the
 extension defines a module, put the extension in the
 platform-specific location as described above, and use
 @racket[require].}

]

@index['("allocation")]{@bold{IMPORTANT:}} With Racket CGC, Racket
values are garbage collected using a conservative garbage collector,
so pointers to Racket objects can be kept in registers, stack
variables, or structures allocated with @cppi{scheme_malloc}. However,
static variables that contain pointers to collectable memory must be
registered using @cppi{scheme_register_extension_global} (see
@secref["im:memoryalloc"]); even then, such static variables must be
thread-local (in the OS-thread sense) to work with multiple
@|tech-place|s (see @secref["places"]).

As an example, the following C code defines an extension that returns
@racket["hello world"] when it is loaded:

@verbatim[#:indent 2]{
 #include "escheme.h"
 Scheme_Object *scheme_initialize(Scheme_Env *env) {
   return scheme_make_utf8_string("hello world");
 }
 Scheme_Object *scheme_reload(Scheme_Env *env) {
   return scheme_initialize(env); /* Nothing special for reload */
 }
 Scheme_Object *scheme_module_name() {
   return scheme_false;
 }
}

Assuming that this code is in the file @filepath{hw.c}, the extension
is compiled on Unix with the following two commands:

@commandline{raco ctool --cgc --cc hw.c}
@commandline{raco ctool --cgc --ld hw.so hw.o}

(Note that the @DFlag{cgc}, @DFlag{cc}, and @DFlag{ld} flags are each
prefixed by two dashes, not one.)

The @filepath{collects/mzscheme/examples} directory in the Racket
distribution contains additional examples.

@subsection{3m Extensions}

To build an extension to work with Racket 3m, the CGC instructions
must be extended as follows:

@itemize[

 @item{Adjust code to cooperate with the garbage collector as
 described in @secref["im:3m"]. Using @|mzc| with the
 @as-index{@DFlag{xform}} might convert your code to implement part of
 the conversion, as described in @secref["im:3m:mzc"].}

 @item{In either your source in the in compiler command line,
 @cpp{#define} @cpp{MZ_PRECISE_GC} before including
 @filepath{escheme.h}. When using @|mzc| with the @DFlag{cc} and
 @as-index{@DFlag{3m}} flags, @cpp{MZ_PRECISE_GC} is automatically
 defined.}

 @item{Link with @as-index{@filepath{mzdyn3m.o}} (Unix, Mac OS X) or
 @as-index{@filepath{mzdyn3m.obj}} (Windows) to create a shared
 object.  When using @|mzc|, use the @DFlag{ld} and @DFlag{3m} flags
 to link to these libraries.}

]

For a relatively simple extension @filepath{hw.c}, the extension is
compiled on Unix for 3m with the following three commands:

@commandline{raco ctool --xform hw.c}
@commandline{raco ctool --3m --cc hw.3m.c}
@commandline{raco ctool --3m --ld hw.so hw_3m.o}

Some examples in @filepath{collects/mzscheme/examples} work with
Racket 3m in this way. A few examples are manually instrumented, in
which case the @DFlag{xform} step should be skipped.

@subsection{Declaring a Module in an Extension}

To create an extension that behaves as a module, return a symbol from
@cpp{scheme_module_name}, and have @cpp{scheme_initialize} and
@cpp{scheme_rename} declare a module using @cpp{scheme_primitive_module}.

For example, the following extension implements a module named
@racket[hello] that exports a binding @racket[greeting]:

@verbatim[#:indent 2]{
  #include "escheme.h"

  Scheme_Object *scheme_initialize(Scheme_Env *env) {
    Scheme_Env *mod_env;
    mod_env = scheme_primitive_module(scheme_intern_symbol("hi"), 
                                      env);
    scheme_add_global("greeting", 
                      scheme_make_utf8_string("hello"), 
                      mod_env);
    scheme_finish_primitive_module(mod_env);
    return scheme_void;
  }

  Scheme_Object *scheme_reload(Scheme_Env *env) {
    return scheme_initialize(env); /* Nothing special for reload */
  }

  Scheme_Object *scheme_module_name() {
    return scheme_intern_symbol("hi");
  }
}

This extension could be compiled for 3m on i386 Linux, for
example, using the following sequence of @exec{mzc} commands:

@commandline{raco ctool --xform hi.c}
@commandline{raco ctool --3m --cc hi.3m.c}
@commandline{mkdir -p compiled/native/i386-linux/3m}
@commandline{raco ctool --3m --ld compiled/native/i386-linux/3m/hi_rkt.so hi_3m.o}

The resulting module can be loaded with

@racketblock[(require "hi.rkt")]

@; ----------------------------------------------------------------------

@section[#:tag "embedding"]{Embedding Racket into a Program}

@section-index["embedding Racket"]

Like creating extensions, the embedding process for Racket CGC or
Racket 3m is essentially the same, but the process for Racket
3m is most easily understood as a variant of the process for
Racket CGC.

@subsection{CGC Embedding}

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

  On 32-bit Windows, when support for parallelism is enabled in the Racket
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
  }}

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


@subsection{3m Embedding}

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

@; ----------------------------------------------------------------------

@section[#:tag "places"]{Racket and Places}

Each Racket @|tech-place| corresponds to a separate OS-implemented
thread. Each place has its own memory manager. Pointers to GC-managed
memory cannot be communicated from one place to another, because such
pointers in one place are invisible to the memory manager of another
place.

When @|tech-place| support is enabled, static variables in an
extension or an embedding generally cannot hold pointers to GC-managed
memory, since the static variable may be used from multiple places.
For some OSes, a static variable can be made thread-local, in which
case it has a different address in each OS thread, and each different
address can be registered with the GC for a given place.

The OS thread that originally calls @cpp{scheme_basic_env} is the OS
thread of the original place. When @cpp{scheme_basic_env} is called a
second time to reset the interpreter, it can be called in an OS thread
that is different from the original call to
@cpp{scheme_basic_env}. Thereafter, the new thread is the OS thread
for the original place.

@; ----------------------------------------------------------------------

@section{Racket and Threads}

Racket implements threads for Racket programs without aid from the
operating system, so that Racket threads are cooperative from the
perspective of C code. On Unix, stand-alone Racket uses a single
OS-implemented thread. On Windows and Mac OS X, stand-alone
Racket uses a few private OS-implemented threads for background
tasks, but these OS-implemented threads are never exposed by the
Racket API.

In an embedding application, Racket can co-exist with additional
OS-implemented threads, but the additional OS threads must not call
any @cpp{scheme_} function.  Only the OS thread representing a
particular @|tech-place| can call @cpp{scheme_} functions. (This
restriction is stronger than saying all calls for a given place must
be serialized across threads. Racket relies on properties of specific
threads to avoid stack overflow and garbage collection.) For the
original place, only the OS thread used to call @cpp{scheme_basic_env}
can call @cpp{scheme_} functions. For any other place, only the OS
thread that is created by Racket for the place can be used to call
@cpp{scheme_} functions.

See @secref["threads"] for more information about threads, including
the possible effects of Racket's thread implementation on extension
and embedding C code.

@; ----------------------------------------------------------------------

@section[#:tag "im:unicode"]{Racket, Unicode, Characters, and Strings}

A character in Racket is a Unicode code point. In C, a character
value has type @cppi{mzchar}, which is an alias for @cpp{unsigned} ---
which is, in turn, 4 bytes for a properly compiled Racket. Thus, a
@cpp{mzchar*} string is effectively a UCS-4 string.

Only a few Racket functions use @cpp{mzchar*}. Instead, most
functions accept @cpp{char*} strings. When such byte strings are to be
used as a character strings, they are interpreted as UTF-8
encodings. A plain ASCII string is always acceptable in such cases,
since the UTF-8 encoding of an ASCII string is itself.

See also @secref["im:strings"] and @secref["im:encodings"].

@; ----------------------------------------------------------------------

@section[#:tag "im:intsize"]{Integers}

Racket expects to be compiled in a mode where @cppi{short} is a
16-bit integer, @cppi{int} is a 32-bit integer, and @cppi{intptr_t} has
the same number of bits as @cpp{void*}. The @cppi{long} type can match
either @cpp{int} or @cpp{intptr_t}, depending on the platform.
The @cppi{mzlonglong} type has
64 bits for compilers that support a 64-bit integer type, otherwise it
is the same as @cpp{intptr_t}; thus, @cpp{mzlonglong} tends to match
@cpp{long long}. The @cppi{umzlonglong} type is the unsigned version
of @cpp{mzlonglong}.

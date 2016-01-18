#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "Writing Racket Extensions"]{Writing Racket Extensions}

@section-index["extending Racket"]

As noted in @secref["embedding-and-extending"], writing Racket code
and using the @seclink["top" #:doc '(lib
"scribblings/foreign/foreign.scrbl")]{foreign-function interface} is
usually a better option than writing an extension to Racket, but
Racket also supports C-implemented extensions that plug more directly
into the run-time system.

The process of creating an extension for Racket 3m or Racket CGC (see
@secref["CGC versus 3m"]) is essentially the same, but the process for
3m is most easily understood as a variant of the process for CGC.

@section{CGC Extensions}


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

@section{3m Extensions}

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

@section{Declaring a Module in an Extension}

To create an extension that behaves as a module, return a symbol from
@cpp{scheme_module_name}, and have @cpp{scheme_initialize} and
@cpp{scheme_reload} declare a module using @cpp{scheme_primitive_module}.

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


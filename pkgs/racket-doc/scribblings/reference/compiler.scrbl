#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "compiler"]{Controlling and Inspecting Compilation}

Racket programs and expressions are compiled automatically and
on-the-fly. The @exec{raco make} tool (see @secref[#:doc raco-doc
"make"]) can compile a Racket module to a compiled @filepath{.zo}
file, but that kind of ahead-to-time compilation simply allows a
program takes to start more quickly, and it does not affect the
performance of a Racket program.

@; ------------------------------------------------------------

@section[#:tag "compiler-modes"]{Compilation Modes}

All Racket variants suppose a machine-independent compilation mode,
which generates compiled @filepath{.zo} files that work with all
Racket variants on all platforms. To select machine-independent
compilation mode, set the @racket[current-compile-target-machine]
parameter to @racket[#f] or supplying the @DFlag{compile-any}/@Flag{M}
flag on startup. See @racket[current-compile-target-machine] for more
information.

Other compilation modes depend on the Racket variant
(3m/CGC versus CS).


@subsection[#:tag "3m-compiler-modes"]{3m and CGC Compilation Modes}

The 3m and CGC variants of Racket support two
compilation modes: bytecode and machine-independent. The bytecode
format is also machine-independent in the sense that it works the same
on all operating systems for the 3m and/or CGC variants
of Racket, but it does not work with the CS variant of Racket.

Bytecode is further compiled to machine code at run time, unless the
JIT compiler is disabled. See @racket[eval-jit-enabled].


@subsection[#:tag "cs-compiler-modes"]{CS Compilation Modes}

The CS variant of Racket supports several compilation modes:
machine code, machine-independent, interpreted, and JIT. Machine code
is the primary mode, and the machine-independent mode is the same as
for 3m and CGC. Interpreted mode uses an interpreter at
the level of core @tech{linklet} forms with no compilation. JIT mode
triggers compilation of individual function forms on demand.

The default mode is a hybrid of machine-code and interpreter modes,
where interpreter mode is used only for the outer contour of an
especially large linklet, and machine-code mode is used for functions
that are small enough within that outer contour. ``Small enough'' is
determined by the @envvar-indexed{PLT_CS_COMPILE_LIMIT} environment
variable, and the default value of 10000 means that most Racket
modules have no interpreted component.

JIT compilation mode is used only if the @envvar-indexed{PLT_CS_JIT}
environment variable is set on startup, otherwise pure interpreter
mode is used only if @envvar-indexed{PLT_CS_INTERP} is set on startup,
and the default hybrid machine code and interpreter mode is used if
@envvar-indexed{PLT_CS_MACH} is set and @envvar{PLT_CS_JIT} is not set
or if none of those environment variables is set. A module compiled in
any mode can be loaded into the CS variant of Racket independent of
the current compilation mode.

The @envvar{PLT_CS_DEBUG} environment variable, as described in
@secref["debugging"], affects only compilation in machine-code mode.
Generated machine code is much larger when @envvar{PLT_CS_DEBUG} is
enabled, but performance is not otherwise affected.

@; ------------------------------------------------------------

@section[#:tag "compiler-inspect"]{Inspecting Compiler Passes}

When the @envvar-indexed{PLT_LINKLET_SHOW} environment variable is set
on startup, the Racket process's standard output shows intermediate
compiled forms whenever a Racket form is compiled. For all Racket
variants, the output shows one or more @tech{linklets} that are
generated from the original Racket form.

For the CS variant of Racket, a ``schemified'' version of the linklet
is also shown as the translation of the @racket[linklet] form to a
Chez Scheme procedure form. The following environment variables imply
@envvar{PLT_LINKLET_SHOW} and show additional intermediate compiled
forms or adjust the way forms are displayed:

@itemlist[

  @item{@envvar-indexed{PLT_LINKLET_SHOW_GENSYM} --- prints full
        generated names, instead of abbreviations that may conflate
        different symbols}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_PRE_JIT} --- shows a
         schemified forms before a transformation to JIT mode, which
         applies only when @envvar{PLT_CS_JIT} is set}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_LAMBDA} --- shows individual
         schemified forms that are compiled within a larger form that
         has an interpreted outer contour}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_POST_LAMBDA} --- shows an
         outer form after inner individual forms are compiled}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_POST_INTERP} --- shows an
         outer form after its transformation to interpretable form}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_JIT_DEMAND} --- shows JIT
         compilation of form that were previously prepared by
         compilation with @envvar{PLT_CS_JIT} set}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_PATHS} --- show lifted
         path and serialization information alongside a schemified form}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_KNOWN} --- show recorded
         known-binding information alongside a schemified form}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_CP0} --- show a schemified
         form after transformation by Chez Scheme's front-end
         optimizer}

   @item{@envvar-indexed{PLT_LINKLET_SHOW_ASSEMBLY} --- show the
         compiled form of a schemified linklet in Chez Scheme's
         abstraction of machine instructions}

]

When the @envvar-indexed{PLT_LINKLET_TIMES} environment variable is
set on startup, then Racket prints cumulative timing information about
compilation and evaluation times on exit. When the
@envvar-indexed{PLT_EXPANDER_TIMES} environment variable is set,
information about macro-expansion time is printed on exit.

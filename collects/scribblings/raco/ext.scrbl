#lang scribble/doc
@(require scribble/manual "common.rkt")

@title[#:tag "ext"]{Compiling to Native Code via C}

The @DFlag{extension}/@Flag{e} mode for @exec{raco ctool} is similar
to the @exec{raco make --zo} (see @secref["zo"]), except that the
compiled form of the module is a native-code shared library instead of
bytecode. Native code is generated with the help of the host system's
C compiler. This mode is rarely useful, because the just-in-time (JIT)
compiler that is built into Racket provides better performance
with lower overhead on the platforms where it is supported (see
@secref[#:doc '(lib "scribblings/guide/guide.scrbl") "performance"]).

As with @DFlag{zo} mode, the generated shared library by default is
placed in the same directory as the source file---which is not where
it will be found automatically when loading the source. Use the
@as-index{@DFlag{auto-dir}} flag to redirect the output to a
@racket[(build-path "compiled" "native" (system-library-subpath))]
subdirectory, where it will be found automatically when loading the
source file.

The @DFlag{c-source}/@Flag{c} mode for @exec{raco ctool} is like the
@DFlag{extension}/@Flag{e} mode, except that compilation stops with
the generation of C code.

All of the C compiler and linker flags that apply to @DFlag{cc} and
@DFlag{ld} mode also apply to @DFlag{extension} mode; see
@secref["cc"].  In addition, a few flag provide some control over the
Racket-to-C compiler: @as-index{@DFlag{no-prop}},
@as-index{@DFlag{inline}}, @as-index{@DFlag{no-prim}},
@as-index{@DFlag{stupid}},
@as-index{@DFlag{unsafe-disable-interrupts}},
@as-index{@DFlag{unsafe-skip-tests}}, and
@as-index{@DFlag{unsafe-fixnum-arithmetic}}. Use @exec{mzc --help} for
an explanation of each flag.


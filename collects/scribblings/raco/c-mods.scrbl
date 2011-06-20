#lang scribble/doc
@(require scribble/manual "common.rkt")

@title[#:tag "c-mods"]{Embedding Modules via C}

The @DFlag{c-mods} mode for @exec{raco ctool} takes a set of Racket
modules and generates a C source file that can be used as part of
program that embeds the Racket run-time system. See @secref[#:doc
inside-doc "embedding"] in @other-manual[inside-doc] for an
explanation of embedding programs.

The generated source file embeds the specified modules, and it defines
a @tt{declare_modules} function that puts the module declarations into
a namespace. Thus, using the output of @exec{raco ctool --c-mods}, a
program can embed Racket with a set of modules so that it does not
need a @filepath{collects} directory to load modules at run time.

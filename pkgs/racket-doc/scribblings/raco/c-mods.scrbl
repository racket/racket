#lang scribble/doc
@(require scribble/manual
          "common.rkt"
          scribble/bnf)

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

If the embedded modules refer to runtime files, the files can be
gathers by supplying the @DFlag{runtime} argument to @exec{raco ctool
--cmods}, specifying a directory @nonterm{dir} to hold the files.
Normally, @nonterm{dir} is a relative path, and files are found at run
time in @nonterm{dir} relative to the executable, but a separate path
(usually relative) for run time can be specified with
@DFlag{runtime-access}.


Typically, @exec{raco ctool --c-mods} is used with @DPFlag{lib} to
specify a collection-based module path. For example,

@commandline{raco ctool --c-mods base.c ++lib racket/base}

generates a @filepath{base.c} whose @tt{declare_modules} function
makes @racketmodname[racket/base] available for use via the
@tt{scheme_namespace_require} or @tt{scheme_dynamic_require} functions
within the embedding application.

When a module file is provided to @exec{raco ctool --c-mods}, then
@tt{declare_modules} declares a module with the symbolic name of the
module file. For example, 

@commandline{raco ctool --c-mods base.c hello.rkt}

creates a @tt{declare_modules} that defines the module
@racket['hello], which could be required into the current namespace
with @racket[(namespace-require ''hello)] or similarly at the C level:

@verbatim[#:indent 2]{
  p = scheme_make_pair(scheme_intern_symbol("quote"),
                       scheme_make_pair(scheme_intern_symbol("hello"),
                                        scheme_make_null()));
  scheme_namespace_require(p);
}

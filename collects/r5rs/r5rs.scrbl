#lang scribble/doc
@(require scribble/manual
          (for-label r5rs
                     (only-in mzscheme #%plain-module-begin)
                     (only-in scheme/mpair mmap)
                     (only-in scheme/contract one-of/c)
                     (only-in scheme/base
                              require find-system-path namespace? mcons mcdr
                              namespace-require namespace-require/copy
                              read-case-sensitive
                              read-accept-infix-dot
                              read-curly-brace-as-paren
                              read-square-bracket-as-paren
                              print-vector-length
                              print-mpair-curly-braces)))


@(define r5rs @elem{R@superscript{5}RS})
@(define drs-doc '(lib "scribblings/drscheme/drscheme.scrbl"))

@title{@bold{R5RS}: Legacy Standard Language}

The ``The Revised@superscript{5} Report on the Algorithmic Language
Scheme'' @cite["Kelsey98"] defines a dialect of Scheme. We use
@defterm{@|r5rs|} to refer to both the standard and the language
defined by the standard.

The default dialect of Scheme provided by @exec{mzscheme} and other
PLT Scheme tools differs from @|r5rs| in many ways, but PLT Scheme
includes tools and libraries for running @|r5rs| programs.

@table-of-contents[]

@; ----------------------------------------

@section[#:tag "running"]{Running @|r5rs| Programs}

PLT Scheme provides several layers of support for programs written
according to @|r5rs|:

@itemize{

 @item{DrScheme provides an @onscreen{R5RS} language, which can be
       selected via the @menuitem["Language" "Choose Language..."]
       menu item. See @secref[#:doc drs-doc "choose-language"] in
       @other-manual[drs-doc] for more information.}

 @item{The @exec{plt-r5rs} executable runs an @|r5rs| program or
       provides a read-eval-print loop for evaluating @|r5rs|
       expressions and definitions. See @secref["plt-r5rs"] (later in
       this manual) for more information.}

 @item{The @schememodname[r5rs] library implemented @|r5rs| procedures
       and syntactic forms. It can also be used with @hash-lang[] to
       create a module whose body is implemented in an @|r5rs|-like
       language.  See @secref["r5rs-mod"] (later in this manual) for
       more information.}

 @item{The @schememodname[r5rs/init] library extends
       @schememodname[r5rs] to set parameters (such as
       case-insensitive symbol reading) for @|r5rs| loading or an
       @|r5rs| read-eval-print loop. See @secref["r5rs/init-mod"]
       (later in this manual) for more information.}

}

@; ----------------------------------------

@section[#:tag "plt-r5rs"]{@exec{plt-r5rs}}

The @exec{plt-r5rs} executable runs an @|r5rs| program from a file
that is supplied on the command line. If no program file is provided
as a command-line argument, then a read-eval-print loop is started.

Before starting a read-eval-print loop, an initialization file is
loaded, if it exists. The file is the same as the file reported by
@scheme[(find-system-path 'init-file)], but with the characters
@litchar{mzscheme} in the filename replaced by @litchar{pltr5rs}. For
example, under Unix, the file is @filepath{~/.pltr5rsrc}.

By default, @exec{plt-r5rs} departs from @|r5rs| conformance in one
crucial way: the initial bindings of primitives correspond to module
imports into the top-level environment, instead of variable bindings.
This difference is visible if the name of a primitive is redefined at
the top level. Use the @as-index{@DFlag{no-prim}} command-line
flag---before a file to load, if any---to obtain the standard behavior
for primitive bindings (at the cost of performance).

@; ----------------------------------------

@section[#:tag "r5rs-mod"]{@|r5rs| Module Language}

@defmodulelang[r5rs]

As a library, @schememodname[r5rs] provides the syntactic forms and
procedures defined by @|r5rs|. When used as a language via
@hash-lang[], the program is read with the following
parameterizations:

@schemeblock[
  (read-case-sensitive #f)
  (read-accept-infix-dot #f)
  (read-curly-brace-as-paren #f)
  (read-square-bracket-as-paren #f)
]

The @schememodname[r5rs] bindings can be imported into a top-level
environment, and then evaluation in that top-level environment
corresponds to @|r5rs|. Use @scheme[(namespace-require/copy 'r5rs)]
with an empty namespace to maximize conformance with @|r5rs|; Using
@scheme[(namespace-require 'r5rs)], in contrast, creates primitive
bindings as imports, which is the same as using
@seclink["plt-r5rs"]{@exec{plt-r5rs}} without the @DFlag{no-prim} flag.
More simply, use @scheme[(scheme-report-environment 5)].  See also
@schememodname[r5rs/init], which sets reader and printer parameters to
increase conformance.

Using @schememodname[r5rs] via @hash-lang[] creates a module whose
body is implemented with an @|r5rs|-like language. The main difference
from @|r5rs| is that, as a module language, @schememodname[r5rs] does
not allow redefinition of top-level bindings, and expressions
evaluated through @scheme[load] and @scheme[eval] cannot automatically
access bindings defined within the module.

@; --------------------

@subsection{Non-@|r5rs| Bindings from @schememodname[r5rs]}

In addition to the bindings defined by @|r5rs|, the
@schememodname[r5rs] library provides the following bindings from
@schememodname[mzscheme] (which are not legal identifiers in @|r5rs|
syntax, so there is no danger of collisions in @|r5rs| programs):

@schemeblock[
#%app #%datum #%top #%top-interaction #%require #%provide
]

It also provides @schememodname[mzscheme]'s
@scheme[#%plain-module-begin] as @scheme[#%module-begin]. Note that
@scheme[#%require] can be used to import PLT Scheme libraries into an
otherwise @|r5rs| program, and @scheme[#%provide] can be used to
export from a module that is implemented in an @|r5rs|-like language.

@; --------------------

@subsection{Notes on @|r5rs| Functions}

The @scheme[cons] of @schememodname[r5rs] corresponds to
@schememodname[scheme/base]'s @scheme[mcons]. Similarly, @scheme[cdr]
is @scheme[mcdr], and @scheme[map] is @schememodname[scheme/mpair]'s
@scheme[mmap], and so on.

@defproc[(eval [expr any/c] [environment namespace?]) any]{

An @|r5rs| @defterm{environment} is implemented as a
@scheme[scheme/base] @defterm{namespace}. Also, relative to
@scheme[scheme/base], the @scheme[expr] passed to @scheme[eval] is
constructed using mutable pairs.}

@defproc[(scheme-report-environment [n (one-of/c 5)]) namespace?]{

Returns a namespace containing the bindings of @schememodname[r5rs].
Procedure values are installed into the namespace using
@scheme[namespace-require/copy], so that they can be redefined.}

@defproc[(scheme-null-environment [n (one-of/c 5)]) namespace?]{

Returns a namespace containing the syntactic forms of
@schememodname[r5rs], not including @scheme[#%module-begin] (which is
not useful outside of a module).}

@; ----------------------------------------

@section[#:tag "r5rs/init-mod"]{@|r5rs| Initialization Library}

@defmodule[r5rs/init]

The @schememodname[r5rs/init] module re-exports @schememodname[r5rs],
and also sets parameters as follows:

@schemeblock[
  (read-case-sensitive #f)
  (read-accept-infix-dot #f)
  (read-curly-brace-as-paren #f)
  (read-square-bracket-as-paren #f)
  (print-mpair-curly-braces #f)
]

The side-effect of setting these parameters is useful when the module
is @scheme[require]d before loading an @|r5rs| program, so that the
reader and printer behave more as specified in @|r5rs|. In particular,
the @seclink["plt-r5rs"]{@exec{plt-r5rs} executable} initializes by
importing @schememodname[r5rs/init].

@; ----------------------------------------

@(bibliography

 (bib-entry #:key "Kelsey98"
            #:author "Richard Kelsey, William Clinger, and Jonathan Rees (editors)"
            #:title @elem{The Revised@superscript{5} Report on the Algorithmic Language Scheme}
            #:date "1998"
            #:url "http://schemers.org/Documents/Standards/R5RS/")

)

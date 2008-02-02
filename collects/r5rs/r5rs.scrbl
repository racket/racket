#lang scribble/doc
@(require scribble/manual
          (for-label r5rs
                     (only-in scheme/base
                              require find-system-path
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

 @item{The @schememodname[r5rs] language supports an @|r5rs|-like
       language within a module body.  See @secref["r5rs-mod"] (later
       in this manual) for more information.}

 @item{The @schememodname[r5rs/init] library extends
       @schememodname[r5rs] to set parameters (such as case-sensitive
       symbol reading) for @|r5rs| loading or an @|r5rs|
       read-eval-print loop. See @secref["r5rs/init-mod"] (later in
       this manual) for more information.}

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
the top level. Use the @as-index{@DFlag{slow}} command-line
flag---before a file to load, if any---to obtain the standard behavior
for primitive bindings (at the cost of performance).

@; ----------------------------------------

@section[#:tag "r5rs-mod"]{@|r5rs| Module Language}

@defmodulelang[r5rs]

The @schememodname[r5rs] language resembles the language defined by
@|r5rs|. The main difference is that, as a module language,
@schememodname[r5rs] does not allow redefinition of top-level
bindings, and expressions evaluated through @scheme[load] and
@scheme[eval] cannot automatically access bindings defined within the
module.

The @schememodname[r5rs] module provides bindings that can be imported
into a top-level environment, and then evaluation in the top-level
environment corresponds more closely to @|r5rs|.

In particular, the @exec{plt-r5rs} executable imports
@schememodname[r5rs] into the top-level environment, though it does so
via @schememodname[r5rs/init].

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
is @scheme[require]d before loading an @|r5rs| program, so tha the
reader and printer behave more as specified in @|r5rs|. In particular,
the @seclink["plt-r5rs"]{@exec{plt-r5rs} executable} initializes by
importing @schememodname[r5rs/init].

Use @scheme[(namespace-require/copy 'r5rs/init)] with an empty
namespace to maximize conformance with @|r5rs|. Using
@scheme[(namespace-require 'r5rs/init)] creates primitive bindings as
imports, not variables, which is the same as using
@seclink["plt-r5rs"]{@exec{plt-r5rs}} without the @DFlag{slow} flag.

@; ----------------------------------------

@(bibliography

 (bib-entry #:key "Kelsey98"
            #:author "Richard Kelsey, William Clinger, and Jonathan Rees (editors)"
            #:title @elem{The Revised@superscript{5} Report on the Algorithmic Language Scheme}
            #:date "1998"
            #:url "http://schemers.org/Documents/Standards/R5RS/")

)

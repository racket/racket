#lang scribble/doc
@(require "common.rkt")

@title{MzLib: Legacy Libraries}

The @filepath{mzlib} collection contains wrappers and libraries for
compatibility with older versions of Racket. In many ways, the
libraries of the @filepath{mzlib} collection go with the
@racketmodname[mzscheme] legacy language. Newer variants of many
libraries reside in the @filepath{scheme} collection.

@table-of-contents[]

@; ----------------------------------------------------------------------

@mzlib[a-signature]

@deprecated[@racketmodname[racket/signature]]{}

Like @racketmodname[scheme/signature] in @hash-lang[] form for
defining a single signature within a module, but based on
@racketmodname[mzscheme] instead of @racketmodname[scheme/base].

@; ----------------------------------------------------------------------

@mzlib[a-unit]

@deprecated[@racketmodname[racket/unit]]{}

Like @racketmodname[scheme/unit] in @hash-lang[] form for defining a
single unit within a module, but based on @racketmodname[mzscheme]
instead of @racketmodname[scheme/base].

@; ----------------------------------------------------------------------

@mzlib[async-channel]

@deprecated[@racketmodname[racket/async-channel]]{}

Re-exports @racketmodname[scheme/async-channel].

@; ----------------------------------------------------------------------

@include-section["awk.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[class]

@deprecated[@racketmodname[racket/class]]{}

Re-exports @racketmodname[scheme/class], except for the contract
constructors.

@; ----------------------------------------------------------------------

@include-section["class100.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[cm]

@deprecated[@racketmodname[compiler/cm]]{}

Re-exports @racketmodname[compiler/cm].

@; ----------------------------------------------------------------------

@mzlib[cm-accomplice]

@deprecated[@racketmodname[compiler/cm-accomplice]]{}

Re-exports @racketmodname[compiler/cm-accomplice].

@; ----------------------------------------------------------------------

@include-section["cmdline.scrbl"]

@; ----------------------------------------------------------------------

@include-section["cml.scrbl"]

@; ----------------------------------------------------------------------

@include-section["compat.scrbl"]

@; ----------------------------------------------------------------------

@include-section["compile.scrbl"]

@; ----------------------------------------------------------------------

@include-section["contract.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[control]

@deprecated[@racketmodname[racket/control]]{}

Re-exports @racketmodname[scheme/control].

@; ----------------------------------------------------------------------

@mzlib[date]

@deprecated[@racketmodname[racket/date]]{}

Re-exports @racketmodname[scheme/date].

@; ----------------------------------------------------------------------

@mzlib[deflate]

@deprecated[@racketmodname[file/gzip]]{}

Re-exports @racketmodname[file/gzip].

@; ----------------------------------------------------------------------

@mzlib[defmacro]

@deprecated[@racketmodname[compatibility/defmacro]]{}

Re-exports @racketmodname[compatibility/defmacro].

@; ----------------------------------------------------------------------

@include-section["etc.scrbl"]

@; ----------------------------------------------------------------------

@include-section["file.scrbl"]

@; ----------------------------------------------------------------------

@include-section["for.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[foreign]

@deprecated[@racketmodname[ffi/unsafe]]{}

Re-exports @racketmodname[scheme/foreign].

@; ----------------------------------------------------------------------

@include-section["include.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[inflate]

@deprecated[@racketmodname[file/gunzip]]{}

Re-exports @racketmodname[file/gunzip].

@; ----------------------------------------------------------------------

@include-section["integer-set.scrbl"]

@; ----------------------------------------------------------------------

@include-section["kw.scrbl"]

@; ----------------------------------------------------------------------

@include-section["list.scrbl"]

@; ----------------------------------------------------------------------

@include-section["match.scrbl"]

@; ----------------------------------------------------------------------

@include-section["math.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[md5]

@deprecated[@racketmodname[file/md5]]{}

Re-exports @racketmodname[file/md5].

@; ----------------------------------------------------------------------

@include-section["os.scrbl"]

@; ----------------------------------------------------------------------

@include-section["pconvert.scrbl"]

@; ----------------------------------------------------------------------

@include-section["pconvert-prop.scrbl"]

@; ----------------------------------------------------------------------

@include-section["plt-match.scrbl"]

@; ----------------------------------------------------------------------

@include-section["port.scrbl"]

@; ----------------------------------------------------------------------

@include-section["pregexp.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[pretty]

@deprecated[@racketmodname[racket/pretty]]{}

Re-exports @racketmodname[scheme/pretty].

@; ----------------------------------------------------------------------

@mzlib[process]

@deprecated[@racketmodname[racket/system]]{}

Re-exports @racketmodname[scheme/system].

@; ----------------------------------------------------------------------

@include-section["restart.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[runtime-path]

@deprecated[@racketmodname[racket/runtime-path]]{}

Re-exports @racketmodname[scheme/runtime-path].

@; ----------------------------------------------------------------------

@include-section["sandbox.scrbl"]

@; ----------------------------------------------------------------------

@include-section["sendevent.scrbl"]

@; ----------------------------------------------------------------------

@include-section["serialize.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[shared]

@deprecated[@racketmodname[racket/shared]]{}

Re-exports @racketmodname[scheme/shared].

@; ----------------------------------------------------------------------

@include-section["string.scrbl"]

@; ----------------------------------------------------------------------

@include-section["struct.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[stxparam]

@deprecated[@racketmodname[racket/stxparam]]{
Also see @racketmodname[racket/stxparam-exptime].
}

Re-exports @racketmodname[scheme/stxparam] and
@racketmodname[scheme/stxparam-exptime] (both at phase level 0).

@; ----------------------------------------------------------------------

@mzlib[surrogate]

@deprecated[@racketmodname[racket/surrogate]]{}

Re-exports @racketmodname[scheme/surrogate].

@; ----------------------------------------------------------------------

@mzlib[tar]

@deprecated[@racketmodname[file/tar]]{}

Re-exports @racketmodname[file/tar].

@; ----------------------------------------------------------------------

@include-section["thread.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[trace]

@deprecated[@racketmodname[racket/trace]]{}

Re-exports @racketmodname[racket/trace].

@; ----------------------------------------------------------------------

@include-section["traceld.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[trait]

@deprecated[@racketmodname[racket/trait]]{}

Re-exports @racketmodname[scheme/trait].

@; ----------------------------------------------------------------------

@include-section["transcr.scrbl"]

@; ----------------------------------------------------------------------

@include-section["unit.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[unit-exptime]

@deprecated[@racketmodname[racket/unit-exptime]]{}

Re-exports @racketmodname[scheme/unit-exptime].

@; ----------------------------------------

@mzlib[unit200]

@deprecated[@racketmodname[racket/unit]]{}

The @racketmodname[mzlib/unit200] library provides an old
implementation of units. See archived version 360 documentation on the
@filepath{unit.ss} library of the @filepath{mzlib} collection for
information about this library.

@; ----------------------------------------

@mzlib[unitsig200]

@deprecated[@racketmodname[racket/unit]]{}

The @racketmodname[mzlib/unit200] library provides an old
implementation of units. See archived version 360 documentation on the
@filepath{unitsig.ss} library of the @filepath{mzlib} collection for
information about this library.

@; ----------------------------------------

@mzlib[zip]

@deprecated[@racketmodname[file/zip]]{}

Re-exports @racketmodname[file/zip].

@; ----------------------------------------------------------------------

@(bibliography
 
  (bib-entry #:key "Shivers06"
             #:title "Scsh Reference Manual"
             #:author "Olin Shivers, Brian D. Carlstrom, Martin Gasbichler, and Mike Sperber"
             #:date "2006")

  (bib-entry #:key "Reppy99"
             #:title @italic{Concurrent Programming in ML}
             #:author "John H. Reppy"
             #:date "1999")

 )

@;------------------------------------------------------------------------

@index-section[]

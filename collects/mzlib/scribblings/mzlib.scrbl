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

Like @racketmodname[scheme/signature] in @hash-lang[] form for
defining a single signature within a module, but based on
@racketmodname[mzscheme] instead of @racketmodname[scheme/base].

@; ----------------------------------------------------------------------

@mzlib[a-unit]

Like @racketmodname[scheme/unit] in @hash-lang[] form for defining a
single unit within a module, but based on @racketmodname[mzscheme]
instead of @racketmodname[scheme/base].

@; ----------------------------------------------------------------------

@mzlib[async-channel]

Re-exports @racketmodname[scheme/async-channel].

@; ----------------------------------------------------------------------

@include-section["awk.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[class]

Re-exports @racketmodname[scheme/class], except for the contract
constructors.

@; ----------------------------------------------------------------------

@include-section["class100.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[cm]

Re-exports @racketmodname[compiler/cm].

@; ----------------------------------------------------------------------

@mzlib[cm-accomplice]

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

Re-exports @racketmodname[scheme/control].

@; ----------------------------------------------------------------------

@mzlib[date]

Re-exports @racketmodname[scheme/date].

@; ----------------------------------------------------------------------

@mzlib[deflate]

Re-exports @racketmodname[file/gzip].

@; ----------------------------------------------------------------------

@include-section["defmacro.scrbl"]

@; ----------------------------------------------------------------------

@include-section["etc.scrbl"]

@; ----------------------------------------------------------------------

@include-section["file.scrbl"]

@; ----------------------------------------------------------------------

@include-section["for.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[foreign]

Re-exports @racketmodname[scheme/foreign].

@; ----------------------------------------------------------------------

@include-section["include.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[inflate]

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

Re-exports @racketmodname[scheme/pretty].

@; ----------------------------------------------------------------------

@mzlib[process]

Re-exports @racketmodname[scheme/system].

@; ----------------------------------------------------------------------

@include-section["restart.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[runtime-path]

Re-exports @racketmodname[scheme/runtime-path].

@; ----------------------------------------------------------------------

@include-section["sandbox.scrbl"]

@; ----------------------------------------------------------------------

@include-section["sendevent.scrbl"]

@; ----------------------------------------------------------------------

@include-section["serialize.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[shared]

Re-exports @racketmodname[scheme/shared].

@; ----------------------------------------------------------------------

@include-section["string.scrbl"]

@; ----------------------------------------------------------------------

@include-section["struct.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[stxparam]

Re-exports @racketmodname[scheme/stxparam] and
@racketmodname[scheme/stxparam-exptime] (both at phase level 0).

@; ----------------------------------------------------------------------

@mzlib[surrogate]

Re-exports @racketmodname[scheme/surrogate].

@; ----------------------------------------------------------------------

@mzlib[tar]

Re-exports @racketmodname[file/tar].

@; ----------------------------------------------------------------------

@include-section["thread.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[trace]

Re-exports @racketmodname[racket/trace].

@; ----------------------------------------------------------------------

@include-section["traceld.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[trait]

Re-exports @racketmodname[scheme/trait].

@; ----------------------------------------------------------------------

@include-section["transcr.scrbl"]

@; ----------------------------------------------------------------------

@include-section["unit.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[unit-exptime]

Re-exports @racketmodname[scheme/unit-exptime].

@; ----------------------------------------

@mzlib[unit200]

The @racketmodname[mzlib/unit200] library provides an old
implementation of units. See archived version 360 documentation on the
@filepath{unit.ss} library of the @filepath{mzlib} collection for
information about this library.

@; ----------------------------------------

@mzlib[unitsig200]

The @racketmodname[mzlib/unit200] library provides an old
implementation of units. See archived version 360 documentation on the
@filepath{unitsig.ss} library of the @filepath{mzlib} collection for
information about this library.

@; ----------------------------------------

@mzlib[zip]

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

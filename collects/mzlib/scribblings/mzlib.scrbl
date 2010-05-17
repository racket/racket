#lang scribble/doc
@(require "common.rkt")

@title{@bold{MzLib}: Legacy Racket Libraries}

The @filepath{mzlib} collection contains wrappers and libraries for
compatibility with older versions of Racket. In many ways, the
libraries of the @filepath{mzlib} collection go with the
@schememodname[mzscheme] legacy language. Newer variants of many
libraries reside in the @filepath{scheme} collection.

@table-of-contents[]

@; ----------------------------------------------------------------------

@mzlib[a-signature]

Like @schememodname[scheme/signature] in @hash-lang[] form for
defining a single signature within a module, but based on
@schememodname[mzscheme] instead of @schememodname[scheme/base].

@; ----------------------------------------------------------------------

@mzlib[a-unit]

Like @schememodname[scheme/unit] in @hash-lang[] form for defining a
single unit within a module, but based on @schememodname[mzscheme]
instead of @schememodname[scheme/base].

@; ----------------------------------------------------------------------

@mzlib[async-channel]

Re-exports @schememodname[scheme/async-channel].

@; ----------------------------------------------------------------------

@include-section["awk.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[class]

Re-exports @schememodname[scheme/class], except for the contract
constructors.

@; ----------------------------------------------------------------------

@include-section["class100.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[cm]

Re-exports @schememodname[compiler/cm].

@; ----------------------------------------------------------------------

@mzlib[cm-accomplice]

Re-exports @schememodname[compiler/cm-accomplice].

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

Re-exports @schememodname[scheme/control].

@; ----------------------------------------------------------------------

@mzlib[date]

Re-exports @schememodname[scheme/date].

@; ----------------------------------------------------------------------

@mzlib[deflate]

Re-exports @schememodname[file/gzip].

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

Re-exports @schememodname[scheme/foreign].

@; ----------------------------------------------------------------------

@include-section["include.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[inflate]

Re-exports @schememodname[file/gunzip].

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

Re-exports @schememodname[file/md5].

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

Re-exports @schememodname[scheme/pretty].

@; ----------------------------------------------------------------------

@mzlib[process]

Re-exports @schememodname[scheme/system].

@; ----------------------------------------------------------------------

@include-section["restart.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[runtime-path]

Re-exports @schememodname[scheme/runtime-path].

@; ----------------------------------------------------------------------

@include-section["sandbox.scrbl"]

@; ----------------------------------------------------------------------

@include-section["sendevent.scrbl"]

@; ----------------------------------------------------------------------

@include-section["serialize.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[shared]

Re-exports @schememodname[scheme/shared].

@; ----------------------------------------------------------------------

@include-section["string.scrbl"]

@; ----------------------------------------------------------------------

@include-section["struct.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[stxparam]

Re-exports @schememodname[scheme/stxparam] and
@schememodname[scheme/stxparam-exptime] (both at phase level 0).

@; ----------------------------------------------------------------------

@mzlib[surrogate]

Re-exports @schememodname[scheme/surrogate].

@; ----------------------------------------------------------------------

@mzlib[tar]

Re-exports @schememodname[file/tar].

@; ----------------------------------------------------------------------

@include-section["thread.scrbl"]

@; ----------------------------------------------------------------------

@include-section["trace.scrbl"]

@; ----------------------------------------------------------------------

@include-section["traceld.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[trait]

Re-exports @schememodname[scheme/trait].

@; ----------------------------------------------------------------------

@include-section["transcr.scrbl"]

@; ----------------------------------------------------------------------

@include-section["unit.scrbl"]

@; ----------------------------------------------------------------------

@mzlib[unit-exptime]

Re-exports @schememodname[scheme/unit-exptime].

@; ----------------------------------------

@mzlib[unit200]

The @schememodname[mzlib/unit200] library provides an old
implementation of units. See archived version 360 documentation on the
@filepath{unit.ss} library of the @filepath{mzlib} collection for
information about this library.

@; ----------------------------------------

@mzlib[unitsig200]

The @schememodname[mzlib/unit200] library provides an old
implementation of units. See archived version 360 documentation on the
@filepath{unitsig.ss} library of the @filepath{mzlib} collection for
information about this library.

@; ----------------------------------------

@mzlib[zip]

Re-exports @schememodname[file/zip].

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

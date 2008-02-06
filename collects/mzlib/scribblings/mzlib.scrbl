#lang scribble/doc
@(require "common.ss")

@title{@bold{MzLib}: Legacy PLT Libraries}

The @filepath{mzlib} collection contains wrappers and libraries for
compatibility with older versions of PLT Scheme. In many ways, the
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

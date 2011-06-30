#lang scribble/manual

@title[#:tag "top"]{The Typed Racket Reference}

@author[@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]
        @author+email["Vincent St-Amour" "stamourv@racket-lang.org"]]

@(defmodulelang* (typed/racket/base typed/racket)
                 #:use-sources
                    (typed-scheme/typed-scheme
                     typed-scheme/base-env/prims
                     typed-scheme/base-env/extra-procs
                     typed-scheme/base-env/base-types
                     typed-scheme/base-env/base-types-extra))

@local-table-of-contents[]

@include-section["reference/types.scrbl"]
@include-section["reference/special-forms.scrbl"]
@include-section["reference/libraries.scrbl"]
@include-section["reference/utilities.scrbl"]
@include-section["reference/no-check.scrbl"]
@include-section["reference/typed-regions.scrbl"]
@include-section["reference/optimization.scrbl"]
@include-section["reference/legacy.scrbl"]
@include-section["reference/compatibility-languages.scrbl"]
@include-section["reference/experimental.scrbl"]

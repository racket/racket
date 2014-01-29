#lang scribble/manual

@title[#:tag "top"]{The Typed Racket Reference}

@author[@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]
        @author+email["Vincent St-Amour" "stamourv@racket-lang.org"]
        @author+email["Eric Dobson" "endobson@racket-lang.org"]
        @author+email["Asumu Takikawa" "asumu@racket-lang.org"]
        ]

This manual describes the Typed Racket language, a sister language
of Racket with a static type-checker. The types, special forms, and
other tools provided by Typed Racket are documented here.

For a friendly introduction, see the companion manual
@other-doc['(lib "typed-racket/scribblings/ts-guide.scrbl")].

@(defmodulelang* (typed/racket/base typed/racket)
                 #:use-sources
                    (typed-racket/typed-racket
                     typed-racket/base-env/prims
                     typed-racket/base-env/extra-procs
                     typed-racket/base-env/base-types
                     typed-racket/base-env/base-types-extra))

@local-table-of-contents[]

@include-section["reference/types.scrbl"]
@include-section["reference/special-forms.scrbl"]
@include-section["reference/libraries.scrbl"]
@include-section["reference/typed-classes.scrbl"]
@include-section["reference/utilities.scrbl"]
@include-section["reference/exploring-types.scrbl"]
@include-section["reference/no-check.scrbl"]
@include-section["reference/typed-regions.scrbl"]
@include-section["reference/optimization.scrbl"]
@include-section["reference/legacy.scrbl"]
@include-section["reference/compatibility-languages.scrbl"]
@include-section["reference/experimental.scrbl"]

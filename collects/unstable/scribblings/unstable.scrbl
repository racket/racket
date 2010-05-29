#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-syntax racket/base racket/path)
          (for-label scribble/base))

@title[#:tag "unstable"]{Unstable}

@defmodule[unstable]

This manual documents some of the libraries available in the
@racketmodname[unstable] collection.

The name @racketmodname[unstable] is intended as a warning that the @bold{interfaces} in particular are unstable. Developers of planet packages and external projects should avoid using modules in the unstable collection. Contracts may change, names may change or disappear, even entire modules may move or disappear without warning to the outside world.

Developers of unstable libraries must follow the guidelines in
@secref{guidelines}.

@local-table-of-contents[#:style 'immediate-only]

@;{--------}

@section[#:tag "guidelines"]{Guidelines for developing @racketmodname[unstable] libraries}

Any collection developer may add modules to the
@racketmodname[unstable] collection.

Every module needs an owner to be responsible for it.

@itemize[

@item{If you add a module, you are its owner. Add a comment with your
name at the top of the module.}

@item{If you add code to someone else's module, tag your additions
with your name. The module's owner may ask you to move your code to a
separate module if they don't wish to accept responsibility for it.}
]

When changing a library, check all uses of the library in the
collections tree and update them if necessary. Notify users of major
changes.

Place new modules according to the following rules. (These rules are
necessary for maintaining PLT's separate text, gui, and drracket
distributions.)

@itemize[

@item{Non-GUI modules go under @tt{unstable} (or subcollections
thereof). Put the documentation in @tt{unstable/scribblings} and
include with @racket[include-section] from
@tt{unstable/scribblings/unstable.scrbl}.}

@item{GUI modules go under @tt{unstable/gui}. Put the documentation
in @tt{unstable/scribblings/gui} and include them with
@racket[include-section] from @tt{unstable/scribblings/gui.scrbl}.}

@item{Do not add modules depending on DrRacket to the
@racketmodname[unstable] collection.}

@item{Put tests in @tt{tests/unstable}.}
]

Keep documentation and tests up to date.


@;{--------}

@;{
  Add new documentation links to the list immediately below.
}

@include-section["bytes.scrbl"]
@include-section["contract.scrbl"]
@include-section["dirs.scrbl"]
@include-section["exn.scrbl"]
@include-section["file.scrbl"]
@include-section["function.scrbl"]
@include-section["list.scrbl"]
@include-section["net.scrbl"]
@include-section["path.scrbl"]
@include-section["pretty.scrbl"]
@include-section["srcloc.scrbl"]
@include-section["string.scrbl"]
@include-section["struct.scrbl"]
@include-section["syntax.scrbl"]
@include-section["poly-c.scrbl"]
@include-section["mutated-vars.scrbl"]
@include-section["find.scrbl"]
@include-section["class-iop.scrbl"]
@include-section["sequence.scrbl"]
@include-section["hash.scrbl"]
@include-section["match.scrbl"]
@include-section["skip-list.scrbl"]
@include-section["interval-map.scrbl"]
@include-section["generics.scrbl"]
@include-section["markparam.scrbl"]
@include-section["debug.scrbl"]
@include-section["byte-counting-port.scrbl"]

;; This addition is temporary while integrating (planet cce/scheme:7):
@include-section["../cce/reference/manual.scrbl"]

@;{--------}

@;{
  Include the "Unstable GUI libraries docs" if it is part of
  the current distribution.
}

@(begin
   ;; Include gui.scrbl if present.
   (define-for-syntax here
     (build-path (collection-path "unstable") "scribblings"))
   (define-for-syntax gui-file
     "gui.scrbl")
   (define-syntax (if-gui-present stx)
     (syntax-case stx ()
       [(_ then else)
        (if (file-exists? (build-path here gui-file))
            #'then
            #'else)])))

@if-gui-present[(include-section "gui.scrbl") (begin)]

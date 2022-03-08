#lang scribble/manual
@(require (for-label (except-in zuo-doc/fake-zuo
                                identifier?
                                syntax-e
                                syntax->datum
                                datum->syntax
                                bound-identifier=?)
                     zuo-doc/fake-zuo-hygienic)
          "real-racket.rkt")

@title[#:tag "zuo-hygienic"]{Zuo with Hygienic Macros}

@defmodulelang[zuo/hygienic #:no-declare #:packages ()]
@declare-exporting[zuo/hygienic #:packages () #:use-sources(zuo-doc/fake-zuo-hygienic)]

The @racketmodname[zuo/hygienic] language provides the same set of
bindings as @racketmodname[zuo/base], but with hygienic macros. Its
macro-expansion protocol uses a different representation of
identifiers and binding scope, and different rules for
@racket[quote-syntax] and macros:

@itemlist[

 @item{A @racketmodname[zuo/hygienic] term's representation always
       uses identifier syntax objects in place of symbols. A macro
       will never receive a plain symbol in its input, and if the
       macro produces a term with plain symbol, it is automatically
       coerced to a syntax object using the scope of the module that
       defines the macro.}

 @item{A syntax object's context includes a @defterm{set of scopes},
       instead of just one @tech{scope}. Before expanding forms in a
       new context, a fresh scope representation is added to every
       identifier appearing within the context. An reference is
       resolved by finding the binding identifier with the most
       specific set of scopes that is a subset of the referencing
       identifier's scopes.}

 @item{In addition to binding contexts, a specific macro invocation is
       also represented by a scope: a fresh scope is added to every
       syntax object introduced by a macro expansion. This fresh scope
       means that an identifier introduced by the expansion can only
       bind identifiers that were introduced by the same expansion.
       Meanwhile, a @racket[quote-syntax]-imposed scope on an
       introduced identifier prevents it from being bound by an
       identifier that's at the macro-use site and not visible at the
       macro-definition site.}

 @item{The @racket[quote-syntax] form produces an identifier syntax
       object with all of its scope intact. That syntax object
       acquires additional scope if it is returned from a macro
       expander into a new context.}

]

These differences particularly affect the functions that operate on
@tech{syntax objects}:

@deftogether[(
@defproc[(identifier? [v any?]) boolean?]
@defproc[(syntax-e [v identifier?]) symbol?]
@defproc[(syntax->datum [v any?]) any?]
@defproc[(datum->syntax [ctx identifier?] [v any?]) any?]
@defproc[(bound-identifier=? [id1 identifier?]
                             [id2 identifier?]) boolean?]
)]{

Unlike the @racketmodname[zuo] function, @racket[identifier?] does not
recognize a plain symbol as an identifier. The @racket[datum->syntax]
function converts symbols in @racket[v] to syntax objects using the
context of @racket[ctx].}

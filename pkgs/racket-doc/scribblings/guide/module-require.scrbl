#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "module-require"]{Imports: @racket[require]}

The @racket[require] form imports from another module. A
@racket[require] form can appear within a module, in which case it
introduces bindings from the specified module into importing module. A
@racket[require] form can also appear at the top level, in which case
it both imports bindings and @deftech{instantiates} the specified
module; that is, it evaluates the body definitions and expressions of
the specified module, if they have not been evaluated already.

A single @racket[require] can specify multiple imports at once:

@specform[(require require-spec ...)]{}

Specifying multiple @racket[_require-spec]s in a single
@racket[require] is essentially the same as using multiple
@racket[require]s, each with a single @racket[_require-spec]. The
difference is minor, and confined to the top-level: a single
@racket[require] can import a given identifier at most once, whereas a
separate @racket[require] can replace the bindings of a previous
@racket[require] (both only at the top level, outside of a module).

The allowed shape of a @racket[_require-spec] is defined recursively:

@;------------------------------------------------------------------------
@specspecsubform[module-path]{

In its simplest form, a @racket[_require-spec] is a
@racket[module-path] (as defined in the previous section,
@secref["module-paths"]). In this case, the bindings introduced
by @racket[require] are determined by @racket[provide] declarations
within each module referenced by each @racket[module-path].

@examples[
(module m racket
  (provide color)
  (define color "blue"))
(module n racket
  (provide size)
  (define size 17))
(require 'm 'n)
(eval:alts (list color size) (eval '(list color size)))
]

}

@;------------------------------------------------------------------------
@specspecsubform/subs[#:literals (only-in)
                      (only-in require-spec id-maybe-renamed ...)
                      ([id-maybe-renamed id
                                         [orig-id bind-id]])]{

An @racket[only-in] form limits the set of bindings that would be introduced
by a base @racket[require-spec]. Also, @racket[only-in] optionally
renames each binding that is preserved: in a @racket[[orig-id
bind-id]] form, the @racket[orig-id] refers to a binding implied by
@racket[require-spec], and @racket[bind-id] is the name that will be
bound in the importing context instead of @racket[orig-id].

@examples[
(module m (lib "racket")
  (provide tastes-great?
           less-filling?)
  (define tastes-great? #t)
  (define less-filling? #t))
(require (only-in 'm tastes-great?))
(eval:alts tastes-great? (eval 'tastes-great?))
less-filling?
(require (only-in 'm [less-filling? lite?]))
(eval:alts lite? (eval 'lite?))
]}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except-in)
                 (except-in require-spec id ...)]{

This form is the complement of @racket[only-in]: it excludes specific
bindings from the set specified by @racket[require-spec].

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (rename-in)
                 (rename-in require-spec [orig-id bind-id] ...)]{

This form supports renaming like @racket[only-in], but leaving alone
identifiers from @racket[require-spec] that are not mentioned as an
@racket[orig-id].  }

@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-in)
                 (prefix-in prefix-id require-spec)]{

This is a shorthand for renaming, where @racket[prefix-id] is added to
the front of each identifier specified by @racket[require-spec].

}

The @racket[only-in], @racket[except-in], @racket[rename-in], and
@racket[prefix-in] forms can be nested to implement more complex
manipulations of imported bindings. For example,

@racketblock[(require (prefix-in m: (except-in 'm ghost)))]

imports all bindings that @racket[m]
exports, except for the @racket[ghost] binding, and with local names
that are prefixed with @racket[m:].

Equivalently, the @racket[prefix-in] could be applied before
@racket[except-in], as long as the omission with @racket[except-in] is
specified using the @racket[m:] prefix:

@racketblock[(require (except-in (prefix-in m: 'm) m:ghost))]

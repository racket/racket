#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "module-require"]{Imports: @scheme[require]}

The @scheme[require] form imports from another module. A
@scheme[require] form can appear within a module, in which case it
introduces bindings from the specified module into importing module. A
@scheme[require] form can also appear at the top level, in which case
it both imports bindings and @deftech{instantiates} the specified
module; that is, it evaluates the body definitions and expressions of
the specified module, if they have not been evaluated already.

A single @scheme[require] can specify multiple imports at once:

@specform[(require require-spec ...)]{}

Specifying multiple @scheme[_require-spec]s in a single
@scheme[require] is essentially the same as using multiple
@scheme[require]s, each with a single @scheme[_require-spec]. The
difference is minor, and confined to the top-level: a single
@scheme[require] can import a given identifier at most once, whereas a
separate @scheme[require] can replace the bindings of a previous
@scheme[require] (both only at the top level, outside of a module).

The allowed shape of a @scheme[_require-spec] is defined recursively:

@;------------------------------------------------------------------------
@specspecsubform[module-path]{

In its simplest form, a @scheme[_require-spec] is a
@scheme[module-path] (as defined in the previous section,
@secref["module-paths"]). In this case, the bindings introduced
by @scheme[require] are determined by @scheme[provide] declarations
within each module referenced by each @scheme[module-path].

@examples[
(module m scheme
  (provide color)
  (define color "blue"))
(module n scheme
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

An @scheme[only-in] form limits the set of bindings that would be introduced
by a base @scheme[require-spec]. Also, @scheme[only-in] optionally
renames each binding that is preserved: in a @scheme[[orig-id
bind-id]] form, the @scheme[orig-id] refers to a binding implied by
@scheme[require-spec], and @scheme[bind-id] is the name that will be
bound in the importing context instead of @scheme[bind-id].

@examples[
(module m (lib "scheme")
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

This form is the complement of @scheme[only]: it excludes specific
bindings from the set specified by @scheme[require-spec].

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (rename-in)
                 (rename-in require-spec [orig-id bind-id] ...)]{

This form supports renaming like @scheme[only-in], but leaving alone
identifiers from @scheme[require-spec] that are not mentioned as an
@scheme[orig-id].  }

@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-in)
                 (prefix-in prefix-id require-spec)]{

This is a shorthand for renaming, where @scheme[prefix-id] is added to
the front of each identifier specified by @scheme[require-spec].

}

The @scheme[only-in], @scheme[except-in], @scheme[rename-in], and
@scheme[prefix-in] forms can be nested to implement more complex
manipulations of imported bindings. For example,

@schemeblock[(require (prefix-in m: (except-in 'm ghost)))]

imports all bindings that @scheme[m]
exports, except for the @scheme[ghost] binding, and with local names
that are prefixed with @scheme[m:].

Equivalently, the @scheme[prefix-in] could be applied before
@scheme[except-in], as long as the omission with @scheme[except-in] is
specified using the @scheme[m:] prefix:

@schemeblock[(require (except-in (prefix m: 'm) m:ghost))]

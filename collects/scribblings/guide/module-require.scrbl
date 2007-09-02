#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

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
(module m mzscheme
  (provide color)
  (define color "blue"))
(module n mzscheme
  (provide size)
  (define size 17))
(require m n)
(eval:alts (list color size) (eval '(list color size)))
]

}

@;------------------------------------------------------------------------
@specspecsubform/subs[#:literals (only)
                      (only require-spec id-maybe-renamed ...)
                      ([id-maybe-renamed id
                                         [orig-id bind-id]])]{

An @scheme[only] form limits the set of bindings would be introduced
by a base @scheme[require-spec]. Also, @scheme[only] optionally
renames each binding that is preserved: in a @scheme[[orig-id
bind-id]] form, the @scheme[orig-id] refers to a binding implied by
@scheme[require-spec], and @scheme[bind-id] is the name that will be
bound in the importing context instead of @scheme[bind-id].

@examples[
(module m mzscheme
  (provide tastes-great?
           less-filling?)
  (define tastes-great? #t)
  (define less-filling? #t))
(require (only m tastes-great?))
(eval:alts tastes-great? (eval 'tastes-great?))
less-filling?
(require (only m [less-filling? lite?]))
(eval:alts lite? (eval 'lite?))
]}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except)
                 (except require-spec id ...)]{

This form is the complement of @scheme[only]: it excludes specific
bindings from the set specified by @scheme[require-spec].

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (rename)
                 (rename require-spec [orig-id bind-id] ...)]{

The form supports renaming like @scheme[only], but leaving alone
identifiers from @scheme[require-spec] that are not mentioned as an
@scheme[orig-id].  }

@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix)
                 (prefix require-spec prefix-id)]{

This is a shorthand for renaming, where @scheme[prefix-id] is added to
the front each identifier specified by @scheme[require-spec].

}

The @scheme[only], @scheme[except], @scheme[rename], and
@scheme[prefix] forms can be nested to implement more complex
manipulations of imported bindings. For example,

@schemeblock[(require (prefix (except m ghost) m:))]

imports all bindings that @scheme[m]
exports, except for the @scheme[ghost] binding, and with local names
that are prefixed with @scheme[m:].

Equivalently, the @scheme[prefix] could be applied before
@scheme[except], as long as the omission with @scheme[except] is
specified using the @scheme[m:] prefix:

@schemeblock[(require (except (prefix m m:) m:ghost))]

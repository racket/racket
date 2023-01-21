#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/phase+space))

@(define stx-eval (make-base-eval))
@examples[#:hidden #:eval stx-eval (require (for-syntax racket/base))]

@title[#:tag "stxcmp"]{Syntax Object Bindings}

@defproc[(bound-identifier=? [a-id syntax?] [b-id syntax?]
                             [phase-level (or/c exact-integer? #f)
                                          (syntax-local-phase-level)])
         boolean?]{

Returns @racket[#t] if the identifier @racket[a-id] would bind
@racket[b-id] (or vice versa) if the identifiers were substituted in a
suitable expression context at the @tech{phase level} indicated by
@racket[phase-level], @racket[#f] otherwise. A @racket[#f] value for
@racket[phase-level] corresponds to the @tech{label phase level}.

@examples[
#:eval stx-eval
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ x y)
     (if (bound-identifier=? #'x #'y)
         #'(let ([y 'wrong]) (let ([x 'binds]) y))
         #'(let ([y 'no-binds]) (let ([x 'wrong]) y)))]))
(check a a)
(check a b)
(define-syntax-rule (check-a x) (check a x))
(check-a a)
]}


@defproc[(free-identifier=? [a-id identifier?] [b-id identifier?]
                            [a-phase-level (or/c exact-integer? #f)
                                           (syntax-local-phase-level)]
                            [b-phase-level (or/c exact-integer? #f)
                                           a-phase-level])
         boolean?]{

Returns @racket[#t] if @racket[a-id] and @racket[b-id] access the same
@tech{local binding}, @tech{module binding}, or @tech{top-level
binding}---perhaps via @tech{rename transformers}---at the @tech{phase
levels} indicated by @racket[a-phase-level] and
@racket[b-phase-level], respectively. A @racket[#f] value for
@racket[a-phase-level] or @racket[b-phase-level] corresponds to the
@tech{label phase level}.

``Same module binding'' means that the identifiers refer to the same
original definition site, and not necessarily to the same
@racket[require] or @racket[provide] site. Due to renaming in
@racket[require] and @racket[provide], or due to a transformer binding
to a @tech{rename transformer}, the identifiers may return distinct
results with @racket[syntax-e].

@examples[
#:eval stx-eval
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ x)
     (if (free-identifier=? #'car #'x)
         #'(list 'same: x)
         #'(list 'different: x))]))
(check car)
(check mcar)
(let ([car list])
  (check car))
(require (rename-in racket/base [car kar]))
(check kar)
]}

@defproc[(free-transformer-identifier=? [a-id identifier?] [b-id identifier?]) boolean?]{

Same as @racket[(free-identifier=? a-id b-id (add1 (syntax-local-phase-level)))].}

@defproc[(free-template-identifier=? [a-id identifier?] [b-id identifier?]) boolean?]{

Same as @racket[(free-identifier=? a-id b-id (sub1 (syntax-local-phase-level)))].}

@defproc[(free-label-identifier=? [a-id identifier?] [b-id identifier?]) boolean?]{

Same as @racket[(free-identifier=? a-id b-id #f)].}


@defproc[(check-duplicate-identifier [ids (listof identifier?)])
         (or/c identifier? #f)]{

Compares each identifier in @racket[ids] with every other identifier
in the list with @racket[bound-identifier=?]. If any comparison
returns @racket[#t], one of the duplicate identifiers is returned (the
first one in @racket[ids] that is a duplicate), otherwise the result
is @racket[#f].}


@defproc[(identifier-binding [id-stx identifier?]
                             [phase-level (or/c exact-integer? #f)
                                          (syntax-local-phase-level)]
                             [top-level-symbol? any/c #f]
                             [exact-scopes? any/c #f])
         (or/c 'lexical
               #f
               (list/c module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       phase+space-shift?
                       phase+space?)
               (list/c symbol?))]{

Returns one of three (if @racket[top-level-symbol?] is @racket[#f])
or four (if @racket[top-level-symbol?] is true) kinds of values, depending on the binding of
@racket[id-stx] at the @tech{phase level} indicated by
@racket[phase-level] (where a @racket[#f] value for
@racket[phase-level] corresponds to the @tech{label phase level}):

@itemize[ 

      @item{The result is @indexed-racket['lexical] if @racket[id-stx]
      has a @tech{local binding}.}

      @item{The result is a list of seven items when @racket[id-stx]
      has a @tech{module binding}: @racket[(list _from-mod _from-sym
      _nominal-from-mod _nominal-from-sym _from-phase _import-phase+space-shift
      _nominal-export-phase)].

        @itemize[

        @item{@racket[_from-mod] is a module path index (see
        @secref["modpathidx"]) that indicates the defining module. It
        is the ``self'' module path index if the binding refers to a
        definition in the enclosing module of @racket[id-stx].}

        @item{@racket[_from-sym] is a symbol for the identifier's name
        at its definition site in the originating module. This can be
        different from the local name returned by
        @racket[syntax->datum] for several reasons: the identifier is
        renamed on import, it is renamed on export, or it is
        implicitly renamed because the binding site was generated by a
        macro invocation. In that last case, it may be an
        @tech{unreadable symbol}, and it may be different from the
        result of @racket[syntax->datum] on the identifier in the
        original source definition.}

        @item{@racket[_nominal-from-mod] is a module path index (see
        @secref["modpathidx"]) that indicates the binding's module as
        it appears locally in the source around @racket[id-stx]: it
        indicates a module @racket[require]d into the context of
        @racket[id-stx] to provide its binding, or it is the same
        ``self'' as @racket[_from-mod] for a binding that refers to a
        definition in the enclosing module of @racket[id-stx]. It can
        be different from @racket[_from-mod] due to a re-export in
        @racket[_nominal-from-mod] of some imported identifier. If the
        same binding is imported in multiple ways, an arbitrary
        representative is chosen.}

        @item{@racket[_nominal-from-sym] is a symbol for the binding's
        identifier as it appears locally in the source around
        @racket[id-stx]: it is the identifier's name as exported by
        @racket[_nominal-from-mod], or it is the source identifier's
        symbol for a definition within the enclosing module of
        @racket[id-stx]. It can be different from @racket[_from-sym]
        due to a renaming @racket[provide], even if @racket[_from-mod]
        and @racket[_nominal-from-mod] are the same, or due to a
        definition that was introduced by a macro expansion.}

        @item{@racket[_from-phase] is an exact non-negative integer
        representing the originating phase. For example, it is
        @racket[1] if the definition is for-syntax.}

        @item{@racket[_import-phase+space-shift] is @racket[0] if the binding
        import of @racket[_nominal-from-mode] is from a definition
        or a plain @racket[require], @racket[1] if it is from a
        @racket[for-syntax] import, a phase combined with a space name if it
        is from a @racket[for-space] import, etc.}

        @item{@racket[_nominal-export-phase+space] is the @tech{phase level}
        and @tech{binding space}
        of the export from @racket[_nominal-from-mod] for an
        imported binding, or it is the phase level of the definition for a
        binding from the enclosing module of @racket[id-stx].}

        ]}

      @item{The result is @racket[(list _top-sym)] if @racket[id-stx]
            has a @tech{top-level binding} and
            @racket[top-level-symbol?] is true. The @racket[_top-sym]
            can different from the name returned by
            @racket[syntax->datum] when the binding definition was
            generated by a macro invocation.}

      @item{The result is @racket[#f] if @racket[id-stx] has a
            @tech{top-level binding} and @racket[top-level-symbol?] is
            @racket[#f] or if @racket[id-stx] is @tech{unbound}. An
            unbound identifier is typically treated the same as an
            identifier whose top-level binding is a variable.}

      ]

If @racket[id-stx] is bound to a @tech{rename-transformer}, the result
from @racket[identifier-binding] is for the identifier in the
transformer, so that @racket[identifier-binding] is consistent with
@racket[free-identifier=?].

If @racket[exact-scopes?] is a true value, then the result is
@racket[#f] unless the binding for @racket[id-stx] has exactly the
@tech{scopes} of @racket[id-stx]. An exact-scopes check is useful for
detecting whether an identifier is already bound in a specific
definition context, for example.

@history[#:changed "6.6.0.4" @elem{Added the @racket[top-level-symbol?] argument to report
                                   information on top-level bindings.}
        #:changed "8.2.0.3" @elem{Generalized phase results to phase--space combinations.}
        #:changed "8.6.0.9" @elem{Added the @racket[exact-scopes?] argument.}]}


@defproc[(identifier-transformer-binding [id-stx identifier?]
                                         [rt-phase-level (or/c exact-integer? #f)
                                                         (syntax-local-phase-level)])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       phase+space-shift?
                       phase+space?))]{

Same as @racket[(identifier-binding id-stx (and rt-phase-level (add1 rt-phase-level)))].

@history[#:changed "8.2.0.3" @elem{Generalized phase results to phase--space combinations.}]}


@defproc[(identifier-template-binding [id-stx identifier?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       phase+space?
                       phase+space-shift?
                       phase+space?))]{

Same as @racket[(identifier-binding id-stx (sub1 (syntax-local-phase-level)))].

@history[#:changed "8.2.0.3" @elem{Generalized phase results to phase--space combinations.}]}


@defproc[(identifier-label-binding [id-stx identifier?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       phase+space-shift?
                       phase+space?))]{

Same as @racket[(identifier-binding id-stx #f)].

@history[#:changed "8.2.0.3" @elem{Generalized phase results to phase--space combinations.}]}


@defproc[(identifier-distinct-binding [id-stx identifier?]
                                      [wrt-id-stx identifier?]
                                      [phase-level (or/c exact-integer? #f)
                                                   (syntax-local-phase-level)]
                                      [top-level-symbol? any/c #f])
         (or/c 'lexical
               #f
               (list/c module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       phase+space-shift?
                       phase+space?)
               (list/c symbol?))]{

Like @racket[(identifier-binding id-stx phase-level top-level-symbol?)], but the result
is @racket[#f] if the binding for @racket[id-stx] has scopes that are
a subset of the scopes for @racket[wrt-id-stx]. That is, if
@racket[id-stx] and @racket[wrt-id-stx] have the same symbolic name, a
binding for @racket[id-stx] is returned only if the binding does not
also apply to @racket[wrt-id-stx].

@history[#:added "8.3.0.8"
         #:changed "8.8.0.2" @elem{Added the @racket[top-level-symbol?] argument.}]}


@defproc[(identifier-binding-symbol [id-stx identifier?]
                                    [phase-level (or/c exact-integer? #f)
                                                 (syntax-local-phase-level)])
         symbol?]{

Like @racket[identifier-binding], but produces a symbol that
corresponds to the binding. The symbol result is the same for any
identifiers that are @racket[free-identifier=?], but the result may
also be the same for identifiers that are not
@racket[free-identifier=?] (i.e., different symbols imply different
bindings, but the same symbol does not imply the same binding).

When @racket[identifier-binding] would produce a list, then the second
element of that list is the result that
@racket[identifier-binding-symbol] produces.}


@defproc[(identifier-binding-portal-syntax [id-stx identifier?]
                                           [phase-level (or/c exact-integer? #f)
                                                        (syntax-local-phase-level)])
         (or/c #f syntax?)]{

If @racket[id-stx] is bound at @racket[phase-level] to @tech{portal
syntax}, either via @racket[define-syntax] or @racket[#%require], then
the portal syntax content is returned. The module that binds
@racket[id-stx] must be declared, but it need not be instantiated at
the relevant phase, and @racket[identifier-binding-portal-syntax] does
not instantiate the module.

@history[#:added "8.3.0.8"]}

@defproc[(syntax-bound-symbols [stx stx?]
                               [phase-level (or/c exact-integer? #f)
                                            (syntax-local-phase-level)]
                               [exact-scopes? any/c #f])
         (listof symbol?)]{

Returns a list of all @tech{interned} symbols for which
@racket[(identifier-binding (datum->syntax stx _sym) phase-level #f exact-scopes?)]
would produce a non-@racket[#f] value. This procedure takes time
proportional to the number of scopes on @racket[stx] plus the length
of the result list.

@history[#:added "8.6.0.6"
         #:changed "8.6.0.9" @elem{Added the @racket[exact-scopes?] argument.}]}

@defproc[(syntax-bound-phases [stx stx?])
         (listof (or/c exact-integer? #f))]{

Returns a list that includes all @racket[_phase-level]s for which
@racket[(syntax-bound-symbols stx _phase-level)] might produce a
non-empty list.

@examples[
#:eval stx-eval
(syntax-bound-phases #'anything)
(require (for-meta 8 racket/base))
(syntax-bound-phases #'anything)
]

@history[#:added "8.6.0.8"]}

@close-eval[stx-eval]

#lang scribble/doc
@(require "mz.rkt")

@(define stx-eval (make-base-eval))
@(interaction-eval #:eval stx-eval (require (for-syntax racket/base)))

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
                                          (syntax-local-phase-level)])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Returns one of three kinds of values, depending on the binding of
@racket[id-stx] at the @tech{phase level} indicated by
@racket[phase-level] (where a @racket[#f] value for
@racket[phase-level] corresponds to the @tech{label phase level}):

@itemize[ 

      @item{The result is @indexed-racket['lexical] if @racket[id-stx]
      has a @tech{local binding}. If @racket['lexical] is produced for
      any @racket[phase-level] value, then it is produced for all
      @racket[phase-level] values.}

      @item{The result is a list of seven items when @racket[id-stx]
      has a @tech{module binding}: @racket[(list _source-mod _source-id
      _nominal-source-mod _nominal-source-id _source-phase _import-phase 
      _nominal-export-phase)].

        @itemize[

        @item{@racket[_source-mod] is a module path index (see
        @secref["modpathidx"]) that indicates the defining module.}

        @item{@racket[_source-id] is a symbol for the identifier's name
        at its definition site in the source module. This can be
        different from the local name returned by
        @racket[syntax->datum] for several reasons: the identifier is
        renamed on import, it is renamed on export, or it is
        implicitly renamed because the identifier (or its import) was
        generated by a macro invocation.}

        @item{@racket[_nominal-source-mod] is a module path index (see
        @secref["modpathidx"]) that indicates the module
        @racket[require]d into the context of @racket[id-stx] to
        provide its binding. It can be different from
        @racket[_source-mod] due to a re-export in
        @racket[_nominal-source-mod] of some imported identifier.  If
        the same binding is imported in multiple ways, an arbitrary
        representative is chosen.}

        @item{@racket[_nominal-source-id] is a symbol for the
        identifier's name as exported by
        @racket[_nominal-source-mod]. It can be different from
        @racket[_source-id] due to a renaming @racket[provide], even if
        @racket[_source-mod] and @racket[_nominal-source-mod] are the
        same.}

        @item{@racket[_source-phase] is an exact non-negative integer
        representing the source phase. For example, it is @racket[1] if the source
        definition is for-syntax.}

        @item{@racket[_import-phase] is @racket[0] if the binding
        import of @racket[_nominal-source-mode] is a plain
        @racket[require], @racket[1] if it is from a
        @racket[for-syntax] import, etc.}

        @item{@racket[_nominal-export-phase] is the @tech{phase level}
        of the export from @racket[_nominal-source-mod].}

        ]}

      @item{The result is @racket[#f] if @racket[id-stx] has a
            @tech{top-level binding} (or, equivalently, if it is
            @tech{unbound}).}

      ]

If @racket[id-stx] is bound to a @tech{rename-transformer}, the result
from @racket[identifier-binding] is for the identifier in the
transformer, so that @racket[identifier-binding] is consistent with
@racket[free-identifier=?].}


@defproc[(identifier-transformer-binding [id-stx identifier?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Same as @racket[(identifier-binding id-stx (add1 (syntax-local-phase-level)))].}


@defproc[(identifier-template-binding [id-stx identifier?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Same as @racket[(identifier-binding id-stx (sub1 (syntax-local-phase-level)))].}


@defproc[(identifier-label-binding [id-stx identifier?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       exact-nonnegative-integer?
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Same as @racket[(identifier-binding id-stx #f)].}


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


@close-eval[stx-eval]

#lang scribble/doc
@(require "mz.rkt" scribble/struct racket/shared (for-label racket/shared))


@(define shared-eval (make-base-eval))
@(interaction-eval #:eval shared-eval (require racket/shared))

@(define maker
   (make-element #f (list
                     (racketvarfont "prefix:")
                     (racketidfont "make-")
                     (racketvarfont "id"))))
@(define typedef
   (make-element #f (list
                     (racketvarfont "prefix:")
                     (racketvarfont "id"))))

@title[#:tag "shared"]{Constructing Graphs: @racket[shared]}

@note-lib[racket/shared]

@defform[(shared ([id expr] ...) body ...+)]{

Binds @racket[id]s with shared structure according to @racket[exprs]
and then evaluates the @racket[body-expr]s, returning the result of
the last expression. 

The @racket[shared] form is similar to @racket[letrec], except that
special forms of @racket[expr] are recognized (after partial macro
expansion) to construct graph-structured data, where the corresponding
@racket[letrec] would instead produce @|undefined-const|s. 

Each @racket[expr] (after partial expansion) is matched against the
following @racket[_shared-expr] grammar, where earlier variants in a
production take precedence over later variants:

@racketgrammar*[
#:literals (cons list list* append vector-immutable box-immutable mcons vector box)
[shared-expr shell-expr
             plain-expr]
[shell-expr (cons in-immutable-expr in-immutable-expr)
            (list in-immutable-expr ...)
            (list* in-immutable-expr ...)
            (append early-expr ... in-immutable-expr)
            (vector-immutable in-immutable-expr ...)
            (box-immutable in-immutable-expr)
            (mcons patchable-expr patchable-expr)
            (vector patchable-expr ...)
            (box patchable-expr ...)
            (@#,|maker| patchable-expr ...)]
[in-immutable-expr shell-id
                   shell-expr
                   early-expr]
[shell-id id]
[patchable-expr expr]
[early-expr expr]
[plain-expr expr]
]

The @|maker| identifier above matches three kinds of references. The
first kind is any binding whose name has @racketidfont{make-} in the
middle, and where @|typedef| has a @tech{transformer binding} to
structure information with a full set of mutator bindings; see
@secref["structinfo"]. The second kind is an identifier that itself has a
@tech{transformer binding} to structure information. The third kind is an
identifier that has a @racket['constructor-for] @tech{syntax property}
whose value is an identifier with a @tech{transformer binding} to structure
information. A @racket[_shell-id], meanwhile, must be one of the
@racket[id]s bound by the @racket[shared] form to a
@racket[_shell-expr].

When the @racket[expr]s of the @racket[shared] form are parsed as
@racket[_shared-expr] (taking into account the order of the variants
for parsing precedence), the sub-expressions that were parsed via
@racket[_early-expr] will be evaluated first when the @racket[shared]
form is evaluated. Among such expressions, they are evaluated in the
order as they appear within the @racket[shared] form. However, any
reference to an @racket[id] bound by @racket[shared] produces
@|undefined-const|, even if the binding for the @racket[id] appears
before the corresponding @racket[_early-expr] within the
@racket[shared] form.

The @racket[_shell-ids] and @racket[_shell-exprs] (not counting
@racket[_patchable-expr] and @racket[_early-expr] sub-expressions) are
effectively evaluated next.  A @racket[_shell-id] reference produces
the same value as the corresponding @racket[_id] will produce within
the @racket[body]s, assuming that @racket[_id] is never mutated with
@racket[set!].  This special handling of a @racket[_shell-id]
reference is one way in which @racket[shared] supports the creation of
cyclic data, including immutable cyclic data.

Next, the @racket[_plain-expr]s are evaluated as for @racket[letrec],
where a reference to an @racket[id] produces @|undefined-const| if it
is evaluated before the right-hand side of the @racket[id] binding.

Finally, the @racket[_patchable-expr]s are evaluated. At this point,
all @racket[id]s are bound, so @racket[_patchable-expr]s also creates
data cycles (but only with cycles that can be created via mutation).

@examples[
#:eval shared-eval
(shared ([a (cons 1 a)])
  a)
(shared ([a (cons 1 b)]
         [b (cons 2 a)])
  a)
(shared ([a (cons 1 b)]
         [b 7])
  a)
(shared ([a a]) (code:comment @#,t{no indirection...})
  a)
(shared ([a (cons 1 b)] (code:comment @#,t{@racket[b] is early...})
         [b a])
  a)
(shared ([a (mcons 1 b)] (code:comment @#,t{@racket[b] is patchable...})
         [b a])
  a)
(shared ([a (vector b b b)]
         [b (box 1)])
  (set-box! b 5)
  a)
(shared ([a (box b)]
         [b (vector (unbox a)   (code:comment @#,t{@racket[unbox] after @racket[a] is patched})
                    (unbox c))] (code:comment @#,t{@racket[unbox] before @racket[c] is patched})
         [c (box b)])
  b)
]}


@; ----------------------------------------------------------------------

@close-eval[shared-eval]

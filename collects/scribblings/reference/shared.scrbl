#lang scribble/doc
@(require "mz.ss"
          scribble/struct
          racket/shared
          (for-label racket/shared))


@(define shared-eval (make-base-eval))
@(interaction-eval #:eval shared-eval (require racket/shared))

@(define maker
   (make-element #f (list
                     (schemevarfont "prefix:")
                     (schemeidfont "make-")
                     (schemevarfont "id"))))
@(define typedef
   (make-element #f (list
                     (schemevarfont "prefix:")
                     (schemevarfont "id"))))

@title[#:tag "shared"]{Constructing Graphs: @scheme[shared]}

@note-lib[racket/shared]

@defform[(shared ([id expr] ...) body ...+)]{

Binds @scheme[id]s with shared structure according to @scheme[exprs]
and then evaluates the @scheme[body-expr]s, returning the result of
the last expression. 

The @scheme[shared] form is similar to @scheme[letrec], except that
special forms of @scheme[expr] are recognized (after partial macro
expansion) to construct graph-structured data, where the corresponding
@scheme[letrec] would instead produce @|undefined-const|s. 

Each @scheme[expr] (after partial expansion) is matched against the
following @scheme[_shared-expr] grammar, where earlier variants in a
production take precedence over later variants:

@schemegrammar*[
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

The @|maker| identifier above references to any binding whose name has
@schemeidfont{make-} in the middle, and where @|typedef| has a
@tech{transformer binding} to structure information with a full set of
mutator bindings; see @secref["structinfo"]. A @scheme[_shell-id] must
be one of the @scheme[id]s bound by the @scheme[shared] form to a
@scheme[_shell-expr].

When the @scheme[expr]s of the @scheme[shared] form are parsed via
@scheme[_shared-expr] (taking into account the order of the variants
for precedence), and sub-expressions that parse via
@scheme[_early-expr] will be evaluated first when the @scheme[shared]
form is evaluated. Among such expressions, they are evaluated in the
order as they appear within the @scheme[shared] form. However, any
reference to an @scheme[id] bound by @scheme[shared] produces
@|undefined-const|, even if the binding for the @scheme[id] appears
before the corresponding @scheme[_early-expr] within the
@scheme[shared] form.

The @scheme[_shell-ids] and @scheme[_shell-exprs] (not counting
@scheme[_patchable-expr] and @scheme[_early-expr] sub-expressions) are
effectively evaluated next.  A @scheme[_shell-id] reference produces
the same value as the corresponding @scheme[_id] will produce within
the @scheme[body]s, assuming that @scheme[_id] is never mutated with
@scheme[set!].  This special handling of a @scheme[_shell-id]
reference is one way in which @scheme[shared] supports the creation of
cyclic data, including immutable cyclic data.

Next, the @scheme[_plain-expr]s are evaluated as for @scheme[letrec],
where a reference to an @scheme[id] produces @|undefined-const| if it
is evaluated before the right-hand side of the @scheme[id] binding.

Finally, the @scheme[_patchable-expr]s are evaluated. At this point,
all @scheme[id]s are bound, so @scheme[_patchable-expr]s also creates
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
(shared ([a (cons 1 b)] (code:comment @#,t{@scheme[b] is early...})
         [b a])
  a)
(shared ([a (mcons 1 b)] (code:comment @#,t{@scheme[b] is patchable...})
         [b a])
  a)
(shared ([a (vector b b b)]
         [b (box 1)])
  (set-box! b 5)
  a)
(shared ([a (box b)]
         [b (vector (unbox a)   (code:comment @#,t{@scheme[unbox] after @scheme[a] is patched})
                    (unbox c))] (code:comment @#,t{@scheme[unbox] before @scheme[c] is patched})
         [c (box b)])
  b)
]}


@; ----------------------------------------------------------------------

@close-eval[shared-eval]

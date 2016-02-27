#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "let"]{Local Binding}

Although internal @racket[define]s can be used for local binding,
Racket provides three forms that give the programmer more
control over bindings: @racket[let], @racket[let*], and
@racket[letrec].

@;------------------------------------------------------------------------
@section{Parallel Binding: @racket[let]}

@refalso["let"]{@racket[let]}

A @racket[let] form binds a set of identifiers, each to the result of
some expression, for use in the @racket[let] body:

@specform[(let ([id expr] ...) body ...+)]{}

The @racket[_id]s are bound ``in parallel.'' That is, no @racket[_id]
is bound in the right-hand side @racket[_expr] for any @racket[_id],
but all are available in the @racket[_body]. The @racket[_id]s must be
different from each other.

@examples[
(let ([me "Bob"])
  me)
(let ([me "Bob"]
      [myself "Robert"]
      [I "Bobby"])
  (list me myself I))
(let ([me "Bob"]
      [me "Robert"])
  me)
]

The fact that an @racket[_id]'s @racket[_expr] does not see its own
binding is often useful for wrappers that must refer back to the old
value:

@interaction[
(let ([+ (lambda (x y)
           (if (string? x)
               (string-append x y)
               (+ x y)))]) (code:comment @#,t{use original @racket[+]})
  (list (+ 1 2)
        (+ "see" "saw")))
]

Occasionally, the parallel nature of @racket[let] bindings is
convenient for swapping or rearranging a set of bindings:

@interaction[
(let ([me "Tarzan"]
      [you "Jane"])
  (let ([me you]
        [you me])
    (list me you)))
]

The characterization of @racket[let] bindings as ``parallel'' is not
meant to imply concurrent evaluation. The @racket[_expr]s are
evaluated in order, even though the bindings are delayed until all
@racket[_expr]s are evaluated.

@;------------------------------------------------------------------------
@section{Sequential Binding: @racket[let*]}

@refalso["let"]{@racket[let*]}

The syntax of @racket[let*] is the same as @racket[let]:

@specform[(let* ([id expr] ...) body ...+)]{}

The difference is that each @racket[_id] is available for use in later
@racket[_expr]s, as well as in the @racket[_body]. Furthermore, the
@racket[_id]s need not be distinct, and the most recent binding is the
visible one.

@examples[
(let* ([x (list "Burroughs")]
       [y (cons "Rice" x)]
       [z (cons "Edgar" y)])
  (list x y z))
(let* ([name (list "Burroughs")]
       [name (cons "Rice" name)]
       [name (cons "Edgar" name)])
  name)
]

In other words, a @racket[let*] form is equivalent to nested
@racket[let] forms, each with a single binding:

@interaction[
(let ([name (list "Burroughs")])
  (let ([name (cons "Rice" name)])
    (let ([name (cons "Edgar" name)])
      name)))
]

@;------------------------------------------------------------------------
@section{Recursive Binding: @racket[letrec]}

@refalso["let"]{@racket[letrec]}

The syntax of @racket[letrec] is also the same as @racket[let]:

@specform[(letrec ([id expr] ...) body ...+)]{}

While @racket[let] makes its bindings available only in the
@racket[_body]s, and @racket[let*] makes its bindings available to any
later binding @racket[_expr], @racket[letrec] makes its bindings
available to all other @racket[_expr]s---even earlier ones. In other
words, @racket[letrec] bindings are recursive.

The @racket[_expr]s in a @racket[letrec] form are most often
@racket[lambda] forms for recursive and mutually recursive functions:

@interaction[
(letrec ([swing
          (lambda (t)
            (if (eq? (car t) 'tarzan)
                (cons 'vine
                      (cons 'tarzan (cddr t)))
                (cons (car t)
                      (swing (cdr t)))))])
  (swing '(vine tarzan vine vine)))
]

@interaction[
(letrec ([tarzan-near-top-of-tree?
          (lambda (name path depth)
            (or (equal? name "tarzan")
                (and (directory-exists? path)
                     (tarzan-in-directory? path depth))))]
         [tarzan-in-directory?
          (lambda (dir depth)
            (cond
              [(zero? depth) #f]
              [else
               (ormap
                (λ (elem)
                  (tarzan-near-top-of-tree? (path-element->string elem)
                                            (build-path dir elem)
                                            (- depth 1)))
                (directory-list dir))]))])
  (tarzan-near-top-of-tree? "tmp" 
                            (find-system-path 'temp-dir)
                            4))
]

While the @racket[_expr]s of a @racket[letrec] form are typically
@racket[lambda] expressions, they can be any expression. The
expressions are evaluated in order, and after each value is obtained,
it is immediately associated with its corresponding @racket[_id]. If
an @racket[_id] is referenced before its value is ready, an
error is raised, just as for internal definitions.

@interaction[
(letrec ([quicksand quicksand])
  quicksand)
]

@; ----------------------------------------
@include-section["named-let.scrbl"]

@; ----------------------------------------
@section{Multiple Values: @racket[let-values], @racket[let*-values], @racket[letrec-values]}

@refalso["let"]{multiple-value binding forms}

In the same way that @racket[define-values] binds multiple
results in a definition (see @secref["multiple-values"]),
@racket[let-values], @racket[let*-values], and
@racket[letrec-values] bind multiple results locally.

@specform[(let-values ([(id ...) expr] ...)
            body ...+)]
@specform[(let*-values ([(id ...) expr] ...)
            body ...+)]
@specform[(letrec-values ([(id ...) expr] ...)
            body ...+)]

Each @racket[_expr] must produce as many values as corresponding
@racket[_id]s. The binding rules are the same for the forms
without @racketkeywordfont{-values} forms: the @racket[_id]s of
@racket[let-values] are bound only in the @racket[_body]s, the
@racket[_id]s of @racket[let*-values]s are bound in
@racket[_expr]s of later clauses, and the @racket[_id]s of
@racket[letrec-value]s are bound for all @racket[_expr]s.

@examples[
(let-values ([(q r) (quotient/remainder 14 3)])
  (list q r))
]

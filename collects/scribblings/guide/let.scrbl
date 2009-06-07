#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "let"]{Local Binding}

Although internal @scheme[define]s can be used for local binding,
Scheme provides three forms that give the programmer more
control over bindings: @scheme[let], @scheme[let*], and
@scheme[letrec].

@;------------------------------------------------------------------------
@section{Parallel Binding: @scheme[let]}

@refalso["let"]{@scheme[let]}

A @scheme[let] form binds a set of identifiers, each to the result of
some expression, for use in the @scheme[let] body:

@specform[(let ([id expr] ...) body ...+)]{}

The @scheme[_id]s are bound ``in parallel.'' That is, no @scheme[_id]
is bound in the right-hand side @scheme[_expr] for any @scheme[_id],
but all are available in the @scheme[_body]. The @scheme[_id]s must be
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

The fact that an @scheme[_id]'s @scheme[_expr] does not see its own
binding is often useful for wrappers that must refer back to the old
value:

@interaction[
(let ([+ (lambda (x y)
           (if (string? x)
               (string-append x y)
               (+ x y)))]) (code:comment @#,t{use original @scheme[+]})
  (list (+ 1 2)
        (+ "see" "saw")))
]

Occasionally, the parallel nature of @scheme[let] bindings is
convenient for swapping or rearranging a set of bindings:

@interaction[
(let ([me "Tarzan"]
      [you "Jane"])
  (let ([me you]
        [you me])
    (list me you)))
]

The characterization of @scheme[let] bindings as ``parallel'' is not
meant to imply concurrent evaluation. The @scheme[_expr]s are
evaluated in order, even though the bindings are delayed until all
@scheme[_expr]s are evaluated.

@;------------------------------------------------------------------------
@section{Sequential Binding: @scheme[let*]}

@refalso["let"]{@scheme[let*]}

The syntax of @scheme[let*] is the same as @scheme[let]:

@specform[(let* ([id expr] ...) body ...+)]{}

The difference is that each @scheme[_id] is available for use in later
@scheme[_expr]s, as well as in the @scheme[_body]. Furthermore, the
@scheme[_id]s need not be distinct, and the most recent binding is the
visible one.

@examples[
(let* ([x (list "Borroughs")]
       [y (cons "Rice" x)]
       [z (cons "Edgar" y)])
  (list x y z))
(let* ([name (list "Borroughs")]
       [name (cons "Rice" name)]
       [name (cons "Edgar" name)])
  name)
]

In other words, a @scheme[let*] form is equivalent to nested
@scheme[let] forms, each with a single binding:

@interaction[
(let ([name (list "Borroughs")])
  (let ([name (cons "Rice" name)])
    (let ([name (cons "Edgar" name)])
      name)))
]

@;------------------------------------------------------------------------
@section{Recursive Binding: @scheme[letrec]}

@refalso["let"]{@scheme[letrec]}

The syntax of @scheme[letrec] is also the same as @scheme[let]:

@specform[(letrec ([id expr] ...) body ...+)]{}

While @scheme[let] makes its bindings available only in the
@scheme[_body]s, and @scheme[let*] makes its bindings available to any
later binding @scheme[_expr], @scheme[letrec] makes its bindings
available to all other @scheme[_expr]s---even earlier ones. In other
words, @scheme[letrec] bindings are recursive.

The @scheme[_expr]s in a @scheme[letrec] form are most often
@scheme[lambda] forms for recursive and mutually recursive functions:

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
(letrec ([tarzan-in-tree?
          (lambda (name path)
            (or (equal? name "tarzan")
                (and (directory-exists? path)
                     (tarzan-in-directory? path))))]
         [tarzan-in-directory?
          (lambda (dir)
            (ormap (lambda (elem)
                     (tarzan-in-tree? (path-element->string elem)
                                      (build-path dir elem)))
                   (directory-list dir)))])
  (tarzan-in-tree? "tmp" (find-system-path 'temp-dir)))
]

While the @scheme[_expr]s of a @scheme[letrec] form are typically
@scheme[lambda] expressions, they can be any expression. The
expressions are evaluated in order, and after each value is obtained,
it is immediately associated with its corresponding @scheme[_id]. If
an @scheme[_id] is referenced before its value is ready, the result is
@|undefined-const|, as just as for internal definitions.

@interaction[
(letrec ([quicksand quicksand])
  quicksand)
]

@; ----------------------------------------
@include-section["named-let.scrbl"]

@; ----------------------------------------
@section{Multiple Values: @scheme[let-values], @scheme[let*-values], @scheme[letrec-values]}

@refalso["let"]{multiple-value binding forms}

In the same way that @scheme[define-values] binds multiple
results in a definition (see @secref["multiple-values"]),
@scheme[let-values], @scheme[let*-values], and
@scheme[letrec-values] bind multiple results locally.

@specform[(let-values ([(id ...) expr] ...)
            body ...+)]
@specform[(let*-values ([(id ...) expr] ...)
            body ...+)]
@specform[(letrec-values ([(id ...) expr] ...)
            body ...+)]

Each @scheme[_expr] must produce as many values as corresponding
@scheme[_id]s. The binding rules are the same for the forms
without @schemekeywordfont{-values} forms: the @scheme[_id]s of
@scheme[let-values] are bound only in the @scheme[_body]s, the
@scheme[_id]s of @scheme[let*-values]s are bound in
@scheme[_expr]s of later clauses, and the @scheme[_id]s of
@scheme[letrec-value]s are bound for all @scheme[_expr]s.

@examples[
(let-values ([(q r) (quotient/remainder 14 3)])
  (list q r))
]

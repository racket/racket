#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@interaction-eval[(require (rename (lib "etc.ss") lambda opt-lambda))]

@title[#:tag "guide:lambda"]{Procedures: @scheme[lambda] and @scheme[case-lambda]}

A @scheme[lambda] expression creates a procedure. In the simplest
case, a @scheme[lambda] expression has the form

@specform[
(lambda (arg-id ...)
  body ...+)
]

A @scheme[lambda] form with @math{n} @scheme[_arg-id]s accepts
@math{n} arguments:

@interaction[
((lambda (x) x)
 1)
((lambda (x y) (+ x y)) 
 1 2)
((lambda (x y) (+ x y)) 
 1)
]

@;------------------------------------------------------------------------
@section{Rest Arguments}

A @scheme[lambda] expression can also have the form

@specform[
(lambda rest-id
  body ...+)
]

That is, a @scheme[lambda] expression can have a single
@scheme[_rest-id] that is not surrounded by parentheses. The resulting
procedure accepts any number of arguments, and the arguments are put
into a list bound to @scheme[_rest-id].

@examples[
((lambda x x) 
 1 2 3)
((lambda x x))
((lambda x (car x))
 1 2 3)
]

Procedures of this form often use @scheme[apply] to call another
procedure that accepts any number of arguments.

@margin-note{See @secref["guide:apply"] for more information on
@scheme[apply].}

@defexamples[
(define max-mag 
  (lambda nums
    (apply max (map magnitude nums))))

(max 1 -2 0)
(max-mag 1 -2 0)
]

Generalizing the fixed-arity and any-arity style, a @scheme[lambda]
expression can have the form

@specform[
(lambda (arg-id ...+ . rest-id)
  body ...+)
]

The result is a procedure that requires at least as many arguments as
@scheme[_arg-id]s, and also accepts any number of additional
arguments.

@defexamples[
(define max-mag 
  (lambda (num . nums)
    (apply max (map magnitude (cons num nums)))))

(max-mag 1 -2 0)
(max-mag)
]

A @scheme[_rest-id] variable is sometimes called a @defterm{rest
argument}, because it accepts the ``rest'' of the procedure arguments.

@;------------------------------------------------------------------------
@section{Optional Arguments}

Instead of just an identifier, an argument (other than a rest
argument) in a @scheme[lambda] form can be specified with an
identifier and a default value:

@specform/subs[
(lambda (arg ...+ . rest-id)
  body ...+)
([arg arg-id
      [arg-id default-expr]])
]{}

A argument of the form @scheme[[arg-id default-expr]] is
optional. When the argument is not supplied in an application,
@scheme[_default-expr] produces the default value. The
@scheme[_default-expr] can refer to any preceding @scheme[_arg-id],
and every following @scheme[_arg-id] must have a default as well.

@defexamples[
(define greet
  (lambda (given [surname "Smith"])
    (string-append "Hello, " given " " surname)))

(greet "John")
(greet "John" "Doe")
]

@def+int[
(define greet
  (lambda (given [surname (if (equal? given "John")
                              "Doe"
                              "Smith")])
    (string-append "Hello, " given " " surname)))

(greet "John")
(greet "Adam")
]

@section{Keyword Arguments}

A @scheme[lambda] form can declare an argument to be passed by
keyword, instead of position. Keyword arguments can be mixed with
by-position arguments, and default-value expressions can be supplied
for either kind of argument:

@specform/subs[
(lambda (arg ...+ . rest-id)
  body ...+)
([arg arg-id
      [arg-id default-expr]
      (code:line arg-keyword arg-id)
      (code:line arg-keyword [arg-id default-expr])])
]{}

As argument specified as @scheme[(code:line _arg-keyword _arg-id)] is
supplied by an application using the same @scheme[_arg-keyword].  The
position of the keyword--identifier pair in the argument list does not
matter for matching with arguments in an application, because it will
be matched to an argument value by keyword instead of by position.

@def+int[
(define greet
  (lambda (given #:last surname)
    (string-append "Hello, " given " " surname)))

(greet "John" #:last "Smith")
(greet #:last "Doe" "John")
]

A @scheme[(code:line _arg-keyword [_arg-id _default-expr])] argument
specifies a keyword-based argument with a default value.

@defexamples[
(define greet
  (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
    (string-append hi ", " given " " surname)))

(greet "John")
(greet "Karl" #:last "Marx")
(greet "John" #:hi "Howdy")
(greet "Karl" #:last "Marx" #:hi "Guten Tag")
]

@;------------------------------------------------------------------------
@section{Arity-Sensitive Procedures: @scheme[case-lambda]}

The @scheme[case-lambda] form creates a procedure that can have
completely different behaviors depending on the number of arguments
that are supplied. A case-lambda expression has the form

@specform/subs[
(case-lambda
  [formals body ...+]
  ...)
([formals (arg-id ...)
          rest-id
          (arg-id ...+ . rest-id)])
]

where each @scheme[_formals _body ...+] is anlogous to @scheme[(lambda
_formals _body ...+)]. Applying a procedure produced by
@scheme[case-lambda] is like applying a @scheme[lambda] for the first
case that matches the number of given arguments.

@defexamples[
(define greet
  (case-lambda
    [(name) (string-append "Hello, " name)]
    [(given surname) (string-append "Hello, " given " " surname)]))

(greet "John")
(greet "John" "Smith")
(greet)
]

A @scheme[case-lambda] procedure cannot directly support optional or
keyword arguments.

@refdetails["mz:lambda"]{procedure expressions}

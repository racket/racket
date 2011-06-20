#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "void+undefined"]{Void and Undefined}

Some procedures or expression forms have no need for a result
value. For example, the @racket[display] procedure is called only for
the side-effect of writing output. In such cases the result value is
normally a special constant that prints as @|void-const|.  When the
result of an expression is simply @|void-const|, the REPL does not
print anything.

The @racket[void] procedure takes any number of arguments and returns
@|void-const|. (That is, the identifier @racketidfont{void} is bound
to a procedure that returns @|void-const|, instead of being bound
directly to @|void-const|.)

@examples[
(void)
(void 1 2 3)
(list (void))
]

A constant that prints as @undefined-const is used as the result of a
reference to a local binding when the binding is not yet
initialized. Such early references are not possible for bindings that
correspond to procedure arguments, @racket[let] bindings, or
@racket[let*] bindings; early reference requires a recursive binding
context, such as @racket[letrec] or local @racket[define]s in a
procedure body. Also, early references to top-level and module-level
bindings raise an exception, instead of producing
@|undefined-const|. For these reasons, @undefined-const rarely
appears.

@def+int[
(define (strange)
  (define x x)
  x)
(strange)
]


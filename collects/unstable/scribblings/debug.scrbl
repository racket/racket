#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket unstable/debug unstable/syntax))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/debug))

@title{Debugging}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/debug]

This module provides macros and functions for printing out debugging
information.

@defform/subs[
(debug options ... expr)
([options (code:line #:name name-expr)
          (code:line #:source srcloc-expr)])
]{

Writes debugging information about the evaluation of @racket[expr] to the
current error port.  The name and source location of the expression may be
overridden by keyword options; their defaults are the syntactic form of the
expression and its syntactic source location, respectively.

@examples[#:eval the-eval
(debug 0)
(debug #:name "one, two, three" (values 1 2 3))
(debug #:source (make-srcloc 'here 1 2 3 4)
  (error 'function "something went wrong"))
]

}

@defproc[(dprintf [fmt string?] [arg any/c] ...) void?]{

Constructs a message in the same manner as @racket[format] and writes it to
@racket[(current-error-port)], with indentation reflecting the number of nested
@racket[debug] forms.

@examples[#:eval the-eval
(dprintf "level: ~a" 0)
(debug (dprintf "level: ~a" 1))
(debug (debug (dprintf "level: ~a" 2)))
]

}

@defform/subs[
(debugf function-expr argument ...)
([argument argument-expr (code:line argument-keyword argument-expr)])
]{

Logs debugging information for @racket[(#%app function-expr argument ...)],
including the evaluation and results of the function and each argument.

@examples[#:eval the-eval
(debugf + 1 2 3)
]

}

@deftogether[(
@defform[(begin/debug expr ...)]
@defform*[[(define/debug id expr)
           (define/debug (head args) body ...+)]]
@defform*[[(define/private/debug id expr)
           (define/private/debug (head args) body ...+)]]
@defform*[[(define/public/debug id expr)
           (define/public/debug (head args) body ...+)]]
@defform*[[(define/override/debug id expr)
           (define/override/debug (head args) body ...+)]]
@defform*[[(define/augment/debug id expr)
           (define/augment/debug (head args) body ...+)]]
@defform*[[(let/debug ([lhs-id rhs-expr] ...) body ...+)
           (let/debug loop-id ([lhs-id rhs-expr] ...) body ...+)]]
@defform[(let*/debug ([lhs-id rhs-expr] ...) body ...+)]
@defform[(letrec/debug ([lhs-id rhs-expr] ...) body ...+)]
@defform[(let-values/debug ([(lhs-id ...) rhs-expr] ...) body ...+)]
@defform[(let*-values/debug ([(lhs-id ...) rhs-expr] ...) body ...+)]
@defform[(letrec-values/debug ([(lhs-id ...) rhs-expr] ...) body ...+)]
@defform[(with-syntax/debug ([pattern stx-expr] ...) body ...+)]
@defform[(with-syntax*/debug ([pattern stx-expr] ...) body ...+)]
@defform[(parameterize/debug ([param-expr value-expr] ...) body ...+)]
)]{

These macros add logging based on @racket[debug] to the evaluation of
expressions in @racket[begin], @racket[define], @racket[define/private],
@racket[define/public], @racket[define/override], @racket[define/augment],
@racket[let], @racket[let*], @racket[letrec], @racket[let-values],
@racket[let*-values], @racket[letrec-values], @racket[with-syntax],
@racket[with-syntax*], and @racket[parameterize].

}

@(close-eval the-eval)

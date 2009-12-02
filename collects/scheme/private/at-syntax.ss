#lang scheme/base

(require (for-template scheme/base))

(provide at-syntax)

;; -------------------------------------------------------------------
;; NOTE: This library is for internal use only, it is can change
;; and/or disappear.  Do not use without protective eyewear!
;; -------------------------------------------------------------------

#|

The `(at-syntax expr)' form is a useful syntax-time utility that can
be used to sort of evaluate an expression at syntax time, and doing so
in a well behaved way (eg, it respects the source for-syntax bindings,
but it does have some issues).  It can be used to implement an escape
to the syntax level that is not restricted like `begin-for-syntax'.

The basic idea of the code is to plant the given expression on the
right hand side of a `let-syntax' -- inside a `(lambda (stx) ...)'  to
make it a valid transformer, with a singe use of this macro so that we
get it to execute with `local-expand'.  The macro returns a 3d
expression that contains the evaluated expression "somehwhere",
depending on the expansion of `let-syntax' -- so to make it easy to
find we plant it inside a thunk (so this works as long as `let-syntax'
does not include 3d procedure values in its expansion).  Finally, the
constructed `let-syntax' is expanded, we search through the resulting
syntax for the thunk, then apply it to get the desired value.

Here's a silly example to demonstrate:

  > (define-syntax (compile-time-if stx)
      (syntax-case stx ()
        [(_ cond expr1 expr2)
         (if (at-syntax #'cond) #'expr1 #'expr2)]))
  > (define-for-syntax x 8)
  > (define x 100)
  > (compile-time-if (< x 10) (+ x 10) (- x 10))
  110

And another example, creating a macro for syntax-time expressions:

  > (define-syntax (compile-time-value stx)
      (syntax-case stx ()
        [(_ expr) #`(quote #,(at-syntax #'expr))]))
  > (compile-time-value (* x 2))
  16

but the `quote' here is a hint that this can get 3d values into
syntax, and all the problems that are involved.  Also, note that it
even works if you try to do something like:

  > (compile-time-value (begin (set! x 11) x))
  11

(but, of course, it cannot be used to define new bindings).

|#

(define (at-syntax expr)
  (let loop ([x (with-syntax ([e expr])
                  (local-expand
                   #'(let-syntax ([here (lambda (stx)
                                          (datum->syntax stx (lambda () e)))])
                       here)
                   'expression '()))])
    (cond [(procedure? x) (x)]
          [(pair? x) (or (loop (car x)) (loop (cdr x)))]
          [(syntax? x) (loop (syntax-e x))]
          [else #f])))

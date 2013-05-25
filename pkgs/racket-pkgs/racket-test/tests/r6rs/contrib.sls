#!r6rs

(library (tests r6rs contrib)
  (export run-contrib-tests)
  (import (rnrs)
          (tests r6rs test)
          (prefix (tests r6rs contrib helper1) L:))

  ;; Definitions ----------------------------------------

  ;; from Derick Eddington:
  (define-syntax my-letrec
    (syntax-rules ()
      [(_ ([v e] ...) . b)
       (let ()
         (define t (list e ...))
         (define v (let ([v (car t)]) (set! t (cdr t)) v))
         ...
         . b)]))

  ;; Expressions ----------------------------------------

  (define (run-contrib-tests)

    ;; from Derick Eddington:
    (test (my-letrec ([f (lambda (x) (g x 2))]
                      [g (lambda (x y) (+ x y))])
            (f 1))
          3)

    ;; from Derick Eddington:
    (test (L:s L:x) 'ok)

    ;;;
    ))

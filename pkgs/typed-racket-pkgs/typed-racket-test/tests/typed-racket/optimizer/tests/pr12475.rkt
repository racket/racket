#;#;
#<<END
TR opt: pr12475.rkt 10:3 (quote 0.0+0.0i) -- unboxed literal
TR opt: pr12475.rkt 4:2 ((letrec-values (((for-loop) (lambda (so-far-init) (if (quote #t) (* so-far-init (quote 1.0)) so-far-init)))) for-loop) (quote 0.0+0.0i)) -- unboxed call site
TR opt: pr12475.rkt 4:21 for-loop -- fun -> unboxed fun
TR opt: pr12475.rkt 5:29 so-far-init -- unboxed var -> table
TR opt: pr12475.rkt 7:26 (* so-far-init (quote 1.0)) -- unboxed binary float complex
TR opt: pr12475.rkt 7:29 so-far-init -- leave var unboxed
TR opt: pr12475.rkt 7:41 (quote 1.0) -- float in complex ops
TR opt: pr12475.rkt 8:26 so-far-init -- dead else branch
TR opt: pr12475.rkt 9:5 for-loop -- unboxed let loop
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(: coefficients->poly (-> Float-Complex))
(define (coefficients->poly)
  ((letrec-values (((for-loop)
                    (lambda (so-far-init)
                      (if '#t
                          (* so-far-init '1.0)
                          so-far-init))))
     for-loop)
   '0.0+0.0i))

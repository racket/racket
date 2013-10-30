#;#;
#<<END
TR opt: pr12475.rkt 18:2 ((letrec-values (((for-loop) (lambda (so-far-init) (if (quote #t) (* so-far-init (quote 1.0)) so-far-init)))) for-loop) (quote 0.0+0.0i)) -- unboxed call site
TR opt: pr12475.rkt 18:21 for-loop -- fun -> unboxed fun
TR opt: pr12475.rkt 19:29 so-far-init -- unboxed var -> table
TR opt: pr12475.rkt 21:26 (* so-far-init (quote 1.0)) -- unboxed binary float complex
TR opt: pr12475.rkt 21:29 so-far-init -- leave var unboxed
TR opt: pr12475.rkt 21:41 (quote 1.0) -- float in complex ops
TR opt: pr12475.rkt 22:26 so-far-init -- dead else branch
TR opt: pr12475.rkt 23:5 for-loop -- unboxed let loop
TR opt: pr12475.rkt 24:3 (quote 0.0+0.0i) -- unboxed literal
END
""
#lang typed/racket

(: coefficients->poly (-> Float-Complex))
(define (coefficients->poly)
  ((letrec-values (((for-loop)
                    (lambda (so-far-init)
                      (if '#t
                          (* so-far-init '1.0)
                          so-far-init))))
     for-loop)
   '0.0+0.0i))

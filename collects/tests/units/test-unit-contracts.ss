(require "test-harness.ss"
         scheme/unit)

(define-signature sig1
  ((contracted [x number?])))
(define-signature sig2
  ((contracted [f (-> number? number?))))
(define-signature sig3 extends sig2
  ((contracted [g (-> number? boolean?))))
(define-signature sig4
  ((contracted [a number?] [b (-> boolean? number?)])))
(define-signature sig5
  ((contracted [c string?])
   (contracted [d symbol?])))

(define-unit unit1
  (import sig1)
  (export sig2)
  (define (f n) x))

(define-unit unit2
  (import sig3 sig4)
  (export)

  (b (g a)))

(define-unit unit3
  (import)
  (export sig5)

  (define-values (c d) (values "foo" 'a)))

#lang racket
(require rackunit
         syntax/parse
         syntax/parse/debug
         syntax/parse/experimental/reflect
         syntax/parse/experimental/splicing
         syntax/parse/experimental/eh
         "setup.rkt"
         (for-syntax syntax/parse))

;; Reflection

(define-syntax-class (nat> x)
  #:description (format "natural number greater than ~s" x)
  (pattern n:nat
           #:when (> (syntax-e #'n) x)
           #:with diff (- (syntax-e #'n) x)))
(define r-nat> (reify-syntax-class nat>))

(tok (1 2 -3 -4 5) ((~or (~reflect yes (r-nat> 1) #:attributes (diff)) no) ...)
     (and (s= (yes ...) '(2 5))
          (s= (yes.diff ...) '(1 4))
          (s= (no ...) '(1 -3 -4))))
(terx 3 (~reflect pos (r-nat> 5))
      #rx"expected natural number greater than 5")
(terx whatever (~reflect x (r-nat> 0) #:attributes (wrong nope)))

(define-splicing-syntax-class opt
  (pattern (~seq #:a a:expr)))
(define r-opt (reify-syntax-class opt))

(tok (#:a 1) ((~splicing-reflect s (r-opt) #:attributes (a)))
     (s= s.a '1))
(tok (#:a 1 #:a 2 #:a 3) ((~splicing-reflect s (r-opt) #:attributes (a)) ...)
     (s= (s.a ...) '(1 2 3)))


;; EH-alternative-sets

(define-eh-alternative-set opts
  (pattern (~once (~seq #:a a:expr) #:name "A option"))
  (pattern (~seq #:b b:expr)))

(tok (#:a 1) ((~eh-var s opts) ...)
     (and (s= s.a 1) (s= (s.b ...) '())))
(tok (#:a 1 #:b 2 #:b 3) ((~eh-var s opts) ...)
     (and (s= s.a 1) (s= (s.b ...) '(2 3))))

(terx (#:b 2 #:b 3) ((~eh-var s opts) ...)
      #rx"missing required occurrence of A option")
(terx (#:a 1 #:a 2) ((~eh-var s opts) ...)
      #rx"too many occurrences of A option")

(define-eh-alternative-set extopts
  (pattern (~eh-var s opts))
  (pattern (~seq #:c c1:expr c2:expr)))

(tok (#:a 1 #:c 2 3 #:c 4 5) ((~eh-var x extopts) ...)
     (and (s= x.s.a 1) (s= (x.s.b ...) '())
          (s= ((x.c1 x.c2) ...) '((2 3) (4 5)))))
(terx (#:c 1 2) ((~eh-var x extopts) ...)
      #rx"missing required occurrence of A option")

;; Splicing

(define-primitive-splicing-syntax-class (foo)
  #:attrs (z x y)
  #:description "foo"
  (lambda (stx fail)
    (syntax-case stx ()
      [(a b c . rest)
       (list #'rest 3 #'a #'b #'c)]
      [_ (fail)])))

(tok (1 2 3 4) (f:foo 4)
     (and (s= f.z 1) (s= f.x 2) (s= f.y 3)))

(terx (1) (f:foo)
      #rx"expected foo")

#lang racket

;; Testing all exp modules in one test module masks lazy-require-related bugs,
;; since some exp modules currently eagerly require impl modules.
;; So create separate submodules with minimal dependencies.

;; common defs
(module common racket
  (require syntax/parse)
  (provide nat>)
  (define-syntax-class (nat> x)
    #:description (format "natural number greater than ~s" x)
    (pattern n:nat
             #:when (> (syntax-e #'n) x)
             #:with diff (- (syntax-e #'n) x))))


;; Reflection
(module reflect racket
  (require rackunit "setup.rkt" syntax/parse syntax/parse/experimental/reflect
           (submod ".." common))

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
  )
(require 'reflect)


;; EH-alternative-sets
(module eh-alts racket
  (require rackunit "setup.rkt" syntax/parse syntax/parse/experimental/eh)

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
  )
(require 'eh-alts)


;; Splicing
(module splicing racket
  (require rackunit "setup.rkt" syntax/parse syntax/parse/experimental/splicing)

  (define-primitive-splicing-syntax-class (foo)
    #:attributes (z x y)
    #:description "foo"
    (lambda (stx fail)
      (syntax-case stx ()
        [(a b c . rest)
         (list 3 #'a #'b #'c)]
        [_ (fail)])))

  (tok (1 2 3 4) (f:foo 4)
       (and (s= f.z 1) (s= f.x 2) (s= f.y 3)))

  (terx (1) (f:foo)
        #rx"expected foo")
  )
(require 'splicing)


;; Specialization
(module specialize racket
  (require rackunit "setup.rkt" syntax/parse syntax/parse/experimental/specialize
           (submod ".." common))

  (define-syntax-class/specialize nat>10 (nat> 10))

  (tok (11 23 45) (n:nat>10 ...))
  (terx (11 10 9) (n:nat>10 ...)
        #rx"expected natural number greater than 10")

  (tcerr "specialize preserves #:no-delimit-cut"
         (let ()
           (define-syntax-class a #:no-delimit-cut (pattern _))
           (define-syntax-class/specialize b a)
           (syntax-parse #'12 [(~not x:b) (void)]))
         #rx"syntax class with #:no-delimit-cut option not allowed within ~not pattern")

  (test-case "specialize preserves lack of #:no-delimit-cut"
    (let ()
      (define-syntax-class a (pattern _:id))
      (define-syntax-class/specialize b a)
      (syntax-parse #'12 [(~not x:b) (void)])))
  )
(require 'specialize)

#lang racket/base
(require syntax/parse
         syntax/parse/debug
         rackunit
         "setup.rkt")

(define-literal-set lits0 #:phase 0
  (define lambda))

(tcerr "litset unbound"
       (let ()
         (define-literal-set lits #:phase 0
           (none-such))
         (void)))

(tcerr "litset unbound, phase"
       (let ()
         (define-literal-set lits #:for-template
           (lambda))
         (void)))

(tcerr "litset ok, use fails"
       (let ()
         (define-literal-set lits #:phase 0
           (define lambda))
         (syntax-parse #'foo #:literal-sets (lits)
           [lambda (void)])))

(define-literal-set lits #:phase 0
  (define lambda))
(require (prefix-in mz: racket/base))

(test-case "litset ok, use ok"
  (syntax-parse #'lambda #:literal-sets (lits)
    [lambda (void)]))

(test-case "litset ok, use ok prefix"
  (syntax-parse #'mz:lambda #:literal-sets (lits)
    [lambda (void)]))

(require (for-meta 2 (only-in '#%kernel #%app)))
(define-literal-set litsk #:phase 2
  (#%app))

(test-case "litset, phase"
  (syntax-parse #'#%plain-app #:literal-sets (litsk)
    [#%app (void)]))

(tcerr "litset, phase fail"
  (syntax-parse #'#%app #:literal-sets (litsk)
    [#%app (void)]))

;; ----


(tcerr "litset, #:at"
       (let ()
         (define-literal-set lits #:phase 0
           (define lambda))
         (define-syntax-rule (getvar var stx)
           (syntax-parse stx #:literal-sets ([lits #:at here])
             [(lambda var _) #'var]))
         ;; check that introduced lambda is a literal:
         (check-exn exn:fail? (lambda () (getvar x #'(a b c))))
         (check-equal? (syntax->datum (getvar x #'(lambda b c)))
                       '(b))
         ;; check that passed lambda is not a literal, but a pattern variable:
         (check-equal? (syntax->datum (getvar lambda #'(lambda b c))))))

;; Litset extension

(tcerr "litset ext, dup 1"
       (let ()
         (define-literal-set lits1 (define))
         (define-literal-set lits2 #:literal-sets (lits1) (define))
         (void)))

(tcerr "litset ext, dup 2"
       (let ()
         (define-literal-set lits1 (define))
         (define-literal-set lits2 (define))
         (define-literal-set lits3 #:literal-sets (lits1 lits2) ())
         (void)))

(test-case "litset ext, works"
  (let ()
    (define-literal-set lits1 (define))
    (define-literal-set lits2 #:literal-sets (lits1) (lambda))
    (define (go x exp)
      (check-equal? (syntax-parse x #:literal-sets (lits2)
                      [lambda 'lambda]
                      [define 'define]
                      [_ #f])
                    exp))
    (go #'lambda 'lambda)
    (go #'define 'define)
    (go #'begin #f)
    (void)))

;; Litsets with datum-lits

(test-case "litset, datum-lits"
  (let ([one 1])
    (define-literal-set lits-d #:datum-literals (one two) ())
    (syntax-parse #'one #:literal-sets (lits-d)
                  [one (void)])
    (let ([one 2])
      (syntax-parse #'one #:literal-sets (lits-d) [one (void)]))))

;; literal-set->predicate

(require (for-label '#%kernel))

(test-case "litset->pred"
  (let ([kernel? (literal-set->predicate kernel-literals)])
    (check-equal? (kernel? #'#%plain-lambda) #t)
    (check-equal? (kernel? #'define-values) #t)
    (check-equal? (kernel? #'define-values #f) #t)
    (check-equal? (kernel? #'define-values 4) #f)
    (check-equal? (kernel? #'foo) #f)
    (void)))

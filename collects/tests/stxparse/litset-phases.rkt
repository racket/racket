#lang scheme
(require syntax/parse
         syntax/parse/debug
         rackunit
         "setup.rkt")
(require (for-syntax syntax/parse))

#|
(module a racket
  (require syntax/parse)
  (define-literal-set lits (begin))
  (provide lits))
(module b racket
  (require (for-syntax 'a syntax/parse))
  (require (for-syntax syntax/parse/private/runtime))
  (define-syntax (snarf stx)
    ;;(printf "slpl of snarf: ~s\n" (syntax-local-phase-level))
    (syntax-parse stx
      #:literal-sets (lits)
      [(snarf (begin e)) #'e]))
  (provide snarf))
(module c racket
  (require (for-syntax 'b racket/base))
  (begin-for-syntax
    (displayln (snarf (begin 5)))))
|#

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

#lang scheme/unit

(require (rename-in "../utils/utils.ss" [infer r:infer])
         "signatures.ss"
         stxclass scheme/match mzlib/trace
         (for-syntax stxclass)
         (types utils)
         (rep type-rep filter-rep object-rep)
         (for-template 
          (only-in '#%kernel [apply k:apply])
          "internal-forms.ss" scheme/base 
          (only-in scheme/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^ tc-dots^ tc-let^)
(export tc-app^)

;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    #:literals (#%plain-app #%plain-lambda letrec-values
                values apply k:apply not list list* call-with-values do-make-object make-object cons
                       andmap ormap)
    [(#%plain-app values arg) (single-value #'arg expected)]
    [(#%plain-app values . args)
     (match expected
       [(tc-results: ets efs eos)
        (match-let ([(list (tc-result1: ts fs os) ...) 
                     (for/list ([arg (syntax->list #'args)]
                                [et ets] [ef efs] [eo eos])
                       (single-value arg (ret et ef eo)))])
          (if (= (length ts) (length ets) (length (syntax->list #'args)))
              (ret ts fs os)
              (tc-error/expr #:return expected "wrong number of values: expected ~a but got ~a"
                             (length ets) (length (syntax->list #'args)))))]
       [_ (match-let ([(list (tc-result1: ts fs os) ...) 
                       (for/list ([arg (syntax->list #'args)])
                         (single-value arg))])
            (ret ts fs os))])]))

;(trace tc/app/internal)

;; syntax -> tc-results
(define (tc/app form) (tc/app/internal form #f))  
  
;; syntax tc-results? -> tc-results?
(define (tc/app/check form expected)
    (define t (tc/app/internal form expected))
    (check-below t expected)
    expected)

;; syntax? syntax? tc-result? (listof tc-results?) (or/c #f tc-results) -> tc-results?
(define (tc/funapp f-stx args-stx ftype0 argtys expected)
  (error "tc/funapp NYI"))

#lang scheme/base

(require syntax/parse
         scheme/list scheme/dict
         "../utils/utils.rkt"
         "../utils/tc-utils.rkt"
         (for-template scheme/base)
         (types abbrev)
         (optimizer utils inexact-complex))

(provide unboxed-let-opt-expr)

;; possibly replace bindings of complex numbers by bindings of their 2 components
;; useful for intermediate results used more than once and for loop variables

(define-syntax-class unboxed-let-opt-expr
  #:literal-sets (kernel-literals)
  (pattern (~and exp (let-values (clause:expr ...) body:expr ...))
           ;; we look for bindings of complexes that are not mutated and only
           ;; used in positions where we would unbox them
           ;; these are candidates for unboxing
           #:with ((candidates ...) (others ...))
           (let-values
               (((candidates others)
                 ;; clauses of form ((v) rhs), currently only supports 1 lhs var
                 (partition (lambda (p)
                              (and (isoftype? (cadr p) -InexactComplex)
                                   (let ((v (car (syntax-e (car p)))))
                                     (not (is-var-mutated? v))
                                     (could-be-unboxed-in? v #'(begin body ...)))))
                            (map syntax->list (syntax->list #'(clause ...))))))
             (list candidates others))
           #:with (opt-candidates:unboxed-let-clause ...) #'(candidates ...)
           #:with (opt-others:let-clause ...) #'(others ...)
           #:with opt
           (begin (log-optimization "unboxed let bindings" #'exp)
                  ;; add the unboxed bindings to the table, for them to be used by
                  ;; further optimizations
                  (for ((v (in-list (syntax->list #'(opt-candidates.id ...))))
                        (r (in-list (syntax->list #'(opt-candidates.real-part ...))))
                        (i (in-list (syntax->list #'(opt-candidates.imag-part ...)))))
                       (dict-set! unboxed-vars-table v (list r i)))
                  #`(let* (opt-candidates.bindings ... ... opt-others.res ...)
                      #,@(map (optimize) (syntax->list #'(body ...)))))))

;; if a variable is only used in complex arithmetic operations, it's safe
;; to unbox it
(define (could-be-unboxed-in? v exp)

  (define (direct-child-of? exp)
    (ormap (lambda (x) (and (identifier? x) (free-identifier=? x v)))
           (syntax->list exp)))

  ;; if v is a direct child of exp, that means it's used in a boxed
  ;; fashion, and is not safe to unboxed
  ;; if not, recur on the subforms
  (define (look-at exp)
    (and (not (direct-child-of? exp))
         (andmap rec (syntax->list exp))))
  
  (define (rec exp)
    (syntax-parse exp
      #:literal-sets (kernel-literals)
      
      ;; used within a complex arithmetic expression? safe to unbox
      [exp:inexact-complex-arith-opt-expr
       (direct-child-of? #'exp)]
      
      ;; recur down
      [((~and op (~or (~literal #%plain-lambda) (~literal define-values)))
        formals e:expr ...)
       (look-at #'(e ...))]
      [(case-lambda [formals e:expr ...] ...)
       (look-at #'(e ... ...))]
      [((~or (~literal let-values) (~literal letrec-values))
        ([ids e-rhs:expr] ...) e-body:expr ...)
       (look-at #'(e-rhs ... e-body ...))]
      [(letrec-syntaxes+values stx-bindings
                               ([(ids ...) e-rhs:expr] ...)
                               e-body:expr ...)
       (look-at #'(e-rhs ... e-body ...))]
      [(kw:identifier expr ...)
       #:when (ormap (lambda (k) (free-identifier=? k #'kw))
                     (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app #'#%expression
                           #'#%variable-reference #'with-continuation-mark))
       (look-at #'(expr ...))]
            
      ;; not used, safe to unbox
      [_ #t]))
  (rec exp))

(define-syntax-class unboxed-let-clause
  (pattern ((v:id) rhs:unboxed-inexact-complex-opt-expr)
           #:with id #'v
           #:with real-part #'rhs.real-part
           #:with imag-part #'rhs.imag-part
           #:with (bindings ...) #'(rhs.bindings ...)))
(define-syntax-class let-clause ; to turn let-values clauses into let clauses
  (pattern ((v:id) rhs:expr)
           #:with res #`(v #,((optimize) #'rhs))))

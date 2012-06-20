#lang racket/base

(require syntax/parse unstable/syntax
         racket/match
         (for-template racket/base racket/unsafe/ops racket/list)
         "../utils/utils.rkt"
         (utils tc-utils)
         (rep type-rep)
         (types type-table utils)
         (typecheck typechecker)
         (optimizer utils logging))

(provide pair-opt-expr)


(define-syntax-class pair-op
  #:commit
  (pattern (~literal car) #:with unsafe #'unsafe-car)
  (pattern (~literal cdr) #:with unsafe #'unsafe-cdr))
(define-syntax-class mpair-op
  #:commit
  (pattern (~literal mcar) #:with unsafe #'unsafe-mcar)
  (pattern (~literal mcdr) #:with unsafe #'unsafe-mcdr)
  (pattern (~literal set-mcar!) #:with unsafe #'unsafe-set-mcar!)
  (pattern (~literal set-mcdr!) #:with unsafe #'unsafe-set-mcdr!))


(define (has-pair-type? e)
  (match (type-of e) ; type of the operand
    [(tc-result1: (Pair: _ _)) #t]
    [_ #f]))
(define (has-mpair-type? e)
  (match (type-of e) ; type of the operand
    [(tc-result1: (MPair: _ _)) #t]
    [_ #f]))

(define (log-pair-missed-opt stx irritant)
  (log-missed-optimization
   "car/cdr on a potentially empty list"
   "According to its type, the highlighted list could be empty. Access to it cannot be safely optimized. To fix this, restrict the type to non-empty lists, maybe by wrapping this expression in a check for non-emptiness."
   stx irritant))

(define pair-opt-msg "Pair check elimination.")

(define (log-pair-opt stx)
  (log-optimization "pair" pair-opt-msg stx))

(define-syntax-class pair-opt-expr
  #:commit
  (pattern e:pair-derived-opt-expr
           ;; no logging here, redundant with actual pair opt
           #:with opt #'e.opt)
  (pattern (#%plain-app op:pair-op p:expr)
           #:when (or (has-pair-type? #'p)
                      ;; in this case, we have a potentially empty list, but
                      ;; it has to be a list, otherwise, there would have been
                      ;; a type error
                      (begin (log-pair-missed-opt this-syntax #'p) #f))
           #:with opt
           (begin (log-pair-opt this-syntax)
                  (add-disappeared-use #'op)
                  #`(op.unsafe #,((optimize) #'p))))
  (pattern (#%plain-app op:mpair-op p:expr e:expr ...)
           #:when (or (has-mpair-type? #'p)
                      (begin (log-pair-missed-opt this-syntax #'p) #f))
           #:with opt
           (begin (log-pair-opt this-syntax)
                  (add-disappeared-use #'op)
                  #`(op.unsafe #,@(syntax-map (optimize) #'(p e ...))))))


;; change the source location of a given syntax object
(define (relocate stx loc-stx)
  (datum->syntax stx (syntax->datum stx) loc-stx stx stx))

;; if the equivalent sequence of cars and cdrs is guaranteed not to fail,
;; we can optimize

;; accessors is a list of syntax objects, all #'car or #'cdr
(define (gen-alt accessors stx)
  (syntax-parse stx
    [(#%plain-app op arg)
     (define (gen-alt-helper accessors)
       (if (null? accessors)
           #'arg
           (quasisyntax/loc stx
             (#%plain-app #,(relocate (car accessors) stx)
                          #,(gen-alt-helper (cdr accessors))))))
     (let ((ty  (type-of stx))
           (obj (gen-alt-helper accessors)))
       ;; we're calling the typechecker, but this is just a shortcut, we're
       ;; still conceptually single pass (we're not iterating). we could get
       ;; the same result by statically destructing the types.
       (tc-expr/check obj ty)
       obj)]))

(define-syntax-rule (gen-pair-derived-expr name (orig seq ...) ...)
  (define-syntax-class name
    #:commit
    (pattern (#%plain-app (~literal orig) x)
             #:with alt (gen-alt (list seq ...) this-syntax))
    ...))
(gen-pair-derived-expr pair-derived-expr
 (caar #'car #'car)
 (cadr #'car #'cdr)
 (cdar #'cdr #'car)
 (cddr #'cdr #'cdr)
 (caaar #'car #'car #'car)
 (caadr #'car #'car #'cdr)
 (cadar #'car #'cdr #'car)
 (caddr #'car #'cdr #'cdr)
 (cdaar #'cdr #'car #'car)
 (cdadr #'cdr #'car #'cdr)
 (cddar #'cdr #'cdr #'car)
 (cdddr #'cdr #'cdr #'cdr)
 (caaaar #'car #'car #'car #'car)
 (caaadr #'car #'car #'car #'cdr)
 (caadar #'car #'car #'cdr #'car)
 (caaddr #'car #'car #'cdr #'cdr)
 (cadaar #'car #'cdr #'car #'car)
 (cadadr #'car #'cdr #'car #'cdr)
 (caddar #'car #'cdr #'cdr #'car)
 (cadddr #'car #'cdr #'cdr #'cdr)
 (cdaaar #'cdr #'car #'car #'car)
 (cdaadr #'cdr #'car #'car #'cdr)
 (cdadar #'cdr #'car #'cdr #'car)
 (cdaddr #'cdr #'car #'cdr #'cdr)
 (cddaar #'cdr #'cdr #'car #'car)
 (cddadr #'cdr #'cdr #'car #'cdr)
 (cdddar #'cdr #'cdr #'cdr #'car)
 (cddddr #'cdr #'cdr #'cdr #'cdr)
 (first   #'car)
 (rest    #'cdr)
 (second  #'car #'cdr)
 (third   #'car #'cdr #'cdr)
 (fourth  #'car #'cdr #'cdr #'cdr)
 (fifth   #'car #'cdr #'cdr #'cdr #'cdr)
 (sixth   #'car #'cdr #'cdr #'cdr #'cdr #'cdr)
 (seventh #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr)
 (eighth  #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr)
 (ninth   #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr)
 (tenth   #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr))

(define-syntax-class pair-derived-opt-expr
  #:commit
  (pattern e:pair-derived-expr
           #:with e*:pair-opt-expr #'e.alt
           #:with opt #'e*.opt))

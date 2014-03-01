#lang racket/base

(require syntax/parse syntax/stx
         racket/match
         (for-template racket/base racket/unsafe/ops racket/list)
         (for-syntax racket/base syntax/parse racket/syntax)
         "../utils/utils.rkt"
         (utils tc-utils)
         (rep type-rep)
         (types type-table utils)
         (typecheck typechecker)
         (optimizer utils logging))

(provide pair-opt-expr)

(define-unsafe-syntax-class car)
(define-unsafe-syntax-class cdr)
(define-unsafe-syntax-class mcar)
(define-unsafe-syntax-class mcdr)
(define-unsafe-syntax-class set-mcar!)
(define-unsafe-syntax-class set-mcdr!)


(define-merged-syntax-class pair-op (car^ cdr^))
(define-merged-syntax-class mpair-op (mcar^ mcdr^ set-mcar!^ set-mcdr!^))


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

(define-syntax-rule (log-pair-opt)
  (log-opt "pair" "Pair check elimination."))

(define-syntax-class pair-opt-expr
  #:commit
  #:attributes (opt)
  #:literal-sets (kernel-literals)

  ;; no logging here, redundant with actual pair opt
  (pattern :pair-derived-opt-expr)
  (pattern (#%plain-app op:pair-op p:opt-expr)
    #:when (or (has-pair-type? #'p)
               ;; in this case, we have a potentially empty list, but
               ;; it has to be a list, otherwise, there would have been
               ;; a type error
               (begin (log-pair-missed-opt this-syntax #'p) #f))
    #:do [(log-pair-opt)]
    #:with opt #'(op.unsafe p.opt))
  (pattern (#%plain-app op:mpair-op p:opt-expr e:opt-expr ...)
    #:when (or (has-mpair-type? #'p)
               (begin (log-pair-missed-opt this-syntax #'p) #f))
    #:do [(log-pair-opt)]
    #:with opt #'(op.unsafe p.opt e.opt ...)))


;; change the source location of a given syntax object
(define (relocate stx loc-stx)
  (datum->syntax stx (syntax->datum stx) loc-stx stx stx))

;; if the equivalent sequence of cars and cdrs is guaranteed not to fail,
;; we can optimize

;; accessors is a list of syntax objects, all #'car or #'cdr
(define (gen-alt accessors op arg stx)
  (define (gen-alt-helper accessors)
    (for/fold [(accum arg)] [(acc (reverse accessors))]
      (quasisyntax/loc stx (#%plain-app #,(relocate acc op) #,accum))))
  (let ((ty  (type-of stx))
        (obj (gen-alt-helper accessors)))
    ;; we're calling the typechecker, but this is just a shortcut, we're
    ;; still conceptually single pass (we're not iterating). we could get
    ;; the same result by statically destructing the types.
    (tc-expr/check obj ty)
    obj))

(define-syntax gen-pair-derived-expr
  (syntax-parser
    [(_ name:id (orig:id seq ...) ...)
     (define/with-syntax (syntax-class-name ...) (generate-temporaries #'(orig ...)))
     (define/with-syntax (lit-class-name ...) (generate-temporaries #'(orig ...)))
     #'(begin
         (begin
           (define-literal-syntax-class lit-class-name (orig))
           (define-syntax-class syntax-class-name
             #:commit
             (pattern (#%plain-app (~var op lit-class-name) arg)
               #:with alt (gen-alt (list seq ...) #'op #'arg this-syntax)))) ...
         (define-merged-syntax-class name (syntax-class-name ...)))]))

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

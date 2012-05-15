#lang racket/base
(require "prop.rkt"
         "misc.rkt"
         "blame.rkt"
         racket/stxparam)
(require (for-syntax racket/base)
         (for-syntax "opt-guts.rkt")
         (for-syntax racket/stxparam))

(provide opt/c define-opt/c define/opter
         opt/direct
         begin-lifted
         (for-syntax
          define-opt/recursive-fn?
          define-opt/recursive-fn-neg-blame?-id))

(define-syntax (define/opter stx)
  (syntax-case stx ()
    [(_ (for opt/i opt/info stx) expr ...)
     (if (identifier? #'for)
         #'(begin
             (begin-for-syntax
               (reg-opter!
                #'for
                (λ (opt/i opt/info stx)
                  expr ...)))
             (void))
         (error 'define/opter "expected opter name to be an identifier, got ~.s" (syntax-e #'for)))]))

;;
;; opt/recursive-call
;;
(define-for-syntax (opt/recursive-call opt/info stx)
  (build-optres
   #:exp (with-syntax ((stx stx)
                       (val (opt/info-val opt/info))
                       (blame (opt/info-blame opt/info)))
           (syntax (let ((ctc stx))
                     (((contract-projection ctc) blame) val))))
   #:lifts null
   #:superlifts null
   #:partials null
   #:flat #f
   #:opt #f
   #:stronger-ribs null
   #:chaperone null))

;; make-stronger : list-of-(union syntax #f) -> syntax
(define-for-syntax (make-stronger strongers)
  (let ((filtered (filter (λ (x) (not (eq? x #f))) strongers)))
    (if (null? filtered)
        #t
        (with-syntax (((stronger ...) strongers))
          (syntax (and stronger ...))))))

(define-for-syntax (coerecable-constant? konst)
  (syntax-case konst (quote)
    ['x
     (identifier? #'x)
     #t]
    [other
     (let ([o (syntax-e #'other)])
       (or (boolean? o)
           (char? o)
           (null? o)
           (string? o)
           (bytes? o)
           (number? o)))]))

(define-for-syntax (opt-constant-contract konst opt/info)
  (define v (opt/info-val opt/info))
  (define-values (predicate word)
    (cond
      [(and (pair? konst) (eq? (car konst) 'quote))
       (values #`(eq? #,konst #,v)
               "eq?")]
      [(or (boolean? konst) (char? konst) (null? konst))
       (values #`(eq? #,konst #,v)
               "eq?")]
      [(or (string? konst) (bytes? konst))
       (values #`(equal? #,konst #,v)
               "equal?")]
      [(number? konst)
       (values #`(and (number? #,v) (= #,konst #,v))
               "=")]))
  (build-optres
   #:exp
   #`(if #,predicate
         #,v
         (opt-constant-contract-failure #,(opt/info-blame opt/info) #,v #,word #,konst))
   #:lifts null
   #:superlifts null
   #:partials null
   #:flat predicate
   #:opt #f
   #:stronger-ribs null
   #:chaperone #t))

(define (opt-constant-contract-failure blame val compare should-be)
  (raise-blame-error blame val '(expected "a value ~a to ~e") compare should-be))

(begin-for-syntax
  (define-struct define-opt/recursive-fn (transformer internal-fn neg-blame?-id)
    #:property prop:procedure 0))

;; opt/i : id opt/info syntax ->
;;         syntax syntax-list syntax-list (union syntax #f) (union syntax #f)
(define-for-syntax (opt/i opt/info stx)
  ;; the case dispatch here must match what top-level-unknown? is doing
  (syntax-case stx ()
    [(ctc arg ...)
     (and (identifier? #'ctc) (opter #'ctc))
     ((opter #'ctc) opt/i opt/info stx)]
    [argless-ctc
     (and (identifier? #'argless-ctc) (opter #'argless-ctc))
     ((opter #'argless-ctc) opt/i opt/info stx)]
    [(f arg ...)
     (and (identifier? #'f) 
          (define-opt/recursive-fn? (syntax-local-value #'f (λ () #f))))
     (let ([d-o/r-f (syntax-local-value #'f)])
       (build-optres
        #:exp
        #`(#,(define-opt/recursive-fn-internal-fn (syntax-local-value #'f))
           #,(opt/info-contract opt/info)
           #,(opt/info-blame opt/info)
           #,(opt/info-val opt/info)
           arg ...)
        #:lifts null
        #:superlifts null
        #:partials null
        #:flat #f
        #:opt #f
        #:stronger-ribs null
        #:chaperone #t
        #:no-negative-blame? 
        (let ([bx (syntax-local-value (define-opt/recursive-fn-neg-blame?-id d-o/r-f)
                                      (λ () #f))])
          (and (box? bx)
               (cond
                 [(eq? 'unknown (unbox bx)) (list #'f)]
                 [else (unbox bx)])))))]
    [konst
     (coerecable-constant? #'konst)
     (opt-constant-contract (syntax->datum #'konst) opt/info)]
    [else
     (opt/unknown opt/i opt/info stx)]))

;; top-level-unknown? : syntax -> boolean
;; this must match what opt/i is doing
(define-for-syntax (top-level-unknown? stx)
  (syntax-case stx ()
    [(ctc arg ...)
     (and (identifier? #'ctc) (opter #'ctc))
     #f]
    [argless-ctc
     (and (identifier? #'argless-ctc) (opter #'argless-ctc))
     #f]
    [konst
     (coerecable-constant? #'konst)
     #f]
    [(f arg ...)
     (and (identifier? #'f) 
          (define-opt/recursive-fn? (syntax-local-value #'f (λ () #f))))
     #f]
    [else
     #t]))

;; opt/c : syntax -> syntax
;; opt/c is an optimization routine that takes in an sexp containing
;; contract combinators and attempts to "unroll" those combinators to save
;; on things such as closure allocation time.
(define-syntax (opt/c stx)  
  (syntax-case stx ()
    [(_ e)
     (let ()
       (define info (make-opt/info #'ctc #'val #'blame #f '() #f #f #'this #'that))
       (define an-optres (opt/i info #'e))
       (bind-superlifts
        (optres-superlifts an-optres)
        (bind-lifts
         (optres-lifts an-optres)
         #`(make-opt-contract
            (λ (ctc)
              (λ (blame)
                #,(bind-superlifts
                   (optres-partials an-optres)
                   #`(λ (val) #,(optres-exp an-optres)))))
            (λ () e)
            (λ (this that) #f)
            (vector)
            (begin-lifted (box #f))
            #,(optres-chaperone an-optres)))))]))

;; this macro optimizes 'e' as a contract,
;; using otherwise-id if it does not recognize 'e'.
(define-syntax (opt/direct stx)
  (syntax-case stx ()
    [(_ e val-e blame-e otherwise-id)
     (identifier? #'otherwise-id)
     (cond
       [(top-level-unknown? #'e) #'(otherwise-id e val-e blame-e)]
       [else
        (define info (make-opt/info #'ctc #'val #'blame #f '() #f #f #'this #'that))
        (define an-optres (opt/i info #'e))
        #`(let ([ctc e] ;;; hm... what to do about this?!
                [val val-e]
                [blame blame-e])
            #,(bind-superlifts
               (optres-superlifts an-optres)
               (bind-lifts
                (optres-lifts an-optres)
                (bind-superlifts
                 (optres-partials an-optres)
                 (optres-exp an-optres)))))])]))

(define-syntax (begin-lifted stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-local-lift-expression #'expr)]))

(define-syntax (define-opt/c stx)
  (syntax-case stx ()
    [(_ (id args ...) e)
     (with-syntax ([(f1 f2 no-neg-blame?)
                    (generate-temporaries (list (format "~a-external" (syntax-e #'id))
                                                (format "~a-internal" (syntax-e #'id))
                                                (format "~a-no-neg-blame?" (syntax-e #'id))))])
       #`(begin
           (define-syntax id
             (define-opt/recursive-fn
               (λ (stx)
                 (syntax-case stx ()
                   [f
                    (identifier? #'f)
                    #'f1]
                   [(f . call-args)
                    (with-syntax ([app (datum->syntax stx '#%app)])
                      #'(app f1 . call-args))]))
               #'f2
               #'no-neg-blame?))
           (define-syntax no-neg-blame? (box 'unknown))
           (define-values (f1 f2) (opt/c-helper f1 f2 no-neg-blame? (id args ...) e))))]))

(define-syntax (opt/c-helper stx)
  (syntax-case stx ()
    [(_ f1 f2 no-neg-blame? (id args ...) e)
     (let ()
       (define info (make-opt/info #'ctc #'val #'blame #f 
                                   (syntax->list #'(args ...)) 
                                   #f #f #'this #'that))
       ;; it seems like this syntax-local-value can fail when expand-once
       ;; is called, but otherwise I think it shouldn't fail
       (define bx (syntax-local-value #'no-neg-blame? (λ () #f)))
       (define an-optres (opt/i info #'e))
       (when bx (set-box! bx (optres-no-negative-blame? an-optres)))
       #`(let ()
           (define (f2 ctc blame val args ...)
             #,(bind-superlifts
                (optres-superlifts an-optres)
                (bind-lifts
                 (optres-lifts an-optres)
                 (bind-superlifts
                  (optres-partials an-optres)
                  (optres-exp an-optres)))))
           (define (f1 args ...)
             #,(bind-superlifts
                (optres-superlifts an-optres)
                (bind-lifts
                 (optres-lifts an-optres)
                 #`(make-opt-contract
                    (λ (ctc)
                      (λ (blame)
                        (λ (val) 
                          (f2 ctc blame val args ...))))
                    (λ () e)
                    (λ (this that) #f)
                    (vector)
                    (begin-lifted (box #f))
                    #,(optres-chaperone an-optres)))))
           (values f1 f2)))]))

;; optimized contracts
;;
;; getting the name of an optimized contract is slow, but it is only
;; called when blame is raised (thankfully).
;;
;; note that lifts, partials, flat, and unknown are all built into the
;; projection itself and should not be exposed to the outside anyhow.
(define-values (orig-ctc-prop orig-ctc-pred? orig-ctc-get)
  (make-struct-type-property 'original-contract))

;; the stronger-vars don't seem to be used anymore for stronger; probably
;; they should be folded into the lifts and then there should be a separate
;; setup for consolidating stronger checks
(define-struct opt-contract (proj orig-ctc stronger stronger-vars stamp chaperone?)
  #:property orig-ctc-prop (λ (ctc) ((opt-contract-orig-ctc ctc)))
  #:property prop:opt-chaperone-contract (λ (ctc) (opt-contract-chaperone? ctc))
  #:property prop:contract
  (build-contract-property
   #:projection (λ (ctc) ((opt-contract-proj ctc) ctc))
   #:name (λ (ctc) (contract-name ((orig-ctc-get ctc) ctc)))
   #:stronger
   (λ (this that)
      (and (opt-contract? that)
           (eq? (opt-contract-stamp this) (opt-contract-stamp that))
           ((opt-contract-stronger this) this that)))))

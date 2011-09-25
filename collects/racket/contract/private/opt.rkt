#lang racket/base
(require "prop.rkt"
         "misc.rkt"
         racket/stxparam)
(require (for-syntax racket/base)
         (for-syntax "opt-guts.rkt")
         (for-syntax racket/stxparam))

(provide opt/c define-opt/c define/opter opt-stronger-vars-ref
         opt/direct
         begin-lifted)

;; define/opter : id -> syntax
;;
;; Takes an expression which is to be expected of the following signature: 
;;
;; opter : id id syntax list-of-ids ->
;;         syntax syntax-list syntax-list syntax-list (union syntax #f) (union syntax #f) syntax
;;         
;;
;; It takes in an identifier for pos, neg, and the original syntax. An identifier
;; that can be used to call the opt/i function is also implicitly passed into
;; every opter. A list of free-variables is implicitly passed if the calling context
;; was define/osc otherwise it is null.
;;
;; Every opter needs to return:
;;  - the optimized syntax
;;  - lifted variables: a list of (id, sexp) pairs
;;  - super-lifted variables: functions or the such defined at the toplevel of the
;;                            calling context of the opt routine.
;;                            Currently this is only used for struct contracts.
;;  - partially applied contracts: a list of (id, sexp) pairs
;;  - if the contract being optimized is flat,
;;    then an sexp that evals to bool,
;;    else #f
;;    This is used in conjunction with optimizing flat contracts into one boolean
;;    expression when optimizing or/c.
;;  - if the contract can be optimized,
;;    then #f (that is, it is not unknown)
;;    else the symbol of the lifted variable
;;    This is used for contracts with subcontracts (like cons) doing checks.
;;  - a list of stronger-ribs
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
;; BUG: currently does not try to optimize the arguments, this requires changing
;;      every opter to keep track of bound variables.
;;
(define-for-syntax (opt/recursive-call opt/info stx)
  (values
   (with-syntax ((stx stx)
                 (val (opt/info-val opt/info))
                 (blame (opt/info-blame opt/info)))
     (syntax (let ((ctc stx))
               (((contract-projection ctc) blame) val))))
   null
   null
   null
   #f
   #f
   null
   null))

;; make-stronger : list-of-(union syntax #f) -> syntax
(define-for-syntax (make-stronger strongers)
  (let ((filtered (filter (λ (x) (not (eq? x #f))) strongers)))
    (if (null? filtered)
        #t
        (with-syntax (((stronger ...) strongers))
          (syntax (and stronger ...))))))

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
          (syntax-parameter-value #'define/opt-recursive-fn)
          (free-identifier=? (syntax-parameter-value #'define/opt-recursive-fn)
                             #'f))
     (values
      #`(#,(syntax-parameter-value #'define/opt-recursive-fn) #,(opt/info-val opt/info) arg ...)
      null
      null
      null
      #f
      #f
      null
      #t)]
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
    [(f arg ...)
     (and (identifier? #'f) 
          (syntax-parameter-value #'define/opt-recursive-fn)
          (free-identifier=? (syntax-parameter-value #'define/opt-recursive-fn)
                             #'f))
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
     (if (top-level-unknown? #'e)
         #'e
         #'(opt/c e ()))]
    [(_ e (opt-recursive-args ...))
     (let*-values ([(info) (make-opt/info #'ctc
                                          #'val
                                          #'blame
                                          #f
                                          (syntax->list #'(opt-recursive-args ...))
                                          #f
                                          #f
                                          #'this
                                          #'that)]
                   [(next lifts superlifts partials _ __ stronger-ribs chaperone?) (opt/i info #'e)])
       (with-syntax ([next next])
         (bind-superlifts
          superlifts
          (bind-lifts
           lifts
           #`(make-opt-contract
              (λ (ctc)
                (λ (blame)
                  #,(if (syntax-parameter-value #'define/opt-recursive-fn)
                        (with-syntax ([f (syntax-parameter-value #'define/opt-recursive-fn)])
                          (bind-superlifts
                           (cons
                            (cons (syntax-parameter-value #'define/opt-recursive-fn)
                                  #'(λ (val opt-recursive-args ...) next))
                            partials)
                           #'(λ (val) 
                               (f val opt-recursive-args ...))))
                        (bind-superlifts
                         partials
                         #`(λ (val) next)))))
              (λ () e)
              (λ (this that) #f)
              (vector)
              (begin-lifted (box #f)))))))]))

;; this macro optimizes 'e' as a contract,
;; using otherwise-id if it does not recognize 'e'.
(define-syntax (opt/direct stx)
  (syntax-case stx ()
    [(_ e val-e blame-e otherwise-id)
     (identifier? #'otherwise-id)
     (if (top-level-unknown? #'e)
         #'(otherwise-id e val-e blame-e)
         (let*-values ([(info) (make-opt/info #'ctc
                                              #'val
                                              #'blame
                                              #f
                                              '()
                                              #f
                                              #f
                                              #'this
                                              #'that)]
                       [(next lifts superlifts partials _ __ stronger-ribs) (opt/i info #'e)])
           #`(let ([ctc e] ;;; hm... what to do about this?!
                   [val val-e]
                   [blame blame-e])
               #,(bind-superlifts
                  superlifts
                  (bind-lifts
                   lifts
                   (bind-superlifts
                    partials
                    next))))))]))

(define-syntax (begin-lifted stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-local-lift-expression #'expr)]))

(define-syntax-parameter define/opt-recursive-fn #f)

(define-syntax (define-opt/c stx)
  (syntax-case stx ()
    [(_ (id args ...) body)
     #'(define (id args ...)
         (syntax-parameterize ([define/opt-recursive-fn #'id])
                              (opt/c body (args ...))))]))

;; optimized contracts
;;
;; getting the name of an optimized contract is slow, but it is only
;; called when blame is raised (thankfully).
;;
;; note that lifts, partials, flat, and unknown are all built into the
;; projection itself and should not be exposed to the outside anyhow.
(define-values (orig-ctc-prop orig-ctc-pred? orig-ctc-get)
  (make-struct-type-property 'original-contract))

(define-struct opt-contract (proj orig-ctc stronger stronger-vars stamp)
  #:property orig-ctc-prop (λ (ctc) ((opt-contract-orig-ctc ctc)))
  #:property prop:contract
  (build-contract-property
   #:projection (λ (ctc) ((opt-contract-proj ctc) ctc))
   ;; I think provide/contract and contract calls this, so we are in effect allocating
   ;; the original once
   #:name (λ (ctc) (contract-name ((orig-ctc-get ctc) ctc)))
   #:stronger
   (λ (this that)
      (and (opt-contract? that)
           (eq? (opt-contract-stamp this) (opt-contract-stamp that))
           ((opt-contract-stronger this) this that)))))

;; opt-stronger-vars-ref : int opt-contract -> any
(define (opt-stronger-vars-ref i ctc)
  (let ((v (opt-contract-stronger-vars ctc)))
    (vector-ref v i)))

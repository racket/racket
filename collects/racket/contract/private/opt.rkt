#lang racket/base
(require "prop.rkt"
         "misc.rkt"
         "blame.rkt"
         racket/stxparam)
(require (for-syntax racket/base)
         (for-syntax "opt-guts.rkt")
         (for-syntax racket/stxparam))

(provide opt/c define-opt/c define/opter opt-stronger-vars-ref
         opt/direct
         begin-lifted)

;; (define/opter (<contract-combinator> opt/i opt/info stx) body)
;;
;; An opter is to a function with the following signature: 
;;
;; opter : (syntax opt/info -> <opter-results>) opt/info list-of-ids ->
;;         (values syntax syntax-list syntax-list
;;                 syntax-list (union syntax #f) (union syntax #f) syntax)
;;
;; The first argument can be used to recursively process sub-contracts
;; It returns what an opter returns and its results should be accumulated
;; into the opter's results.
;;
;; The opt/info struct has a number of identifiers that get used to build
;; contracts; see opt-guts.rkt for the selectors.
;;
;; The last argument is a list of free-variables if the calling context
;; was define/opt otherwise it is null.
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
;;  - a boolean or a syntax object; if it is a boolean,
;;    the boolean indicaties if this contract is a chaperone contract
;;    if it is a syntax object, then evaluating its contents determines
;;    if this is a chaperone contract
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
  (values
   #`(if #,predicate
         #,v
         (opt-constant-contract-failure #,(opt/info-blame opt/info) #,v #,word #,konst))
   null
   null
   null
   predicate
   #f
   null
   #t))

(define (opt-constant-contract-failure blame val compare should-be)
  (raise-blame-error blame val "expected a value ~a to ~e" compare should-be))

(begin-for-syntax
  (define-struct define-opt/recursive-fn (transformer internal-fn)
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
     (values
      #`(#,(define-opt/recursive-fn-internal-fn (syntax-local-value #'f))
         #,(opt/info-contract opt/info)
         #,(opt/info-blame opt/info)
         #,(opt/info-val opt/info)
         arg ...)
      null
      null
      null
      #f
      #f
      null
      #t)]
    [konst
     (coerecable-constant? #'konst)
     (opt-constant-contract (syntax->datum #'konst) opt/info)]
    [else
     (log-info (format "warning in ~a:~a: opt/c doesn't know the contract ~s" 
                       (syntax-source stx)
                       (if (syntax-line stx)
                           (format "~a:~a" (syntax-line stx) (syntax-column stx))
                           (format ":~a" (syntax-position stx)))
                       (syntax->datum stx)))
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
     (let*-values ([(info) (make-opt/info #'ctc
                                          #'val
                                          #'blame
                                          #f
                                          '()
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
                  #,(bind-superlifts
                     partials
                     #`(λ (val) next))))
              (λ () e)
              (λ (this that) #f)
              (vector)
              (begin-lifted (box #f))
              #,chaperone?)))))]))

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

(define-syntax (define-opt/c stx)
  (syntax-case stx ()
    [(_ (id args ...) e)
     (with-syntax ([(f1 f2)
                    (generate-temporaries (list (format "~a-f1" (syntax-e #'id))
                                                (format "~a-f2" (syntax-e #'id))))])
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
               #'f2))
           (define-values (f1 f2) (opt/c-helper f1 f2 (id args ...) e))))]))

(define-syntax (opt/c-helper stx)
  (syntax-case stx ()
    [(_ f1 f2 (id args ...) e)
     (let*-values ([(info) (make-opt/info #'ctc
                                          #'val
                                          #'blame
                                          #f
                                          (syntax->list #'(args ...))
                                          #f
                                          #f
                                          #'this
                                          #'that)]
                   [(next lifts superlifts partials _ __ stronger-ribs chaperone?) (opt/i info #'e)])
       (with-syntax ([next next])
         #`(let ()
             (define (f2 ctc blame val args ...)
               #,(bind-superlifts
                  superlifts
                  (bind-lifts
                   lifts
                   (bind-superlifts
                    partials
                    #'next))))
             (define (f1 args ...)
               #,(bind-superlifts
                  superlifts
                  (bind-lifts
                   lifts
                   #`(make-opt-contract
                      (λ (ctc)
                        (λ (blame)
                          (λ (val) 
                            (f2 ctc blame val args ...))))
                      (λ () e)
                      (λ (this that) #f)
                      (vector)
                      (begin-lifted (box #f))
                      #,chaperone?))))
             (values f1 f2))))]))

;; optimized contracts
;;
;; getting the name of an optimized contract is slow, but it is only
;; called when blame is raised (thankfully).
;;
;; note that lifts, partials, flat, and unknown are all built into the
;; projection itself and should not be exposed to the outside anyhow.
(define-values (orig-ctc-prop orig-ctc-pred? orig-ctc-get)
  (make-struct-type-property 'original-contract))

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

;; opt-stronger-vars-ref : int opt-contract -> any
(define (opt-stronger-vars-ref i ctc)
  (let ((v (opt-contract-stronger-vars ctc)))
    (vector-ref v i)))

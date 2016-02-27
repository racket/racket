#lang racket/base
(require syntax/private/boundmap ;; needs to be the private one, since the public one has contracts
         (for-template racket/base
                       "guts.rkt"
                       "blame.rkt"
                       "misc.rkt")
         (for-syntax racket/base))

(provide get-opter reg-opter! opter
         interleave-lifts
         
         build-opt/info
         opt/info-contract
         opt/info-val
         opt/info-blame
         opt/info-free-vars
         opt/info-recf
         opt/info-base-pred
         opt/info-this
         opt/info-that
         
         opt/info-swap-blame
         opt/info-add-blame-context
         opt/info-change-val
         opt/info-positive-blame
         opt/info-negative-blame
         
         opt/unknown
         opt-error-name
         
         optres-exp
         optres-lifts
         optres-superlifts
         optres-partials
         optres-flat
         optres-opt
         optres-stronger-ribs
         optres-chaperone
         optres-no-negative-blame?
         optres-name
         build-optres
         
         combine-two-chaperone?s
         combine-two-no-negative-blame
         log-unknown-contract-warning)

;; (define/opter (<contract-combinator> opt/i opt/info stx) body)
;;
;; An opter is a function with the following signature: 
;;
;; opter : (syntax opt/info -> optres) opt/info list-of-ids -> optres
;;
;; The first argument can be used to recursively process sub-contracts
;; It returns what an opter returns and its results should be accumulated
;; into the opter's results.
;;
;; The opt/info struct has a number of identifiers that get used to build
;; contracts; see opt-guts.rkt for the selectors.
;;
;; The last argument is a list of free-variables if the calling context
;; was define/opt, otherwise it is null.
;;
;; The fields of the optres struct are:
;;  - the optimized syntax
;;  - lifted variables: a list of (id, sexp) pairs
;;  - super-lifted variables: functions or the such defined at the toplevel of the
;;                            calling context of the opt routine.
;;                            Currently this is only used for struct contracts.
;;  - partially applied contracts: a list of (id, sexp) pairs
;;  - if the contract being optimized is flat,
;;    then an sexp that evals to bool, indicating if the contract passed or not,
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
;;  - #f -- indicating that negative blame is impossible
;;    #t -- indicating that negative blame may be possible
;;    (listof identifier) -- indicating that negative blame is possible 
;;        if it is possible in any of the identifiers in the list
;;        each identifier is expected to be an identifier bound by
;;        the define-opt/c 

(struct optres (exp 
                lifts
                superlifts
                partials
                flat
                opt
                stronger-ribs
                chaperone
                no-negative-blame?
                name))
(define (build-optres #:exp exp
                      #:lifts lifts
                      #:superlifts superlifts
                      #:partials partials
                      #:flat flat
                      #:opt opt
                      #:stronger-ribs stronger-ribs
                      #:chaperone chaperone
                      #:no-negative-blame? [no-negative-blame? (syntax? flat)]
                      #:name [name #''unknown-name])
  (optres exp 
          lifts
          superlifts
          partials
          flat
          opt
          stronger-ribs
          chaperone
          no-negative-blame?
          name))

;; a hash table of opters
(define opters-table
  (make-module-identifier-mapping))

;; get-opter : syntax -> opter
(define (get-opter ctc)
  (module-identifier-mapping-get opters-table ctc (λ () #f)))

;; opter : (union symbol identifier) -> opter
(define (opter ctc)
  (if (identifier? ctc)
      (get-opter ctc)
      (error 'opter "the argument must be a bound identifier, got ~e" ctc)))

;; reg-opter! : symbol opter ->
(define (reg-opter! ctc opter)
  (module-identifier-mapping-put! opters-table ctc opter))

;; interleave-lifts : list list -> list
;; interleaves a list of variables names and a list of sexps into a list of
;; (var sexp) pairs.
(define (interleave-lifts vars sexps)
  (if (= (length vars) (length sexps))
      (if (null? vars) null
          (cons (cons (car vars) (car sexps))
                (interleave-lifts (cdr vars) (cdr sexps))))
      (error 'interleave-lifts "expected lists of equal length, got ~e and ~e" vars sexps)))


;; struct for color-keeping across opters
(define-struct opt/info
  (contract val blame-original-id blame-stx swap-blame? free-vars recf base-pred this that))
(define (build-opt/info contract val blame-id free-vars this that)
  (make-opt/info contract val blame-id blame-id #f free-vars #f #f this that))

(define (opt/info-blame oi)
  (if (opt/info-swap-blame? oi)
    #`(blame-swap #,(opt/info-blame-stx oi))
    (opt/info-blame-stx oi)))

;; returns syntax that, when evaluated, computes
;; the name of the positive blame party
(define (opt/info-positive-blame oi)
  (if (opt/info-swap-blame? oi)
      #`(blame-positive #,(opt/info-blame-original-id oi))
      #`(blame-negative #,(opt/info-blame-original-id oi))))
(define (opt/info-negative-blame oi)
  (if (opt/info-swap-blame? oi)
      #`(blame-negative #,(opt/info-blame-original-id oi))
      #`(blame-positive #,(opt/info-blame-original-id oi))))

;; opt/info-swap-blame : opt/info -> opt/info
;; swaps pos and neg
(define (opt/info-swap-blame info)
  (struct-copy opt/info info [swap-blame? (not (opt/info-swap-blame? info))]))

;; opt/info-change-val : identifier opt/info -> opt/info
;; changes the name of the variable that the value-to-be-contracted is bound to
(define (opt/info-change-val val info)
  (struct-copy opt/info info [val val]))


;; opt/info-add-blame-context : opt/info (stx -> stx) -> opt/info
;; calls 'f' on the current blame syntax to build a new one
;; (presumably wrapping it with a call to (blame-add-context ...),
;;  possibly via a helper function) and returns an adjusted opt/info record
(define (opt/info-add-blame-context info f)
  (struct-copy opt/info info
               [blame-stx (f (opt/info-blame-stx info))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  stronger helper functions
;;

;; new-stronger-var : identifier (identifier identifier -> exp) -> stronger-rib
;; the second identifier should be bound (in a lift) to an expression whose value has to be saved. 
;; The ids passed to cogen are expected to be bound to two contracts' values of that expression, when
;; those contracts are being compared for strongerness
(define (new-stronger-var id cogen)
  (with-syntax ([(var-this var-that) (generate-temporaries (list id id))])
    (make-stronger-rib (syntax var-this)
                       (syntax var-that)
                       id
                       (cogen (syntax var-this)
                              (syntax var-that)))))

(define empty-stronger '())

(define-struct stronger-rib (this-var that-var save-id stronger-exp))

(provide new-stronger-var 
         (struct-out stronger-rib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lifting helper functions
;;
(provide lift/binding lift/effect empty-lifts bind-lifts bind-superlifts lifts-to-save)

;; lift/binding : syntax[expression] identifier lifts -> (values syntax lifts)
;; adds a new id to `lifts' that is bound to `e'. Returns the
;; variable that was bound
;; values that are lifted are also saved in the wrapper to make sure that the rhs's are evaluated at the right time.
(define (lift/binding e id-hint lifts)
  (syntax-case e ()
    [x
     (or (identifier? e)
         (number? (syntax-e e))
         (boolean? (syntax-e e)))
     (values e lifts)]
    [else
     (let ([x (car (generate-temporaries (list id-hint)))])
       (values x
               (snoc (cons x e) lifts)))]))

;; lift/effect : syntax[expression] lifts -> lifts
;; adds a new lift to `lifts' that is evaluated for effect. no variable returned
(define (lift/effect e lifts)
  (let ([x (car (generate-temporaries '(lift/effect)))])
    (snoc (cons #f e) lifts)))

(define (snoc x l) (append l (list x)))

;; empty-lifts : lifts
;; the initial lifts
(define empty-lifts '())

(define (bind-lifts lifts stx) (do-bind-lifts lifts stx #'let*-values))
(define (bind-superlifts lifts stx) (do-bind-lifts lifts stx #'letrec-values))

(define (do-bind-lifts lifts stx binding-form)
  (if (null? lifts)
      stx
      (with-syntax ([((lifts-x . lift-e) ...) lifts])
        (with-syntax ([(lifts-x ...) (map (λ (x) (cond
                                                   [(identifier? x) (list x)]
                                                   [(let ([lst (syntax->list x)])
                                                      (and lst
                                                           (andmap identifier? lst)))
                                                    x]
                                                   [else
                                                    (generate-temporaries '(junk))]))
                                          (syntax->list (syntax (lifts-x ...))))]
                      [binding-form binding-form])
          #`(binding-form ([lifts-x lift-e] ...)
                          #,stx)))))

(define (lifts-to-save lifts) (filter values (map car lifts)))

;;
;; opt/unknown : opt/i id id syntax
;;
(define (opt/unknown opt/i opt/info uctc [extra-warning ""])
  (log-unknown-contract-warning uctc extra-warning)
  (with-syntax ([(lift-var partial-var partial-flat-var)
                 (generate-temporaries '(lift partial partial-flat))]
                [val (opt/info-val opt/info)]
                [uctc uctc]
                [blame (opt/info-blame opt/info)])
    (build-optres
     #:exp #'(partial-var val)
     #:lifts (list (cons #'lift-var 
                         #`(coerce-contract '#,(opt-error-name) uctc)))
     #:superlifts null
     #:partials
     (list (cons
            #'partial-var
            #'((contract-projection lift-var) blame))
           (cons
            #'partial-flat-var
            #'(if (flat-contract? lift-var)
                  (flat-contract-predicate lift-var)
                  (lambda (x) (error 'opt/unknown "flat called on an unknown that had no flat pred ~s ~s"
                                     lift-var
                                     x)))))
     #:flat #f
     #:opt #'lift-var
     #:stronger-ribs null
     #:chaperone #'(chaperone-contract? lift-var)
     #:name #'(contract-name lift-var))))

(define unknown-contract-logger (make-logger 'racket/contract (current-logger)))
(define (log-unknown-contract-warning exp [extra-warning ""])
  (when (log-level? unknown-contract-logger 'warning)
    (define datum (syntax->datum exp))
    (log-message unknown-contract-logger
                 'warning 
                 (string-append (format "warning in ~a:~a: opt/c doesn't know the contract ~s" 
                                        (syntax-source exp)
                                        (if (syntax-line exp)
                                            (format "~a:~a" (syntax-line exp) (syntax-column exp))
                                            (format ":~a" (syntax-position exp)))
                                        datum)
                                extra-warning)
                 datum)))


(define opt-error-name (make-parameter 'opt/c))

;; combine-two-chaperone?s : (or/c boolean? syntax?) (or/c boolean? syntax?) -> (or/c boolean? syntax?)
(define (combine-two-chaperone?s chaperone-a? chaperone-b?)
  (cond
    [(and (boolean? chaperone-a?) (boolean? chaperone-b?))
     (and chaperone-a? chaperone-b?)]
    [(boolean? chaperone-a?)
     (and chaperone-a? chaperone-b?)]
    [(boolean? chaperone-b?)
     (and chaperone-b? chaperone-a?)]
    [else
     #`(and #,chaperone-a? #,chaperone-b?)]))

(define (combine-two-no-negative-blame a b)
  (cond
    [(eq? a #t) b]
    [(eq? a #f) #f]
    [(eq? b #t) a]
    [(eq? b #f) #f]
    [else (append a b)]))

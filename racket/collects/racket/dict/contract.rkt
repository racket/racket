#lang racket/base

(provide dict/c)

(require racket/contract
         racket/dict
         racket/match
         racket/list
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/stx))

;; ---------------------------------------------------------

;; dict/c :
;;   ChaperoneContract
;;   Contract
;;   [Contract]
;;   [#:immutable (U Bool 'dont-care)]
;;   [#:flat? Bool]
;;   ->
;;   Contract
;; The iter contract is only checked if it's not flat
(define (dict/c dom rng
                [iter any/c]
                #:immutable [immutable 'dont-care]
                #:flat? [flat? #f])
  (unless (member immutable '(#t #f dont-care))
    (raise-argument-error 'dict/c
                          "(or/c #t #f 'dont-care) for the #:immutable argument"
                          immutable))
  (define dom-ctc (if flat?
                      (coerce-flat-contract 'dict/c dom)
                      (coerce-chaperone-contract 'dict/c dom)))
  (define rng-ctc (if flat?
                      (coerce-flat-contract 'dict/c rng)
                      (coerce-contract 'dict/c rng)))
  (define iter-ctc (if flat?
                       any/c
                       (coerce-contract 'dict/c iter)))
  (cond
    [(or flat?
         (and (eq? immutable #t)
              (flat-contract? dom-ctc)
              (flat-contract? rng-ctc)
              (flat-contract? iter-ctc)))
     (flat-dict/c dom-ctc rng-ctc any/c immutable)]
    [else
     (impersonator-dict/c dom-ctc rng-ctc iter-ctc immutable)]))

;; immutable-dict? : Any -> Bool
(define (immutable-dict? d)
  (and (dict? d)
       (dict-implements? d 'dict-set 'dict-remove)
       (not (or (dict-implements? d 'dict-set!)
                (dict-implements? d 'dict-remove!)))))

;; mutable-dict? : Any -> Bool
(define (mutable-dict? d)
  (and (dict? d)
       (dict-implements? d 'dict-set! 'dict-remove!)
       (not (or (dict-implements? d 'dict-set)
                (dict-implements? d 'dict-remove)))))

;; dict-key-chaperone-allowed? : Any -> Bool
;; Returns true if `d` can work with chaperones on keys.
;; Hashes based on `equal?` can, but not on `eq?` or `eqv?`
(define (dict-key-chaperone-allowed? d)
  (cond [(hash? d) (hash-equal? d)]
        [else #t]))

;; ---------------------------------------------------------

;; ... --> boolean
;;  returns #t when it called raise-blame-error, #f otherwise
(define (check-dict/c dom-ctc immutable flat? val blame neg-party) 
  (cond
    [(dict? val)
     (cond
       [(and (not flat?)
             (not (flat-contract? dom-ctc))
             (not (dict-key-chaperone-allowed? val)))
        (raise-blame-error
         blame val #:missing-party neg-party
         '(expected
           "equal?-based dict due to higher-order domain contract"
           given:
           "~e")
         val)
        #t]
       [else
        (case immutable
          [(#t) 
           (cond
             [(immutable-dict? val) 
              #f]
             [else
              (raise-blame-error 
               blame val #:missing-party neg-party
               '(expected "an immutable dict" given: "~e") val)
              #t])]
          [(#f)
           (cond
             [(mutable-dict? val)
              #f]
             [else
              (raise-blame-error 
               blame val #:missing-party neg-party
               '(expected "a mutable dict" given: "~e") val)
              #t])]
          [(dont-care) #f])])]
    [else 
     (raise-blame-error blame val #:missing-party neg-party
                        '(expected "a dict" given: "~e") val)
     #t]))

(define (dict/c-first-order ctc)
  (define dom-ctc (base-dict/c-dom ctc))
  (define rng-ctc (base-dict/c-rng ctc))
  (define immutable (base-dict/c-immutable ctc))
  (define flat? (flat-dict/c? ctc))
  (λ (val)
    (and (dict? val)
         (or flat?
             (flat-contract? dom-ctc)
             (dict-key-chaperone-allowed? val))
         (case immutable
           [(#t) (immutable-dict? val)]
           [(#f) (mutable-dict? val)]
           [else #t])
         (for/and ([(k v) (in-dict val)])
           (and (contract-first-order-passes? dom-ctc k)
                (contract-first-order-passes? rng-ctc v))))))

(define (dict/c-name ctc)
  (apply 
   build-compound-type-name
   'dict/c (base-dict/c-dom ctc) (base-dict/c-rng ctc)
   (append
    (if (or (flat-dict/c? ctc) (equal? (base-dict/c-iter ctc) any/c))
        '()
        (list (base-dict/c-iter ctc)))
    (if (and (flat-dict/c? ctc)
             (not (eq? (base-dict/c-immutable ctc) #t)))
        (list '#:flat? #t)
        '())
    (case (base-dict/c-immutable ctc)
      [(dont-care) '()]
      [(#t)
       (list '#:immutable #t)]
      [(#f)
       (list '#:immutable #f)]))))

(struct base-dict/c [dom rng iter immutable])

(define (dict/c-stronger this that)
  (define this-dom (base-dict/c-dom this))
  (define this-rng (base-dict/c-rng this))
  (define this-iter (base-dict/c-iter this))
  (define this-immutable (base-dict/c-immutable this))
  (cond
    [(base-dict/c? that)
     (define that-dom (base-dict/c-dom that))
     (define that-rng (base-dict/c-rng that))
     (define that-iter (base-dict/c-iter that))
     (define that-immutable (base-dict/c-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (and (contract-stronger? this-dom that-dom)
             (contract-stronger? this-rng that-rng)
             (contract-stronger? this-iter that-iter))]
       [(or (equal? that-immutable 'dont-care)
            (equal? this-immutable that-immutable))
        (and (contract-equivalent? this-dom that-dom)
             (contract-equivalent? this-rng that-rng)
             (contract-equivalent? this-iter that-iter))]
       [else #f])]
    [else #f]))

(define (dict/c-equivalent this that)
  (cond
    [(base-dict/c? that)
     (define this-dom (base-dict/c-dom this))
     (define this-rng (base-dict/c-rng this))
     (define this-iter (base-dict/c-iter this))
     (define this-immutable (base-dict/c-immutable this))
     (define that-dom (base-dict/c-dom that))
     (define that-rng (base-dict/c-rng that))
     (define that-iter (base-dict/c-iter that))
     (define that-immutable (base-dict/c-immutable that))
     (and (equal? this-immutable that-immutable)
          (contract-equivalent? this-dom that-dom)
          (contract-equivalent? this-rng that-rng)
          (contract-equivalent? this-iter that-iter))]
    [else #f]))

;; Will periodically generate empty dicts and dicts with multiple elements
;; Only generates hashes, ignores iter
(define (dict/c-generate ctc)
  (define this-dom (base-dict/c-dom ctc))
  (define this-rng (base-dict/c-rng ctc))
  (define this-immutable (base-dict/c-immutable ctc))
  (define hsh (hash/c this-dom this-rng #:immutable this-immutable))
  (λ (fuel)
    (contract-random-generate/choose hsh fuel)))

(struct flat-dict/c base-dict/c []
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name dict/c-name
   #:first-order dict/c-first-order
   #:generate dict/c-generate
   #:stronger dict/c-stronger
   #:equivalent dict/c-equivalent
   #:late-neg-projection
   (λ (ctc)
     (define dom-ctc (base-dict/c-dom ctc))
     (define immutable (base-dict/c-immutable ctc))
     (define flat? (flat-dict/c? ctc))
     (λ (blame)
       (define dom-proj ((get/build-late-neg-projection (base-dict/c-dom ctc))
                         (blame-add-key-context blame #f)))
       (define rng-proj ((get/build-late-neg-projection (base-dict/c-rng ctc))
                         (blame-add-value-context blame #f)))
       (λ (val neg-party)
         (cond
           [(check-dict/c dom-ctc immutable flat? val blame neg-party)
            val]
           [else
            (for ([(k v) (in-dict val)])
              (dom-proj k neg-party)
              (rng-proj v neg-party))
            val]))))))

(define ho-projection
  (λ (ctc)
    (define immutable (base-dict/c-immutable ctc))
    (define dom-ctc (base-dict/c-dom ctc))
    (define rng-ctc (base-dict/c-rng ctc))
    (define iter-ctc (base-dict/c-iter ctc))
    (define flat? (flat-dict/c? ctc))
    (λ (blame)
      (λ (val neg-party)
        (cond
          [(check-dict/c dom-ctc immutable flat? val blame neg-party)
           val]
          [else
           (handle-the-dict val
                            neg-party
                            dom-ctc
                            rng-ctc
                            iter-ctc
                            blame)])))))

(define (blame-add-key-context blame swap?)
  (blame-add-context blame "the keys of" #:swap? swap?))
(define (blame-add-value-context blame swap?)
  (blame-add-context blame "the values of" #:swap? swap?))

(define (handle-the-dict val
                         neg-party
                         dom-ctc
                         rng-ctc
                         iter-ctc
                         blame)
  (define blame+neg-party (blame-replace-negative blame neg-party))
  (wrap/add-layer (layer blame+neg-party dom-ctc rng-ctc iter-ctc) val))

(struct impersonator-dict/c base-dict/c ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name dict/c-name
   #:first-order dict/c-first-order
   #:stronger dict/c-stronger
   #:equivalent dict/c-equivalent
   #:late-neg-projection ho-projection))

;; ---------------------------------------------------------

;; A Dict/Contract is a (dict/contract [Listof Layer] Dict)
;; meant as a parent struct for the actual wrappers for dicts
;; immutable, mutable, and ?mutable
;; A Layer is a (layer Blame ChaperoneCtc Ctc Ctc)
;; inputs go through layers in the list order
;; outputs go through layers in reverse order
(struct dict/contract [layers dict])
(struct layer [blame key/c value/c iter/c])

;; Definig all the methods based on the roles of the inputs and outputs.
;; Self is the Dict/Contract instance
;; Key must be checked with key/c
;; Value must be checked with value/c
;; Iter must be checked with iter/c
;; Default must use the escape-continuation

(begin-for-syntax
  ;; A Role is a one of:
  ;;  - 'self
  ;;  - (role (U Identifier #f) (U Identifier #f) Boolean)
  ;; if escape is false:
  ;;   wrap-in is an id for a function [[Listof Layer] X -> X]
  ;;   wrap-out is an id for a function [[Listof Layer] X -> X]
  ;; if escape is true:
  ;;   wrap-in is for a function [[Listof Layer] EscapeContinuation X -> X]
  ;;   wrap-out is for a function [[Listof Layer] EscapeContinuation X -> X]
  ;; a #f for wrap-in means the role shouldn't be used for inputs
  ;; a #f for wrap-out means the role shouldn't be used for outputs
  (struct role [wrap-in wrap-out escape])
  (define (role/self? r) (or (eq? r 'self) (role? r)))
  (define (role-escape? r) (and (not (eq? r 'self)) (role-escape r)))
  (define (lookup-role/record id) (syntax-local-value/record id role/self?)))

(define-syntax define-dict-op-role
  (λ (stx)
    (syntax-case stx []
      [(_ id #:self) #'(define-syntax id 'self)]
      [(_ id #:in in) #'(define-syntax id (role (quote-syntax in) #f #f))]
      [(_ id #:in in #:escape) #'(define-syntax id (role (quote-syntax in) #f #t))]
      [(_ id #:out out) #'(define-syntax id (role #f (quote-syntax out) #f))]
      [(_ id #:in in #:out out)
       #'(define-syntax id (role (quote-syntax in) (quote-syntax out) #f))])))

(define-dict-op-role Self #:self)
(define-dict-op-role Dict #:in dict-in #:out dict-out)
(define-dict-op-role Key #:in key-in #:out key-out)
(define-dict-op-role Value #:in value-in #:out value-out)
(define-dict-op-role Iter #:in iter-in)
(define-dict-op-role MaybeIter #:out maybe-iter-out)
(define-dict-op-role Default #:in default-in #:escape)
(define-dict-op-role Void #:out void-out)
(define-dict-op-role Nat #:out nat-out)

(define-syntax-rule (define-dict-ops clause ...)
  (begin (define-dict-op . clause) ...))

(define-syntax define-dict-op
  (λ (stx)
    (syntax-case stx []
      [(_ id op #:: in ... #:-> out)
       (with-disappeared-uses
         (define-values [mand opt]
           (splitf-at (syntax->list #'(in ...)) identifier?))
         (define mand-roles (map lookup-role/record mand))
         (define opt-roles (map (compose lookup-role/record stx-car) opt))
         (define out-role (lookup-role/record #'out))
         (define mand-tmps (generate-temporaries mand))
         (define opt-tmps (generate-temporaries opt))
         (define self-tmp (list-ref mand-tmps (index-of mand-roles 'self)))
         (define layers-tmp (generate-temporary 'layers))
         ;; escape-index : (U #f Nat)
         ;; The first case index where an escape is needed, if at all
         (define escape-index
           (cond [(or (ormap role-escape? mand-roles) (role-escape? out-role)) 0]
                 [(index-where opt-roles role-escape?) => add1]
                 [else #f]))
         (define escape-tmp (and escape-index (generate-temporary 'escape)))
         (define (in-expr role expr)
           (cond [(eq? role 'self) #`(dict/contract-dict #,expr)]
                 [(role-escape? role)
                  #`(#,(role-wrap-in role) #,layers-tmp #,escape-tmp #,expr)]
                 [else #`(#,(role-wrap-in role) #,layers-tmp #,expr)]))
         (define (out-expr expr)
           (cond [(role-escape? out-role)
                  #`(#,(role-wrap-out out-role) #,layers-tmp #,escape-tmp #,expr)]
                 [else #`(#,(role-wrap-out out-role) #,layers-tmp #,expr)]))
         (define mand-exprs (map in-expr mand-roles mand-tmps))
         (define opt-exprs (map in-expr opt-roles opt-tmps))
         (define lam-cases
           (for/list ([i (in-range (add1 (length opt-roles)))])
             #`[(#,@mand-tmps #,@(take opt-tmps i))
                (let ([#,layers-tmp (dict/contract-layers #,self-tmp)])
                  #,(if (and escape-index (<= escape-index i))
                        #`(let/ec #,escape-tmp
                            #,(out-expr #`(op #,@mand-exprs #,@(take opt-exprs i))))
                        (out-expr #`(op #,@mand-exprs #,@(take opt-exprs i)))))]))
         #`(define id (case-lambda #,@lam-cases)))])))

(define-dict-ops
  [dref dict-ref #:: Self Key [Default] #:-> Value]
  [dset! dict-set! #:: Self Key Value #:-> Void]
  [dset dict-set #:: Self Key Value #:-> Dict]
  [dremove! dict-remove! #:: Self Key #:-> Void]
  [dremove dict-remove #:: Self Key #:-> Dict]
  [dcount dict-count #:: Self #:-> Nat]
  [diterate-first dict-iterate-first #:: Self #:-> MaybeIter]
  [diterate-next dict-iterate-next #:: Self Iter #:-> MaybeIter]
  [diterate-key dict-iterate-key #:: Self Iter #:-> Key]
  [diterate-value dict-iterate-value #:: Self Iter #:-> Value])

;; a wrapper for immutable dicts
(struct immutable-dict/contract dict/contract []
  #:property prop:dict
  (vector-immutable dref #f dset #f dremove dcount
                    diterate-first diterate-next diterate-key diterate-value)
  #:methods gen:equal+hash
  [;; immutable can use equal
   (define (equal-proc self other rec)
     (rec (dict/contract-dict self) (dict/contract-dict other)))
   (define (hash-proc self rec)
     (rec (dict/contract-dict self)))
   (define (hash2-proc self rec)
     (rec (dict/contract-dict self)))])

;; a wrapper for mutable dicts
(struct mutable-dict/contract dict/contract []
  #:property prop:dict
  (vector-immutable dref dset! #f dremove! #f dcount
                    diterate-first diterate-next diterate-key diterate-value)
  #:methods gen:equal+hash
  [;; mutable uses eq
   (define (equal-proc self other rec)
     (eq? (dict/contract-dict self) (dict/contract-dict other)))
   (define (hash-proc self rec)
     (eq-hash-code (dict/contract-dict self)))
   (define (hash2-proc self rec)
     (eq-hash-code (dict/contract-dict self)))])

;; a wrapper for dicts of unknown mutability
(struct ?mutable-dict/contract dict/contract []
  #:property prop:dict
  (vector-immutable dref dset! dset dremove! dremove dcount
                    diterate-first diterate-next diterate-key diterate-value)
  #:methods gen:equal+hash
  [;; potentially mutable uses eq
   (define (equal-proc self other rec)
     (eq? (dict/contract-dict self) (dict/contract-dict other)))
   (define (hash-proc self rec)
     (eq-hash-code (dict/contract-dict self)))
   (define (hash2-proc self rec)
     (eq-hash-code (dict/contract-dict self)))])

;; wrap/add-layer : Layer Dict -> Dict/Contract
(define (wrap/add-layer l1 d1)
  (match d1
    [(immutable-dict/contract ls2 d2) (immutable-dict/contract (cons l1 ls2) d2)]
    [(mutable-dict/contract ls2 d2)   (mutable-dict/contract (cons l1 ls2) d2)]
    [(?mutable-dict/contract ls2 d2)  (?mutable-dict/contract (cons l1 ls2) d2)]
    [_                                (wrap/all-layers (list l1) d1)]))

;; wrap/all-layers : [Listof Layer] Dict -> Dict/Contract
(define (wrap/all-layers ls d)
  (cond [(immutable-dict? d) (immutable-dict/contract ls d)]
        [(mutable-dict? d)   (mutable-dict/contract ls d)]
        [else                (?mutable-dict/contract ls d)]))

;; ---------------------------------------------------------

(define (dict-out ls d) (wrap/all-layers ls d))

;; inputs go through layers in the list order, so first layer
(define (default-in ls escape default)
  (cond [(empty? ls) default]
        [else
         (define d
           (project-in (layer-blame (first ls)) failure-result/c default))
         (cond [(procedure? d) (λ () (escape (d)))]
               [else (λ () (escape d))])]))
;; outputs go through layers reverse order, so last layer
(define (void-out ls v)
  (cond [(empty? ls) v]
        [else (project-out (layer-blame (last ls)) void? v)]))
(define (nat-out ls n)
  (cond [(empty? ls) n]
        [else (project-out (layer-blame (last ls)) natural-number/c n)]))

(define (key-in ls k) (layers-in ls layer-key/c k))
(define (key-out ls k) (layers-out ls layer-key/c k))

(define (value-in ls v) (layers-in ls layer-value/c v))
(define (value-out ls v) (layers-out ls layer-value/c v))

(define (iter-in ls i) (layers-in ls layer-iter/c i))
(define (maybe-iter-out ls i) (and i (layers-out ls layer-iter/c i)))

;; layers-in : [Listof Layer] [Layer -> Contract] Any -> Any
;; inputs go through layers in the list order
(define (layers-in ls l->c v)
  (for/fold ([v v]) ([l (in-list ls)])
    (project-in (layer-blame l) (l->c l) v)))

;; layers-out : [Listof Layer] [Layer -> Contract] Any -> Any
;; outputs go through layers reverse order
(define (layers-out ls l->c v)
  (for/fold ([v v]) ([l (in-list (reverse ls))])
    (project-out (layer-blame l) (l->c l) v)))

;; project-in : Blame Contract Any -> Any
(define (project-in blame ctc v)
  (((contract-projection ctc) (blame-swap blame)) v))

;; project-out : Blame Contract Any -> Any
(define (project-out blame ctc v)
  (((contract-projection ctc) blame) v))

;; ---------------------------------------------------------

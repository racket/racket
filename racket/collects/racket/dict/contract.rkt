#lang racket/base

(provide dict/c)

(require racket/contract
         racket/dict
         racket/generic
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

;; ho-projection : Boolean -> [Base-Dict/c -> [Blame -> [Dict Any -> Dict]]]
;; chaperone-mode? #true means can use chaperones, #false means use impersonators
(define (ho-projection chaperone-mode?)
  (λ (ctc)
    (define immutable (base-dict/c-immutable ctc))
    (define dom-ctc (base-dict/c-dom ctc))
    (define flat? (flat-dict/c? ctc))
    (define dom-proc (get/build-late-neg-projection dom-ctc))
    (define rng-proc (get/build-late-neg-projection (base-dict/c-rng ctc)))
    (define iter-proc (get/build-late-neg-projection (base-dict/c-iter ctc)))
    (λ (blame)
      (define-values (dom-filled? maybe-pos-dom-proj maybe-neg-dom-proj)
        (contract-pos/neg-doubling (dom-proc (blame-add-key-context blame #f))
                                   (dom-proc (blame-add-key-context blame #t))))
      (define-values (rng-filled? maybe-pos-rng-proj maybe-neg-rng-proj)
        (contract-pos/neg-doubling (rng-proc (blame-add-value-context blame #f))
                                   (rng-proc (blame-add-value-context blame #t))))
      (define-values (iter-filled? maybe-pos-iter-proj maybe-neg-iter-proj)
        (contract-pos/neg-doubling (iter-proc (blame-add-iter-context blame #f))
                                   (iter-proc (blame-add-iter-context blame #t))))
      (cond
        [(and dom-filled? rng-filled? iter-filled?)
         (λ (val neg-party)
           (cond
             [(check-dict/c dom-ctc immutable flat? val blame neg-party)
              val]
             [else
              (handle-the-dict val neg-party
                               maybe-pos-dom-proj maybe-neg-dom-proj
                               maybe-pos-rng-proj maybe-neg-rng-proj
                               maybe-pos-iter-proj maybe-neg-iter-proj
                               chaperone-mode? ctc blame)]))]
        [else
         (define tc (make-thread-cell #f))
         (λ (val neg-party)
           (define-values (pos-dom-proj
                           neg-dom-proj
                           pos-rng-proj
                           neg-rng-proj
                           pos-iter-proj
                           neg-iter-proj)
             (cond
               [(thread-cell-ref tc)
                =>
                (λ (v) (values (vector-ref v 1)
                               (vector-ref v 2)
                               (vector-ref v 3)
                               (vector-ref v 4)
                               (vector-ref v 5)
                               (vector-ref v 6)))]
               [else
                (define pos-dom-proj (maybe-pos-dom-proj))
                (define neg-dom-proj (maybe-neg-dom-proj))
                (define pos-rng-proj (maybe-pos-rng-proj))
                (define neg-rng-proj (maybe-neg-rng-proj))
                (define pos-iter-proj (maybe-pos-iter-proj))
                (define neg-iter-proj (maybe-neg-iter-proj))
                (thread-cell-set!
                 tc
                 (vector-immutable pos-dom-proj
                                   neg-dom-proj
                                   pos-rng-proj
                                   neg-rng-proj
                                   pos-iter-proj
                                   neg-iter-proj))
                (values pos-dom-proj
                        neg-dom-proj
                        pos-rng-proj
                        neg-rng-proj
                        pos-iter-proj
                        neg-iter-proj)]))
           (cond
             [(check-dict/c dom-ctc immutable flat? val blame neg-party)
              val]
             [else
              (handle-the-dict val neg-party
                               pos-dom-proj neg-dom-proj
                               pos-rng-proj neg-rng-proj
                               pos-iter-proj neg-iter-proj
                               chaperone-mode? ctc blame)]))]))))

(define (blame-add-key-context blame swap?)
  (blame-add-context blame "the keys of" #:swap? swap?))
(define (blame-add-value-context blame swap?)
  (blame-add-context blame "the values of" #:swap? swap?))
(define (blame-add-iter-context blame swap?)
  (blame-add-context blame "the iteration-indexes of" #:swap? swap?))

;; A proj struct contains the rest of the arguments to
;; handle-the-dict or handle-the-struct-dict after `val`
;; chaperone-mode? #true means can use chaperones, #false means use impersonators
(struct proj
  [neg-party
   pos-dom-proj neg-dom-proj
   pos-rng-proj neg-rng-proj
   pos-iter-proj neg-iter-proj
   chaperone-mode? ctc blame])

(define (handle-the-dict/proj val prj)
  (match-define
    (proj neg-party
          pos-dom-proj neg-dom-proj
          pos-rng-proj neg-rng-proj
          pos-iter-proj neg-iter-proj
          chaperone-mode? ctc blame)
    prj)
  (handle-the-dict val neg-party
                   pos-dom-proj neg-dom-proj
                   pos-rng-proj neg-rng-proj
                   pos-iter-proj neg-iter-proj
                   chaperone-mode? ctc blame))

;; chaperone-mode? #true means can use chaperones, #false means use impersonators
(define (handle-the-dict val neg-party
                         pos-dom-proj neg-dom-proj
                         pos-rng-proj neg-rng-proj
                         pos-iter-proj neg-iter-proj
                         chaperone-mode? ctc blame)
  (cond
    [(hash? val)
     (handle-the-hash val neg-party
                      pos-dom-proj neg-dom-proj
                      pos-rng-proj neg-rng-proj
                      chaperone-mode? ctc blame)]
    [(vector? val)
     (handle-the-vector val neg-party
                        pos-dom-proj neg-dom-proj
                        pos-rng-proj neg-rng-proj
                        chaperone-mode? ctc blame)]
    [(and (list? val) (andmap pair? val))
     (handle-the-assoc val neg-party
                       pos-dom-proj
                       pos-rng-proj)]
    [else
     (handle-the-struct-dict val neg-party
                             pos-dom-proj neg-dom-proj
                             pos-rng-proj neg-rng-proj
                             pos-iter-proj neg-iter-proj
                             chaperone-mode? ctc blame)]))

(define (handle-the-hash val neg-party
                         pos-dom-proj neg-dom-proj
                         pos-rng-proj neg-rng-proj
                         chaperone-mode? ctc blame)
  (define blame+neg-party (cons blame neg-party))
  (define chaperone-or-impersonate-hash
    (if chaperone-mode? chaperone-hash impersonate-hash))
  (if (immutable? val) 
      (for/fold ([h val]) ([(k v) (in-hash val)])
        (hash-set h
                  (pos-dom-proj k neg-party)
                  (pos-rng-proj v neg-party)))
      (chaperone-or-impersonate-hash
       val
       ; ref-proc
       (λ (h k)
         (values (with-contract-continuation-mark
                   blame+neg-party
                   (neg-dom-proj k neg-party))
                 (λ (h k v)
                   (with-contract-continuation-mark
                     blame+neg-party
                     (pos-rng-proj v neg-party)))))
       ; set-proc
       (λ (h k v)
         (with-contract-continuation-mark
           blame+neg-party
           (values (neg-dom-proj k neg-party)
                   (neg-rng-proj v neg-party))))
       ; remove-proc
       (λ (h k)
         (with-contract-continuation-mark
           blame+neg-party
           (neg-dom-proj k neg-party)))
       ; key-proc
       (λ (h k)
         (with-contract-continuation-mark
           blame+neg-party
           (pos-dom-proj k neg-party)))
       impersonator-prop:contracted ctc
       impersonator-prop:blame blame)))

(define (handle-the-vector val neg-party
                           pos-dom-proj neg-dom-proj
                           pos-rng-proj neg-rng-proj
                           chaperone-mode? ctc blame)
  (define blame+neg-party (cons blame neg-party))
  (define chaperone-or-impersonate-vector
    (if chaperone-mode? chaperone-vector impersonate-vector))
  (if (immutable? val)
      (vector->immutable-vector
       (for/vector #:length (vector-length val)
         ([(v k) (in-indexed (in-vector val))])
         (pos-dom-proj k neg-party)
         (pos-rng-proj v neg-party)))
      (chaperone-or-impersonate-vector
       val
       ; ref-proc
       (λ (h k v)
         (with-contract-continuation-mark blame+neg-party
           (neg-dom-proj k neg-party))
         (with-contract-continuation-mark blame+neg-party
           (pos-rng-proj v neg-party)))
       ; set-proc
       (λ (h k v)
         (with-contract-continuation-mark blame+neg-party
           (neg-dom-proj k neg-party))
         (with-contract-continuation-mark blame+neg-party
           (neg-rng-proj v neg-party)))
       impersonator-prop:contracted ctc
       impersonator-prop:blame blame)))

(define (handle-the-assoc val neg-party
                          pos-dom-proj
                          pos-rng-proj)
  (for/list ([p (in-list val)])
    (cons (pos-dom-proj (car p) neg-party)
          (pos-rng-proj (cdr p) neg-party))))

(define (handle-the-struct-dict val neg-party
                                pos-dom-proj neg-dom-proj
                                pos-rng-proj neg-rng-proj
                                pos-iter-proj neg-iter-proj
                                chaperone-mode? ctc blame)
  (define prj
    (proj neg-party
          pos-dom-proj neg-dom-proj
          pos-rng-proj neg-rng-proj
          pos-iter-proj neg-iter-proj
          chaperone-mode? ctc blame))
  (redirect-generics chaperone-mode? gen:dict val
    ; Primitive
    [dict-ref (redirect-ref prj)]
    [dict-set! (redirect-set! prj)]
    [dict-set (redirect-set prj)]
    [dict-remove! (redirect-remove! prj)]
    [dict-remove (redirect-remove prj)]
    [dict-count (redirect-count prj)]
    [dict-iterate-first (redirect-iterate-first prj)]
    [dict-iterate-next (redirect-iterate-next prj)]
    [dict-iterate-key (redirect-iterate-key prj)]
    [dict-iterate-value (redirect-iterate-value prj)]
    ; Derived
    #|
    [dict-has-key? (redirect- prj)]
    [dict-set*! (redirect- prj)]
    [dict-set* (redirect- prj)]
    [dict-ref! (redirect- prj)]
    [dict-update! (redirect- prj)]
    [dict-update (redirect- prj)]
    [dict-map (redirect- prj)]
    [dict-for-each (redirect- prj)]
    [dict-empty? (redirect- prj)]
    [dict-count (redirect- prj)]
    [dict-copy (redirect- prj)]
    [dict-clear (redirect- prj)]
    [dict-clear! (redirect- prj)]
    [dict-keys (redirect- prj)]
    [dict-values (redirect- prj)]
    [dict->list (redirect- prj)]
    |#))

(struct impersonator-dict/c base-dict/c ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name dict/c-name
   #:first-order dict/c-first-order
   #:stronger dict/c-stronger
   #:equivalent dict/c-equivalent
   #:late-neg-projection (ho-projection #f)))

;; ---------------------------------------------------------

;; Definig redirect functions for all the methods based on
;; the roles of the inputs and outputs.
;; Self is the Dict whose methods are being redirected
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
(define-dict-op-role Dict #:out dict-out)
(define-dict-op-role Key #:in key-in #:out key-out)
(define-dict-op-role Value #:in value-in #:out value-out)
(define-dict-op-role Iter #:in iter-in)
(define-dict-op-role MaybeIter #:out maybe-iter-out)
(define-dict-op-role Default #:in default-in #:escape)
(define-dict-op-role Void #:out void-out)
(define-dict-op-role Nat #:out nat-out)

(define-syntax-rule (define-dict-redirect-ops clause ...)
  (begin (define-dict-redirect-op . clause) ...))

(define-syntax define-dict-redirect-op
  (λ (stx)
    (syntax-case stx []
      [(_ id #:: [in ...] #:-> out)
       #'(define-dict-redirect-op id #:: [in ...] [] #:-> out)]
      [(_ id #:: [mand-in ...] [opt-in ...] #:-> out)
       (with-disappeared-uses
         (define mand-roles (stx-map lookup-role/record #'(mand-in ...)))
         (define opt-roles (stx-map lookup-role/record #'(opt-in ...)))
         (define out-role (lookup-role/record #'out))
         ;; escape-index : (U #f Nat)
         ;; The first case index where an escape is needed, if at all
         (define escape-index
           (cond [(or (ormap role-escape? mand-roles) (role-escape? out-role)) 0]
                 [(index-where opt-roles role-escape?) => add1]
                 [else #f]))
         (define/with-syntax prj (generate-temporary 'proj))
         (define/with-syntax op (generate-temporary 'op))
         (define mand-tmps (generate-temporaries #'(mand-in ...)))
         (define opt-tmps (generate-temporaries #'(opt-in ...)))
         (define/with-syntax escape (and escape-index (generate-temporary 'escape)))
         (define (in-expr role expr)
           (cond [(eq? role 'self) expr]
                 [(role-escape? role)
                  #`(#,(role-wrap-in role) prj escape #,expr)]
                 [else #`(#,(role-wrap-in role) prj #,expr)]))
         (define (out-expr expr)
           (cond [(role-escape? out-role)
                  #`(#,(role-wrap-out out-role) prj escape #,expr)]
                 [else #`(#,(role-wrap-out out-role) prj #,expr)]))
         (define mand-exprs (map in-expr mand-roles mand-tmps))
         (define opt-exprs (map in-expr opt-roles opt-tmps))
         (define lam-cases
           (for/list ([i (in-range (add1 (length opt-roles)))])
             #`[(#,@mand-tmps #,@(take opt-tmps i))
                #,(if (and escape-index (<= escape-index i))
                      #`(let/ec escape
                          #,(out-expr #`(op #,@mand-exprs #,@(take opt-exprs i))))
                      (out-expr #`(op #,@mand-exprs #,@(take opt-exprs i))))]))
         #`(define ((id prj) op)
             (and op (case-lambda #,@lam-cases))))])))

(define-dict-redirect-ops
  [redirect-ref #:: [Self Key] [Default] #:-> Value]
  [redirect-set! #:: [Self Key Value] #:-> Void]
  [redirect-set #:: [Self Key Value] #:-> Dict]
  [redirect-remove! #:: [Self Key] #:-> Void]
  [redirect-remove #:: [Self Key] #:-> Dict]
  [redirect-count #:: [Self] #:-> Nat]
  [redirect-iterate-first #:: [Self] #:-> MaybeIter]
  [redirect-iterate-next #:: [Self Iter] #:-> MaybeIter]
  [redirect-iterate-key #:: [Self Iter] #:-> Key]
  [redirect-iterate-value #:: [Self Iter] #:-> Value])

;; ---------------------------------------------------------

(define (dict-out prj d) (handle-the-dict/proj d prj))

(define (key-in prj k)  ((proj-neg-dom-proj prj) k (proj-neg-party prj)))
(define (key-out prj k) ((proj-pos-dom-proj prj) k (proj-neg-party prj)))

(define (value-in prj k)  ((proj-neg-rng-proj prj) k (proj-neg-party prj)))
(define (value-out prj k) ((proj-pos-rng-proj prj) k (proj-neg-party prj)))

(define (iter-in prj i) ((proj-neg-iter-proj prj) i (proj-neg-party prj)))
(define (maybe-iter-out prj i)
  (and i ((proj-pos-iter-proj prj) i (proj-neg-party prj))))

(define (default-in prj escape default)
  (define d (project-in prj failure-result/c default))
  (cond [(procedure? d) (λ () (escape (d)))]
        [else (λ () (escape d))]))

(define (void-out prj v) (project-out prj void? v))
(define (nat-out prj n) (project-out prj natural-number/c n))

;; project-in : Proj Contract Any -> Any
(define (project-in prj ctc v)
  (((contract-late-neg-projection ctc)
    (blame-swap (proj-blame prj)))
   v
   (proj-neg-party prj)))

;; project-out : Proj Contract Any -> Any
(define (project-out prj ctc v)
  (((contract-late-neg-projection ctc)
    (proj-blame prj))
   v
   (proj-neg-party prj)))

;; ---------------------------------------------------------

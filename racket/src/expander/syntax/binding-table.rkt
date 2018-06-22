#lang racket/base
(require (for-syntax racket/base)
         "../common/set.rkt"
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "syntax.rkt"
         "module-binding.rkt")

;; A binding table within a scope maps symbol plus scope set
;; combinations (where the scope binding the binding table is always
;; included in the set).
;;
;; A binding table is one of
;;
;;  - hash of sym -> scope-set -> binding
;;
;;  - (table-with-bulk-bindings hash[as above] list-of-bulk-binding-at)
;;
;; In the latter case, the symbol-keyed hash table overrides bindings
;; supplied (for the same scope sets) in the bulk bindings.

(provide empty-binding-table
         binding-table-add
         binding-table-add-bulk
         binding-table-empty?
         
         in-binding-table
         in-full-non-bulk-binding-table
         
         binding-table-symbols
         
         prop:bulk-binding
         (struct-out bulk-binding-class)
         
         binding-table-prune-to-reachable
         binding-table-register-reachable
         prop:implicitly-reachable
         
         deserialize-table-with-bulk-bindings
         deserialize-bulk-binding-at)

(define empty-binding-table #hasheq())

(struct table-with-bulk-bindings (syms
                                  syms/serialize ; copy of `syms`, but maybe with less nominal info
                                  bulk-bindings)
  #:property prop:serialize
  (lambda (twbb ser-push! state)
    (ser-push! 'tag '#:table-with-bulk-bindings)
    (ser-push! (table-with-bulk-bindings-syms/serialize twbb))
    (ser-push! (table-with-bulk-bindings-bulk-bindings twbb)))
  #:authentic)

(define (deserialize-table-with-bulk-bindings syms bulk-bindings)
  (table-with-bulk-bindings syms syms bulk-bindings))

;; ----------------------------------------

(struct bulk-binding-at (scopes ; scope set
                         bulk)  ; bulk-binding
  #:property prop:serialize
  (lambda (bba ser-push! state)
    ;; Data that is interpreted by the deserializer:
    (ser-push! 'tag '#:bulk-binding-at)
    (ser-push! (bulk-binding-at-scopes bba))
    (ser-push! (bulk-binding-at-bulk bba)))
  #:property prop:reach-scopes
  (lambda (sms reach)
    ;; bulk bindings are pruned depending on whether all scopes
    ;; in `scopes` are reachable, and we shouldn't get here
    ;; when looking for scopes
    (error "shouldn't get here"))
  #:authentic)

(define (deserialize-bulk-binding-at scopes bulk)
  (bulk-binding-at scopes bulk))

;; Bulk bindings are represented by a property, so that the implementation
;; can be separate and manage serialization:
(define-values (prop:bulk-binding bulk-binding? bulk-binding-ref)
  (make-struct-type-property 'bulk-binding))

;; Value of `prop:bulk-binding`
(struct bulk-binding-class (get-symbols ; bulk-binding list-of-shift -> sym -> binding-info
                            create))    ; bul-binding -> binding-info sym -> binding
(define (bulk-binding-symbols b s extra-shifts)
  ;; Providing the identifier `s` supports its shifts
  ((bulk-binding-class-get-symbols (bulk-binding-ref b))
   b 
   (append extra-shifts (if s (syntax-mpi-shifts s) null))))
(define (bulk-binding-create b)
  (bulk-binding-class-create (bulk-binding-ref b)))

;; ----------------------------------------

(define (binding-table-empty? bt)
  (and (hash? bt) (zero? (hash-count bt))))

;; Adding a binding for a single symbol
(define (binding-table-add bt scopes sym binding just-for-nominal?)
  (cond
   [(hash? bt)
    (hash-set bt sym (hash-set (hash-ref bt sym #hash()) scopes binding))]
   [else
    (define new-syms
      (binding-table-add (table-with-bulk-bindings-syms bt)
                         scopes
                         sym
                         binding
                         just-for-nominal?))
    ;; Keep `syms/serialize` in sync with `syms`, except for bindings
    ;; that are just to extend the set of nominal imports. We keep those
    ;; separate --- and don't serialize them --- because they interfere
    ;; with bulk representations of binding and they're used only to
    ;; commuincate to `provide`.
    (define new-syms/serialize
      (cond
       [just-for-nominal? (table-with-bulk-bindings-syms/serialize bt)]
       [(eq? (table-with-bulk-bindings-syms bt)
             (table-with-bulk-bindings-syms/serialize bt))
        new-syms]
       [else (binding-table-add (table-with-bulk-bindings-syms/serialize bt)
                                scopes
                                sym
                                binding
                                #f)]))
    (struct-copy table-with-bulk-bindings bt 
                 [syms new-syms]
                 [syms/serialize new-syms/serialize])]))

(define-values (prop:implicitly-reachable implicitly-reachable? implicitly-reachable-ref)
  (make-struct-type-property 'implicitly-reachable))

;; Adding a binding for a computed-on-demand set of symbols
(define (binding-table-add-bulk bt scopes bulk
                                #:shadow-except [shadow-except #f])
  (cond
   [(table-with-bulk-bindings? bt)
    (define new-syms (remove-matching-bindings (table-with-bulk-bindings-syms bt)
                                               scopes
                                               bulk
                                               #:except shadow-except))
    (define new-syms/serialize (if (eq? (table-with-bulk-bindings-syms bt)
                                        (table-with-bulk-bindings-syms/serialize bt))
                                   new-syms
                                   (remove-matching-bindings (table-with-bulk-bindings-syms/serialize bt)
                                                             scopes
                                                             bulk
                                                             #:except shadow-except)))
    (table-with-bulk-bindings new-syms
                              new-syms/serialize
                              (cons (bulk-binding-at scopes bulk)
                                    (table-with-bulk-bindings-bulk-bindings bt)))]
   [else
    (binding-table-add-bulk (table-with-bulk-bindings bt bt null) scopes bulk)]))

;; The bindings of `bulk` at `scopes` should shadow any existing
;; mappings in `sym-bindings`, except one for `except`
(define (remove-matching-bindings syms scopes bulk #:except except)
  (define bulk-symbols (bulk-binding-symbols bulk #f null))
  (cond
   [((hash-count syms) . < . (hash-count bulk-symbols))
    ;; Faster to consider each sym in `syms`
    (for/fold ([syms syms]) ([(sym sym-bindings) (in-immutable-hash syms)])
      (if (hash-ref bulk-symbols sym #f)
          (remove-matching-binding syms sym sym-bindings scopes #:except except)
          syms))]
   [else
    ;; Faster to consider each sym in `bulk-symbols`
    (for/fold ([syms syms]) ([sym (in-immutable-hash-keys bulk-symbols)])
      (define sym-bindings (hash-ref syms sym #f))
      (if sym-bindings
          (remove-matching-binding syms sym sym-bindings scopes #:except except)
          syms))]))

;; Update an individual symbol's bindings to remove a mapping
;; for a given set of scopes
(define (remove-matching-binding syms sym sym-bindings scopes #:except except)
  (cond
    [(and except
          (let ([b (hash-ref sym-bindings scopes #f)])
            (and (module-binding? b)
                 (eq? except (module-binding-module b)))))
     ;; Don't replace a shadowing definition
     syms]
    [else
     (hash-set syms sym (hash-remove sym-bindings scopes))]))

;; ----------------------------------------

;; Iterate through all scope+binding combinations for a given symbol;
;; the syntax object and extra shifts expressions may be used for
;; loading bulk bindings.
(define-sequence-syntax in-binding-table
  (lambda () #'do-not-use-in-binding-table-as-an-expression)
  (lambda (stx)
    (syntax-case stx ()
      [[(scopes-id binding-id) (_ sym table-expr s-expr extra-shifts-expr)]
       (identifier? #'sym)
       #'[(scopes-id binding-id)
          (:do-in
           ([(ht bulk-bindings)
             (let ([table table-expr])
               (if (hash? table)
                   (values (hash-ref table sym #hash()) null)
                   (values (hash-ref (table-with-bulk-bindings-syms table) sym #hash())
                           (table-with-bulk-bindings-bulk-bindings table))))]
            [(s) s-expr]
            [(extra-shifts) extra-shifts-expr])
           #t
           ;; The current index is either a number index for a hash table
           ;; (extracted from the symbol-keyed hash table) or it is a pair
           ;; for walking down the list of bulk bindings
           ([i (or (hash-iterate-first ht)
                   bulk-bindings)])
           ;; We're done when we've moved on to the bulk-binding part
           ;; and none are left:
           (not (null? i))
           ;; At each step, extract the current scope set and binding;
           ;; either can be #f, in which case the consumer of the
           ;; sequence should move on the the next result
           ([(scopes-id) (cond
                          [(pair? i) (bulk-binding-at-scopes (car i))]
                          [else (hash-iterate-key ht i)])]
            [(binding-id) (cond
                           [(pair? i)
                            (define bulk (bulk-binding-at-bulk (car i)))
                            (define b-info (and (symbol-interned? sym) ; don't `require` non-interned
                                                (hash-ref (bulk-binding-symbols bulk s extra-shifts) sym #f)))
                            (and b-info
                                 ((bulk-binding-create bulk) bulk b-info sym))]
                           [else (hash-iterate-value ht i)])])
           #t
           #t
           ;; Next value for the index `i`:
           [(cond
             [(pair? i) (cdr i)]
             [else (or (hash-iterate-next ht i)
                       bulk-bindings)])])]])))

;; ----------------------------------------

;; Iterate through all non-bulk symbol+scope+binding combinations.
;; This iterator allocates; its intended for use in situations
;; that don't need a tight loop, which should generally be the
;; case for somethign that's inspecting all bindings.
(define-sequence-syntax in-full-non-bulk-binding-table
  (lambda () #'do-not-use-in-full-non-bulk-binding-table-as-an-expression)
  (lambda (stx)
    (syntax-case stx ()
      [[(sym-id scopes-id binding-id) (_ table-expr)]
       #'[(scopes-id binding-id)
          (:do-in
           ([(sym-ht)
             (let ([table table-expr])
               (if (hash? table)
                   table
                   (table-with-bulk-bindings-syms table)))])
           #t
           ([state (let loop ([sym-i (hash-iterate-first sym-ht)])
                     (if sym-i
                         (next-state-in-full-binding-table sym-ht sym-i)
                         '(#f . #f)))])
           (car state)
           ;; At each step, extract the current scope set and binding;
           ;; either can be #f, in which case the consumer of the
           ;; sequence should move on the the next result
           ([(sym-id) (vector-ref (car state) 1)]
            [(scopes-id) (hash-iterate-key (vector-ref (car state) 2) (cdr state))]
            [(binding-id) (hash-iterate-value (vector-ref (car state) 2) (cdr state))])
           #t
           #t
           [(let* ([ht (vector-ref (car state) 2)]
                   [i (hash-iterate-next ht (cdr state))])
              (if i
                  (cons (car state) i)
                  (next-state-in-full-binding-table sym-ht
                                                    (hash-iterate-next sym-ht (vector-ref (car state) 0)))))])]])))

(define (next-state-in-full-binding-table sym-ht sym-i)
  (if sym-i
      (let* ([ht (hash-iterate-value sym-ht sym-i)]
             [i (hash-iterate-first ht)])
        (if i
            (cons (vector sym-i
                          (hash-iterate-key sym-ht sym-i) ; symbol
                          ht)
                  i)
            (next-state-in-full-binding-table sym-ht (hash-iterate-next sym-ht sym-i))))
      '(#f . #f)))

;; ----------------------------------------

;; Return a set of symbols that have bindings for a given scope set
(define (binding-table-symbols table scs s extra-shifts)
  (define-values (ht bulk-bindings)
    (if (hash? table)
        (values table null)
        (values (table-with-bulk-bindings-syms table)
                (table-with-bulk-bindings-bulk-bindings table))))
  (set-union
   (for/seteq ([(sym at-sym) (in-hash ht)]
               #:when (for/or ([an-scs (in-hash-keys at-sym)])
                        (subset? an-scs scs)))
              sym)
   (for*/seteq ([bba (in-list bulk-bindings)]
                #:when (subset? (bulk-binding-at-scopes bba) scs)
                [sym (in-hash-keys
                      (bulk-binding-symbols (bulk-binding-at-bulk bba) s extra-shifts))])
               sym)))

;; ----------------------------------------
;; Pruning functions are called by scope serialization

(define (binding-table-prune-to-reachable bt state)
  (or (hash-ref (serialize-state-bindings-intern state) bt #f)
      (let ([reachable-scopes (serialize-state-reachable-scopes state)])
        (define new-syms
          (for*/hasheq ([(sym bindings-for-sym) (in-immutable-hash
                                                 (if (hash? bt)
                                                     bt
                                                     (table-with-bulk-bindings-syms/serialize bt)))]
                        [new-bindings-for-sym
                         (in-value
                          (for/hash ([(scopes binding) (in-immutable-hash bindings-for-sym)]
                                     #:when (subset? scopes reachable-scopes))
                            (values (intern-scopes scopes state) binding)))]
                        #:when (positive? (hash-count new-bindings-for-sym)))
            (values sym new-bindings-for-sym)))
        (define new-bulk-bindings
          (if (hash? bt)
              null
              (for/list ([bba (in-list (table-with-bulk-bindings-bulk-bindings bt))]
                         #:when (subset? (bulk-binding-at-scopes bba) reachable-scopes))
                (struct-copy bulk-binding-at bba
                             [scopes (intern-scopes (bulk-binding-at-scopes bba) state)]))))
        (define new-bt
          (if (pair? new-bulk-bindings)
              (table-with-bulk-bindings new-syms new-syms new-bulk-bindings)
              new-syms))
        (hash-set! (serialize-state-bulk-bindings-intern state) bt new-bt)
        new-bt)))

(define (binding-table-register-reachable bt get-reachable-scopes reach register-trigger)
  ;; Check symbol-specific scopes for both `free-id=?` reachability and
  ;; for implicitly reachable scopes
  (for* ([(sym bindings-for-sym) (in-immutable-hash (if (hash? bt)
                                                        bt
                                                        (table-with-bulk-bindings-syms/serialize bt)))]
         [(scopes binding) (in-immutable-hash bindings-for-sym)])
    (define v (and (binding-reach-scopes? binding)
                   ((binding-reach-scopes-ref binding) binding)))
    (scopes-register-reachable scopes v get-reachable-scopes reach register-trigger))
  ;; Need to check bulk-binding scopes for implicitly reachable
  (when (table-with-bulk-bindings? bt)
    (for ([bba (in-list (table-with-bulk-bindings-bulk-bindings bt))])
      (scopes-register-reachable (bulk-binding-at-scopes bba) #f get-reachable-scopes reach register-trigger))))

(define (scopes-register-reachable scopes v get-reachable-scopes reach register-trigger)
  (define reachable-scopes (get-reachable-scopes))
  (cond
    [(subset? scopes reachable-scopes)
     (reach v)]
    [else
     ;; There may be implicitly reachable scopes (i.e., multi-scope
     ;; representatives that should only be reachable if they
     ;; participate in a binding)
     (define pending-scopes
       (for/seteq ([sc (in-set scopes)]
                   #:unless (or (set-member? reachable-scopes sc)
                                (implicitly-reachable? sc)))
         sc))
     (define (check-trigger reach)
       (when (zero? (hash-count pending-scopes))
         ;; All scopes became reachable, so make the value reachable,
         ;; and declare implcitily reachables as explicitly reachable
         (reach v)
         (for ([sc (in-set scopes)])
           (when (implicitly-reachable? sc)
             (reach sc)))))
     (for ([sc (in-set pending-scopes)])
       (register-trigger sc (lambda (reach)
                              (set! pending-scopes (hash-remove pending-scopes sc))
                              (check-trigger reach))))
     ;; In case there were only implicitly reachable scopes:
     (check-trigger reach)]))

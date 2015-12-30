#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         (for-meta 2 racket/base)
         racket/contract/base
         racket/contract/combinator
         racket/dict
         (rename-in (except-in "private/id-table.rkt"
                               make-free-id-table
                               make-immutable-free-id-table
                               make-bound-id-table
                               make-immutable-bound-id-table
                               mutable-free-id-table?
                               immutable-free-id-table?
                               mutable-bound-id-table?
                               immutable-bound-id-table?
                               free-id-table-set
                               free-id-table-remove
                               bound-id-table-set
                               bound-id-table-remove)
                    [mutable-free-id-table mutable-free-id-table*]
                    [immutable-free-id-table immutable-free-id-table*]
                    [mutable-bound-id-table mutable-bound-id-table*]
                    [immutable-bound-id-table immutable-bound-id-table*]))

;; ========

(define dict-contract-methods
  (vector-immutable identifier?
                    any/c
                    id-table-iter?
                    #f #f #f))

(begin-for-syntax
  (define (replace old new template)
    (datum->syntax new
      (string->symbol
        (regexp-replace
          (regexp-quote old)
          (symbol->string template)
          (regexp-replace-quote (symbol->string (syntax-e new)))))))
  (define-syntax (define-templates stx)
    (syntax-case stx ()
      [(_ old new (template ...))
       #`(begin
           (define/with-syntax template (replace old new 'template)) ...)])))

;; ========

(define-struct base-id-table/c (dom rng immutable))

(define-values (id-table/c-dom-pos-proj
                id-table/c-dom-neg-proj
                id-table/c-rng-pos-proj
                id-table/c-rng-neg-proj)
  (let ()
    (define (proj acc location swap)
      (lambda (ctc blame)
        ((contract-late-neg-projection (acc ctc))
         (blame-add-context blame location #:swap? swap))))
    (values
     (proj base-id-table/c-dom "the keys of" #f)
     (proj base-id-table/c-dom "the keys of" #t)
     (proj base-id-table/c-rng "the values of" #f)
     (proj base-id-table/c-rng "the values of" #t))))

(define (make-id-table/c idtbl/c-symbol
                         idtbl?
                         mutable-idtbl?
                         immutable-idtbl?
                         immutable-idtbl)

  (define (id-table/c-name ctc)
    (apply build-compound-type-name
           idtbl/c-symbol
           (base-id-table/c-dom ctc)
           (base-id-table/c-rng ctc)
           (case (base-id-table/c-immutable ctc)
             [(dont-care) null]
             [(#t)
              (list '#:immutable #t)]
             [(#f)
              (list '#:immutable #f)])))

  (define (id-table/c-first-order ctc)
    (define dom-ctc (base-id-table/c-dom ctc))
    (define rng-ctc (base-id-table/c-rng ctc))
    (define immutable (base-id-table/c-immutable ctc))
    (λ (val)
       (and (idtbl? val)
            (case immutable
              [(#t) (immutable-idtbl? val)]
              [(#f) (mutable-idtbl? val)]
              [else #t])
            (for/and ([(k v) (in-dict val)])
              (and (contract-first-order-passes? dom-ctc k)
                   (contract-first-order-passes? rng-ctc v))))))

  (define (check-id-table/c ctc val blame neg-party)
    (define immutable (base-id-table/c-immutable ctc))
    (case immutable
      [(#t)
       (unless (immutable-idtbl? val)
         (raise-blame-error blame val #:missing-party neg-party
           '(expected "an immutable ~a," given: "~e") 'idtbl val))]
      [(#f)
       (unless (mutable-idtbl? val)
         (raise-blame-error blame val #:missing-party neg-party
           '(expected "a mutable ~a," given: "~e") 'idtbl val))]
      [(dont-care)
       (unless (idtbl? val)
         (raise-blame-error blame val #:missing-party neg-party
           '(expected "a ~a," given: "~e") 'idtbl val))]))

  (define (late-neg-fo-projection ctc)
    (λ (blame)
       (define dom-proj (id-table/c-dom-pos-proj ctc blame))
       (define rng-proj (id-table/c-rng-pos-proj ctc blame))
       (λ (val neg-party)
          (check-id-table/c ctc val blame neg-party)
          (for ([(k v) (in-dict val)])
            (dom-proj k neg-party)
            (rng-proj v neg-party))
          val)))

  (define (late-neg-ho-projection ctc)
    (lambda (blame)
      (define pos-dom-proj (id-table/c-dom-pos-proj ctc blame))
      (define neg-dom-proj (id-table/c-dom-neg-proj ctc blame))
      (define pos-rng-proj (id-table/c-rng-pos-proj ctc blame))
      (define neg-rng-proj (id-table/c-rng-neg-proj ctc blame))
      (lambda (tbl neg-party)
        (check-id-table/c ctc tbl blame neg-party)
        ;;TODO for immutable hash tables optimize this chaperone to a flat
        ;;check if possible
        (if (immutable-idtbl? tbl)
            (chaperone-immutable-id-table tbl
                                          (λ (val) (pos-dom-proj val neg-party))
                                          (λ (val) (pos-rng-proj val neg-party))
                                          impersonator-prop:contracted ctc)
            (chaperone-mutable-id-table tbl
                                        (λ (val) (neg-dom-proj val neg-party))
                                        (λ (val) (pos-dom-proj val neg-party))
                                        (λ (val) (neg-rng-proj val neg-party))
                                        (λ (val) (pos-rng-proj val neg-party))
                                        impersonator-prop:contracted ctc)))))

  (struct flat-id-table/c base-id-table/c ()
    #:omit-define-syntaxes
    #:property prop:flat-contract
    (build-flat-contract-property
     #:name id-table/c-name
     #:first-order id-table/c-first-order
     #:late-neg-projection late-neg-fo-projection))

  (struct chaperone-id-table/c base-id-table/c ()
    #:omit-define-syntaxes
    #:property prop:chaperone-contract
    (build-chaperone-contract-property
     #:name id-table/c-name
     #:first-order id-table/c-first-order
     #:late-neg-projection late-neg-ho-projection))

  ;; Note: impersonator contracts not currently supported.
  (struct impersonator-id-table/c base-id-table/c ()
    #:omit-define-syntaxes
    #:property prop:contract
    (build-contract-property
     #:name id-table/c-name
     #:first-order id-table/c-first-order
     #:late-neg-projection late-neg-ho-projection))

  (define (id-table/c key/c value/c #:immutable [immutable 'dont-care])
    (define key/ctc (coerce-contract idtbl/c-symbol key/c))
    (define value/ctc (coerce-contract idtbl/c-symbol value/c))
    (cond [(and (eq? immutable #t)
                (flat-contract? key/ctc)
                (flat-contract? value/ctc))
           (flat-id-table/c key/ctc value/ctc immutable)]
          [(chaperone-contract? value/ctc)
           (chaperone-id-table/c key/ctc value/ctc immutable)]
          [else
           (impersonator-id-table/c key/ctc value/ctc immutable)]))

  (procedure-rename id-table/c idtbl/c-symbol))

;; ========

(define-syntax (make-code stx)
  (syntax-case stx ()
    [(_ idtbl)
     (let ()
       (define-templates "idtbl" #'idtbl
         (mutable-idtbl* immutable-idtbl* mutable-idtbl immutable-idtbl
          make-idtbl make-mutable-idtbl make-immutable-idtbl
          idtbl? immutable-idtbl? mutable-idtbl?
          idtbl-hash idtbl-phase
          idtbl-ref
          idtbl-set! idtbl-set
          idtbl-remove! idtbl-remove
          idtbl-set/constructor idtbl-remove/constructor
          idtbl-set* idtbl-set*/constructor idtbl-set*! idtbl-ref!
          idtbl-update idtbl-update/constructor idtbl-update!
          idtbl-count
          idtbl-iterate-first idtbl-iterate-next
          idtbl-iterate-key idtbl-iterate-value
          idtbl-keys idtbl-values in-idtbl
          idtbl-map idtbl-for-each
          idtbl-mutable-methods idtbl-immutable-methods
          idtbl/c))
       #'(begin

           ;; Struct defs at end, so that dict methods can refer to earlier procs

           (define (make-idtbl [init-dict null]
                               #:phase [phase (syntax-local-phase-level)])
             (let ([t (mutable-idtbl (make-hasheq) phase)])
               (for ([(k v) (in-dict init-dict)])
                 (unless (identifier? k)
                   (raise-type-error 'make-idtbl
                                     "dictionary with identifier keys" init-dict))
                 (idtbl-set! t k v))
               t))

           (define (make-immutable-idtbl [init-dict null]
                                         #:phase [phase (syntax-local-phase-level)])
             (for/fold ([t (immutable-idtbl '#hasheq() phase)])
                 ([(k v) (in-dict init-dict)])
               (unless (identifier? k)
                 (raise-type-error 'make-immutable-idtbl
                                   "dictionary with identifier keys" init-dict))
               (idtbl-set t k v)))

           ;; Replace to use new constructor
           (define (idtbl-set d id v)
             (idtbl-set/constructor d id v immutable-idtbl))
           (define (idtbl-remove d id)
             (idtbl-remove/constructor d id immutable-idtbl))
           (define (idtbl-set* d . rst)
             (apply idtbl-set*/constructor d immutable-idtbl rst))
           (define not-given (gensym 'not-given))
           (define (idtbl-update d id updater [default not-given])
             (if (eq? default not-given)
                 (idtbl-update/constructor d id updater immutable-idtbl)
                 (idtbl-update/constructor d id updater immutable-idtbl default)))
           (define idtbl-immutable-methods
             (vector-immutable idtbl-ref
                               #f
                               idtbl-set
                               #f
                               idtbl-remove
                               idtbl-count
                               idtbl-iterate-first
                               idtbl-iterate-next
                               idtbl-iterate-key
                               idtbl-iterate-value))

           (struct mutable-idtbl mutable-idtbl* ()
             #:property prop:dict/contract
             (list idtbl-mutable-methods
                   dict-contract-methods))
           (struct immutable-idtbl immutable-idtbl* ()
             #:property prop:dict/contract
             (list idtbl-immutable-methods
                   dict-contract-methods))

           (define idtbl/c
             (make-id-table/c 'idtbl/c
                              idtbl? mutable-idtbl? immutable-idtbl?
                              immutable-idtbl))

           (provide/contract
            [make-idtbl
             (->* () (dict? #:phase (or/c #f exact-integer?)) mutable-idtbl?)]
            [make-immutable-idtbl
             (->* () (dict? #:phase (or/c #f exact-integer?)) immutable-idtbl?)]
            [idtbl?
             (-> any/c boolean?)]
            [mutable-idtbl?
             (-> any/c boolean?)]
            [immutable-idtbl?
             (-> any/c boolean?)]
            [idtbl-ref
             (->* (idtbl? identifier?) (any/c) any)]
            [idtbl-set!
             (-> mutable-idtbl? identifier? any/c void?)]
            [idtbl-set
             (-> immutable-idtbl? identifier? any/c immutable-idtbl?)]
            [idtbl-remove!
             (-> mutable-idtbl? identifier? void?)]
            [idtbl-remove
             (-> immutable-idtbl? identifier? immutable-idtbl?)]
            [idtbl-set*
             (->* [immutable-idtbl?]
                  #:rest (flat-rec-contract key-value-pairs
                           (or/c null
                                 (cons/c identifier? (cons/c any/c key-value-pairs))))
                  immutable-idtbl?)]
            [idtbl-set*!
             (->* [mutable-idtbl?]
                  #:rest (flat-rec-contract key-value-pairs
                           (or/c null
                                 (cons/c identifier? (cons/c any/c key-value-pairs))))
                  void?)]
            [idtbl-ref!
             (-> mutable-idtbl? identifier? any/c any)]
            [idtbl-update
             (->* [immutable-idtbl? identifier? (-> any/c any/c)]
                  [any/c]
                  immutable-idtbl?)]
            [idtbl-update!
             (->* [mutable-idtbl? identifier? (-> any/c any/c)]
                  [any/c]
                  void?)]
            [idtbl-count
             (-> idtbl? exact-nonnegative-integer?)]
            [idtbl-iterate-first
             (-> idtbl? (or/c #f id-table-iter?))]
            [idtbl-iterate-next
             (-> idtbl? id-table-iter? (or/c #f id-table-iter?))]
            [idtbl-iterate-key
             (-> idtbl? id-table-iter? identifier?)]
            [idtbl-iterate-value
             (-> idtbl? id-table-iter? any)]
            [idtbl-keys
             (-> idtbl? (listof identifier?))]
            [idtbl-values
             (-> idtbl? list?)]
            [in-idtbl
             (-> idtbl? sequence?)]
            [idtbl-map
             (-> idtbl? (-> identifier? any/c any) list?)]
            [idtbl-for-each
             (-> idtbl? (-> identifier? any/c any) any)]
            [idtbl/c
             (->* (flat-contract? chaperone-contract?)
                  (#:immutable (or/c 'dont-care #t #f))
                  contract?)])))]))

(make-code bound-id-table)
(make-code free-id-table)

(provide/contract
 [id-table-iter? (-> any/c boolean?)])

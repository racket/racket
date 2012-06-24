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
          idtbl-count
          idtbl-iterate-first idtbl-iterate-next
          idtbl-iterate-key idtbl-iterate-value
          idtbl-map idtbl-for-each
          idtbl-mutable-methods idtbl-immutable-methods
          idtbl/c
          chaperone-idtbl idtbl-chaperone-keys+values/constructor))
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

           (define-struct base-idtbl/c (dom rng immutable))

           (define (idtbl/c-name ctc)
             (apply
              build-compound-type-name
              'idtbl/c (base-idtbl/c-dom ctc) (base-idtbl/c-rng ctc)
              (append
               (if (flat-idtbl/c? ctc)
                   (list '#:flat? #t)
                   null)
               (case (base-idtbl/c-immutable ctc)
                 [(dont-care) null]
                 [(#t)
                  (list '#:immutable #t)]
                 [(#f)
                  (list '#:immutable #f)]))))

           (define-values (idtbl/c-dom-pos-proj
                           idtbl/c-dom-neg-proj
                           idtbl/c-rng-pos-proj
                           idtbl/c-rng-neg-proj)
             (let ()
               (define (proj acc location swap)
                 (lambda (ctc blame)
                   ((contract-projection (acc ctc))
                    (blame-add-context blame "the keys of" #:swap swap))))
               (values
                 (proj base-idtbl/c-dom "the keys of" #f)
                 (proj base-idtbl/c-dom "the keys of" #t)
                 (proj base-idtbl/c-rng "the values of" #f)
                 (proj base-idtbl/c-rng "the values of" #t))))

           (define (idtbl/c-first-order ctc)
             (define dom-ctc (base-idtbl/c-dom ctc))
             (define rng-ctc (base-idtbl/c-rng ctc))
             (define immutable (base-idtbl/c-immutable ctc))
             (λ (val)
               (and (idtbl? val)
                    (case immutable
                      [(#t) (immutable? val)]
                      [(#f) (not (immutable? val))]
                      [else #t])
                    (for/and ([(k v) (in-dict val)])
                      (and (contract-first-order-passes? dom-ctc k)
                           (contract-first-order-passes? rng-ctc v))))))

           (define (check-idtbl/c ctc val blame)
             (define immutable (base-idtbl/c-immutable ctc))
             (unless (idtbl? val)
               (raise-blame-error blame val
                 '(expected "a ~a," given: "~e") 'idtbl val))
             (case immutable
               [(#t)
                (unless (immutable? val)
                  (raise-blame-error blame val
                    '(expected "an immutable ~a," given: "~e") 'idtbl val))]
               [(#f)
                (when (immutable? val)
                  (raise-blame-error blame val
                    '(expected "a mutable ~a," given: "~e") 'idtbl val))]
               [(dont-care) (void)]))

           (define ho-projection
             (lambda (ctc)
               (lambda (blame)
                (lambda (b)
                  (define pos-dom-proj (idtbl/c-dom-pos-proj ctc blame))
                  (define neg-dom-proj (idtbl/c-dom-pos-proj ctc blame))
                  (define pos-rng-proj (idtbl/c-dom-pos-proj ctc blame))
                  (define neg-rng-proj (idtbl/c-dom-pos-proj ctc blame))
                  (lambda (tbl)
                    (check-idtbl/c ctc tbl blame)
                    (if (immutable? tbl)
                      (idtbl-chaperone-keys+values/constructor
                        tbl pos-dom-proj pos-rng-proj immutable-idtbl)
                      (chaperone-idtbl tbl
                        (λ (t k)
                          (values (neg-dom-proj k)
                                  (λ (h k v)
                                    (pos-rng-proj v))))
                        (λ (t k v)
                          (values (neg-dom-proj k)
                                  (neg-rng-proj v)))
                        (λ (t k)
                          (neg-dom-proj k))
                        (λ (t k)
                          (pos-dom-proj k))
                        impersonator-prop:contracted ctc)))))))



           (struct flat-idtbl/c base-idtbl/c ()
             #:omit-define-syntaxes
             #:property prop:flat-contract
             (build-flat-contract-property
              #:name idtbl/c-name
              #:first-order idtbl/c-first-order
              #:projection
              (λ (ctc)
                (λ (blame)
                  (λ (val)
                    (check-idtbl/c ctc val blame)
                    (define dom-proj (idtbl/c-dom-pos-proj ctc))
                    (define rng-proj (idtbl/c-rng-pos-proj ctc))
                    (for ([(k v) (in-dict val)])
                      (dom-proj k)
                      (rng-proj v))
                    val)))))

           (struct chaperone-idtbl/c base-idtbl/c ()
             #:omit-define-syntaxes
             #:property prop:chaperone-contract
             (build-chaperone-contract-property
              #:name idtbl/c-name
              #:first-order idtbl/c-first-order
              #:projection ho-projection))

           (struct impersonator-idtbl/c base-idtbl/c ()
             #:omit-define-syntaxes
             #:property prop:contract
             (build-contract-property
              #:name idtbl/c-name
              #:first-order idtbl/c-first-order
              #:projection ho-projection))


           (struct mutable-idtbl mutable-idtbl* ()
             #:property prop:dict/contract
             (list idtbl-mutable-methods
                   dict-contract-methods))
           (struct immutable-idtbl immutable-idtbl* ()
             #:property prop:dict/contract
             (list idtbl-immutable-methods
                   dict-contract-methods))

           (define (idtbl/c key/c value/c #:immutable (immutable 'dont-care))
             (define key/ctc (coerce-contract 'idtbl/c key/c))
             (define value/ctc (coerce-contract 'idtbl/c value/c))
             (cond
              ((and (eq? immutable #t)
                    (flat-contract? key/ctc)
                    (flat-contract? value/ctc))
               (flat-idtbl/c key/ctc value/ctc immutable))
              ((chaperone-contract? value/ctc)
               (chaperone-idtbl/c key/ctc value/ctc immutable))
              (else
               (impersonator-idtbl/c key/ctc value/ctc immutable))))

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
            [idtbl-map
             (-> idtbl? (-> identifier? any/c any) list?)]
            [idtbl-for-each
             (-> idtbl? (-> identifier? any/c any) any)]
            [idtbl/c
             (->* (chaperone-contract? contract?)
                  (#:immutable (or/c 'dont-care #t #f))
                  contract?)])))]))

(make-code bound-id-table)
(make-code free-id-table)

(provide/contract
 [id-table-iter? (-> any/c boolean?)])

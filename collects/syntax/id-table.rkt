#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/contract/base
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

(define-syntax (make-code stx)
  (syntax-case stx ()
    [(_ idtbl)
     (with-syntax ([mutable-idtbl
                    (format-id #'idtbl "mutable-~a" (syntax-e #'idtbl))]
                   [immutable-idtbl
                    (format-id #'idtbl "immutable-~a" (syntax-e #'idtbl))]
                   [mutable-idtbl*
                    (format-id #'idtbl "mutable-~a*" (syntax-e #'idtbl))]
                   [immutable-idtbl*
                    (format-id #'idtbl "immutable-~a*" (syntax-e #'idtbl))]
                   [make-idtbl
                    (format-id #'idtbl "make-~a" (syntax-e #'idtbl))]
                   [make-mutable-idtbl
                    (format-id #'idtbl "make-mutable-~a" (syntax-e #'idtbl))]
                   [make-immutable-idtbl
                    (format-id #'idtbl "make-immutable-~a" (syntax-e #'idtbl))]
                   [mutable-idtbl?
                    (format-id #'idtbl "mutable-~a?" (syntax-e #'idtbl))]
                   [immutable-idtbl?
                    (format-id #'idtbl "immutable-~a?" (syntax-e #'idtbl))])
       (define (s x) (format-id #'idtbl "~a~a" (syntax-e #'idtbl) x))
       (with-syntax ([idtbl? (s '?)]
                     [idtbl-hash (s '-hash)]
                     [idtbl-phase (s '-phase)]
                     [idtbl-ref (s '-ref)]
                     [idtbl-set! (s '-set!)]
                     [idtbl-set (s '-set)]
                     [idtbl-remove! (s '-remove!)]
                     [idtbl-remove (s '-remove)]
                     [idtbl-set/constructor (s '-set/constructor)]
                     [idtbl-remove/constructor (s '-remove/constructor)]
                     [idtbl-count (s '-count)]
                     [idtbl-iterate-first (s '-iterate-first)]
                     [idtbl-iterate-next (s '-iterate-next)]
                     [idtbl-iterate-key (s '-iterate-key)]
                     [idtbl-iterate-value (s '-iterate-value)]
                     [idtbl-map (s '-map)]
                     [idtbl-for-each (s '-for-each)]
                     [idtbl-mutable-methods (s '-mutable-methods)]
                     [idtbl-immutable-methods (s '-immutable-methods)])
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

             (struct mutable-idtbl mutable-idtbl* ()
               #:property prop:dict/contract
               (list idtbl-mutable-methods
                     dict-contract-methods))
             (struct immutable-idtbl immutable-idtbl* ()
               #:property prop:dict/contract
               (list idtbl-immutable-methods
                     dict-contract-methods))

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
               (-> idtbl? (-> identifier? any/c any) any)]))))]))

(make-code bound-id-table)
(make-code free-id-table)

(provide/contract
 [id-table-iter? (-> any/c boolean?)])

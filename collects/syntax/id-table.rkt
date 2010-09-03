#lang racket/base
(require (for-syntax scheme/base)
         racket/contract/base
         racket/dict
         "private/id-table.ss")
#|
(provide id-table-position?)

(define id-table-position/c
  (flat-named-contract "id-table-position or false"
                       (lambda (x) (or (id-table-position? x)
                                       (eq? x #f)))))
|#

(define-for-syntax (format-id stx fmt . args)
  (datum->syntax stx (string->symbol (apply format fmt args))))

(define-syntax (make-code stx)
  (syntax-case stx ()
    [(_ idtbl)
     (with-syntax ([make-idtbl
                    (format-id #'idtbl "make-~a" (syntax-e #'idtbl))]
                   [make-immutable-idtbl
                    (format-id #'idtbl "make-immutable-~a" (syntax-e #'idtbl))]
                   [mutable-idtbl?
                    (format-id #'idtbl "mutable-~a?" (syntax-e #'idtbl))]
                   [immutable-idtbl?
                    (format-id #'idtbl "immutable-~a?" (syntax-e #'idtbl))])
       (define (s x) (format-id #'idtbl "~a~a" (syntax-e #'idtbl) x))
       (with-syntax ([idtbl? (s '?)]
                     [idtbl-ref (s '-ref)]
                     [idtbl-set! (s '-set!)]
                     [idtbl-set (s '-set)]
                     [idtbl-remove! (s '-remove!)]
                     [idtbl-remove (s '-remove)]
                     [idtbl-count (s '-count)]
                     [idtbl-iterate-first (s '-iterate-first)]
                     [idtbl-iterate-next (s '-iterate-next)]
                     [idtbl-iterate-key (s '-iterate-key)]
                     [idtbl-iterate-value (s '-iterate-value)]
                     [idtbl-map (s '-map)]
                     [idtbl-for-each (s '-for-each)])
         #'(begin
             (provide idtbl?
                      mutable-idtbl?
                      immutable-idtbl?)
             (provide/contract
              [make-idtbl (->* () (dict? #:phase (or/c exact-integer? #f)) any)]
              [make-immutable-idtbl (->* () (dict? #:phase (or/c exact-integer? #f)) any)]
              [idtbl-ref (->* (idtbl? any/c) (any/c)
                              any)]
              [idtbl-set! (-> mutable-idtbl? any/c any/c
                              any)]
              [idtbl-set (-> immutable-idtbl? any/c any/c
                             immutable-idtbl?)]
              [idtbl-remove! (-> mutable-idtbl? any/c
                                 any)]
              [idtbl-remove (-> immutable-idtbl? any/c
                                immutable-idtbl?)]
              [idtbl-count (-> idtbl? exact-nonnegative-integer?)]
              #|
              [idtbl-iterate-first (-> idtbl? id-table-position/c)]
              [idtbl-iterate-next (-> idtbl? id-table-position/c id-table-position/c)]
              [idtbl-iterate-key (-> idtbl? id-table-position/c identifier?)]
              [idtbl-iterate-value (-> idtbl? id-table-position/c any)]
              |#
              [idtbl-map (-> idtbl? (-> any/c any/c any) any)]
              [idtbl-for-each (-> idtbl? (-> any/c any/c any) any)]))))]))

(make-code bound-id-table)
(make-code free-id-table)
;; (make-code free*-id-table)

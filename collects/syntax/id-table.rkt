#lang racket/base
(require (for-syntax racket/base
                     unstable/syntax)
         racket/contract/base
         racket/dict)

(define-struct id-table-iter (a b))

;; ========

(define (alist-set identifier=? phase l0 id v)
  ;; To minimize allocation
  ;;   - add new pairs to front
  ;;   - avoid allocation on idempotent sets
  (let* ([not-found? #f]
         [new-l
          (let loop ([l l0])
            (cond [(null? l) (begin (set! not-found? #t) null)]
                  [(identifier=? (caar l) id phase)
                   (if (eq? v (cdar l)) ;; idempotent; just leave it alone
                       l
                       (cons (cons id v) (cdr l)))]
                  [else
                   (let ([rest* (loop (cdr l))])
                     (if (eq? (cdr l) rest*)
                         l
                         (cons (car l) rest*)))]))])
    (if not-found?
        (cons (cons id v) l0)
        new-l)))

(define (alist-remove identifier=? phase l0 id)
  ;; To minimize allocation
  ;;   - avoid allocation on idempotent removes
  (let loop ([l l0])
    (cond [(null? l) null]
          [(identifier=? (caar l) id phase)
           (cdr l)]
          [else
           (let ([rest* (loop (cdr l))])
             (if (eq? (cdr l) rest*)
                 l
                 (cons (car l) rest*)))])))

(define not-given (gensym 'not-given))

;; ========

(define-syntax (make-code stx)
  (syntax-case stx ()
    [(_ idtbl
        identifier->symbol
        identifier=?)
     (with-syntax ([mutable-idtbl
                    (format-id #'idtbl "mutable-~a" (syntax-e #'idtbl))]
                   [immutable-idtbl
                    (format-id #'idtbl "immutable-~a" (syntax-e #'idtbl))]
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
                     [idtbl-count (s '-count)]
                     [idtbl-iterate-first (s '-iterate-first)]
                     [idtbl-iterate-next (s '-iterate-next)]
                     [idtbl-iterate-key (s '-iterate-key)]
                     [idtbl-iterate-value (s '-iterate-value)]
                     [idtbl-map (s '-map)]
                     [idtbl-for-each (s '-for-each)])
         #'(begin

             ;; Struct defs at end, so that dict methods can refer to earlier procs

             (define (make-idtbl [init-dict null]
                                 #:phase [phase (syntax-local-phase-level)])
               ;; init-dict is good candidate for object/c like dict/c
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

             (define (idtbl-ref d id [default not-given])
               (let ([phase (idtbl-phase d)])
                 (let ([i (ormap (lambda (i) (and (identifier=? (car i) id phase) i))
                                 (hash-ref (idtbl-hash d)
                                           (identifier->symbol id phase)
                                           null))])
                   (if i
                       (cdr i)
                       (cond [(eq? default not-given)
                              (error 'idtbl-ref "no mapping for ~e" id)]
                             [(procedure? default) (default)]
                             [else default])))))

             (define (idtbl-set! d id v)
               (let* ([phase (idtbl-phase d)]
                      [sym (identifier->symbol id phase)]
                      [l (hash-ref (idtbl-hash d) sym null)])
                 (hash-set! (idtbl-hash d)
                            sym
                            (alist-set identifier=? phase l id v))))

             (define (idtbl-set d id v)
               (let* ([phase (idtbl-phase d)]
                      [sym (identifier->symbol id phase)]
                      [l (hash-ref (idtbl-hash d) sym null)])
                 (immutable-idtbl
                  (hash-set (idtbl-hash d)
                            sym
                            (alist-set identifier=? phase l id v))
                  phase)))

             (define (idtbl-remove! d id)
               (let* ([phase (idtbl-phase d)]
                      [sym (identifier->symbol id phase)]
                      [l (hash-ref (idtbl-hash d) sym null)]
                      [newl (alist-remove identifier=? phase l id)])
                 (if (pair? newl)
                     (hash-set! (idtbl-hash d) sym newl)
                     (hash-remove! (idtbl-hash d) sym))))

             (define (idtbl-remove d id)
               (let* ([phase (idtbl-phase d)]
                      [sym (identifier->symbol id phase)]
                      [l (hash-ref (idtbl-hash d) sym null)]
                      [newl (alist-remove identifier=? phase l id)])
                 (immutable-idtbl
                  (if (pair? newl)
                      (hash-set (idtbl-hash d) sym newl)
                      (hash-remove (idtbl-hash d) sym))
                  phase)))

             (define (idtbl-count d)
               (apply + (hash-map (idtbl-hash d) (lambda (k v) (length v)))))

             (define (idtbl-for-each d p)
               (define (pp i) (p (car i) (cdr i)))
               (hash-for-each (idtbl-hash d)
                              (lambda (k v) (for-each pp v))))

             (define (idtbl-map d f)
               (define (fp i) (f (car i) (cdr i)))
               (apply append
                      (hash-map (idtbl-hash d)
                                (lambda (k v) (map fp v)))))

             (define (idtbl-iterate-first d)
               (let ([h (idtbl-hash d)])
                 (let ([a (dict-iterate-first h)])
                   (and a
                        (let ([b (dict-iterate-first (dict-iterate-value h a))])
                          (and b (make-id-table-iter a b)))))))

             (define (idtbl-iterate-next d pos)
               (let ([h (idtbl-hash d)]
                     [a (id-table-iter-a pos)]
                     [b (id-table-iter-b pos)])
                 (let ([v (dict-iterate-value h a)])
                   (let ([b2 (dict-iterate-next v b)])
                     (if b2
                         (make-id-table-iter a b2)
                         (let ([a2 (dict-iterate-next h a)])
                           (and a2
                                (let ([b2 (dict-iterate-first
                                           (dict-iterate-value h a2))])
                                  (and b2 (make-id-table-iter a2 b2))))))))))

             (define (idtbl-iterate-key d pos)
               (let ([h (idtbl-hash d)]
                     [a (id-table-iter-a pos)]
                     [b (id-table-iter-b pos)])
                 (dict-iterate-key (dict-iterate-value h a) b)))

             (define (idtbl-iterate-value d pos)
               (let ([h (idtbl-hash d)]
                     [a (id-table-iter-a pos)]
                     [b (id-table-iter-b pos)])
                 (dict-iterate-value (dict-iterate-value h a) b)))

             (struct idtbl (hash phase))
             (struct mutable-idtbl idtbl ()
               #:property prop:dict/contract
               (list (vector-immutable idtbl-ref
                                       idtbl-set!
                                       #f
                                       idtbl-remove!
                                       #f
                                       idtbl-count
                                       idtbl-iterate-first
                                       idtbl-iterate-next
                                       idtbl-iterate-key
                                       idtbl-iterate-value)
                     (vector-immutable identifier?
                                       any/c
                                       id-table-iter?
                                       #f #f #f)))
             (struct immutable-idtbl idtbl ()
               #:property prop:dict/contract
               (list (vector-immutable idtbl-ref
                                       #f
                                       idtbl-set
                                       #f
                                       idtbl-remove
                                       idtbl-count
                                       idtbl-iterate-first
                                       idtbl-iterate-next
                                       idtbl-iterate-key
                                       idtbl-iterate-value)
                     (vector-immutable identifier?
                                       any/c
                                       id-table-iter?
                                       #f #f #f)))

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

(define (bound-identifier->symbol id phase) (syntax-e id))

(make-code bound-id-table
           bound-identifier->symbol
           bound-identifier=?)

(define (free-identifier->symbol id phase)
  (let ([binding (identifier-binding id phase)])
    (if (pair? binding)
        (cadr binding)
        (syntax-e id))))

(make-code free-id-table
           free-identifier->symbol
           free-identifier=?)

(provide/contract
 [id-table-iter? (-> any/c boolean?)])

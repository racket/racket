#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         (for-meta 2 racket/base)
         racket/private/dict
         racket/promise)

;; No-contract version.

(define-struct id-table (hash phase))
;; where hash maps symbol => (nonempty-listof (cons identifier value))
;;       phase is a phase-level (integer or #f)

(define (make-id-table-hash-code identifier->symbol)
  (lambda (d hash-code)
    (let ([phase (id-table-phase d)])
      (+ (hash-code phase)
         (for/sum (((k v) (in-dict d)))
           (* (hash-code (identifier->symbol k phase)) (hash-code v)))))))

(define (make-id-table-equal? idtbl-count idtbl-ref)
  (lambda (left right equal?)
    ;; gen:equal+hash guarantees that left, right are same kind of hash
    (and (equal? (id-table-phase left) (id-table-phase right))
         (equal? (idtbl-count left) (idtbl-count right))
         (let/ec k
           (for*/and ([l-alist (in-hash-values (id-table-hash left))]
                      [entry (in-list l-alist)])
             (equal? (idtbl-ref right (car entry) (lambda () (k #f))) (cdr entry)))))))

#|
prop:id-table-impersonator : (vector wrapped-id-table key-in key-out value-in value-out)
The {key,value}-{in-out} functions should all return a chaperone of their argument.
|#
(define-values (prop:id-table-impersonator
                id-table-impersonator?
                id-table-impersonator-value)
  (make-impersonator-property 'id-table-impersonator))

(define (chaperone-mutable-id-table d key-in key-out value-in value-out . args)
  (apply chaperone-struct d
         ;; FIXME: chaperone-struct currently demands at least one orig-proc+redirect-proc pair
         id-table-phase (lambda (d p) p)
         prop:id-table-impersonator (vector d key-in key-out value-in value-out)
         args))

(define (chaperone-immutable-id-table d wrap-key wrap-value . args)
  (apply chaperone-struct d
         id-table-hash
         (let ([hash (for/hasheq ([(sym alist) (id-table-hash d)])
                       (values sym
                               (for/list ([entry (in-list alist)])
                                 (cons (wrap-key (car entry)) (wrap-value (cdr entry))))))])
           (lambda (d v) hash))
         args))

;; ========

(define (make-id-table/constructor who init-dict phase make identifier->symbol identifier=?)
  (let ([t (make (make-hasheq) phase)])
    (for ([(k v) (in-dict init-dict)])
      (unless (identifier? k)
        (raise-type-error who "dictionary with identifier keys" init-dict))
      (id-table-set! who t k v identifier->symbol identifier=?))
    t))

(define (make-immutable-id-table/constructor who init-dict phase make identifier->symbol identifier=?)
  (for/fold ([t (make '#hasheq() phase)])
      ([(k v) (in-dict init-dict)])
    (unless (identifier? k)
      (raise-type-error who "dictionary with identifier keys" init-dict))
    (id-table-set/constructor who t k v make identifier->symbol identifier=?)))

;; ========

(define (id-table-ref who d id default identifier->symbol identifier=?)
  (let do-ref ([d d] [id id] [escape #f])
    (if (id-table-impersonator? d)
        (let-values ([(wrapped key-in key-out value-in value-out)
                      (vector->values (id-table-impersonator-value d))])
          (let/ec k
            (value-out (do-ref wrapped (key-in id) (or escape k)))))
        (let ([phase (id-table-phase d)])
          (let* ([sym (identifier->symbol id phase)]
                 [l (hash-ref (id-table-hash d) sym null)]
                 [i (for/first ([i (in-list l)] #:when (identifier=? (car i) id phase)) i)])
            (if i
                (cdr i)
                (cond [(eq? default not-given)
                       (error who "no mapping for ~e" id)]
                      [(procedure? default) ((or escape values) (default))]
                      [else ((or escape values) default)])))))))

(define (id-table-set! who d id v identifier->symbol identifier=?)
  (let do-set! ([d d] [id id] [v v])
    (if (id-table-impersonator? d)
        (let-values ([(wrapped key-in key-out value-in value-out)
                      (vector->values (id-table-impersonator-value d))])
          (do-set! wrapped (key-in id) (value-in v)))
        (let* ([phase (id-table-phase d)]
               [sym (identifier->symbol id phase)]
               [l (hash-ref (id-table-hash d) sym null)]
               [new-l (alist-set identifier=? phase l id v)])
          (hash-set! (id-table-hash d) sym new-l)))))

(define (id-table-remove! who d id identifier->symbol identifier=?)
  (let do-remove! ([d d] [id id])
    (if (id-table-impersonator? d)
        (let-values ([(wrapped key-in key-out value-in value-out)
                      (vector->values (id-table-impersonator-value d))])
          (do-remove! wrapped (key-in id)))
        (let* ([phase (id-table-phase d)]
               [sym (identifier->symbol id phase)]
               [l (hash-ref (id-table-hash d) sym null)]
               [newl (alist-remove identifier=? phase l id)])
          (if (pair? newl)
              (hash-set! (id-table-hash d) sym newl)
              (hash-remove! (id-table-hash d) sym))))))

(define (id-table-set/constructor who d id v constructor identifier->symbol identifier=?)
  (let* ([phase (id-table-phase d)]
         [sym (identifier->symbol id phase)]
         [l (hash-ref (id-table-hash d) sym null)]
         [new-l (alist-set identifier=? phase l id v)])
    (constructor (hash-set (id-table-hash d) sym new-l)
                 phase)))

(define (id-table-remove/constructor who d id constructor identifier->symbol identifier=?)
  (let* ([phase (id-table-phase d)]
         [sym (identifier->symbol id phase)]
         [l (hash-ref (id-table-hash d) sym null)]
         [newl (alist-remove identifier=? phase l id)])
    (constructor
     (if (pair? newl)
         (hash-set (id-table-hash d) sym newl)
         (hash-remove (id-table-hash d) sym))
     phase)))

(define (id-table-count d)
  (for/sum ([(k v) (in-hash (id-table-hash d))])
     (length v)))

(define-struct id-table-iter (d a br b))
;; where d is an id-table
;;       a is hash iter
;;       br is non-empty-alist "root"
;;       b is non-empty-alist current position, b = (list-tail br N) for some N

#|
For mutable table, the alist iter can be invalidated because we replace entire
alist on update. So, we store alist root and if root has been replaced, we
try to re-find the previous position in the new alist (see rebase-iter).
Not an issue for immutable tables.

Notes (FIXME?):
- we don't (can't?) check that hash iter is okay (would require hash revision?)
  so internal errors raised by hash-iterate-{next,value} can slip out
|#

(define (id-table-iterate-first d)
  (let* ([h (id-table-hash d)]
         [a (hash-iterate-first h)])
    (and a
         (let ([b (hash-iterate-value h a)])
           (make-id-table-iter d a b b)))))

(define (id-table-iterate-next who d pos)
  (let-values ([(h a br b) (rebase-iter who d pos)])
    (let ([b2 (cdr b)])
      (if (pair? b2)
          (make-id-table-iter d a br b2)
          (let ([a2 (hash-iterate-next h a)])
            (and a2
                 (let ([b2 (hash-iterate-value h a2)])
                   (make-id-table-iter d a2 b2 b2))))))))

(define (id-table-iterate-key who d pos)
  (if (id-table-impersonator? d)
      (let-values ([(wrapped key-in key-out value-in value-out)
                    (vector->values (id-table-impersonator-value d))])
        (key-out (id-table-iterate-key who wrapped pos)))
      (let-values ([(h a br b) (rebase-iter who d pos)])
        (caar b))))

;; TODO figure out how to provide API compatibility with hashes with regards
;; to iterate-key and provide the checking from rebase-iter
(define (id-table-iterate-value who d pos identifier->symbol identifier=?)
  (let do-iterate-value ([d d])
    (if (id-table-impersonator? d)
        (let-values ([(wrapped key-in key-out value-in value-out)
                      (vector->values (id-table-impersonator-value d))])
          (value-out (do-iterate-value wrapped)))
        (let-values ([(h a br b) (rebase-iter who d pos)])
          (cdar b)))))

(define (rebase-iter who d pos)
  (unless (chaperone-of? (id-table-iter-d pos) d)
    (error who "invalid iteration position for identifier table"))
  (let* ([h (id-table-hash d)]
         [a (id-table-iter-a pos)]
         [br (id-table-iter-br pos)]
         [b (id-table-iter-b pos)]
         [v (hash-iterate-value h a)]) ;; FIXME: may expose internal error
    (if (eq? br v)
        (values h a br b)
        ;; hash entry has changed from br to v, so find (caar b) in v
        (let ([id (caar b)])
          (let loop ([v* v])
            (cond [(null? v*)
                   (error who "invalid iteration position for identifier table")]
                  [(eq? (caar v*) id) ;; relies on id staying same; see alist-set
                   (values h a v v*)]
                  [else (loop (cdr v*))]))))))

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
                       ;; Note: use (caar l) instead of id so that rebase-iter
                       ;; above can find entry by stxobj identity.
                       (cons (cons (caar l) v) (cdr l)))]
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
    [(_ idtbl
        identifier->symbol
        identifier=?)
     (let ()
       (define-templates "idtbl" #'idtbl
         (mutable-idtbl immutable-idtbl
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
          idtbl-mutable-methods idtbl-immutable-methods))
       #'(begin

           ;; Struct defs at end, so that dict methods can refer to earlier procs

           (define (make-idtbl [init-dict null]
                               #:phase [phase (syntax-local-phase-level)])
             (make-id-table/constructor 'make-idtbl init-dict phase mutable-idtbl
                                        identifier->symbol identifier=?))

           (define (make-immutable-idtbl [init-dict null]
                                         #:phase [phase (syntax-local-phase-level)])
             (make-immutable-id-table/constructor 'make-immutable-idtbl init-dict phase immutable-idtbl
                                                  identifier->symbol identifier=?))

           (define (idtbl-ref d id [default not-given])
             (id-table-ref 'idtbl-ref d id default identifier->symbol identifier=?))
           (define (idtbl-set! d id v)
             (id-table-set! 'idtbl-set! d id v identifier->symbol identifier=?))
           (define (idtbl-set/constructor d id v constructor)
             (id-table-set/constructor 'idtbl-set d id v constructor identifier->symbol identifier=?))
           (define (idtbl-set d id v)
             (idtbl-set/constructor d id v immutable-idtbl))
           (define (idtbl-remove! d id)
             (id-table-remove! 'idtbl-remove! d id identifier->symbol identifier=?))

           (define (idtbl-remove/constructor d id constructor)
             (id-table-remove/constructor 'idtbl-remove d id constructor identifier->symbol identifier=?))
           (define (idtbl-remove d id)
             (idtbl-remove/constructor d id immutable-idtbl))
           (define (idtbl-count d)
             (id-table-count d))
           (define (idtbl-for-each d p)
             (dict-for-each d p))
           (define (idtbl-map d f)
             (dict-map d f))
           (define (idtbl-iterate-first d)
             (id-table-iterate-first d))
           (define (idtbl-iterate-next d pos)
             (id-table-iterate-next 'idtbl-iterate-next d pos))
           (define (idtbl-iterate-key d pos)
             (id-table-iterate-key 'idtbl-iterate-key d pos))
           (define (idtbl-iterate-value d pos)
             (id-table-iterate-value 'idtbl-iterate-value d pos identifier->symbol identifier=?))

           (define idtbl-mutable-methods
             (vector-immutable idtbl-ref
                               idtbl-set!
                               #f
                               idtbl-remove!
                               #f
                               id-table-count
                               idtbl-iterate-first
                               idtbl-iterate-next
                               idtbl-iterate-key
                               idtbl-iterate-value))

           (define idtbl-immutable-methods
             (vector-immutable idtbl-ref
                               #f
                               idtbl-set
                               #f
                               idtbl-remove
                               id-table-count
                               idtbl-iterate-first
                               idtbl-iterate-next
                               idtbl-iterate-key
                               idtbl-iterate-value))

           (struct idtbl id-table ())
           (struct mutable-idtbl idtbl ()
             #:property prop:dict idtbl-mutable-methods
             #:property prop:equal+hash
                        (let ([hash-code (make-id-table-hash-code identifier->symbol)]
                              [eql (make-id-table-equal? idtbl-count idtbl-ref)])
                          (list eql hash-code hash-code)))

           (struct immutable-idtbl idtbl ()
             #:property prop:dict idtbl-immutable-methods
             #:property prop:equal+hash
                        (let ([hash-code (make-id-table-hash-code identifier->symbol)]
                              [eql (make-id-table-equal? idtbl-count idtbl-ref)])
                          (list eql hash-code hash-code)))

           (provide make-idtbl
                    make-immutable-idtbl
                    idtbl?
                    mutable-idtbl?
                    immutable-idtbl?
                    idtbl-ref
                    idtbl-set!
                    idtbl-set
                    idtbl-remove!
                    idtbl-remove
                    idtbl-count
                    idtbl-iterate-first
                    idtbl-iterate-next
                    idtbl-iterate-key
                    idtbl-iterate-value
                    idtbl-map
                    idtbl-for-each

                    ;; just for use/extension by syntax/id-table
                    idtbl-set/constructor
                    idtbl-remove/constructor
                    idtbl-mutable-methods
                    mutable-idtbl
                    immutable-idtbl)))]))

(define (bound-identifier->symbol id phase) (syntax-e id))

(make-code bound-id-table
           bound-identifier->symbol
           bound-identifier=?)

(define (free-identifier->symbol id phase)
  (identifier-binding-symbol id phase))

(make-code free-id-table
           free-identifier->symbol
           free-identifier=?)

(provide id-table-iter?
         ;; just for use by syntax/id-table
         chaperone-mutable-id-table
         chaperone-immutable-id-table)

#lang scheme/base
(require (for-syntax scheme/base
                     unstable/syntax)
         scheme/dict)
(provide id-table-position?)

#|
(require (rename-in scheme/base [car s:car]))
(define-syntax (car stx)
  (syntax-case stx ()
    [(car x)
     #`(begin (unless (pair? x)
                (error 'car (format "~s:~s"
                                    '#,(syntax-line stx)
                                    '#,(syntax-column stx))))
              (s:car x))]))
|#

(define-struct id-table-position (a b))

(define empty-immutable-hasheq (make-immutable-hasheq null))

(define (check-id x who)
  (unless (identifier? x)
    (raise-type-error who "identifier" x)))

(define (check-pos x who)
  (unless (id-table-position? x)
    (raise-type-error who "id-table-position" x)))

(define (wrap f protectors [arity (length protectors)])
  (define name (object-name f))
  (procedure-reduce-arity
   (procedure-rename
    (lambda args
      (let loop ([args args] [protectors protectors])
        (when (pair? args)
          (unless (pair? protectors)
            (error name "out of guards"))
          ((car protectors) (car args) name)
          (loop (cdr args) (cdr protectors))))
      (apply f args))
    name)
   arity))
;; ========

;; Inline alist operations...
;;   id, v args must be variables (to avoid duplicating expressions)

(define-syntax-rule (alist-set identifier=? phase l0 id v)
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

(define-syntax-rule (alist-remove identifier=? phase l0 id)
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

             (define mk
               (let ([make-idtbl
                      (lambda ([init-dict null] #:phase [phase (syntax-local-phase-level)])
                        (let ([t (make-mutable-idtbl (make-hasheq) phase)])
                          (for ([(k v) (in-dict init-dict)])
                            (idtbl-set! t k v))
                          t))])
                 make-idtbl))
             (define mkimm
               (let ([make-immutable-idtbl
                      (lambda ([init-dict null] #:phase [phase (syntax-local-phase-level)])
                        (for/fold ([t (make-immutable-idtbl empty-immutable-hasheq phase)])
                            ([(k v) (in-dict init-dict)])
                          (idtbl-set t k v)))])
                 make-immutable-idtbl))

             (define (idtbl-ref d id [fail (lambda ()
                                             (error 'idtbl-ref
                                                    "no mapping for ~e" id))])
               (let ([phase (idtbl-phase d)])
                 (let ([i (ormap (lambda (i) (and (identifier=? (car i) id phase) i))
                                 (hash-ref (idtbl-hash d)
                                           (identifier->symbol id phase)
                                           null))])
                   (if i
                       (cdr i)
                       (if (procedure? fail)
                           (fail)
                           fail)))))

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
                 (make-immutable-idtbl
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
                 (make-immutable-idtbl
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
                          (and b (make-id-table-position a b)))))))

             (define (idtbl-iterate-next d pos)
               (let ([h (idtbl-hash d)]
                     [a (id-table-position-a pos)]
                     [b (id-table-position-b pos)])
                 (let ([v (dict-iterate-value h a)])
                   (let ([b2 (dict-iterate-next v b)])
                     (if b2
                         (make-id-table-position a b2)
                         (let ([a2 (dict-iterate-next h a)])
                           (and a2
                                (let ([b2 (dict-iterate-first
                                           (dict-iterate-value h a2))])
                                  (and b2 (make-id-table-position a2 b2))))))))))

             (define (idtbl-iterate-key d pos)
               (let ([h (idtbl-hash d)]
                     [a (id-table-position-a pos)]
                     [b (id-table-position-b pos)])
                 (dict-iterate-key (dict-iterate-value h a) b)))

             (define (idtbl-iterate-value d pos)
               (let ([h (idtbl-hash d)]
                     [a (id-table-position-a pos)]
                     [b (id-table-position-b pos)])
                 (dict-iterate-value (dict-iterate-value h a) b)))

             (define (check-idtbl x who)
               (unless (idtbl? x)
                 (raise-type-error who (symbol->string 'idtbl) x)))
             (define (check-mutable-idtbl x who)
               (unless (mutable-idtbl? x)
                 (raise-type-error who (symbol->string 'mutable-idtbl) x)))
             (define (check-immutable-idtbl x who)
               (unless (immutable-idtbl? x)
                 (raise-type-error who (symbol->string 'immutable-idtbl) x)))

             (define-struct idtbl (hash phase))
             (define-struct (mutable-idtbl idtbl) ()
               #:property prop:dict
               (vector (wrap idtbl-ref (list check-idtbl check-id void) '(2 3))
                       (wrap idtbl-set! (list check-mutable-idtbl check-id void))
                       #f
                       (wrap idtbl-remove! (list check-mutable-idtbl check-id))
                       #f
                       (wrap idtbl-count (list check-idtbl))
                       (wrap idtbl-iterate-first (list check-idtbl))
                       (wrap idtbl-iterate-next (list check-idtbl check-pos))
                       (wrap idtbl-iterate-key (list check-idtbl check-pos))
                       (wrap idtbl-iterate-value (list check-idtbl check-pos))))
             (define-struct (immutable-idtbl idtbl) ()
               #:property prop:dict
               (vector (wrap idtbl-ref (list check-idtbl check-id void) '(2 3))
                       #f
                       (wrap idtbl-set (list check-immutable-idtbl check-id void))
                       #f
                       (wrap idtbl-remove (list check-immutable-idtbl check-id))
                       (wrap idtbl-count (list check-idtbl))
                       (wrap idtbl-iterate-first (list check-idtbl))
                       (wrap idtbl-iterate-next (list check-idtbl check-pos))
                       (wrap idtbl-iterate-key (list check-idtbl check-pos))
                       (wrap idtbl-iterate-value (list check-idtbl check-pos))))

             (#%provide (rename mk make-idtbl)
                        (rename mkimm make-immutable-idtbl)
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
                        idtbl-for-each))))]))

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

#|
;; free*-id-tables only phase = (syntax-local-phase-level)

(define (resolve id phase)
  (if (and (syntax-transforming?) (equal? phase (syntax-local-phase-level)))
      (let-values ([(v next)
                    (syntax-local-value/immediate id (lambda () (values #f #f)))])
        (if next
            (resolve next phase)
            id))
      id))

(define (free*-identifier->symbol id phase)
  (free-identifier->symbol (resolve id phase) phase))

(define (free*-identifier=? a b phase)
  (free-identifier=? (resolve a phase) (resolve b phase) phase))

(make-code free*-id-table
           free*-identifier->symbol
           free*-identifier=?)
|#

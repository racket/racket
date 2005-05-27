
(module boundmap mzscheme
  (require (lib "contract.ss")
	   (lib "etc.ss"))
  
  (define-syntax (make-mapping-code stx)
    (syntax-case stx ()
      [(_ identifier->symbol
          make-identifier-mapping
          identifier-mapping-ht
          identifier-mapping?
          identifier-mapping-get
          identifier-mapping-put!
          identifier-mapping-for-each
          identifier-mapping-map
          identifier=?)
       (and (identifier? (syntax identifier-mapping))
            (identifier? (syntax identifier-mapping-get))
            (identifier? (syntax identifier-mapping-put!))
            (identifier? (syntax identifier-mapping-for-each))
            (identifier? (syntax identifier-mapping-map)))
       (syntax
        (begin
          
          (define mk-identifier-mapping
            (let ([make-identifier-mapping
                   (lambda ()
                     (make-identifier-mapping
                      (make-hash-table)))])
              make-identifier-mapping))
          
          (define identifier-mapping-get
            (opt-lambda (bi id [fail (lambda () 
                                       (error 'identifier-mapping-get
                                              "no mapping for ~e"
                                              id))])
              (or (ormap (lambda (i)
                           (and (identifier=? (car i) id)
                                (cdr i)))
                         (hash-table-get (identifier-mapping-ht bi)
                                         (identifier->symbol id) 
                                         (lambda () null)))
                  (fail))))
          
          (define identifier-mapping-put!
            (lambda (bi id v)
              (let ([l (hash-table-get
                        (identifier-mapping-ht bi)
                        (identifier->symbol id) 
                        (lambda () null))])
                (hash-table-put!
                 (identifier-mapping-ht bi)
                 (identifier->symbol id) 
                 (let loop ([l l])
                   (cond
                     [(null? l) (list (cons id v))]
                     [(identifier=? (caar l) id)
                      (cons (cons id v) (cdr l))]
                     [else (cons (car l) (loop (cdr l)))]))))))
          
          (define identifier-mapping-for-each
            (lambda (bi f)
              (hash-table-for-each (identifier-mapping-ht bi)
                                   (lambda (k v)
                                     (for-each (lambda (i)
                                                 (f (car i) (cdr i)))
                                               v)))))
          
          (define identifier-mapping-map
            (lambda (bi f)
              (let* ([first (cons #f null)]
                     [last first])
                (identifier-mapping-for-each
                 bi
                 (lambda (k v)
                   (let ([pr (cons (f k v) null)])
                     (set-cdr! last pr)
                     (set! last pr))))
                (cdr first))))
          
          (provide (rename mk-identifier-mapping make-identifier-mapping))
          (provide/contract
           [identifier-mapping? (any/c . -> . boolean?)]
           [identifier-mapping-get (opt->*
                                    (identifier-mapping?
                                     identifier?)
                                    ((-> any))
                                    any)]
           [identifier-mapping-put! (identifier-mapping?
                                     identifier?
                                     any/c
                                     . -> .
                                     void?)]
           [identifier-mapping-for-each (identifier-mapping?
                                         (identifier? any/c . -> . any)
                                         . -> .
                                         void?)]
           [identifier-mapping-map (identifier-mapping?
                                    (identifier? any/c . -> . any)
                                    . -> .
                                    (listof any/c))])))]))
  
  ;; ht : hash-table[symbol(key) -> (listof (cons syntax[identifier] any))]
  ;; the entries in the hash-table narrow the mapping to 
  ;; the identifiers that match that key.
  (define-struct bound-identifier-mapping (ht))

  (define (bound-identifier->symbol id) (syntax-e id))
  
  (make-mapping-code
   bound-identifier->symbol
   make-bound-identifier-mapping
   bound-identifier-mapping-ht
   bound-identifier-mapping?
   bound-identifier-mapping-get
   bound-identifier-mapping-put!
   bound-identifier-mapping-for-each
   bound-identifier-mapping-map
   bound-identifier=?)

  ;; ht : hash-table[symbol(key) -> (listof (cons syntax[identifier] any))]
  ;; the entries in the hash-table narrow the mapping to 
  ;; the identifiers that match that key.
  (define-struct module-identifier-mapping (ht))

  (define (module-identifier->symbol id) 
    (let ([binding (identifier-binding id)])
      (if (pair? binding)
          (cadr binding)
          (syntax-e id))))
  
  (make-mapping-code
   module-identifier->symbol
   make-module-identifier-mapping
   module-identifier-mapping-ht
   module-identifier-mapping?
   module-identifier-mapping-get
   module-identifier-mapping-put!
   module-identifier-mapping-for-each
   module-identifier-mapping-map
   module-identifier=?))

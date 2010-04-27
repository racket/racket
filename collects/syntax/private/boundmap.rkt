
(module boundmap racket/base
  (require (for-syntax racket/base))
  
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
                      (make-hasheq)))])
              make-identifier-mapping))
          
          (define identifier-mapping-get
            (lambda (bi id [fail (lambda () 
                                   (error 'identifier-mapping-get
                                          "no mapping for ~e"
                                          id))])
              (let ([i (ormap (lambda (i)
                                (and (identifier=? (car i) id)
                                     i))
                              (hash-ref (identifier-mapping-ht bi)
                                        (identifier->symbol id) 
                                        null))])
                (if i
                    (cdr i)
                    (fail)))))
          
          (define identifier-mapping-put!
            (lambda (bi id v)
              (let ([l (hash-ref
                        (identifier-mapping-ht bi)
                        (identifier->symbol id) 
                        null)])
                (hash-set!
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
              (hash-for-each (identifier-mapping-ht bi)
                             (lambda (k v)
                               (for-each (lambda (i)
                                           (f (car i) (cdr i)))
                                         v)))))
          
          (define identifier-mapping-map
            (lambda (bi f)
              (let* ([r null])
                (identifier-mapping-for-each
                 bi
                 (lambda (k v)
                   (set! r (cons (f k v) r))))
                (reverse r))))
          
          (provide (rename-out [mk-identifier-mapping make-identifier-mapping]))
          (provide identifier-mapping?
		   identifier-mapping-get
		   identifier-mapping-put!
		   identifier-mapping-for-each
		   identifier-mapping-map)))]))
  
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
   free-identifier=?))

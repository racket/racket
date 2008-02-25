#lang scheme/base

(require scheme/mpair
         rnrs/arithmetic/bitwise-6
         (for-syntax scheme/base))

(provide make-enumeration
         enum-set-universe
         enum-set-indexer
         enum-set-constructor
         enum-set-member?
         enum-set-subset?
         enum-set=?
         enum-set-union
         enum-set-intersection
         enum-set-difference
         enum-set-complement
         enum-set-projection
         define-enumeration)

(define-struct enum-set (val ht))

(define (make-enumeration-universe enum)
  (let ([bad (lambda ()
               (raise-type-error
                'make-enumeration
                "list of symbols"
                enum))])
  (unless (mlist? enum) (bad))
  (let ([enum (mlist->list enum)])
    (unless (andmap symbol? enum) (bad))
    (let ([ht (make-hash-table)])
      (for ([s (in-list enum)])
        (unless (hash-table-get ht s #f)
          (hash-table-put! ht s (arithmetic-shift 1 (hash-table-count ht)))))
      ht))))

(define (make-enumeration enum)
  (let ([ht (make-enumeration-universe enum)])
    (make-enum-set (sub1 (arithmetic-shift 1 (hash-table-count ht)))
                   ht)))

(define (enum-set-universe enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-universe
                      "enumeration set"
                      enum))
  (let ([ht (enum-set-ht enum)])
    (make-enum-set (sub1 (arithmetic-shift 1 (hash-table-count ht))) ht)))

(define (enum-set-indexer enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-indexer
                      "enumeration set"
                      enum))
  (let ([ht (enum-set-ht enum)])
    (lambda (sym)
      (let ([v (hash-table-get ht sym #f)])
        (if v
            (bitwise-first-bit-set v)
            (error 'generated-enum-set-indexer
                   (if (symbol? sym)
                       "symbol not in universe: ~e"
                       "not a symbol: ~e")
                   sym))))))

(define (enum-set-constructor enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-constructor
                      "enumeration set"
                      enum))
  (let ([ht (enum-set-ht enum)])
    (lambda (orig-syms)
      (let loop ([syms orig-syms][val 0])
        (cond
         [(null? syms) (make-enum-set val ht)]
         [(not (mpair? syms))
          (raise-type-error 'make-enum-set
                            "list of symbols"
                            orig-syms)]
         [(hash-table-get ht (mcar syms) #f)
          => (lambda (n)
               (loop (mcdr syms) (bitwise-ior val n)))]
         [else
          (error 'make-enum-set
                 (if (symbol? (mcar syms))
                     "symbol not in universe: ~e"
                     "not a symbol: ~e")
                 (mcar syms))])))))

(define (enum-set-member? sym enum)
  (unless (symbol? sym)
    (raise-type-error 'enum-set-member?
                      "symbol"
                      sym))
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-member?
                      "enumeration set"
                      enum))
  (let ([v (hash-table-get (enum-set-ht enum) sym #f)])
    (and v
         (not (zero? (bitwise-and v (enum-set-val enum)))))))

(define (check-2-enums who enum1 enum2)
  (unless (and (enum-set? enum1)
               (enum-set? enum2))
    (raise-type-error who
                      "enumeration set"
                      (if (enum-set? enum1)
                          enum2
                          enum1))))

(define (enum-set-subset? enum1 enum2)
  (check-2-enums 'enum-set-subset? enum1 enum2)
  (if (eq? (enum-set-ht enum1) (enum-set-ht enum2))
      (= (enum-set-val enum1) 
         (bitwise-and (enum-set-val enum1) (enum-set-val enum2)))
      (let ([ht2 (enum-set-ht enum2)]
            [v1 (enum-set-val enum1)]
            [v2 (enum-set-val enum2)])
        (for/fold ([sub? #t])
            (#:when sub?
                    [(key1 val1) (in-hash-table (enum-set-ht enum1))])
          (or (zero? (bitwise-and v1 val1))
              (let ([val2 (hash-table-get ht2 key1 #f)])
                (and val2
                     (not (zero? (bitwise-and v2 val2))))))))))

(define (enum-set=? enum1 enum2)
  (check-2-enums 'enum-set=? enum1 enum2)
  (if (eq? (enum-set-ht enum1) (enum-set-ht enum2))
      (= (enum-set-val enum1) (enum-set-val enum2))
      (and (enum-set-subset? enum1 enum2)
           (enum-set-subset? enum2 enum1))))

(define (check-2-enums/same who enum1 enum2)
  (check-2-enums who enum1 enum2)
  (unless (eq? (enum-set-ht enum1)
               (enum-set-ht enum2))
    (error who
           "enumeration sets are not the same enumeration type: ~e ~e"
           enum1 enum2)))

(define (enum-set-union enum1 enum2)
  (check-2-enums/same 'enum-set-union enum1 enum2)
  (make-enum-set (bitwise-ior (enum-set-val enum1)
                              (enum-set-val enum2))
                 (enum-set-ht enum1)))

(define (enum-set-intersection enum1 enum2)
  (check-2-enums/same 'enum-set-intersection enum1 enum2)
  (make-enum-set (bitwise-and (enum-set-val enum1)
                              (enum-set-val enum2))
                 (enum-set-ht enum1)))

(define (enum-set-difference enum1 enum2)
  (check-2-enums/same 'enum-set-intersection enum1 enum2)
  (make-enum-set (- (enum-set-val enum1)
                    (bitwise-and (enum-set-val enum1)
                                 (enum-set-val enum2)))
                 (enum-set-ht enum1)))

(define (enum-set-complement enum1)
  (unless (enum-set? enum1)
    (raise-type-error 'enum-set-complement
                      "enumeration set"
                      enum1))
  (make-enum-set (bitwise-xor (sub1 (arithmetic-shift 
                                     1 
                                     (hash-table-count (enum-set-ht enum1))))
                              (enum-set-val enum1))
                 (enum-set-ht enum1)))

(define (enum-set-projection enum1 enum2)
  (check-2-enums 'enum-set-projection enum1 enum2)
  (let ([ht2 (enum-set-ht enum2)]
        [v1 (enum-set-val enum1)]
        [v2 (enum-set-val enum2)])
    (make-enum-set
     (for/fold ([val 0])
         ([(key1 val1) (in-hash-table (enum-set-ht enum1))])
       (if (zero? (bitwise-and v1 val1))
           val
           (let ([val2 (hash-table-get ht2 key1 #f)])
             (if val2
                 (bitwise-ior val val2)
                 val))))
     ht2)))
  
(define-syntax (define-enumeration stx)
  (syntax-case stx ()
    [(_ type-name (sym ...) constructor)
     (let ([syms (syntax->list #'(sym ...))]
           [ht (make-hash-table)])
       (unless (identifier? #'type-name)
         (raise-syntax-error #f
                             "not an identifier for type name"
                             stx
                             #'type-name))
       (for-each (lambda (sym)
                   (unless (identifier? sym)
                     (raise-syntax-error #f
                                         "not an identifier (to be used as a symbol)"
                                         stx
                                         sym)))
                 syms)
       (unless (identifier? #'constructor)
         (raise-syntax-error #f
                             "not an identifier for type name"
                             stx
                             #'constructor))
       (for ([s (in-list syms)])
         (unless (hash-table-get ht (syntax-e s) #f)
           (hash-table-put! ht (syntax-e s)
                            (arithmetic-shift 1 (hash-table-count ht)))))
       (with-syntax ([(val ...)
                      (map (lambda (s) (hash-table-get ht (syntax-e s))) syms)])
       #'(begin
           (define enum-universe (make-enumeration-universe (mlist 'sym ...)))
           (define-syntax (type-name stx)
             (syntax-case stx (sym ...)
               [(_ sym) #''sym]
               ...
               [(_ other)
                (identifier? #'other)
                (raise-syntax-error #f "not in enumeration" stx #'other)]))
           (define-syntax (bit-value stx)
             (syntax-case stx (sym ...)
               [(_ orig sym) #'val]
               ...
               [(_ orig s)
                (raise-syntax-error #f "not in enumeration" #'orig #'s)]))
           (...
            (define-syntax (constructor stx)
              (syntax-case stx ()
                [(_ s ...)
                 (andmap identifier? (syntax->list #'(s ...)))
                 (with-syntax ([orig stx])
                   #'(make-enum-set (bitwise-ior (bit-value orig s) ...)
                                    enum-universe))]))))))]))

#lang scheme/base

(require scheme/mpair
         rnrs/arithmetic/bitwise-6
         (for-syntax scheme/base))

(provide make-enumeration
         enum-set-universe
         enum-set-indexer
         enum-set-constructor
         enum-set->list
         enum-set-member?
         enum-set-subset?
         enum-set=?
         enum-set-union
         enum-set-intersection
         enum-set-difference
         enum-set-complement
         enum-set-projection
         define-enumeration)

(define-struct universe (ht syms))
(define-struct enum-set (val uni))

(define (make-enumeration-universe enum)
  (let ([bad (lambda ()
               (raise-type-error
                'make-enumeration
                "list of symbols"
                enum))])
  (unless (mlist? enum) (bad))
  (let ([enum (mlist->list enum)])
    (unless (andmap symbol? enum) (bad))
    (let ([ht (make-hasheq)])
      (make-universe
       ht
       (for/list ([s (in-list enum)]
                  #:when (not (hash-ref ht s #f)))
         (hash-set! ht s (arithmetic-shift 1 (hash-count ht)))
         s))))))

(define (make-enumeration enum)
  (let ([uni (make-enumeration-universe enum)])
    (make-enum-set (sub1 (arithmetic-shift 1 (hash-count (universe-ht uni))))
                   uni)))

(define (enum-set-universe enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-universe
                      "enumeration set"
                      enum))
  (let ([uni (enum-set-uni enum)])
    (make-enum-set (sub1 (arithmetic-shift 1 (hash-count 
                                              (universe-ht uni))))
                   uni)))

(define (enum-set-indexer enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-indexer
                      "enumeration set"
                      enum))
  (let ([ht (universe-ht (enum-set-uni enum))])
    (lambda (sym)
      (let ([v (hash-ref ht sym #f)])
        (if v
            (bitwise-first-bit-set v)
            (if (symbol? sym)
                #f
                (error 'generated-enum-set-indexer
                       "not a symbol: ~e"
                       sym)))))))

(define (enum-set-constructor enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-constructor
                      "enumeration set"
                      enum))
  (let* ([uni (enum-set-uni enum)]
         [ht (universe-ht uni)])
    (lambda (orig-syms)
      (let loop ([syms orig-syms][val 0])
        (cond
         [(null? syms) (make-enum-set val uni)]
         [(not (mpair? syms))
          (raise-type-error 'make-enum-set
                            "list of symbols"
                            orig-syms)]
         [(hash-ref ht (mcar syms) #f)
          => (lambda (n)
               (loop (mcdr syms) (bitwise-ior val n)))]
         [else
          (error 'make-enum-set
                 (if (symbol? (mcar syms))
                     "symbol not in universe: ~e"
                     "not a symbol: ~e")
                 (mcar syms))])))))

(define (enum-set->list enum)
  (unless (enum-set? enum)
    (raise-type-error 'enum-set->list
                      "enumeration set"
                      enum))
  (let ([v (enum-set-val enum)])
    (list->mlist
     (for/list ([sym (in-list (universe-syms (enum-set-uni enum)))]
                [i (in-naturals)]
                #:when (not (zero? (bitwise-and (arithmetic-shift 1 i) v))))
       sym))))

(define (enum-set-member? sym enum)
  (unless (symbol? sym)
    (raise-type-error 'enum-set-member?
                      "symbol"
                      sym))
  (unless (enum-set? enum)
    (raise-type-error 'enum-set-member?
                      "enumeration set"
                      enum))
  (let ([v (hash-ref (universe-ht (enum-set-uni enum)) sym #f)])
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
  (if (eq? (enum-set-uni enum1) (enum-set-uni enum2))
      (= (enum-set-val enum1) 
         (bitwise-and (enum-set-val enum1) (enum-set-val enum2)))
      (let ([ht2 (universe-ht (enum-set-uni enum2))]
            [v1 (enum-set-val enum1)]
            [v2 (enum-set-val enum2)])
        (for/fold ([sub? #t])
            ([(key1 val1) (in-hash (universe-ht (enum-set-uni enum1)))]
             #:when sub?)
          (let ([val2 (hash-ref ht2 key1 #f)])
            (and val2
                 (or (zero? (bitwise-and v1 val1))
                     (not (zero? (bitwise-and v2 val2))))))))))

(define (enum-set=? enum1 enum2)
  (check-2-enums 'enum-set=? enum1 enum2)
  (if (eq? (enum-set-uni enum1) (enum-set-uni enum2))
      (= (enum-set-val enum1) (enum-set-val enum2))
      (and (enum-set-subset? enum1 enum2)
           (enum-set-subset? enum2 enum1))))

(define (check-2-enums/same who enum1 enum2)
  (check-2-enums who enum1 enum2)
  (unless (eq? (enum-set-uni enum1)
               (enum-set-uni enum2))
    (error who
           "enumeration sets are not the same enumeration type: ~e ~e"
           enum1 enum2)))

(define (enum-set-union enum1 enum2)
  (check-2-enums/same 'enum-set-union enum1 enum2)
  (make-enum-set (bitwise-ior (enum-set-val enum1)
                              (enum-set-val enum2))
                 (enum-set-uni enum1)))

(define (enum-set-intersection enum1 enum2)
  (check-2-enums/same 'enum-set-intersection enum1 enum2)
  (make-enum-set (bitwise-and (enum-set-val enum1)
                              (enum-set-val enum2))
                 (enum-set-uni enum1)))

(define (enum-set-difference enum1 enum2)
  (check-2-enums/same 'enum-set-intersection enum1 enum2)
  (make-enum-set (- (enum-set-val enum1)
                    (bitwise-and (enum-set-val enum1)
                                 (enum-set-val enum2)))
                 (enum-set-uni enum1)))

(define (enum-set-complement enum1)
  (unless (enum-set? enum1)
    (raise-type-error 'enum-set-complement
                      "enumeration set"
                      enum1))
  (make-enum-set (bitwise-xor (sub1 (arithmetic-shift 
                                     1 
                                     (hash-count 
                                      (universe-ht (enum-set-uni enum1)))))
                              (enum-set-val enum1))
                 (enum-set-uni enum1)))

(define (enum-set-projection enum1 enum2)
  (check-2-enums 'enum-set-projection enum1 enum2)
  (let* ([uni2 (enum-set-uni enum2)]
         [ht2 (universe-ht uni2)]
         [v1 (enum-set-val enum1)]
         [v2 (enum-set-val enum2)])
    (make-enum-set
     (for/fold ([val 0])
         ([(key1 val1) (in-hash (universe-ht (enum-set-uni enum1)))])
       (if (zero? (bitwise-and v1 val1))
           val
           (let ([val2 (hash-ref ht2 key1 #f)])
             (if val2
                 (bitwise-ior val val2)
                 val))))
     uni2)))
  
(define-syntax (define-enumeration stx)
  (syntax-case stx ()
    [(_ type-name (sym ...) constructor)
     (let ([syms (syntax->list #'(sym ...))]
           [ht (make-hasheq)])
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
         (unless (hash-ref ht (syntax-e s) #f)
           (hash-set! ht (syntax-e s)
                      (arithmetic-shift 1 (hash-count ht)))))
       (with-syntax ([(val ...)
                      (map (lambda (s) (hash-ref ht (syntax-e s))) syms)])
       #'(begin
           (define enum-universe (make-enumeration-universe (mlist 'sym ...)))
           (define-syntax (type-name stx)
             (syntax-case* stx (sym ...) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
               [(_ sym) #''sym]
               ...
               [(_ other)
                (identifier? #'other)
                (raise-syntax-error #f "not in enumeration" stx #'other)]))
           (define-syntax (bit-value stx)
             (syntax-case* stx (sym ...) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
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

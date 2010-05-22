#lang scheme

(require "define.ss" (for-syntax syntax/parse))

(define-if-unbound (hash-has-key? table key)
  (let/ec return
    (hash-ref table key (lambda () (return #f)))
    #t))

(define-if-unbound (hash-equal? table)
  (and (hash? table)
       (not (hash-eq? table))
       (not (hash-eqv? table))))

(define (hash-ref/check table key)
  (hash-ref table key))

(define (hash-ref/identity table key)
  (hash-ref table key (lambda () key)))

(define (hash-ref/default table key default)
  (hash-ref table key (lambda () default)))

(define (hash-ref/failure table key failure)
  (hash-ref table key (lambda () (failure))))

(define (hash-domain table)
  (for/list ([i (in-hash-keys table)]) i))

(define (hash-range table)
  (for/list ([i (in-hash-values table)]) i))

(define ((hash-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

(define (hash-union
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (hash-duplicate-error 'hash-union))]
         one . rest)
  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-hash two)])
    (hash-set one k (if (hash-has-key? one k)
                        (combine/key k (hash-ref one k) v)
                        v))))

(define (hash-union!
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (hash-duplicate-error 'hash-union))]
         one . rest)
  (for* ([two (in-list rest)] [(k v) (in-hash two)])
    (hash-set! one k (if (hash-has-key? one k)
                         (combine/key k (hash-ref one k) v)
                         v))))

(define-syntaxes [ hash hash! ]
  (let ()

    (define-syntax-class key/value
      #:attributes [key value]
      (pattern [key:expr value:expr]))

    (define-splicing-syntax-class immutable-hash-type
      #:attributes [constructor]
      (pattern (~seq #:eqv) #:attr constructor #'make-immutable-hasheqv)
      (pattern (~seq #:eq) #:attr constructor #'make-immutable-hasheq)
      (pattern (~seq (~optional #:equal))
               #:attr constructor #'make-immutable-hash))

    (define-splicing-syntax-class mutable-hash-type
      #:attributes [constructor]
      (pattern (~seq #:base constructor:expr))
      (pattern (~seq (~or (~once #:eqv) (~once #:weak)) ...)
               #:attr constructor #'(make-weak-hasheqv))
      (pattern (~seq (~or (~once #:eq) (~once #:weak)) ...)
               #:attr constructor #'(make-weak-hasheq))
      (pattern (~seq (~or (~optional #:equal) (~once #:weak)) ...)
               #:attr constructor #'(make-weak-hash))
      (pattern (~seq #:eqv) #:attr constructor #'(make-hasheqv))
      (pattern (~seq #:eq) #:attr constructor #'(make-hasheq))
      (pattern (~seq (~optional #:equal)) #:attr constructor #'(make-hash)))

    (define (parse-hash stx)
      (syntax-parse stx
        [(_ (~seq type:immutable-hash-type) elem:key/value ...)
         (syntax/loc stx
           (type.constructor (list (cons elem.key elem.value) ...)))]
        [(_ #:base h:expr elem:key/value ...)
         (syntax/loc stx
           (for/fold
               ([table h])
               ([key (in-list (list elem.key ...))]
                [value (in-list (list elem.value ...))])
             (hash-set table key value)))]))

    (define (parse-hash! stx)
      (syntax-parse stx
        [(_ (~seq type:mutable-hash-type) elem:key/value ...)
         (syntax/loc stx
           (let ([table type.constructor])
             (for ([key (in-list (list elem.key ...))]
                   [value (in-list (list elem.value ...))])
               (hash-set! table key value))
             table))]))

    (values parse-hash parse-hash!)))

(provide hash hash! hash-has-key? hash-equal?)
(provide/contract
 [hash-ref/identity (-> hash? any/c any/c)]
 [hash-ref/default (-> hash? any/c any/c any/c)]
 [hash-ref/failure (-> hash? any/c (-> any/c) any/c)]
 [hash-ref/check
  (->d ([table hash?] [key any/c]) ()
       #:pre-cond (hash-has-key? table key)
       [_ any/c])]
 [hash-domain (-> hash? list?)]
 [hash-range (-> hash? list?)]
 [hash-union (->* [(and/c hash? immutable?)]
                  [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                  #:rest (listof hash?)
                  (and/c hash? immutable?))]
 [hash-union! (->* [(and/c hash? (not/c immutable?))]
                   [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                   #:rest (listof hash?)
                   void?)])

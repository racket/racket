#lang racket/base

(require racket/match racket/contract/base racket/contract/combinator
         racket/promise)

(define undef (letrec ([x x]) x))

(define (traverse b)
  (define (fail v)
    (raise-blame-error 
     (blame-swap b) v 
     "Attempted to use a higher-order value passed as `Any` in untyped code"))

  (define (t v)
    (define (wrap-struct s)
      (define (extract-functions struct-type)
        (define-values (sym init auto ref set! imms par skip?)
          (struct-type-info struct-type))
        (when skip? (fail s)) ;; "Opaque struct type!")
        (define-values (fun/chap-list _)
          (for/fold ([res null]
                     [imms imms])
              ([n (in-range (+ init auto))])
            (if (and (pair? imms) (= (car imms) n))
                ;; field is immutable
                (values 
                 (list* (make-struct-field-accessor ref n)
                        (lambda (s v) (t v))
                        res)
                 (cdr imms))
                ;; field is mutable
                (values
                 (list* (make-struct-field-accessor ref n)
                        (lambda (s v) (t v))
                        (make-struct-field-mutator set! n)
                        (lambda (s v) (fail s))
                        res)
                 imms))))
        (cond
          [par (append fun/chap-list (extract-functions par))]
          [else fun/chap-list]))
      (define-values (type skipped?) (struct-info s))
      (when skipped? (fail s));  "Opaque struct type!"
      (apply chaperone-struct s (extract-functions type)))

    (match v
      [(? (lambda (e)
            (or (number? e) (string? e) (char? e) (symbol? e)
                (null? e) (regexp? e) (eq? undef e) (path? e)
		(regexp? e) (keyword? e) (bytes? e) (boolean? e) (void? e))))
       v]
      [(cons x y) (cons (t x) (t y))]
      [(? vector? (? immutable?))
       ;; fixme -- should have an immutable for/vector
       (vector->immutable-vector
	(for/vector #:length (vector-length v)
		    ([i (in-vector v)]) (t i)))]
      [(? box? (? immutable?)) (box-immutable (t (unbox v)))]
      ;; fixme -- handling keys
      ;; [(? hasheq? (? immutable?))
      ;;  (for/hasheq ([(k v) (in-hash v)]) (values k v))]
      ;; [(? hasheqv? (? immutable?))
      ;;  (for/hasheqv ([(k v) (in-hash v)]) (values k v))]
      
      [(? hash? (? immutable?))
       (for/hash ([(k v) (in-hash v)]) (values (t k) (t v)))]
      [(? vector?) (chaperone-vector v
                                     (lambda (v i e) (t e))
                                     (lambda (v i e) (fail v)))]
      [(? box?) (chaperone-box v
                               (lambda (v e) (t e))
                               (lambda (v e) (fail v)))]
      [(? hash?) (chaperone-hash v
                                 (lambda (h k) (values k (lambda (h k v) (t v)))) ;; ref
                                 (lambda (h k n) (if (immutable? v) (values k n) (fail v))) ;; set
                                 (lambda (h v) v) ;; remove
                                 (lambda (h k) (t k)))] ;; key
      [(? evt?) (chaperone-evt v (lambda (e) (values e t)))]
      [(? struct?) (wrap-struct v)]
      [(? procedure?) 
       (if (procedure-arity-includes? v 0)
           (chaperone-procedure v (case-lambda [() (values)]
                                               [_ (fail v)]))
           (chaperone-procedure v (lambda args (fail v))))]
      [(? promise?)
       ;; for promises, just apply Any in the promise
       (contract (promise/c any-wrap/c) v
                 (blame-positive b) (blame-negative b))]
      [_ (fail v)]))
  t)

(define any-wrap/c
  (make-chaperone-contract
   #:name 'Any
   #:first-order (lambda (x) #t)
   #:projection traverse))

(provide any-wrap/c)

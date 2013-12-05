#lang racket/base

(require racket/match racket/contract/base racket/contract/combinator
         racket/promise racket/set)

(define undef (letrec ([x x]) x))

(define (base-val? e)
  (or (number? e) (string? e) (char? e) (symbol? e)
      (null? e) (regexp? e) (eq? undef e) (path? e)
      (regexp? e) (keyword? e) (bytes? e) (boolean? e) (void? e)))

(define (val-first-projection b)
  (define (fail neg-party v)
    (raise-blame-error 
     (blame-swap b) #:mising-party neg-party
     v 
     "Attempted to use a higher-order value passed as `Any` in untyped code: ~v" v))
  
  (define (wrap-struct neg-party s)
    (define (extract-functions struct-type)
      (define-values (sym init auto ref set! imms par skip?)
        (struct-type-info struct-type))
      (when skip? (fail neg-party s)) ;; "Opaque struct type!")
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
                      (lambda (s v) (fail neg-party s))
                      res)
               imms))))
      (cond
        [par (append fun/chap-list (extract-functions par))]
        [else fun/chap-list]))
    (define-values (type skipped?) (struct-info s))
    (when skipped? (fail neg-party s));  "Opaque struct type!"
    (apply chaperone-struct s (extract-functions type)))
 
  (define (t neg-party v)
    (match v
      [(? base-val?)
       v]
      [(cons x y) (cons (t neg-party x) (t neg-party y))]
      [(? vector? (? immutable?))
       ;; fixme -- should have an immutable for/vector
       (vector->immutable-vector
        (for/vector #:length (vector-length v)
          ([i (in-vector v)]) (t neg-party i)))]
      [(? box? (? immutable?)) (box-immutable (t neg-party (unbox v)))]
      ;; fixme -- handling keys properly makes it not a chaperone
      ;; [(? hasheq? (? immutable?))
      ;;  (for/hasheq ([(k v) (in-hash v)]) (values k v))]
      ;; [(? hasheqv? (? immutable?))
      ;;  (for/hasheqv ([(k v) (in-hash v)]) (values k v))]
      
      [(? (λ (e) 
            (and (hash? e) (immutable? e) 
                 (not (hash-eqv? e)) (not (hash-eq? e)))))
       (for/hash ([(k v) (in-hash v)]) (values (t neg-party k) (t neg-party v)))]
      [(? vector?) (chaperone-vector v
                                     (lambda (v i e) (t neg-party e))
                                     (lambda (v i e) (fail neg-party v)))]
      [(? box?) (chaperone-box v
                               (lambda (v e) (t neg-party e))
                               (lambda (v e) (fail neg-party v)))]
      [(? hash?) (chaperone-hash v
                                 (lambda (h k)
                                   (values k (lambda (h k v) (t neg-party v)))) ;; ref
                                 (lambda (h k n)
                                   (if (immutable? v) 
                                       (values k n)
                                       (fail neg-party v))) ;; set
                                 (lambda (h v) v) ;; remove
                                 (lambda (h k) (t neg-party k)))] ;; key
      [(? evt?) (chaperone-evt v (lambda (e) (values e t)))]
      [(? set?)
       (for/set ([i (in-set v)]) (t neg-party i))]
      ;; could do something with generic sets here if they had
      ;; chaperones, or if i could tell if they were immutable.
      [(? struct?) (wrap-struct neg-party v)]
      [(? procedure?)
       (if (procedure-arity-includes? v 0)
           (chaperone-procedure v (case-lambda [() (values)]
                                               [_ (fail neg-party v)]))
           (chaperone-procedure v (lambda args (fail neg-party v))))]
      [(? promise?)
       ;; for promises, just apply Any in the promise
       (contract (promise/c any-wrap/c) v
                 (blame-positive b) (blame-negative b))]
      [_ (fail neg-party v)]))
  (λ (v) (λ (neg-party) (t neg-party v))))

(define any-wrap/c
  (make-chaperone-contract
   #:name 'Any
   #:first-order (lambda (x) #t)
   #:projection (λ (blame) (λ (val) (((val-first-projection blame) val) #f)))
   #:val-first-projection val-first-projection))

(provide any-wrap/c)

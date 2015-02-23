#lang racket/base

(require racket/generic
         (only-in racket/dict
                  gen:dict prop:dict
                  dict?
                  dict-ref
                  dict-set!
                  dict-set
                  dict-remove!
                  dict-remove
                  dict-count))

(struct hash-box (key))

(define custom-hash-ref
  (case-lambda
      [(d k) (hash-ref (custom-hash-table d)
                       ((custom-hash-make-box d) k)
                       (lambda ()
                         (raise-mismatch-error
                          'dict-ref
                          "no value found for key: "
                          k)))]
    [(d k fail) (hash-ref (custom-hash-table d)
                          ((custom-hash-make-box d) k)
                          fail)]))

(define (custom-hash-set! d k v)
  (hash-set! (custom-hash-table d)
             ((custom-hash-make-box d) k)
             v))

(define (custom-hash-remove! d k)
  (hash-remove! (custom-hash-table d)
                ((custom-hash-make-box d) k)))

(define (custom-hash-count d)
  (hash-count (custom-hash-table d)))


(struct custom-hash (table make-box)
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set! custom-hash-set!)
   (define (dict-set dict key val)
     (error "no functional update"))
   (define dict-remove! custom-hash-remove!)
   (define (dict-remove dict key)
     (error "no functional update"))
   (define dict-count custom-hash-count)]
  #:methods gen:equal+hash
  [(define (equal-proc a b recur)
     (and (recur (custom-hash-make-box a)
                 (custom-hash-make-box b))
          (recur (custom-hash-table a)
                 (custom-hash-table b))))
   (define (hash-proc a recur)
     (recur (custom-hash-table a)))
   (define (hash2-proc a recur)
     (recur (custom-hash-table a)))])


(define (make-custom-hash =? hash [hash2 (lambda (v) 10001)])
  (unless (and (procedure? =?)
               (procedure-arity-includes? =? 2))
    (raise-type-error 'make-custom-hash "procedure (arity 2)" =?))
  (unless (and (procedure? hash)
               (procedure-arity-includes? hash 1))
    (raise-type-error 'make-custom-hash "procedure (arity 1)" hash))
  (unless (and (procedure? hash2)
               (procedure-arity-includes? hash2 1))
    (raise-type-error 'make-custom-hash "procedure (arity 1)" hash2))
  (let ()
    (struct box hash-box ()
            #:methods gen:equal+hash
            [(define (equal-proc a b recur)
               (=? (hash-box-key a) (hash-box-key b)))
             (define (hash-proc v recur)
               (hash (hash-box-key v)))
             (define (hash2-proc v recur)
               (hash2 (hash-box-key v)))])
    (custom-hash (make-hash) box)))


(module+ test
  (require rackunit)

  ;; from the docs
  (define h (make-custom-hash (lambda (a b)
                                (string=? (format "~a" a)
                                          (format "~a" b)))
                              (lambda (a)
                                (equal-hash-code
                                 (format "~a" a)))))
  (dict-set! h 1 'one)
  (check-eq? (dict-ref h "1") 'one))

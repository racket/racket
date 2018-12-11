#lang racket/base
(require "../common/check.rkt"
         "../host/rktio.rkt"
         "../host/thread.rkt"
         "../host/error.rkt"
         "string.rkt")

(provide environment-variables?
         make-environment-variables
         environment-variables-ref
         current-environment-variables
         environment-variables-set!
         environment-variables-copy
         environment-variables-names)

(struct environment-variables ([ht #:mutable]) ; #f => use OS-level environment variables
  #:authentic)

(define/who current-environment-variables
  (make-parameter (environment-variables #f)
                  (lambda (v)
                    (check who environment-variables? v)
                    v)))

(define/who (make-environment-variables . args)
  (let loop ([args args] [ht #hash()])
    (cond
      [(null? args) (environment-variables ht)]
      [else
       (define key0 (car args))
       (define key (if (bytes? key0)
                       (bytes->immutable-bytes key0)
                       key0))
       (check who bytes-environment-variable-name? key)
       (cond
         [(null? args)
          (raise-arguments-error who
                                 "key does not have a value (i.e., an odd number of arguments were provided)"
                                 "key" (car args))]
         [else
          (define val0 (cadr args))
          (define val (and (bytes? val0)
                           (bytes->immutable-bytes val0)
                           val0))
          (check who bytes-no-nuls? val)
          (loop (cddr args) (hash-set ht (normalize-key key) val))])])))

(define/who (environment-variables-ref e k)
  (check who environment-variables? e)
  (check who bytes-environment-variable-name? k)
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     (start-atomic)
     (define v (rktio_getenv rktio k))
     (define s (and (not (rktio-error? v))
                    (begin0
                      (rktio_to_bytes v)
                      (rktio_free v))))
     (end-atomic)
     s]
    [else
     (hash-ref ht (normalize-key k) #f)]))

(define none (gensym 'none))

(define/who (environment-variables-set! e k0 v0 [fail none])
  (check who environment-variables? e)
  (define k (if (bytes? k0) (bytes->immutable-bytes k0) k0))
  (check who bytes-environment-variable-name? k)
  (define v (if (bytes? v0) (bytes->immutable-bytes v0) v0))
  (check who bytes-no-nuls? #:or-false v)
  (unless (eq? fail none)
    (check who (procedure-arity-includes/c 0) fail))
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     (define r (rktio_setenv rktio k v))
     (when (rktio-error? r)
       (cond
         [(eq? fail none)
          (raise-rktio-error who r "change failed")]
         [else (fail)]))]
    [else
     (define nk (normalize-key k))
     (set-environment-variables-ht! e (if v (hash-set ht nk v) (hash-remove ht nk)))]))

(define/who (environment-variables-copy e)
  (check who environment-variables? e)
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     ;; Make a copy of current OS-level environment variables
     (start-atomic)
     (define ev (rktio_envvars rktio))
     (define ht
       (cond
         [(rktio-error? ev) #hash()]
         [else
          (begin0
            (for/hash ([i (in-range (rktio_envvars_count rktio ev))])
              (define k (rktio_envvars_name_ref rktio ev i))
              (define v (rktio_envvars_value_ref rktio ev i))
              (values
               (begin0
                 (bytes->immutable-bytes (rktio_to_bytes k))
                 (rktio_free k))
               (begin0
                 (bytes->immutable-bytes (rktio_to_bytes v))
                 (rktio_free v))))
            (rktio_envvars_free rktio ev))]))
     (end-atomic)
     (environment-variables ht)]
    [else
     ;; Copy wrapper around immutable `ht`:
     (environment-variables ht)]))

(define/who (environment-variables-names e)
  (check who environment-variables? e)
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     (map normalize-key (environment-variables-names (environment-variables-copy e)))]
    [else
     (hash-keys ht)]))

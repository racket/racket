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

;; Keep a mapping from normalized <key> to (cons <key> <val>).
;; That way, we can find values based on the normalized key,
;; but we preserve the original case.
(struct environment-variables ([ht #:mutable]) ; #f => use OS-level environment variables
  #:authentic)

(define/who current-environment-variables
  (make-parameter (environment-variables #f)
                  (lambda (v)
                    (check who environment-variables? v)
                    v)
                  'current-environment-variables))

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
          (define val (if (bytes? val0)
                          (bytes->immutable-bytes val0)
                          val0))
          (check who bytes-no-nuls? val)
          (loop (cddr args) (hash-set ht (normalize-key key) (cons key val)))])])))

(define/who (environment-variables-ref e k)
  (check who environment-variables? e)
  (check who bytes-environment-variable-name? k)
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     (start-rktio)
     (define v (rktio_getenv rktio k))
     (define s (and (not (rktio-error? v))
                    (begin0
                      (rktio_to_bytes v)
                      (rktio_free v))))
     (end-rktio)
     s]
    [else
     (cdr (hash-ref ht (normalize-key k) '(#f . #f)))]))

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
     (define r (rktioly (rktio_setenv rktio k v)))
     (when (rktio-error? r)
       (cond
         [(eq? fail none)
          (raise-rktio-error who r "change failed")]
         [else (fail)]))]
    [else
     (define nk (normalize-key k))
     (set-environment-variables-ht! e (if v
                                          (hash-set ht nk (cons k v))
                                          (hash-remove ht nk)))]))

(define/who (environment-variables-copy e)
  (check who environment-variables? e)
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     ;; Make a copy of current OS-level environment variables
     (start-rktio)
     (define ev (rktio_envvars rktio))
     (define ht
       (cond
         [(rktio-error? ev) #hash()]
         [else
          (begin0
            (for/hash ([i (in-range (rktio_envvars_count rktio ev))])
              (define k (rktio_envvars_name_ref rktio ev i))
              (define v (rktio_envvars_value_ref rktio ev i))
              (define case-k
                (begin0
                  (bytes->immutable-bytes (rktio_to_bytes k))
                  (rktio_free k)))
              (values
               (normalize-key case-k)
               (cons
                case-k
                (begin0
                  (bytes->immutable-bytes (rktio_to_bytes v))
                  (rktio_free v)))))
            (rktio_envvars_free rktio ev))]))
     (end-rktio)
     (environment-variables ht)]
    [else
     ;; Copy wrapper around immutable `ht`:
     (environment-variables ht)]))

(define/who (environment-variables-names e)
  (check who environment-variables? e)
  (define ht (environment-variables-ht e))
  (cond
    [(not ht)
     (environment-variables-names (environment-variables-copy e))]
    [else
     ;; Return unnormalized keys, which makes sense for preserving
     ;; the original case
     (map car (hash-values ht))]))

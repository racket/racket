#lang racket/base
(require "../common/contract.rkt"
         "../host/linklet.rkt"
         "write-linklet.rkt"
         "correlated-linklet.rkt")

(provide linklet-directory?
         linklet-bundle?

         hash->linklet-directory
         hash->linklet-bundle

         linklet-directory->hash
         linklet-bundle->hash)

(struct linklet-directory (ht)
  #:property prop:custom-write (lambda (ld port mode)
                                 (define machine-type (linklet-directory-machine-type ld))
                                 (write-linklet-directory ld
                                                          (not machine-type)
                                                          linklet-directory->hash
                                                          linklet-bundle->hash
                                                           machine-type
                                                          port)))

(struct linklet-bundle (ht)
  #:property prop:custom-write (lambda (b port mode)
                                 (define machine-type (linklet-bundle-machine-type b))
                                 (write-linklet-bundle b
                                                       (not machine-type)
                                                       linklet-bundle->hash
                                                       machine-type
                                                       port)))

(define/who (hash->linklet-directory ht)
  (check who (lambda (ht)
               (and (not (impersonator? ht))
                    (hash? ht)
                    (immutable? ht)
                    (hash-eq? ht)))
         #:contract "(and/c hash? hash-eq? immutable? (not/c impersonator?))"
         ht)
  (for ([(k v) (in-hash ht)])
    (cond
      [(not k)
       (unless (linklet-bundle? v)
         (raise-arguments-error who
                                "value for #f key is not a linklet bundle"
                                "value" v))]
      [(symbol? k)
       (unless (linklet-directory? v)
         (raise-arguments-error who
                                "value for symbol key is not a linklet directory"
                                "value" v))]
      [else
       (raise-arguments-error who
                              "key in given hash is not #f or a symbol"
                              "key" k)]))
  (linklet-directory ht))

(define/who (hash->linklet-bundle ht)
  (check who (lambda (ht)
               (and (not (impersonator? ht))
                    (hash? ht)
                    (immutable? ht)
                    (hash-eq? ht)))
         #:contract "(and/c hash? hash-eq? immutable? (not/c impersonator?))"
         ht)
  (for ([k (in-hash-keys ht)])
    (unless (or (symbol? k) (fixnum? k))
      (raise-arguments-error who
                             "key in given hash is not a symbol or fixnum"
                             "key" k)))
  (linklet-bundle ht))

(define/who (linklet-directory->hash ld)
  (check who linklet-directory? ld)
  (linklet-directory-ht ld))
  
(define/who (linklet-bundle->hash ld)
  (check who linklet-bundle? ld)
  (linklet-bundle-ht ld))

;; ----------------------------------------

;; If there are no values that satisfy `linklet?`, then
;; assume that we have `correlated-linklet?` values.

(define (linklet-directory-machine-type ld)
  (for/or ([(k v) (in-hash (linklet-directory->hash ld))])
    (cond
      [(not k) (linklet-bundle-machine-type v)]
      [(symbol? k) (linklet-directory-machine-type v)]
      [else #f])))
      
(define (linklet-bundle-machine-type b)
  (for/or ([(k v) (in-hash (linklet-bundle->hash b))])
    (and (linklet? v)
         (or (linklet-cross-machine-type v)
             (system-type 'target-machine)))))

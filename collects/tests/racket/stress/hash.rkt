#lang racket
(require tests/stress)

(define (make-random-hash-table n)
  (for/hasheq ([i (in-range n)])
    (values i (random n))))

; hash-keys, hash-values, hash->list
(local [(define ht (make-random-hash-table 100000))]
  (stress 20
   ["for/list, in-hash-keys"
    (for/list ([k (in-hash-keys ht)])
      k)]
   ["hash-keys"
    (hash-keys ht)])
  
  (stress 20
   ["for/list, in-hash-values"
    (for/list ([v (in-hash-values ht)])
      v)]
   ["hash-values"
    (hash-values ht)])
  
  (stress 20
   ["for/list, in-hash"
    (for/list ([(k v) (in-hash ht)])
      (cons k v))]
   ["hash->list"
    (hash->list ht)]))

; hash-set*
(local [(define ht (hasheq))
        (define-syntax (inlined-hash-set* stx)
          (syntax-case stx ()
            [(_ ht 0) #'ht]
            [(_ ht n) #`(hash-set (inlined-hash-set* ht #,(sub1 (syntax->datum #'n))) n #f)]))
        (define-syntax (fun-hash-set* stx)
          (syntax-case stx ()
            [(_ ht n)
             #`(hash-set* ht #,@(apply append
                                       (for/list ([i (in-range (syntax->datum #'n))])
                                         (list i #f))))]))]
  (stress
   20
   ["inlined, hash-set*"
    (inlined-hash-set* ht 4000)]
   ["for/fold, hash-set*"
    (for/fold ([ht ht])
      ([i (in-range 4000)])
      (hash-set ht i #f))]
   ["hash-set*"
    (fun-hash-set* ht 4000)]))

; hash-set*!
(local [(define ht (make-hasheq))
        (define-syntax (inlined-hash-set*! stx)
          (syntax-case stx ()
            [(_ ht 0) #'(void)]
            [(_ ht n) #`(begin (inlined-hash-set*! ht #,(sub1 (syntax->datum #'n))) (hash-set! ht n #f))]))
        (define-syntax (fun-hash-set*! stx)
          (syntax-case stx ()
            [(_ ht n)
             #`(hash-set*! ht #,@(apply append
                                       (for/list ([i (in-range (syntax->datum #'n))])
                                         (list i #f))))]))]
  (stress
   20
   ["inlined hash-set*!"
    (inlined-hash-set*! ht 4000)]
   ["for, hash-set*!"
    (for ([i (in-range 4000)])
      (hash-set! ht i #f))]
   ["hash-set*!"
    (fun-hash-set*! ht 4000)]))
 


#lang racket
(require tests/stress)

(define (make-random-hash-table n)
  (for/hasheq ([i (in-range n)])
    (values i (random n))))

; dict-keys, dict-values, dict->list
(local [(define ht (make-random-hash-table 100000))]
  (stress 20
   ["for/list, in-dict-keys"
    (for/list ([k (in-dict-keys ht)])
      k)]
   ["dict-keys"
    (dict-keys ht)])
  
  (stress 20
   ["for/list, in-dict-values"
    (for/list ([v (in-dict-values ht)])
      v)]
   ["dict-values"
    (dict-values ht)])
  
  (stress 20
   ["for/list, in-dict"
    (for/list ([(k v) (in-dict ht)])
      (cons k v))]
   ["for/list, in-dict-pairs"
    (for/list ([p (in-dict-pairs ht)])
      p)]
   ["dict->list"
    (dict->list ht)]))

; dict-set*
(local [(define ht (hasheq))
        (define-syntax (inlined-dict-set* stx)
          (syntax-case stx ()
            [(_ ht 0) #'ht]
            [(_ ht n) #`(dict-set (inlined-dict-set* ht #,(sub1 (syntax->datum #'n))) n #f)]))
        (define-syntax (fun-dict-set* stx)
          (syntax-case stx ()
            [(_ ht n)
             #`(dict-set* ht #,@(apply append
                                       (for/list ([i (in-range (syntax->datum #'n))])
                                         (list i #f))))]))]
  (stress
   20
   ["inlined, dict-set*"
    (inlined-dict-set* ht 4000)]
   ["for/fold, dict-set*"
    (for/fold ([ht ht])
      ([i (in-range 4000)])
      (dict-set ht i #f))]
   ["dict-set*"
    (fun-dict-set* ht 4000)]))

; dict-set*!
(local [(define ht (make-hasheq))
        (define-syntax (inlined-dict-set*! stx)
          (syntax-case stx ()
            [(_ ht 0) #'(void)]
            [(_ ht n) #`(begin (inlined-dict-set*! ht #,(sub1 (syntax->datum #'n))) (dict-set! ht n #f))]))
        (define-syntax (fun-dict-set*! stx)
          (syntax-case stx ()
            [(_ ht n)
             #`(dict-set*! ht #,@(apply append
                                       (for/list ([i (in-range (syntax->datum #'n))])
                                         (list i #f))))]))]
  (stress
   20
   ["inlined dict-set*!"
    (inlined-dict-set*! ht 4000)]
   ["for, dict-set*!"
    (for ([i (in-range 4000)])
      (dict-set! ht i #f))]
   ["dict-set*!"
    (fun-dict-set*! ht 4000)]))
 
; dict-ref!
(local [(define ht (make-hasheq (list (cons 1 #f))))]
  (stress 
   200
   ["hash-ref! (present)"
    (hash-ref! ht 1 #t)]
   ["dict-ref! (present)"
    (dict-ref! ht 1 #t)]))

; XXX dict-ref! is clearly slower
(local []
  (stress 
   200
   ["hash-ref! (not present)"
    (hash-ref! (make-hasheq (list (cons 1 #f))) 2 #t)]
   ["dict-ref! (not present)"
    (dict-ref! (make-hasheq (list (cons 1 #f))) 2 #t)]))

; dict-has-key?
; XXX dict functions are slower
(local [(define ht (make-hasheq (list (cons 1 #f))))]
  (stress 
   200
   ["hash-has-key? (present)"
    (hash-has-key? ht 1)]
   ["dict-has-key? (present)"
    (dict-has-key? ht 1)]))

(local [(define ht (make-hasheq (list (cons 1 #f))))]
  (stress 
   200
   ["hash-has-key? (not present)"
    (hash-has-key? ht 2)]
   ["dict-has-key? (not present)"
    (dict-has-key? ht 2)]))


(module+ test
  (module config info
    (define random? #t)))

#lang racket/base
(require (for-syntax racket/base))

;; Lightweight variant of sets

(provide set seteq seteqv
         set?
         set-empty?
         set-member?
         set-count
         set-add
         set-remove
         set-first
         subset?
         set=?
         set-subtract
         set-union
         set-intersect
         set-partition
         set->list
         list->set
         list->seteq
         for/set
         for/seteq
         for/seteqv
         for*/set
         for*/seteq
         in-set)

(define the-empty-hash #hash())
(define the-empty-hasheq #hasheq())
(define the-empty-hasheqv #hasheqv())

(define set
  (case-lambda
    [() the-empty-hash]
    [l (for/fold ([s the-empty-hash]) ([e (in-list l)])
         (hash-set s e #t))]))
(define seteq
  (case-lambda
    [() the-empty-hasheq]
    [l (for/fold ([s the-empty-hasheq]) ([e (in-list l)])
         (hash-set s e #t))]))
(define (seteqv) the-empty-hasheqv)

(define (set? s) (hash? s))

(define (set-empty? s) (zero? (hash-count s)))
(define (set-member? s e) (hash-ref s e #f))
(define (set-count s) (hash-count s))

(define (set-add s e) (hash-set s e #t))
(define (set-remove s e) (hash-remove s e))
(define (set-first s) (hash-iterate-key s (hash-iterate-first s)))

(define-syntax in-set (make-rename-transformer #'in-immutable-hash-keys))

(define (subset? s1 s2)
  (hash-keys-subset? s1 s2))

(define (set=? s1 s2)
  (or (eq? s1 s2)
      (and (= (hash-count s1) (hash-count s2))
           (hash-keys-subset? s1 s2))))

(define (set-subtract s1 s2)
  (for/fold ([s1 s1]) ([k (in-set s2)])
    (hash-remove s1 k)))

(define (set-union s1 s2)
  (if ((set-count s1) . < . (set-count s2))
      (set-union s2 s1)
      (for/fold ([s1 s1]) ([k (in-set s2)])
        (hash-set s1 k #t))))

(define (set-intersect s1 s2)
  (if ((set-count s1) . < . (set-count s2))
      (set-intersect s2 s1)
      (for/fold ([s s2]) ([k (in-set s2)])
        (if (hash-ref s1 k #f)
            s
            (hash-remove s k)))))

(define (set-partition s pred empty-y-set empty-n-set)
  (for/fold ([y empty-y-set] [n empty-n-set]) ([v (in-set s)])
    (if (pred v)
        (values (set-add y v) n)
        (values y (set-add n v)))))

(define (set->list s)
  (for/list ([k (in-set s)])
    k))

(define (list->set l)
  (for/set ([k (in-list l)])
    k))

(define (list->seteq l)
  (for/seteq ([k (in-list l)])
    k))

(define-syntax-rule (for/set bindings body ...)
  (for/hash bindings (values
                      (let ()
                        body ...)
                      #t)))

(define-syntax-rule (for/seteq bindings body ...)
  (for/hasheq bindings (values
                        (let ()
                          body ...)
                        #t)))

(define-syntax-rule (for/seteqv bindings body ...)
  (for/hasheqv bindings (values
                         (let ()
                           body ...)
                         #t)))

(define-syntax-rule (for*/set bindings body ...)
  (for*/hash bindings (values
                       (let ()
                         body ...)
                       #t)))

(define-syntax-rule (for*/seteq bindings body ...)
  (for*/hasheq bindings (values
                         (let ()
                           body ...)
                         #t)))

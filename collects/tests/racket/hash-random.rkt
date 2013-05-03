#lang racket/base

(struct s (code val)
  #:property 
  prop:equal+hash
  (list
   (λ (a b eql?) (and (eql? (s-code a) (s-code b))
                      (eql? (s-val a) (s-val b))))
   (λ (a hash) (s-code a))
   (λ (b hash) 1)))
  
(define (exercise ht)
   ;; test our hash
  (for ([(k v) (in-hash ht)]) v))

;; permute : (listof x) -> (listof x)
;; randomly permute l
(define (permute l)
  (cond
    [(null? l) '()]
    [else (define i (random (length l)))
          (cons (list-ref l i)
                (for/list ([e (in-list l)]
                           [j (in-naturals)]
                           #:unless (= i j))
                  e))]))

(define-syntax-rule
  (time/msg e str)
  (time (begin (printf "~a:\n  " str)
               e)))

;; randomly insert a bunch with random 
;; hash codes and then remove them all
;; in a random order; bias the keys to
;; be more likely to use -1 than other numbers
(time/msg
 (for ([_ (in-range 1000)])
   (define in-there '())
   (define ht (hash))
   (for ([y (in-range 100)]
         [i (in-naturals)])
     (define nxt (s (if (zero? (random 4))
                        -1
                        (random 100))
                    i))
     (set! in-there (cons nxt in-there))
     (set! ht (hash-set ht nxt #f))
     (exercise ht))
   (for ([x (in-list (permute in-there))])
     (set! ht (hash-remove ht x))
     (exercise ht)))
 "different hash codes, add and remove")

;; focus in on trees with the same hash-code
;; add a bunch and then remove them all
(time/msg
 (for ([_ (in-range 200)])
   (define v (build-vector 100 (λ (i) (s 0 i))))
   (define ht (hash))
   (for ([e (in-vector v)])
     (set! ht (hash-set ht e #f))
     (exercise ht))
   (for ([index (in-list (permute (build-list (vector-length v) values)))])
     (set! ht (hash-remove ht (vector-ref v index)))
     (exercise ht)))
 "same hash code, add and remove")

;; add a bunch and then remove them, but as you remove
;; them, add 3 new ones ones in. Abstract over
;; the initial hash-table
(define (add-and-remove base-ht iterations)
  (define elements 40)
  (time/msg
   (for ([_ (in-range iterations)])
     (define v (build-vector elements (λ (i) (s 0 i))))
     (define ht base-ht)
     (for ([e (in-vector v)])
       (set! ht (hash-set ht e #f))
       (exercise ht))
     (for ([index (in-list (permute (build-list elements values)))]
           [i (in-naturals)])
       (set! ht (hash-remove ht (vector-ref v index)))
       (exercise ht)
       (for ([j (in-range 3)])
         (set! ht (hash-set ht (s 0 (+ (* i 100) j)) #f))
         (exercise ht))))
   "add-and-remove"))

(add-and-remove (hash) 100)
(add-and-remove 
 (time/msg 
  (for/hash ([i (in-range (expt 2 17))])
    (values (s i i) #f))
  "building big no-collisions hash")
 1)
(add-and-remove 
 (time/msg
  (for/hash ([i (in-range (expt 2 11))])
    (values (s -123 i) #f))
  "building big -123 hash")
 1)
(add-and-remove 
 (time/msg
  (for/hash ([i (in-range (expt 2 11))])
    (values (s 0 (cons i i)) #f))
  "building big 0 hash")
 1)

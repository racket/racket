#lang racket/base

(provide hash-code-combine
         hash-code-combine*
         hash-code-combine-unordered
         hash-code-combine-unordered*)

(require racket/fixnum)

;; Adapted from racket/src/cs/rumble/hash-code.ss
;; which is adapted from ChezScheme/s/newhash.ss

;; maps non-fixnum integers to positive fixnums
(define (->fx v [who '->fx])
  (cond
    [(fixnum? v) v]
    [(exact-integer? v) (bitwise-and v (most-positive-fixnum))]
    [else (raise-argument-error who "exact-integer?" v)]))

;; Adapted from `mix-hash-code` in racket/src/cs/rumble/hash-code.ss
;; which is adapted from an `update` function in ChezScheme/s/newhash.ss
;; and adapted from One-at-a-Time Hash in "A Hash Function for Hash Table Lookup"
;; Bob Jenkins's expansion of his September 1997 Dr Dobbs article on Hash Functions
;; http://www.burtleburtle.net/bob/hash/doobs.html#one
(define (fxmix-hash-code hc)
  (let ([hc2 (fx+/wraparound hc (fxlshift/wraparound (fx+/wraparound hc 1) 10))])
    (fxxor hc2 (fxrshift/logical hc2 6))))

;; NOTE: this `hash-code-combine` function is NOT the same as the
;; `hash-code-combine` from racket/src/cs/rumble/hash-code.ss
;; (combine) = 0
;; (combine A) = (mix A)
;; (combine A B) = (unordered (mix A) (mix (unordered (mix A) B)))
;; Example reshuffles to avoid collision:
;; (unordered (combine A B) C) != (unordered (combine A C) B)
;; (combine A (mix B)) != (combine B (mix A))
(define hash-code-combine
  (let ([who 'hash-code-combine])
    (case-lambda
      [() 0]
      [(a) (fxmix-hash-code (->fx a who))]
      [(a b) (let ([mxa (fxmix-hash-code (->fx a who))])
               (fx+/wraparound
                mxa
                (fxmix-hash-code (fx+/wraparound mxa (->fx b who)))))]
      [hcs (hash-codes-combine 0 hcs who)])))

(define hash-code-combine*
  (let ([who 'hash-code-combine*])
    (case-lambda
      [(l) (hash-codes-combine 0 (check-list l who) who)]
      [(f . r)
       (let loop ([accum (list f)] [r r])
         (if (null? (cdr r))
             (hash-codes-combine
              (hash-codes-combine 0 (reverse accum) who)
              (check-list (car r) who)
              who)
             (loop (cons (car r) accum) (cdr r))))])))

(define (hash-codes-combine acc hcs who)
  (for/fold ([acc acc]) ([hc (in-list hcs)])
    (fx+/wraparound
     acc
     (fxmix-hash-code (fx+/wraparound acc (->fx hc who))))))

;; hash-code-combine-unordered is commutative and associative
;; (unordered) = 0
;; (unordered A) = A
;; (unordered A B) = (unordered B A)
;; (unordered A B C) = (unordered A (unordered B C)) = (unordered (unordered A B) C)
(define hash-code-combine-unordered
  (let ([who 'hash-code-combine-unordered])
    (case-lambda
      [() 0]
      [(a) (->fx a who)]
      [(a b) (fx+/wraparound (->fx a who) (->fx b who))]
      [hcs (hash-codes-combine-unordered 0 hcs who)])))

(define hash-code-combine-unordered*
  (let ([who 'hash-code-combine-unordered*])
    (case-lambda
      [(l) (hash-codes-combine-unordered 0 (check-list l who) who)]
      [(f . r)
       (let loop ([accum (list f)] [r r])
         (if (null? (cdr r))
             (hash-codes-combine-unordered
              (hash-codes-combine-unordered 0 (reverse accum) who)
              (check-list (car r) who)
              who)
             (loop (cons (car r) accum) (cdr r))))])))

(define (hash-codes-combine-unordered acc hcs who)
  (for/fold ([acc acc]) ([hc (in-list hcs)])
    (fx+/wraparound acc (->fx hc who))))

(define (check-list hcs who)
  (unless (and (list? hcs)
               (for/and ([e (in-list hcs)])
                 (exact-integer? e)))
    (raise-argument-error who "(listof exact-integer?)" hcs))
  hcs)

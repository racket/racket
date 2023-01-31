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
  (case-lambda
    [() 0]
    [(a) (fxmix-hash-code (->fx a 'hash-code-combine))]
    [(a b)
     (let ([mxa (fxmix-hash-code (->fx a 'hash-code-combine))])
       (fx+/wraparound
        mxa
        (fxmix-hash-code (fx+/wraparound mxa (->fx b 'hash-code-combine)))))]
    [hcs (hash-codes-combine hcs)]))

(define hash-code-combine*
  (case-lambda
    [(l) (hash-codes-combine l)]
    [(f . r) (hash-codes-combine (apply list* f r))]))

(define (hash-codes-combine hcs)
  (for/fold ([acc 0]) ([hc (in-list hcs)])
    (fx+/wraparound
     acc
     (fxmix-hash-code (fx+/wraparound acc (->fx hc 'hash-code-combine*))))))

;; hash-code-combine-unordered is commutative and associative
;; (unordered) = 0
;; (unordered A) = A
;; (unordered A B) = (unordered B A)
;; (unordered A B C) = (unordered A (unordered B C)) = (unordered (unordered A B) C)
(define hash-code-combine-unordered
  (case-lambda
    [() 0]
    [(a) (->fx a 'hash-code-combine-unordered)]
    [(a b)
     (fx+/wraparound (->fx a 'hash-code-combine-unordered)
                     (->fx b 'hash-code-combine-unordered))]
    [hcs (hash-codes-combine-unordered hcs)]))

(define hash-code-combine-unordered*
  (case-lambda
    [(l) (hash-codes-combine-unordered l)]
    [(f . r) (hash-codes-combine-unordered (apply list* f r))]))

(define (hash-codes-combine-unordered hcs)
  (for/fold ([acc 0]) ([hc (in-list hcs)])
    (fx+/wraparound acc (->fx hc 'hash-code-combine-unordered*))))

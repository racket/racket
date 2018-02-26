#lang racket/base
(require "../common/range.rkt"
         "regexp.rkt"
         "lazy-bytes.rkt"
         "interp.rkt"
         "../analyze/must-string.rkt")

(provide search-match)

;; ------------------------------------------------------------
;; The driver iterates through the input (unless the pattern is
;; anchored) to find a match

(define (search-match rx in pos start-pos end-pos state)
  (define must-string (rx:regexp-must-string rx))
  (cond
   [(not (check-must-string must-string in pos end-pos))
    (values #f #f)]
   [else
    (define matcher (rx:regexp-matcher rx))
    (define anchored? (rx:regexp-anchored? rx))
    (define start-range (rx:regexp-start-range rx))
    (let loop ([pos pos])
      (cond
       [(and anchored? (not (= pos start-pos)))
        (values #f #f)]
       [(and start-range
             (if (bytes? in)
                 (= pos end-pos)
                 (not (lazy-bytes-before-end? in pos end-pos))))
        (values #f #f)]
       [(and start-range
             (not (check-start-range start-range in pos end-pos)))
        (loop (add1 pos))]
       [else
        (define pos2 (interp matcher in pos start-pos end-pos state))
        (cond
         [pos2 (values pos pos2)]
         [start-range (loop (add1 pos))]
         [(if (bytes? in)
              (pos . < . end-pos)
              (lazy-bytes-before-end? in pos end-pos))
          (define pos2 (add1 pos))
          (unless (bytes? in)
            (lazy-bytes-advance! in pos2 #f))
          (loop pos2)]
         [else (values #f #f)])]))]))

;; ------------------------------------------------------------------
;; Checking for a must string (before iterating though the input) can
;; speed up a match failure by avoiding backtracking:

(define (check-must-string must-string in pos end-pos)
  (cond
   [(not must-string) #t]
   [(not (bytes? in)) #t]
   [(bytes? must-string)
    (cond
     [(= 1 (bytes-length must-string))
      ;; Check for a single byte
      (define mc (bytes-ref must-string 0))
      (for/or ([c (in-bytes in pos end-pos)])
        (= c mc))]
     [else
      ;; Check for a byte string
      (define mc1 (bytes-ref must-string 0))
      (for/or ([i (in-range pos (- end-pos (sub1 (bytes-length must-string))))])
        (and (= mc1 (bytes-ref in i))
             (for/and ([c (in-bytes in (add1 i))]
                       [mc (in-bytes must-string 1)])
               (= c mc))))])]
   [else
    ;; Check against a sequence of ranges
    (for/or ([i (in-range pos (- end-pos (sub1 (length must-string))))])
      (let loop ([i i] [l must-string])
        (cond
         [(null? l) #t]
         [else
          (define e (car l))
          (and (rng-in? e (bytes-ref in i))
               (loop (add1 i) (cdr l)))])))]))

;; ------------------------------------------------------------------
;; Checking for a startup byte can speed up a match failure by
;; avoiding the general pattern checker:

(define (check-start-range start-range in pos end-pos)
  (rng-in? start-range
           (if (bytes? in)
               (bytes-ref in pos)
               (lazy-bytes-ref in pos))))

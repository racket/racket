#lang racket/base
(require "../common/range.rkt")

(provide (all-defined-out))

(define rx:never 'never)
(define rx:empty 'empty)
(define rx:any 'any)
(define rx:start 'start)
(define rx:end 'end)
(define rx:line-start 'line-start)
(define rx:line-end 'line-end)
(define rx:word-boundary 'word-boundary)
(define rx:not-word-boundary 'not-word-boundary)

;; exact integer : match single byte or char
;; byte string : match content sequence
;; string : match content sequence

(struct rx:alts (rx1 rx2) #:transparent)
(struct rx:sequence (rxs needs-backtrack?) #:transparent)
(struct rx:group (rx number) #:transparent)
(struct rx:repeat (rx min max non-greedy?) #:transparent)
(struct rx:maybe (rx non-greedy?) #:transparent) ; special case in size validation
(struct rx:conditional (tst rx1 rx2 n-start num-n needs-backtrack?) #:transparent)
(struct rx:lookahead (rx match? n-start num-n) #:transparent)
(struct rx:lookbehind (rx match? [lb-min #:mutable] [lb-max #:mutable] ; min & max set by `validate`
                          n-start num-n)
        #:transparent)
(struct rx:cut (rx n-start num-n needs-backtrack?) #:transparent)
(struct rx:reference (n case-sensitive?) #:transparent)
(struct rx:range (range) #:transparent)
(struct rx:unicode-categories (symlist match?) #:transparent)

;; We need to backtrack for `rx` if it has alternatives;
;; we also count as backtracking anything complex enough
;; to match different numbers of elements in an immediate
;; repetition
(define (needs-backtrack? rx)
  (cond
   [(rx:alts? rx) #t]
   [(rx:sequence? rx) (rx:sequence-needs-backtrack? rx)]
   [(rx:group? rx) #t] ; to unwind success mappings
   [(rx:repeat? rx) #t]
   [(rx:maybe? rx) #t]
   [(rx:conditional? rx) (rx:conditional-needs-backtrack? rx)]
   [(rx:cut? rx) (rx:cut-needs-backtrack? rx)] ; doesn't actually backtrack, but count varies
   [(rx:unicode-categories? rx) #t]
   [else #f]))

(define (rx-range range limit-c)
  (cond
   [(range-singleton range) => (lambda (c) c)]
   [(range-includes? range 0 limit-c) rx:any]
   [else (rx:range range)]))

(define (rx-sequence l)
  (cond
   [(null? l) rx:empty]
   [(null? (cdr l)) (car l)]
   [else
    (define merged-l (merge-adjacent l))
    (cond
     [(null? (cdr merged-l)) (car merged-l)]
     [else (rx:sequence merged-l (ormap needs-backtrack? merged-l))])]))

(define (merge-adjacent l)
  ;; `mode` tracks whether `accum` has byte or char strings,
  ;; where a #f `mode` means that `accum` is empty
  (let loop ([mode #f] [accum null] [l l])
    (cond
     [(and (pair? l)
           (rx:sequence? (car l)))
      ;; Flatten nested sequences
      (loop mode accum (append (rx:sequence-rxs (car l)) (cdr l)))]
     [(and (pair? l)
           (or (eq? rx:empty (car l))
               (equal? "" (car l))
               (equal? #"" (car l))))
      ;; Drop empty element
      (loop mode accum (cdr l))]
     [(or (null? l)
          (not (case mode
                 [(byte) (or (byte? (car l))
                             (bytes? (car l)))]
                 [(char) (or (integer? (car l))
                             (string? (car l)))]
                 [else #t])))
      ;; Compatible subsequence ended
      (cond
       [(null? accum)
        ;; Must be of `l`, with nothing in accumulator
        null]
       [(null? (cdr accum))
        ;; Subsequence is just one element after all
        (cons (car accum) (loop #f null l))]
       [else
        ;; Combine elements in `accum`
        (cons (case mode
                [(byte) (apply bytes-append
                               (for/list ([a (in-list (reverse accum))])
                                 (cond
                                  [(byte? a) (bytes a)]
                                  [else a])))]
                [(char) (apply string-append
                               (for/list ([a (in-list (reverse accum))])
                                 (cond
                                  [(integer? a) (string (integer->char a))]
                                  [else a])))]
                [else (error "internal error")])
              (loop #f null l))])]
     [mode
      ;; Continue in same mode
      (loop mode (cons (car l) accum) (cdr l))]
     [(or (byte? (car l))
          (bytes? (car l)))
      ;; Start byte mode
      (loop 'byte (list (car l)) (cdr l))]
     [(or (integer? (car l))
          (string? (car l)))
      ;; Start character mode
      (loop 'char (list (car l)) (cdr l))]
     [else
      ;; No combination possible
      (cons (car l) (loop #f null (cdr l)))])))

(define (rx-alts rx1 rx2 limit-c)
  (cond
   [(eq? rx:never rx1) rx2]
   [(eq? rx:never rx2) rx1]
   [(and (rx:range? rx1) (rx:range? rx2))
    (rx-range (range-union (rx:range-range rx1)
                           (rx:range-range rx2))
              limit-c)]
   [(and (rx:range? rx1) (rx:alts? rx2) (rx:range? (rx:alts-rx1 rx2)))
    (rx-alts (rx-alts rx1 (rx:alts-rx1 rx2) limit-c)
             (rx:alts-rx2 rx2)
             limit-c)]
   [(and (rx:range? rx1) (integer? rx2))
    (rx-range (range-add (rx:range-range rx1) rx2) limit-c)]
   [(and (rx:range? rx2) (integer? rx1))
    (rx-alts rx2 rx1 limit-c)]
   [(and (integer? rx1) (integer? rx2))
    (rx-range (range-add (range-add empty-range rx1) rx2) limit-c)]
   [else
    (rx:alts rx1 rx2)]))

(define (rx-group rx n)
  (rx:group rx n))

(define (rx-cut rx n-start num-n)
  (rx:cut rx n-start num-n (needs-backtrack? rx)))

(define (rx-conditional tst pces1 pces2 n-start num-n)
  (rx:conditional tst pces1 pces2 n-start num-n (or (needs-backtrack? pces1)
                                                    (needs-backtrack? pces2))))

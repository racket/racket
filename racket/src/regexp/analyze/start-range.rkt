#lang racket/base
(require "../parse/ast.rkt"
         "../common/range.rkt")

(provide get-start-range)

;; Returns a compiled range for bytes that must appear at the start,
;; or #f if no such set is known
(define (get-start-range rx)
  (define r (start-range rx))
  (and r (compile-range r)))

(define (start-range rx)
  (cond
   [(integer? rx) (range-add empty-range rx)]
   [(bytes? rx) (range-add empty-range (bytes-ref rx 0))]
   [(rx:sequence? rx)
    (let loop ([l (rx:sequence-rxs rx)])
      (cond
       [(null? l) #f]
       [else
        (define rx (car l))
        (cond
         [(zero-sized? rx)
          ;; Zero-sized element, so look at rest
          (loop (cdr l))]
         [else
          (start-range rx)])]))]
   [(rx:alts? rx)
    (union (start-range (rx:alts-rx1 rx))
           (start-range (rx:alts-rx2 rx)))]
   [(rx:conditional? rx)
    (union (start-range (rx:conditional-rx1 rx))
           (start-range (rx:conditional-rx2 rx)))]
   [(rx:group? rx)
    (start-range (rx:group-rx rx))]
   [(rx:cut? rx)
    (start-range (rx:cut-rx rx))]
   [(rx:repeat? rx)
    (and (positive? (rx:repeat-min rx))
         (start-range (rx:repeat-rx rx)))]
   [(rx:range? rx) (rx:range-range rx)]
   [else #f]))

(define (zero-sized? rx)
  (or (eq? rx rx:empty)
      (eq? rx rx:start)
      (eq? rx rx:line-start)
      (eq? rx rx:word-boundary)
      (eq? rx rx:not-word-boundary)
      (rx:lookahead? rx)
      (rx:lookbehind? rx)
      (and (rx:group? rx)
           (zero-sized? (rx:group-rx rx)))
      (and (rx:cut? rx)
           (zero-sized? (rx:cut-rx rx)))))

(define (union a b)
  (and a b (range-union a b)))

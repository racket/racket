#lang racket/base
(require "../parse/ast.rkt")

(provide anchored?)

;; Determine whether a regexp definitely can only match at the start
;; of input. (A converative `#f` is ok.)

(define (anchored? rx)
  (cond
   [(eq? rx rx:start) #t]
   [(rx:sequence? rx)
    (let loop ([rxs (rx:sequence-rxs rx)])
      (cond
       [(null? rxs) #f]
       [(rx:lookahead? (car rxs)) (loop (cdr rxs))]
       [(rx:lookbehind? (car rxs)) (loop (cdr rxs))]
       [else (anchored? (car rxs))]))]
   [(rx:alts? rx)
    (and (anchored? (rx:alts-rx1 rx))
         (anchored? (rx:alts-rx2 rx)))]
   [(rx:conditional? rx)
    (and (anchored? (rx:conditional-rx1 rx))
         (anchored? (rx:conditional-rx2 rx)))]
   [(rx:group? rx)
    (anchored? (rx:group-rx rx))]
   [(rx:cut? rx)
    (anchored? (rx:cut-rx rx))]
   [else #f]))

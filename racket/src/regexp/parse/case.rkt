#lang racket/base
(require "../common/range.rkt"
         "config.rkt")

;; Add case-insensitive mappins as specified by `config`
(provide range-add*
         range-add-span*)

(define (range-add* range c config)
  (cond
   [(not c) range]
   [else
    (define range2 (range-add range c))
    (cond
     [(parse-config-case-sensitive? config) range2]
     [else
      (define range3 (range-add range2 (char->integer (char-upcase (integer->char c)))))
      (define range4 (range-add range3 (char->integer (char-foldcase (integer->char c)))))
      (range-add range4 (char->integer (char-downcase (integer->char c))))])]))

(define (range-add-span* range from-c to-c config)
  (cond
   [(parse-config-case-sensitive? config)
    (range-add-span range from-c to-c)]
   [else
    (for/fold ([range range]) ([c (in-range from-c (add1 to-c))])
      (range-add* range c config))]))

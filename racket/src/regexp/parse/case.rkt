#lang racket/base
(require "../common/range.rkt"
         "config.rkt")

;; Add case-insensitive mappings as specified by `config`; when
;; `limit-c` is 255 for a byte regexp, only ASCII characters are
;; mapped, which also keeps the range within 0 to 255
(provide range-add*
         range-add-span*)

(define (range-add* range c config limit-c)
  (cond
   [(not c) range]
   [else
    (define range2 (range-add range c))
    (cond
     [(parse-config-case-sensitive? config) range2]
     [(and (c . > . 127) (limit-c . < . 256)) range2]
     [else
      (define range3 (range-add range2 (char->integer (char-upcase (integer->char c)))))
      (define range4 (range-add range3 (char->integer (char-foldcase (integer->char c)))))
      (range-add range4 (char->integer (char-downcase (integer->char c))))])]))

(define (range-add-span* range from-c to-c config limit-c)
  (cond
   [(parse-config-case-sensitive? config)
    (range-add-span range from-c to-c)]
   [else
    (for/fold ([range range]) ([c (in-range from-c (add1 to-c))])
      (range-add* range c config limit-c))]))

#lang racket/base
(require "config.rkt"
         "readtable.rkt"
         "parameter.rkt")

(provide readtable-char-delimiter?
         char-delimiter?)

(define (readtable-char-delimiter? rt c config)
  (define dc (or (and rt
                      (hash-ref (readtable-delimiter-ht rt) c #f)) ; #f => default for `c`
                 c))
  (cond
   [(eq? dc 'no-delimit) #f]
   [(not (char? dc)) #t]
   [else
    (or (char-whitespace? dc)
        (char=? dc #\()
        (char=? dc #\))
        (char=? dc #\[)
        (char=? dc #\])
        (char=? dc #\{)
        (char=? dc #\})
        (char=? dc #\')
        (char=? dc #\`)
        (char=? dc #\,)
        (char=? dc #\;)
        (char=? dc #\")
        (and (char=? dc #\.)
             (check-parameter read-cdot config)))]))

(define (char-delimiter? c config)
  (readtable-char-delimiter? (read-config-readtable config) c config))

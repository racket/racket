#lang racket/base
(require "write-with-max.rkt"
         "mode.rkt")

(provide print-mlist)

(define (print-mlist p who v mode o max-length graph config)
  (define unquoted? (eq? mode PRINT-MODE/UNQUOTED))
  (let ([max-length
         (cond
           [unquoted? (write-string/max "(mcons " o max-length)]
           [else (write-string/max "{" o max-length)])])
    (let loop ([v v] [max-length max-length])
      (cond
        [(eq? max-length 'full) 'full]
        [(and (null? (mcdr v))
              (not unquoted?))
         (let ([max-length (p who (mcar v) mode o max-length graph config)])
           (write-string/max "}" o max-length))]
        [(and (mpair? (mcdr v))
              (or (not graph) (not (hash-ref graph (mcdr v) #f)))
              (not unquoted?))
         (let ([max-length (p who (mcar v) mode o max-length graph config)])
           (loop (mcdr v) (write-string/max " " o max-length)))]
        [else
         (let* ([max-length (p who (mcar v) mode o max-length graph config)]
                [max-length (if unquoted?
                                (write-string/max " " o max-length)
                                (write-string/max " . " o max-length))]
                [max-length (p who (mcdr v) mode o max-length graph config)])
           (write-string/max (if unquoted? ")" "}") o max-length))]))))

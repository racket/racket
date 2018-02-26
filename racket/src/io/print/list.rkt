#lang racket/base
(require "write-with-max.rkt"
         "mode.rkt"
         "graph.rkt")

(provide print-list)

(define (print-list p who v mode o max-length graph config alt-list-prefix alt-list-constructor)
  (define unquoted-pairs?
    (and (eq? mode PRINT-MODE/UNQUOTED)
         (not alt-list-constructor)
         (not (uninterrupted-list? v graph))))
  (let ([max-length
         (cond
           [(eq? mode PRINT-MODE/UNQUOTED)
            (let ([max-length
                   (if unquoted-pairs?
                       (write-string/max "(cons" o max-length)
                       (write-string/max (or alt-list-constructor "(list") o max-length))])
              (cond
                [(null? v) max-length]
                [else (write-string/max " " o max-length)]))]
           [else (write-string/max (or alt-list-prefix "(") o max-length)])])
    (let loop ([v v] [max-length max-length])
      (cond
        [(eq? max-length 'full) 'full]
        [(null? v) (write-string/max ")" o max-length)]
        [(and (null? (cdr v))
              (not unquoted-pairs?))
         (let ([max-length (p who (car v) mode o max-length graph config)])
           (write-string/max ")" o max-length))]
        [(and (pair? (cdr v))
              (or (not graph) (non-graph? (hash-ref graph (cdr v) #f)))
              (not unquoted-pairs?))
         (let ([max-length (p who (car v) mode o max-length graph config)])
           (loop (cdr v) (write-string/max " " o max-length)))]
        [else
         (let* ([max-length (p who (car v) mode o max-length graph config)]
                [max-length (if unquoted-pairs?
                                (write-string/max " " o max-length)
                                (write-string/max " . " o max-length))]
                [max-length (p who (cdr v) mode o max-length graph config)])
           (write-string/max ")" o max-length))]))))

(define (uninterrupted-list? v graph)
  (and (list? v)
       (let loop ([v v])
         (cond
           [(null? v) #t]
           [(non-graph? (hash-ref graph v #f))
            (loop (cdr v))]
           [else #f]))))

(define (non-graph? g)
  (or (not g)
      (and (as-constructor? g)
           (not (as-constructor-tag g)))))

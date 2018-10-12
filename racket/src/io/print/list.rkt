#lang racket/base
(require "write-with-max.rkt"
         "mode.rkt"
         "graph.rkt"
         "config.rkt"
         "parameter.rkt"
         "symbol.rkt")

(provide print-list)

(define (print-list p who v mode o max-length graph config alt-list-prefix alt-list-constructor)
  (define unquoted-pairs?
    (and (eq? mode PRINT-MODE/UNQUOTED)
         (not alt-list-constructor)
         (not (uninterrupted-list? v graph))))
  (define (abbreviation v)
    (and (eq? mode PRINT-MODE/QUOTED)
         (pair? v)
         (pair? (cdr v))
         (null? (cddr v))
         (not alt-list-constructor)
         (config-get config print-reader-abbreviations)
         (let ([starts-@? (lambda (v)
                            (and (symbol? v)
                                 (let ([s (symbol->print-string v #:config config)])
                                   (char=? #\@ (string-ref s 0)))))])
           (case (car v)
             [(quote) "'"]
             [(quasiquote) "`"]
             [(unquote) (if (starts-@? (cadr v)) ", " ",")]
             [(unquote-splicing) ",@"]
             [(syntax) "#'"]
             [(quasisyntax) "#`"]
             [(unsyntax) (if (starts-@? (cadr v)) "#, " "#,")]
             [(unsyntax-splicing) "#,@"]
             [else #f]))))
  (cond
    [(abbreviation v)
     => (lambda (prefix)
          (p who (cadr v) mode o (write-string/max prefix o max-length) graph config))]
    [else
     (let ([max-length
            (cond
              [(eq? mode PRINT-MODE/UNQUOTED)
               (let ([max-length
                      (if unquoted-pairs?
                          (if (multiple-pairs? v graph)
                              (write-string/max "(list*" o max-length)
                              (write-string/max "(cons" o max-length))
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
                 (not (abbreviation (cdr v))))
            (let ([max-length (p who (car v) mode o max-length graph config)])
              (loop (cdr v) (write-string/max " " o max-length)))]
           [(abbreviation v)
            => (lambda (prefix)
                 ;; Assume a "." has already printed
                 (p who (cadr v) mode o (write-string/max prefix o max-length) graph config))]
           [else
            (let* ([max-length (p who (car v) mode o max-length graph config)]
                   [max-length (if unquoted-pairs?
                                   (write-string/max " " o max-length)
                                   (write-string/max " . " o max-length))]
                   [max-length (p who (cdr v) mode o max-length graph config)])
              (write-string/max ")" o max-length))])))]))

(define (uninterrupted-list? v graph)
  (and (list? v)
       (let loop ([v (cdr v)])
         (cond
           [(null? v) #t]
           [(non-graph? (hash-ref graph v #f))
            (loop (cdr v))]
           [else
            #f]))))

(define (multiple-pairs? v graph)
  (define d (cdr v))
  (and (pair? d)
       (non-graph? (hash-ref graph d #f))))

(define (non-graph? g)
  (or (not g)
      (and (as-constructor? g)
           (not (as-constructor-tag g)))))

#lang racket/base
(require "../port/string-output.rkt"
         (only-in "../path/path.rkt" path?)
         (only-in "../path/string.rkt" path->string)
         "write-with-max.rkt"
         "symbol.rkt")

(provide print-named)

(define (print-named what v mode o max-length)
  (define name (object-name v))
  (let* ([max-length (write-string/max "#<" o max-length)]
         [max-length (write-string/max what o max-length)]
         [name-str
          (cond
            [(symbol? name)
             (symbol->print-string name #:for-type? #t)]
            [(path? name) ; especially for input & output ports
             (path->string name)]
            [(string? name)
             name]
            [else #f])])
    (cond
      [name-str
       (let* ([max-length (write-string/max ":" o max-length)]
              [max-length (write-string/max name-str o max-length)])
         (write-string/max ">" o max-length))]
      [else
       (write-string/max ">" o max-length)])))

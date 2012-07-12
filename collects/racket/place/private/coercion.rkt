#lang racket/base

(provide ->path
         ->writeable-module-path
         ->module-path
         ->number
         ->string
         ->length
         ->symbol)

(define (->path-bytes x)
  (cond
    [(path? x) (path->bytes x)]
    [else x]))

(define (path-bytes->path x)
  (cond
    [(bytes? x) (bytes->path x)]
    [else x]))

(define (path-bytes->string x)
  (cond
    [(bytes? x) (bytes->string/locale x)]
    [else x]))

(define (->path x)
  (cond [(path? x) x]
        [(string? x) (string->path x)]
        [(bytes? x) (bytes->path x)]))

(define (->writeable-module-path x)
  (cond [(path? x) (path->bytes x)]
      [(list? x) (map ->path-bytes x)]
      [(string? x) (string->bytes/locale x)]
      [(bytes? x) x]))

(define (->module-path x)
  (cond [(path? x) x]
        [(and (list? x) (pair? x))
         (cond
           [(equal? (car x) 'file) (map path-bytes->string x)]
           [else (map path-bytes->path x)])]
        [(bytes? x) (bytes->path x)]
        [(string? x) (string->path x)]))

(define (->number x)
  (cond [(number? x) x]
        [(string? x) (string->number x)]))

(define (->string x)
  (cond [(string? x) x]
        [(number? x) (number->string x)]
        [(symbol? x) (symbol->string x)]
        [(bytes? x) (bytes->string/locale x)]
        [else (raise-type-error '->string "a string, number, symbol, or bytes" x)]
        ))

(define (->length x)
  (cond [(string? x) (string-length x)]
        [(bytes? x) (bytes-length x)]
        [(list?  x) (length x)]))

(define (->symbol x)
  (cond
    [(symbol? x) x]
    [(string? x) (string->symbol x)]
    [(bytes? x) (string->symbol (bytes->string/locale x))]))


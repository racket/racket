#lang racket/base
(require "../port/string-output.rkt"
         "write-with-max.rkt")

(provide print-hash)

(define (print-hash v o max-length p who mode graph config)
  (define tag (cond
                [(hash-eq? v) "#hasheq("]
                [(hash-eqv? v) "#hasheqv("]
                [else "#hash("]))
  (define keys (try-sort (hash-keys v)))
  (let loop ([keys keys] [max-length (write-string/max tag o max-length)] [first? #t])
    (cond
      [(eq? max-length 'full) 'full]
      [(null? keys)
       (write-string/max ")" o max-length)]
      [else
       (define key (car keys))
       (define val (hash-ref v key none))
       (cond
         [(eq? val none)
          ;; hash table changed, or maybe an impersonator does strange things to the table
          (loop (cdr keys) max-length first?)]
         [else
          (let* ([max-length (write-string/max (if first? "(" " (") o max-length)]
                 [max-length (p who key mode o max-length graph config)]
                 [max-length (write-string/max " . " o max-length)]
                 [max-length (p who val mode o max-length graph config)])
            (loop (cdr keys) (write-string/max ")" o max-length) #f))])])))

(define none (gensym 'none))

(define (try-sort keys)
  (cond
    [(null? keys) null]
    [(real? (car keys))
     (if (andmap real? (cdr keys))
         (sort keys <)
         keys)]
    [(symbol? (car keys))
     (if (andmap symbol? (cdr keys))
         (sort keys symbol<?)
         keys)]
    [(keyword? (car keys))
     (if (andmap keyword? (cdr keys))
         (sort keys keyword<?)
         keys)]
    [(string? (car keys))
     (if (andmap string? (cdr keys))
         (sort keys string<?)
         keys)]
    [(bytes? (car keys))
     (if (andmap bytes? (cdr keys))
         (sort keys bytes<?)
         keys)]
    [else keys]))

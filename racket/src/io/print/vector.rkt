#lang racket/base
(require "list.rkt"
         "mode.rkt"
         "config.rkt"
         "parameter.rkt")

(provide print-vector)

(define (print-vector p who v mode o max-length graph config fx/l-prefix v-length v-ref equ?)
  (define (v->list v len)
    (cond
      [(zero? len) '()]
      [else (let loop ([i (sub1 len)] [accum '()])
              (define val (v-ref v i))
              (cond
                [(zero? i) (cons val accum)]
                [else (loop (sub1 i) (cons val accum))]))]))
  (define cns (string-append "(" fx/l-prefix "vector"))
  (cond
    [(and (not (eq? mode PRINT-MODE/UNQUOTED))
          (not (eq? mode DISPLAY-MODE))
          (config-get config print-vector-length))
     (define len (v-length v))
     (define same-n
       (cond
         [(<= len 1) 0]
         [else
          (define last (v-ref v (sub1 len)))
          (let loop ([i (- len 2)] [accum 0])
            (cond
              [(< i 0) accum]
              [(equ? (v-ref v i) last)
               (loop (sub1 i) (add1 accum))]
              [else accum]))]))
     (define lst (v->list v (- len same-n)))
     (define lbl (string-append "#" fx/l-prefix (number->string len) "("))
     (print-list p who lst mode o max-length graph config lbl cns)]
    [else
     (define lbl (string-append "#" fx/l-prefix "("))
     (print-list p who (v->list v (v-length v)) mode o max-length graph config lbl cns)]))

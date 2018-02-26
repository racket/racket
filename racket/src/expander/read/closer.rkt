#lang racket/base
(require "parameter.rkt"
         "config.rkt"
         "readtable.rkt")

(provide char-closer?
         closer-name
         closer->opener
         opener-name
         dot-name
         all-openers-str)

(define (char-closer? ec config)
  (and (not (eof-object? ec))
       (or (char=? ec #\))
           (char=? ec #\])
           (char=? ec #\}))))

(define (closer-name c config)
  (effective-char-names c config "closer"))

(define (opener-name c config)
  (effective-char-names c config "opener"))

(define (effective-char-names c config fallback-str)
  (define rt (read-config-readtable config))
  (cond
   [(not rt)
    (format "`~a`" c)]
   [else
    (define cs (readtable-equivalent-chars rt c))
    (cond
     [(null? cs) fallback-str]
     [(null? (cdr cs)) (format "`~a`" (car cs))]
     [(null? (cddr cs)) (format "`~a` or `~a`" (car cs) (cadr cs))]
     [else
      (apply
       string-append
       (let loop ([cs cs])
         (cond
          [(null? (cdr cs)) (list (format "or `~a`" (car cs)))]
          [else (cons (format "`~a`, " (car cs))
                      (loop (cdr cs)))])))])]))

(define (closer->opener c)
  (case c
    [(#\)) #\(]
    [(#\]) #\[]
    [(#\}) #\{]
    [else c]))

(define (dot-name config)
  "`.`")

(define (all-openers-str config)
  (define p (opener-name #\( config))
  (define s (and (check-parameter read-square-bracket-as-paren config)
                 (opener-name #\[ config)))
  (define c (and (check-parameter read-curly-brace-as-paren config)
                 (opener-name #\{ config)))
  (cond
   [(and s c) (format "~a, ~a, or ~a" p s c)]
   [(or s c) (format "~a or ~a" p (or s c))]
   [else p]))

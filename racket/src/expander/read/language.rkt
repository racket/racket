#lang racket/base
(require "config.rkt"
         "whitespace.rkt"
         "consume.rkt"
         "parameter.rkt"
         "special.rkt"
         "error.rkt"
         "location.rkt"
         "extension.rkt")

(provide read-language/get-info)

(define (read-language/get-info read-one in config fail-k)
  (define c (read-char/skip-whitespace-and-comments #f read-one in config))
  (define-values (line col pos) (port-next-location* in c))

  (define l-config (override-parameter read-accept-reader 
                                       (reading-at config line col pos)
                                       #t))

  (cond
   [(not (eqv? c #\#))
    (if fail-k
        (fail-k)
        (lang-error in l-config "" c))]
   [else
    (define c2 (read-char/special in l-config))
    (cond
     [(eqv? c2 #\l)
      (read-extension-lang read-one c in l-config #:get-info? #t)]
     [(eqv? c2 #\!)
      (read-extension-#! read-one c in l-config #:get-info? #t)]
     [else
      (if fail-k
          (fail-k)
          (lang-error in l-config (string c) c2))])]))


(define (lang-error in config prefix c)
  (define (add-prefix s)
    (if (string=? prefix "")
        (format "`~a` followed by ~a" prefix s)
        s))
  (reader-error in config
                #:due-to c
                #:who 'read-language
                (string-append "expected (after whitespace and comments) `#lang ` or `#!` followed"
                               " immediately by a language name, found ~a")
                (cond
                 [(eof-object? c) (add-prefix "end-of-file")]
                 [(not (char? c)) (add-prefix "non-character")]
                 [else (format "`~a~a`" prefix c)])))

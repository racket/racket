#lang racket/base
(require "../common/check.rkt"
         "../path/path.rkt"
         "../path/relativity.rkt")

(provide current-write-relative-directory
         print-syntax-width)

(define-syntax-rule (define-boolean-parameter print-x init-val)
  (begin
    (provide print-x)
    (define print-x (make-parameter init-val (lambda (v) (and v #t))))))

(define-boolean-parameter print-graph #f)
(define-boolean-parameter print-struct #t)
(define-boolean-parameter print-box #t)
(define-boolean-parameter print-unreadable #t)
(define-boolean-parameter print-hash-table #t)
(define-boolean-parameter print-as-expression #t)
(define-boolean-parameter print-vector-length #f)
(define-boolean-parameter print-pair-curly-braces #f)
(define-boolean-parameter print-mpair-curly-braces #t)
(define-boolean-parameter print-boolean-long-form #f)
(define-boolean-parameter print-reader-abbreviations #t)

(define-boolean-parameter read-accept-bar-quote #t)
(define-boolean-parameter read-case-sensitive #t)

(define/who current-write-relative-directory
  (make-parameter #f (lambda (v)
                       (check who (lambda (v)
                                    (or (not v)
                                        (and (path-string? v)
                                             (complete-path? v))
                                        (and (pair? v)
                                             (path-string? (car v))
                                             (complete-path? (car v))
                                             (path-string? (cdr v))
                                             (complete-path? (cdr v)))))
                              #:contract (string-append
                                          "(or/c (and/c path-string? complete-path?)\n"
                                          "      (cons/c (and/c path-string? complete-path?)\n"
                                          "              (and/c path-string? complete-path?))"
                                          "      #f)")
                              v)
                       (cond
                         [(string? v) (->path v)]
                         [(pair? v) (cons (->path (car v)) (->path (cdr v)))]
                         [else v]))))

(define print-syntax-width
  (make-parameter 32 (lambda (v)
                       (unless (or (eqv? v +inf.0)
                                   (and (exact-integer? v)
                                        (v . >= . 3)))
                         (raise-argument-error 'print-syntax-width
                                               "(or/c +inf.0 0 (and/c exact-integer? (>/c 3)))"
                                               v))
                       v)))



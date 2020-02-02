#lang racket/base

(provide with-deterministic-gensym
         deterministic-gensym)

(define gensym-counter (make-parameter #f))

(define-syntax-rule (with-deterministic-gensym body ...)
  (parameterize ([gensym-counter (box 0)])
    body ...))

(define (deterministic-gensym prefix)
  (define b (gensym-counter))
  (unless b (error 'deterministic-gensym "not in `call-with-deterministic-gensym`"))
  (set-box! b (add1 (unbox b)))
  (string->uninterned-symbol (string-append (if (string? prefix)
                                                prefix
                                                (symbol->string prefix))
                                            (number->string (unbox b)))))

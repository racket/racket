#lang racket/base

(require racket/match)

(define module-name (make-parameter #f))
(define to-require null)
(define (add-mod! m)
  ;(printf ">> adding module ~v\n" m)
  (set! to-require (cons m to-require)))

(define (fix m)
  (match m
    [`(file ,(? bytes? b))
     `(file ,(bytes->string/utf-8 b))]
    [_ m]))
(define (do-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      ;(printf ">> loading ~a\n" m)
      (dynamic-require `(submod ,(fix m) type-decl) #f))))

(provide add-mod! do-requires module-name)
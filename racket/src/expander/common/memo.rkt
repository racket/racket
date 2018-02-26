#lang racket/base
(require (for-syntax racket/base))

(provide define-memo-lite)

;; Lightweight memorization by storing only the most recent result
(define-syntax (define-memo-lite stx)
  (syntax-case stx ()
    [(_ (id arg ...) body0 body ...)
     (with-syntax ([(prev-val ...) (generate-temporaries #'(arg ...))])
       #'(begin
           (define prev-val #f) ...
           (define prev-result #f)
           (define (id arg ...)
             (cond
              [(and (eq? prev-val arg) ...)
               prev-result]
              [else
               (define r (let ()
                           body0
                           body ...))
               (set! prev-val arg) ...
               (set! prev-result r)
               r]))))]))

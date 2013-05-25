#lang racket/base
(require racket/file)

(provide get-preference*
         put-preferences*)

(define (old-name sym)
  (string->symbol
   (regexp-replace #rx"gracket"
                   (regexp-replace #rx"^GRacket" 
                                   (symbol->string sym) 
                                   "MrEd")
                   "mred")))

(define (get-preference* sym [fail-thunk (lambda () #f)])
  (get-preference
   sym
   ;; on fail, fall back to old name of pref:
   (lambda () (get-preference (old-name sym)
                              fail-thunk
                              #:timeout-lock-there (lambda (fn) (fail-thunk))))
   #:timeout-lock-there (lambda (fn) (fail-thunk))))

(define (put-preferences* syms vals)
  (put-preferences syms
                   vals
                   ;; Locked? Too bad.
                   (lambda (fn) (void))))

#lang racket/base
(require (for-syntax racket/base))
(provide debug-printf debug-when)

;; all of the steps in the tcp connection
(define mz-tcp? #f)
(define mr-tcp? mz-tcp?)

;; administrative messages about preferences files and
;; command line flags
(define admin? #t)

;; tests that passed and those that failed
(define schedule? #t)

;; all of the sexpression transactions between mz and mred
(define messages? #f)

(define-syntax (debug-when stx)
  (syntax-case stx (mr-tcp mz-tcp admin schedule messages)
    [(_ mr-tcp rest ...)   #'(when mr-tcp?      (let () rest ...))]
    [(_ mz-tcp rest ...)   #'(when mz-tcp?      (let () rest ...))]
    [(_ admin rest ...)    #'(when admin?       (let () rest ...))]
    [(_ schedule rest ...) #'(when schedule?    (let () rest ...))]
    [(_ messages rest ...) #'(when messages?    (let () rest ...))]
    [(_ unk rest ...) (raise-syntax-error #f "unknown flag" stx #'unk)]))

(define-syntax (debug-printf stx)
  (syntax-case stx ()
    [(_ flag fmt x ...)
     #'(debug-when flag (printf ">> ~a: ~a" 'flag (format fmt x ...)))]))

#lang racket/base

;; Ensure that `or` with lots of arguments doesn't
;; use excessive memory to expand --- which can happen
;; if there's not enough sharing of scope sets, for
;; example.

(define ns (make-base-namespace))

(define c (make-custodian))
(custodian-limit-memory c (* 500 1024 1024))

(define done? #f)

(thread-wait
 (parameterize ([current-namespace ns]
                [current-custodian c])
   (thread
    (lambda ()
      (namespace-require '(for-syntax racket/base))
      (eval
       '(define-syntax (m stx)
         (syntax-case stx ()
           [(_ id)
            #`(define (id x)
                (or #,@(for/list ([i 1000])
                         #`(= x #,i))))])))
      (eval '(m f))
      (set! done? #t)))))

(unless done?
  (error "failed"))



#lang scheme/base

(require (for-syntax scheme/base))

(require scheme/control)
(require scheme/stxparam)

(provide yield lambda-generator define-generator)

(define-syntax-parameter yield
  (lambda (stx)
    (raise-syntax-error #f "yield is only bound inside a sequence generator")))

;; better version of shift/reset using continuation tags also use a
;; unique value to determine the end of the sequence instead of using #f
(define-syntax lambda-generator
  (syntax-rules ()
    [(_ (args ...) body0 bodies ...)
     (lambda (args ...)
       (let* ([last (lambda () (void))]
              ;; current is a function that invokes user code and
              ;; produces values
              [current
               (lambda ()
                 ;; a unique tag to jump to
                 (define tag (make-continuation-prompt-tag))
                 ;; give the value to the sequence
                 (define next (lambda (value)
                                (shift-at tag f (values value f))))
                 (syntax-parameterize ([yield (make-rename-transformer #'next)])
                   (reset-at tag body0 bodies ... (values #f last))))]
              [seq
               (make-do-sequence
                (lambda ()
                  (values
                   ;; produce a value and a continuation
                   (lambda (i)
                     (let-values ([(value next) (current)])
                       ;; set! is ugly but can we do better?
                       (set! current next)
                       value))
                   (lambda (x) (add1 x))
                   0
                   (lambda (x) (not (eq? last current)))
                   (lambda (v) (not (eq? last current)))
                   (lambda (x v) (not (eq? last current))))))])
         seq))]))

(define-syntax define-generator
  (syntax-rules ()
    [(_ (name args ...) body0 bodies ...)
     (define name (lambda-generator (args ...) body0 bodies ...))]))

#|
;; example
(define-generator (blah)
  (for ([x (in-range 0 10)])
    (yield x)))
|#

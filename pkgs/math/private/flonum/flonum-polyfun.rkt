#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         "flonum-functions.rkt")

(provide make-flpolyfun
         make-even-flpolyfun
         make-odd-flpolyfun
         make-quotient-flpolyfun)

(define-for-syntax (syntax-list-reverse stx-lst)
  (datum->syntax stx-lst (reverse (syntax->list stx-lst)) stx-lst))

(define-syntax (horner-iter stx)
  (syntax-case stx ()
    [(_ y z ())  (syntax/loc stx y)]
    [(_ y z (c0 c ...))
     (syntax/loc stx
       (horner-iter (fl+ (fl* y z) c0) z (c ...)))]))

(define-syntax (make-flpolyfun stx)
  (syntax-parse stx
    [(_ (c0:expr c:expr ...))
     (with-syntax ([(c0 c ...)  (syntax-list-reverse #'(c0 c ...))])
       (syntax/loc stx
         (λ: ([z : Float])
           (horner-iter c0 z (c ...)))))]))

(define-syntax (make-even-flpolyfun stx)
  (syntax-parse stx
    [(_ (c0:expr c:expr ...))
     (syntax/loc stx
       (λ: ([z : Float])
         ((make-flpolyfun (c0 c ...)) (fl* z z))))]))

(define-syntax (make-odd-flpolyfun stx)
  (syntax-parse stx
    [(_ (c0:expr c:expr ...))
     (syntax/loc stx
       (λ: ([z : Float])
         (fl+ c0 (fl* z ((make-polyfun (c ...)) (fl* z z))))))]))

(define-syntax (make-quotient-flpolyfun stx)
  (syntax-parse stx
    [(_ (a:expr ...) (b:expr ...))
     (with-syntax ([(a-rev ...)  (syntax-list-reverse #'(a ...))]
                   [(b-rev ...)  (syntax-list-reverse #'(b ...))])
       (syntax/loc stx
         (λ: ([z : Float])
           (cond [((flabs z) . fl<= . 1.0)
                  (fl/ ((make-flpolyfun (a ...)) z)
                       ((make-flpolyfun (b ...)) z))]
                 [else
                  (let ([z  (fl/ 1.0 z)])
                    (fl/ ((make-flpolyfun (a-rev ...)) z)
                         ((make-flpolyfun (b-rev ...)) z)))]))))]))

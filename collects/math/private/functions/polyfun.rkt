#lang typed/racket/base

(require (for-syntax racket/base syntax/parse))

(provide make-polyfun
         make-even-polyfun
         make-odd-polyfun
         make-quotient-polyfun
         make-chebyshev-polyfun)

(define-for-syntax (syntax-list-reverse stx-lst)
  (datum->syntax stx-lst (reverse (syntax->list stx-lst)) stx-lst))

(define-syntax (horner-iter stx)
  (syntax-case stx ()
    [(_ y z ())  (syntax/loc stx y)]
    [(_ y z (c0 c ...))
     (syntax/loc stx
       (let ([y  (+ (* y z) c0)])
         (horner-iter y z (c ...))))]))

(define-syntax (make-polyfun stx)
  (syntax-parse stx
    [(_ T:expr (c0:expr c:expr ...))
     (with-syntax ([(c0 c ...)  (syntax-list-reverse #'(c0 c ...))])
       (syntax/loc stx
         (λ: ([z : T])
           (let ([y c0])
             (horner-iter y z (c ...))))))]))

(define-syntax (make-even-polyfun stx)
  (syntax-parse stx
    [(_ T:expr (c0:expr c:expr ...))
     (syntax/loc stx
       (λ: ([z : T])
         ((make-polyfun T (c0 c ...)) (* z z))))]))

(define-syntax (make-odd-polyfun stx)
  (syntax-parse stx
    [(_ T:expr (c0:expr c:expr ...))
     (syntax/loc stx
       (λ: ([z : T])
         (+ c0 (* z ((make-polyfun T (c ...)) (* z z))))))]))

(define-syntax (make-quotient-polyfun stx)
  (syntax-parse stx
    [(_ T:expr (a:expr ...) (b:expr ...))
     (with-syntax ([(a-rev ...)  (syntax-list-reverse #'(a ...))]
                   [(b-rev ...)  (syntax-list-reverse #'(b ...))])
       (syntax/loc stx
         (λ: ([z : T])
           (cond [((abs z) . <= . 1)  (/ ((make-polyfun T (a ...)) z)
                                         ((make-polyfun T (b ...)) z))]
                 [else  (let ([z  (/ 1 z)])
                          (/ ((make-polyfun T (a-rev ...)) z)
                             ((make-polyfun T (b-rev ...)) z)))]))))]))

(define-syntax (chebyshev-iter stx)
  (syntax-case stx ()
    [(_ y y2 d dd ())  (syntax/loc stx d)]
    [(_ y y2 d dd (c0))
     (syntax/loc stx
       (+ (* y d) (- (/ c0 2) dd)))]
    [(_ y y2 d dd (c0 c ...))
     (syntax/loc stx
       (let-values ([(d dd)  (values (+ (* y2 d) (- c0 dd)) d)])
         (chebyshev-iter y y2 d dd (c ...))))]))

(define-syntax (make-chebyshev-polyfun stx)
  (syntax-parse stx
    [(_ T:expr lower:expr upper:expr (c0:expr c:expr ...))
     (with-syntax ([(c0 c ...)  (syntax-list-reverse #'(c0 c ...))])
       (syntax/loc stx
         (λ: ([z : T])
           (define y (/ (- (+ z z) (+ lower upper))
                        (- upper lower)))
           (define y2 (+ y y))
           (let-values ([(d dd)  (values c0 (- c0 c0))])
             (chebyshev-iter y y2 d dd (c ...))))))]))

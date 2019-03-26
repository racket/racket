#lang racket/base
(require (for-syntax racket/base syntax/parse))

(begin-for-syntax
  (require (for-syntax racket/base))
  (define-syntax => #f)  ;; DEF
  (define-syntax ~thing
    (pattern-expander
     (lambda (stx)
       (syntax-case stx (=>)
         [(_ p1 => p2)
          (with-syntax ([dots (quote-syntax ...)])
            (syntax-property
             #'((~and p1 ~!) dots . p2)
             'disappeared-use
             (syntax-local-introduce (caddr (syntax->list stx)))))])))))

(define-syntax (mac stx)
  (syntax-parse stx
    [(_ (~thing x:id => rest))  ;; USE
     #'(quote ((x ...) rest))]))

(void (mac (a b 3)))

;; Check Syntax should draw an arrow between the occurrence of `=>`
;; marked USE and the occurrence of `=>` marked DEF.

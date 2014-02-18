#lang typed/racket

;; Test that the typed/syntax/stx library can be used

(require typed/syntax/stx
         typed/rackunit)

(check-true (stx-null? null))
(check-true (stx-null? #'()))
(check-false (stx-null? #'(a)))

(check-true (stx-pair? (cons #'a #'b)))
(check-true (stx-pair? #'(a . b)))

(check-true (stx-list? #'(a b c d)))
(check-false (stx-list? #'a))

(syntax-e (car (stx->list #'(a b c d))))
(add1 (car (stx->list '(1 2 3))))

(stx-car #'(a b))
(stx-cdr #'(a b))

(stx-map (λ: ([id : Identifier]) (free-identifier=? id #'a))
         #'(a b c d))

(module-or-top-identifier=? #'a #'b)


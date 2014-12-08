#lang racket/base
(require rackunit
         setup/collection-name)

(check-equal? #t (collection-name-element? "racket"))
(check-equal? #t (collection-name-element? "racket%21"))
(check-equal? #f (collection-name-element? "racket.rkt"))
(check-equal? #f (collection-name-element? "racket!"))
(check-equal? #f (collection-name-element? "rac/ket"))

(check-equal? #t (collection-name? "racket"))
(check-equal? #t (collection-name? "rac/ket"))
(check-equal? #t (collection-name? "racket/%21"))
(check-equal? #f (collection-name? "racket.rkt"))
(check-equal? #f (collection-name? "racket!"))
(check-equal? #f (collection-name? "racket/"))
(check-equal? #f (collection-name? "/racket"))

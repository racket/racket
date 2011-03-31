#lang racket/base
(require stepper/private/find-tag
         racket/gui/base
         rackunit
         racket/class)

(define (mk-txt str)
  (define t (new text%))
  (send t insert str)
  t)

(define (find-tag/end str)
  (find-tag (mk-txt str) (string-length str)))

(check-equal? (find-tag/end "<a>") "a")
(check-equal? (find-tag/end "<abcdef>") "abcdef")
(check-equal? (find-tag/end "<abcdef x=\"3\">") "abcdef")

(check-equal? (find-tag/end "<a></a>") #f)

(check-equal? (find-tag/end "<!-- whatever -->") #f)
(check-equal? (find-tag/end "<!--whatever -->") #f)
(check-equal? (find-tag/end "<!--whatever-->") #f)
(check-equal? (find-tag/end "<!-->") #f)

(check-equal? (find-tag/end "<>") #f)

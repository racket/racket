#lang info

(define name "Documentation Sources")
(define purpose "This collect contains documentation for most of the core system (guide, reference, htdp langs, etc).")

(define test-responsibles '((all (mflatt eli robby matthias))))

(define language-family (list (hash 'family "Racket"
                                    'describe-doc '(lib "scribblings/guide/guide.scrbl")
                                    'order 100)))

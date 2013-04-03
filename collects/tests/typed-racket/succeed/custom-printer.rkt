#lang typed/racket
;; This test ensures that there is some way to customize struct
;; printouts -- if properties are removed or changed, there should be
;; some way to do something equivalent to what's done in this test.
(require mzlib/pconvert-prop)
(struct: foo ()
  #:property prop:custom-print-quotable 'never
  #:property prop:print-convert-constructor-name 'foo
  #:property prop:custom-write (λ (x p w?) (display "#foo" p)))
(unless (equal? "#foo #foo" (format "~a ~s" (foo) (foo)))
  (error "bad output"))

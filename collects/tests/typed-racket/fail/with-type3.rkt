#;
(exn-pred exn:fail:contract?)
#lang scheme

(require typed/scheme)

(define-values (a b)
  (with-type
   #:result (values String (Number -> Number))
   (values "foo" (lambda (x) x))))

(b a)

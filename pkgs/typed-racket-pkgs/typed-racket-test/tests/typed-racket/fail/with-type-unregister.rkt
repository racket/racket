#;
(exn-pred #rx"missing a type annotation")
#lang racket/load

(require (only-in typed/racket with-type String)
         unstable/macro-testing)

;; Ensure that types are unregistered in the type environment for free
;; variables for a `with-type` at the top-level

(define x "foo")

(with-handlers ([exn:fail:syntax? void])
  (convert-compile-time-error
   (with-type #:result String #:freevars ([x String]) (string-append x 3))))

;; should error because `x` shouldn't have a type
(with-type #:result String (string-append x "bar"))

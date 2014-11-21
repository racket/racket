#lang racket/load

(require (only-in typed/racket with-type)
         unstable/macro-testing)

;; Test that the typed-context? flag is properly reset

(with-handlers ([exn:fail:syntax? void])
  (convert-compile-time-error
   (with-type [] (+ 1 "foo"))))

;; this should succeed instead of an error due to the typed-context?
;; flag being set to #t
(with-type [] (+ 1 3))

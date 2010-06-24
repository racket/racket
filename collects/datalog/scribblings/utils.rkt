#lang scheme/base

(require scribble/eval
         (planet cce/scheme:6/planet))

(provide the-eval)

(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require (planet ,(this-package-version-symbol))))
    the-eval))

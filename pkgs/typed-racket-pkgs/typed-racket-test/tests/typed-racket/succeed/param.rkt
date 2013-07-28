#lang typed/racket

(parameterize ([current-directory ".."])
  (current-directory)
  (current-directory ".."))


(: old-param Parameterization)
(define old-param (current-parameterization))

(current-directory "..")

(call-with-parameterization old-param (lambda () (current-directory)))

(parameterization? old-param)


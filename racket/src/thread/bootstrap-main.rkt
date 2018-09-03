#lang racket/base
(require "bootstrap.rkt" ; must be before "main.rkt"
         "main.rkt")

(provide (all-from-out "main.rkt")
         ;; From "bootstrap.rkt":
         register-place-symbol!)

#lang racket/base
(require "unsafe/cvector.rkt")

(provide (except-out (all-from-out "unsafe/cvector.rkt")
                     make-cvector*))

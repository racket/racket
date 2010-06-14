#lang at-exp s-exp "../common.rkt"

(provide page (all-from-out "../common.rkt"))

(define+provide-context "www")

;; needed for sites that use the icon from here (eg, blog.racket-lang.org)
(provide the-resources)

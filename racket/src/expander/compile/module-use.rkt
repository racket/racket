#lang racket/base

;; A `module-use` record is just a part of module path index plus
;; phase, since that combination is commonly needed

(provide (struct-out module-use))

(struct module-use (module phase)
        ;; transparent for hashing; note that module path indices will
        ;; be hashed as `equal?`, which makes sense within a module
        #:transparent)

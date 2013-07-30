#lang info

;; The "base" package exists only as a way to declare dependencies
;; on the current Racket core. If the core gets smaller in the
;; future, then "base" can have new dependencies to cover things
;; moved out of the core, while a new "base2" package can represent
;; the new, smaller core.

(define collection 'multi)

(define deps '())
(define implies '(core))

(define pkg-desc "Racket libraries that are currently always available")

(define pkg-authors '(mflatt))

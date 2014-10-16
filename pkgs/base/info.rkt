#lang info

;; The "base" package exists mainly as a way to declare dependencies
;; on the current Racket core. If the core gets smaller in the
;; future, then "base" can have new dependencies to cover things
;; moved out of the core, while a new "base2" package can represent
;; the new, smaller core.

;; The "base" package also depends on "racket-lib", which ensures that
;; any native libraries needed for a platform are installed along with
;; practically any package installation.

(define collection 'multi)

(define version "6.1.0.7")

(define deps `("racket-lib"
               ["racket" #:version ,version]))

(define implies '(core))

(define pkg-desc "Racket libraries that are currently always available")

(define pkg-authors '(mflatt))


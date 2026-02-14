;; Note that this module only semi-private, in the sense that other packages
;; which are in the main distribution but in separate repositories require
;; it. This means any backwards-incompatible API changes need coordination.
;; The following packages are known to use this module:
;;
;; - compatability-lib
;;
;; Please try not to add more packages which require this file. Instead,
;; consider whether a public API is appropriate, or if a currently-private
;; API should be made public. If not, then make a new file
;; "racket/private/for-yourpackage.rkt" which exports the necessary
;; definitions. See "racket/private/for-compatability-lib.rkt" for an example.

(module increader racket/base
  (define-struct reader (val))
  (provide reader? make-reader reader-val))


#;
(exn-pred #rx"arguments for structure type constructor")
#lang typed/racket

;; Test for PR 13209
;;
;; The use of `node` at the end has the wrong number of
;; type arguments. This should not raise an internal error.

(struct: (α) leaf ({value : α}))
(struct: (α) node ({left : [Tree α]} {right : [Tree α]}))

(define-type (Tree α) (mu Tree (U (leaf α) (node Tree Tree))))


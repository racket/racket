#;
(exn-pred exn:fail:syntax?)
#lang typed/racket

(struct: (α) leaf ({value : α}))
(struct: (α) node ({left : [Tree α]} {right : [Tree α]}))

(define-type (Tree α) (mu Tree (U (leaf α) (node Tree Tree))))


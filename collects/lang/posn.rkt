
;; The posn struct for the teaching languages
(module posn mzscheme
  (define-struct posn (x y) (make-inspector)) ; transparent
  (provide (struct posn (x y))))

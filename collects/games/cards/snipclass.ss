
(module snipclass mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))
  (provide sc)

  (define sc (make-object snip-class%))
  (send sc set-classname "card")
  (send (get-the-snip-class-list) add sc))


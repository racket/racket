(module turtles mzscheme
  (require mzlib/unit
	   mred/mred-sig
	   mred/mred-unit
	   "turtle-sig.ss"
	   "turtle-unit.ss")

  (provide-signature-elements turtle^)

  (define-compound-unit/infer turtle+mred@
    (import)
    (export turtle^)
    (link standard-mred@ turtle@))
  
  (define-values/invoke-unit/infer turtle+mred@))

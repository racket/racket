(module turtles mzscheme
  (require (lib "unit.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "mred-unit.ss" "mred")
	   "turtle-sig.ss"
	   "turtle-unit.ss")

  (provide-signature-elements turtle^)

  (define-compound-unit/infer turtle+mred@
    (import)
    (export turtle^)
    (link standard-mred@ turtle@))
  
  (define-values/invoke-unit/infer turtle+mred@))
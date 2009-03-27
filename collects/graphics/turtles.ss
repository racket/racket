(module turtles mzscheme
  (require mzlib/unit
	   mred/mred-unit
	   "turtle-sig.ss"
	   "turtle-unit.ss")

  (provide-signature-elements turtle^)

  (define-values/invoke-unit/infer (export turtle^) (link turtle@ standard-mred@)))

(module graphics mzscheme
  (require mzlib/unit
	   mred/mred-sig
	   mred/mred-unit
	   "graphics-sig.ss"
	   "graphics-unit.ss")
  (provide-signature-elements graphics^ graphics:posn^)

  (define-values/invoke-unit/infer
    (export graphics^ graphics:posn^)
    (link standard-mred@ graphics@)))

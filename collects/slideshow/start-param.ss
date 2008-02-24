
(module start-param mzscheme
  (provide file-to-load
	   trust-me?)

  (define file-to-load (make-parameter #f))
  (define trust-me? (make-parameter #t)))

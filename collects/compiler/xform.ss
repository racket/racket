
(module xform mzscheme
  (require (lib "compile.ss" "dynext")
	   (prefix xform: "private/xform.ss"))

  (provide xform)

  (define (xform quiet? src dest header-dirs)
    (let ([exe (current-extension-compiler)]
	  [flags (expand-for-compile-variant
		  (current-extension-preprocess-flags))]
	  [headers (apply append
			  (map (current-make-compile-include-strings)
			       header-dirs))])
      (xform:xform quiet?
		   (cons exe
			 (append flags headers))
		   src
		   dest
		   #f #t #t
		   #f #f
		   #f #f
		   #f))))

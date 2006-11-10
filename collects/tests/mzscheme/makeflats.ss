
(define flat-number 0)
(for-each (lambda (f)
	    (parameterize ([current-namespace (make-namespace)])
	      (set! flat-number (add1 flat-number))
	      (eval
	       `(begin
		  (require-for-syntax mzscheme)
		  (define flat-load ,f)
		  (define flat-number ,(format "-~a" flat-number))
		  (load-relative "makeflat.ss")))))
	  '("basic.ss"
	    "unicode.ss"
	    "read.ss"
	    "macro.ss"
	    "syntax.ss"
	    "stx.ss"
	    "module.ss"
	    "number.ss"
	    "object.ss"
	    "struct.ss"
	    "unit.ss"
	    "unitsig.ss"
	    "thread.ss"
	    "sync.ss"
	    "deep.ss"
	    "contmark.ss"
            "prompt.ss"
	    "will.ss"
	    "namespac.ss"
	    "port.ss"
	    "file.ss"
	    "path.ss"))

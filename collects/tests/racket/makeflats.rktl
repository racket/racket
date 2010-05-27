
(define flat-number 0)
(for-each (lambda (f)
            (let ([ns (current-namespace)])
              (parameterize ([current-namespace (make-base-namespace)])
                (set! flat-number (add1 flat-number))
                (namespace-attach-module ns 'racket)
                (namespace-require 'racket)
                (eval
                 `(begin
                    (define flat-load ,f)
                    (define flat-number ,(format "-~a" flat-number))
                    (load-relative "makeflat.rktl"))))))
	  '("basic.rktl"
	    "unicode.rktl"
	    "read.rktl"
	    "macro.rktl"
	    "syntax.rktl"
	    "stx.rktl"
	    "module.rktl"
	    "number.rktl"
	    "object.rktl"
	    "struct.rktl"
	    "unit.rktl"
	    "unitsig.rktl"
	    "thread.rktl"
	    "sync.rktl"
	    "deep.rktl"
	    "contmark.rktl"
            "prompt.rktl"
	    "will.rktl"
	    "namespac.rktl"
	    "port.rktl"
	    "file.rktl"
	    "path.rktl"))

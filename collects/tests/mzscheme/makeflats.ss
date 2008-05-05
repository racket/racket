
(define flat-number 0)
(for-each (lambda (f)
            (let ([ns (current-namespace)])
              (parameterize ([current-namespace (make-base-namespace)])
                (set! flat-number (add1 flat-number))
                (namespace-attach-module ns 'scheme)
                (namespace-require 'scheme)
                (eval
                 `(begin
                    (define flat-load ,f)
                    (define flat-number ,(format "-~a" flat-number))
                    (load-relative "makeflat.ss"))))))
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

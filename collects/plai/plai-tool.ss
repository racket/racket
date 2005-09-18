
#|

The PLAI languages can almost be specified via info.ss fields, but
the default printing style shold be 'constructor instead of 'write

|#

(module plai-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "struct.ss")
	   (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (add-language! modname lang langnum summary)
	(define base%
	  (class* object% (drscheme:language:simple-module-based-language<%>)
	    (define/public (get-language-numbers)
	      langnum)
	    (define/public (get-language-position)
	      (list (string-constant teaching-languages)
		    "Programming Languages: Application and Interpretation"
		    lang))
	    (define/public (get-module)
	      modname)
	    (define/public (get-one-line-summary)
	      summary)
	    (define/public (get-language-url)
	      "http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/")
	    (define/public (get-reader)
	      (lambda (src port)
		(let ([v (read-syntax src port)])
		  (if (eof-object? v)
		      v
		      (namespace-syntax-introduce v)))))
	    (super-instantiate ())))
	(drscheme:language-configuration:add-language 
	 (make-object 
	  ((drscheme:language:get-default-mixin)
	   (class (drscheme:language:module-based-language->language-mixin
		   (drscheme:language:simple-module-based-language->module-based-language-mixin
		    base%))
	     (define/override (use-namespace-require/copy?) #t)
	     
	     ;; Change print style in default settings from 'write to 'constructor:
	     (define/override (default-settings)
	       (let ([s (super default-settings)])
		 (to-style s 'constructor)))

	     (define/private (to-style s v)
	       (drscheme:language:make-simple-settings
		(drscheme:language:simple-settings-case-sensitive s)
		v
		(drscheme:language:simple-settings-fraction-style s)
		(drscheme:language:simple-settings-show-sharing s)
		(drscheme:language:simple-settings-insert-newlines s)
		(drscheme:language:simple-settings-annotations s)))
	     
	     (super-instantiate ()))))))

      (define (phase1) (void))
      (define (phase2)
	(map add-language! 
	     '((lib "plai-beginner.ss" "plai") 
	       (lib "plai-intermediate.ss" "plai") 
	       (lib "plai-advanced.ss" "plai")
	       (lib "plai-pretty-big.ss" "plai"))
	     '("PLAI - Beginning Student"
	       "PLAI - Intermediate Student with lambda"
	       "PLAI - Advanced Student"
	       "PLAI - Pretty Big")
	     '((-500 0 0) (-500 0 1) (-500 0 3) (-500 0 4))
	     '("PLAI: beginning students"
	       "PLAI: beginner plus lexical scope and higher-order functions" 
	       "PLAI: intermediate plus lambda and mutation"
	       "PLAI: PLT Pretty Big plus define-type"))))))

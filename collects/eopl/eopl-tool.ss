
#|

The EoPL language can almost be specified via info.ss fields, but
on-execute needs to install the EoPL exception handler as its 
last action. (The module body can't do that, because a `with-handlers'
wraps the load of the module.)

|#

(module eopl-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define language-base%
	(class* object% (drscheme:language:simple-module-based-language<%>)
	  (define/public (get-language-numbers)
	    '(-500 -400))
	  (define/public (get-language-position)
	    (list (string-constant teaching-languages)
                  "Essentials of Programming Languages (2nd ed.)"))
	  (define/public (get-module)
	    '(lib "eopl.ss" "eopl"))
	  (define/public (get-one-line-summary)
	    "Based on the Friedman, Wand, and Haynes text")
          (define/public (get-language-url)
	    "http://www.cs.indiana.edu/eopl/")
	  (define/public (get-reader)
	    (lambda (src port)
              (let ([v (read-syntax src port)])
                (if (eof-object? v)
                  v
                  (namespace-syntax-introduce v)))))
	  (super-instantiate ())))

      (define language%
	(class (drscheme:language:module-based-language->language-mixin
		(drscheme:language:simple-module-based-language->module-based-language-mixin
		 language-base%))
	  (define/override (use-namespace-require/copy?) #t)
	  (define/override (on-execute settings run-in-user-thread)
	    (super on-execute settings run-in-user-thread)
	    (run-in-user-thread
	     (lambda ()
	       ((namespace-variable-value 'install-eopl-exception-handler)))))
	  (super-instantiate ())))

      (define (phase1) (void))
      (define (phase2)
	(drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) language%)))))))

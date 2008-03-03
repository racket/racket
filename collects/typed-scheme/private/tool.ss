(module tool mzscheme
  (require (lib "unit.ss")
	   (lib "class.ss")
	   (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants")
           (prefix r: "../typed-reader.ss"))
  

  (provide tool@)
  
  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define mbl% (drscheme:language:module-based-language->language-mixin
		    (drscheme:language:simple-module-based-language->module-based-language-mixin 
		     drscheme:language:simple-module-based-language%)))
      
      (define planet-module '(planet "typed-scheme.ss" ("plt" "typed-scheme.plt" 3)))

      (define (typed-scheme-language% cl%)
	(class* cl% (drscheme:language:simple-module-based-language<%>)
	  (define/override (get-language-numbers)
	    '(1000 -401))
	  (define/override (get-language-name) "Typed Scheme")
	  (define/override (get-language-position)
	    (list (string-constant experimental-languages) "Typed Scheme"))
	  (define/override (get-module) planet-module)
          (define/override (get-reader) 
	    (lambda (src port)
       	      (let ([v (r:read-syntax src port)])
                (if (eof-object? v)
                  v
                  (namespace-syntax-introduce v)))))
 	  (define/override (get-one-line-summary)
	    "Scheme with types!")
          (define/override (get-language-url) "http://www.ccs.neu.edu/~samth/typed-scheme.html")
	  (define/override (enable-macro-stepper?) #t)
          (define/override (use-namespace-require/copy?) #f)
	  (super-new [module (get-module)] [language-position (get-language-position)])))
      
      (define (phase1) (void))
      (define (phase2)
        (drscheme:language-configuration:add-language 
	 (make-object (typed-scheme-language%
		       ((drscheme:language:get-default-mixin) 
			mbl%)))))))
  )
      
      
  
  
  

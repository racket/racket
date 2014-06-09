(module tool mzscheme
  (require drscheme/tool
           mred
           mzlib/unit
           mzlib/class
           "parse.rkt"
           "simplify.rkt"
           "compile.rkt"
           compiler/embed
           string-constants
           errortrace/errortrace-lib
           (prefix bd: "bd-tool.rkt"))

  (provide tool@)

  (define base-importing-stx (dynamic-require 'algol60/base
					      'base-importing-stx))

  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define-values/invoke-unit bd:tool@
        (import drscheme:tool^)
        (export (prefix bd: drscheme:tool-exports^)))

      (define (phase1) (bd:phase1))
      (define (phase2) 
	(bd:phase2)
        (drscheme:language-configuration:add-language
         (make-object (override-mrflow-methods
                       ((drscheme:language:get-default-mixin) 
                        lang%)))))
      
      (define (override-mrflow-methods %)
	(if (method-in-interface? 'render-value-set (class->interface %))
	    (class %
	      (inherit [super-render-value-set render-value-set]
		       [super-get-mrflow-primitives-filename get-mrflow-primitives-filename])
	      (define/override (render-value-set . x)
		;; needs to be filled in!
		(super-render-value-set . x))
	      (define/override (get-mrflow-primitives-filename)
		(build-path (collection-path "mrflow")
			    "primitives"
			    "algol60.rkt"))
	      (super-instantiate ()))
	    %))

      (define lang%
        (class* object% (drscheme:language:language<%>)
          (define/public (front-end/finished-complete-program settings) (void))
          (define/public (extra-repl-information settings port) (void))
          (define/public (get-reader-module) #f)
          (define/public (get-metadata a b) #f)
          (define/public (metadata->settings m) #f)
          (define/public (get-metadata-lines) #f)
          
          (define/public (capability-value s) (drscheme:language:get-capability-default s))
          (define/public (first-opened) (void))
          (define/public (config-panel parent)
            (case-lambda
              [() null]
              [(x) (void)]))
          (define/public (get-comment-character) (values "'COMMENT'" #\*))
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)
          (define/private (front-end port settings)
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char port))
                    eof
		    (compile-simplified 
		     (simplify (parse-a60-port port name) base-importing-stx) 
		     base-importing-stx)))))
          (define/public (front-end/complete-program port settings) (front-end port settings))
          (define/public (front-end/interaction port settings) (front-end port settings))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position)
	    (list (string-constant experimental-languages)
		  "Algol 60"))
          (define/public (get-language-name) "Algol 60")
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers) (list 1000 10))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require 'algol60/base #f)
            (let ([path ((current-module-name-resolver) 'algol60/base #f #f #t)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (current-compile (make-errortrace-compile-handler))
                 (with-handlers ([void (lambda (x)
                                         (printf "~a\n"
                                                 (exn-message x)))])
                   (namespace-attach-module n path)
                   (namespace-require path))))))
          (define/public (render-value value settings port) (write value port))
          (define/public (render-value/format value settings port width) (write value port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file)
	    (let ([dst-file (drscheme:language:put-executable
			     parent src-file #f #f
			     (string-constant save-a-mzscheme-stand-alone-executable))])
	      (when dst-file
		(let ([code (compile-simplified (simplify (parse-a60-file src-file)
							  base-importing-stx)
						base-importing-stx)])
		  (make-embedding-executable dst-file
					     #f #f
					     '((#f algol60/base))
					     null
					     (compile
					      `(module m algol60/base
						 ,code))
					     (list "-mvqe" "(require m)"))))))
	  (define/public (get-one-line-summary) "Of historic interest")
          
          (super-instantiate ()))))))

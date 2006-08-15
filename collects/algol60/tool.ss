(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           "parse.ss"
           "simplify.ss"
           "compile.ss"
	   (lib "embed.ss" "compiler")
	   (lib "string-constant.ss" "string-constants")
	   (prefix bd: "bd-tool.ss"))

  (provide tool@)

  (define base-importing-stx (dynamic-require '(lib "base.ss" "algol60")
					     'base-importing-stx))

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define-values/invoke-unit/sig drscheme:tool-exports^
	bd:tool@
	bd
	drscheme:tool^)

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
			    "algol60.ss"))
	      (super-instantiate ()))
	    %))

      (define lang%
        (class* object% (drscheme:language:language<%>)
          (define/public (capability-value s) (drscheme:language:get-capability-default s))
          (define/public (get-language-id) "plt:algol60")
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
          (define/public (front-end/complete-program port settings teachpack-cache) (front-end port settings))
          (define/public (front-end/interaction port settings teachpack-cache) (front-end port settings))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position)
	    (list (string-constant experimental-languages)
		  "Algol 60"))
          (define/public (order-manuals x) 
            (values 
             (list #"drscheme" #"tour" #"help")
	     ;; We allow doc.txt search results, because the Algol60
	     ;; docs are in doc.txt:
             #t))
          (define/public (get-language-name) "Algol 60")
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers) (list 1000 10))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "base.ss" "algol60") #f)
            (let ([path ((current-module-name-resolver) '(lib "base.ss" "algol60") #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (current-eval 
		  (drscheme:debug:make-debug-eval-handler (current-eval)))
                 (with-handlers ([void (lambda (x)
                                         (printf "~a~n"
                                                 (exn-message x)))])
                   (namespace-attach-module n path)
                   (namespace-require path))))))
          (define/public (render-value value settings port) (write value port))
          (define/public (render-value/format value settings port width) (write value port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file teachpacks)
	    (let ([dst-file (drscheme:language:put-executable
			     parent src-file #f #f
			     (string-constant save-a-mzscheme-stand-alone-executable))])
	      (when dst-file
		(let ([code (compile-simplified (simplify (parse-a60-file src-file)
							  base-importing-stx)
						base-importing-stx)])
		  (make-embedding-executable dst-file
					     #f #f
					     '((#f (lib "base.ss" "algol60")))
					     null
					     (compile
					      `(module m (lib "base.ss" "algol60")
						 ,code))
					     (list "-mvqe" "(require m)"))))))
	  (define/public (get-one-line-summary) "Algol 60 (not Scheme at all!)")
          
          (super-instantiate ()))))))

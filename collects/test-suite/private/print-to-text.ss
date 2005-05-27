(module print-to-text mzscheme
  
  (require
   (lib "list.ss")
   (lib "etc.ss")
   (lib "class.ss")
   (lib "contract.ss")
   (lib "unitsig.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "tool.ss" "drscheme"))
  
  (provide print-to-text^
	   print-to-text@)

  (define-signature print-to-text^
    (print-to-text))

  (define print-to-text@
    (unit/sig print-to-text^
      (import drscheme:tool^)

      ;; Using the current languages print operations, print the list of values to the text
      (define (print-to-text atext vals)
	(unless (empty? vals)
	  (send* atext
		 (begin-edit-sequence)
		 (erase))
	  (let ([port
		 (make-output-port
		  'set-actuals
		  always-evt
		  (lambda (s start end block? enable-breaks?)
		    (send atext insert
			  (list->string
			   (map integer->char
				(bytes->list (subbytes s start end)))))
		    (- end start))
		  void
		  (lambda (v block? enable-breaks?)
		    (if (v . is-a? . snip%)
			(send atext insert v)
			(send atext insert (format "~s" v)))
		    #t))])
	    (define (print-one v)
	      (let* ([language-settings
		      (preferences:get
		       (drscheme:language-configuration:get-settings-preferences-symbol))]
		     [language
		      (drscheme:language-configuration:language-settings-language
		       language-settings)]
		     [settings
		      (drscheme:language-configuration:language-settings-settings
		       language-settings)])
		(send language render-value v settings port)))
	    (print-one (first vals))
	    (for-each
	     (lambda (val)
	       (newline port)
	       (print-one val))
	     (rest vals)))
	  (send atext end-edit-sequence))))))

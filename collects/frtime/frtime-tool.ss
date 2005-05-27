
(module frtime-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "list.ss")
	   (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define basic-frtime-language%
	(class* object% (drscheme:language:simple-module-based-language<%>)
	  (define/public (get-language-numbers)
	    '(1000 -400 1))
	  (define/public (get-language-position)
	    (list (string-constant experimental-languages) "FrTime" "Minimal"))
	  (define/public (get-module)
	    '(lib "frtime.ss" "frtime"))
	  (define/public (get-one-line-summary)
	    "FrTime without libraries")
          (define/public (get-language-url) #f)
	  (define/public (get-reader)
	    (lambda (name port offsets)
	      (let ([v (read-syntax name port offsets)])
		(if (eof-object? v)
		    v
		    (namespace-syntax-introduce v)))))
	  (super-instantiate ())))

      (define big-frtime-language%
	(class* object% (drscheme:language:simple-module-based-language<%>)
	  (define/public (get-language-numbers)
	    '(1000 -400 0))
	  (define/public (get-language-position)
	    (list (string-constant experimental-languages) "FrTime" "Standard"))
	  (define/public (get-module)
	    '(lib "frtime-big.ss" "frtime"))
	  (define/public (get-one-line-summary)
	    "Standard FrTime (includes common libraries)")
          (define/public (get-language-url) #f)
	  (define/public (get-reader)
	    (lambda (name port)
	      (let ([v (read-syntax name port)])
		(if (eof-object? v)
		    v
		    (namespace-syntax-introduce v)))))
	  (super-instantiate ())))

      (define (weak-member obj lis)
        (let ([cmp (lambda (v) (eq? v obj))])
          (let loop ([lis lis])
            (and (cons? lis)
                 (or
                  (cond
                    [(weak-box-value (first lis)) => cmp]
                    [else false])
                  (loop (rest lis)))))))
            
      (define (watch watch-list value)
        (foldl
         (lambda (wb acc)
           (cond
             [(weak-box-value wb)
              => (lambda (f) (f acc #t))]
             [else acc]))
         value
         watch-list))
      
      (define (make-frtime-language base)
	(class (drscheme:language:module-based-language->language-mixin
		(drscheme:language:simple-module-based-language->module-based-language-mixin
		 base))
          (field (watch-list empty))
          (inherit get-language-position)
          (define/override (get-language-name)
            (let* ([pos (get-language-position)]
                   [last-part (first (last-pair pos))])
              (if (equal? last-part "Standard")
                  "FrTime"
                  (string-append "FrTime: " last-part))))
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-eventspace (current-eventspace)])
              (super on-execute settings run-in-user-thread)
              (run-in-user-thread
               (lambda ()
                 (let ([new-watch (namespace-variable-value 'render)]
                       [set-evspc (namespace-variable-value 'set-eventspace)])
                   (set-evspc drs-eventspace)
                   (set! watch-list
                         ((if (weak-member new-watch watch-list)
                              identity
                              (lambda (r) (cons (make-weak-box new-watch) r)))
                          (filter weak-box-value watch-list))))))))

          (override render-value/format render-value)
          (define (render-value/format value settings port width)
            (super render-value/format (watch watch-list value)
                                       settings port width))
          (define (render-value value settings port)
            (super render-value (watch watch-list value)
                                settings port))
	  (define/override (use-namespace-require/copy?) #t)
	  (super-instantiate ())))

      (define (phase1) (void))
      (define (phase2)
	(drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) (make-frtime-language basic-frtime-language%))))
        (drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) (make-frtime-language big-frtime-language%))))))))

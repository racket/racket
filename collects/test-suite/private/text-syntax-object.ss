(module text-syntax-object mzscheme
  
  (require
   (lib "unit.ss")
   (lib "class.ss")
   (lib "list.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   (lib "mred.ss" "mred"))
  
  (provide text->syntax-object@
           text->syntax-object^)

  (define top-id #'here)
  
  (define-signature text->syntax-object^ (text->syntax-objects))
  
  (define-unit text->syntax-object@
    
    (import drscheme:tool^)
    (export text->syntax-object^)   
      #;((is-a?/c text%) . -> . (listof syntax-object?))
      ;; a syntax object representing the text with the color of the given object
      (define (text->syntax-objects text default-v)
        (let ([port (open-input-text-editor text)])
          #;(-> (listof syntax-object?))
          ;; Reads all the syntax objects for the text%
          (define (read-all-syntax)
            (let* ([language-settings
                    (preferences:get
                     (drscheme:language-configuration:get-settings-preferences-symbol))]
                   [language
                    (drscheme:language-configuration:language-settings-language
                     language-settings)]
                   [settings
                    (drscheme:language-configuration:language-settings-settings
                     language-settings)])
              (if (drscheme:language-configuration:language-settings? language-settings)
                  (let ([thunk (if (and default-v
					(zero? (send text last-position)))
				   (let ([got? #f])
				     (lambda () 
				       (begin0
					(if got?
					    eof
					    default-v)
					(set! got? #t))))
				   (send language front-end/interaction
					 (open-input-text-editor text)
					 settings
					 (drscheme:teachpack:new-teachpack-cache '())))])
                    (let loop ()
                      (let ([expr (thunk)])
                        (cond [(eof-object? expr) empty]
                              [else (cons expr (loop))]))))
                  (error 'text->syntax-object "Invalid language settings"))))
          (read-all-syntax)))
      ))
  

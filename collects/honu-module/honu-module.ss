(module honu-module mzscheme
  (require (rename "dynamic.ss" #%dynamic-honu-module-begin #%dynamic-honu-module-begin))

  (define-syntax (honu-module-begin stx)
    (syntax-case stx (dynamic)
      [(_ dynamic . body)
       #`(#%module-begin
	  (#%dynamic-honu-module-begin
	   . body))]
      [(_ other . body)
       (identifier? #'other)
       (raise-syntax-error
	#f
	"unknown Honu dialect"
	#'other)]
      [else
       (raise-syntax-error
	#f
	"expected a Honu dialect name before module body"
	stx)]))
  
  (provide (rename honu-module-begin #%module-begin)))


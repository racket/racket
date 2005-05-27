(module ssax "restricted-mzscheme.ss"
  
  (provide xml-token?
	   xml-token-kind xml-token-head
	   make-empty-attlist attlist-add
	   attlist-null?
	   attlist-remove-top
	   attlist->alist attlist-fold
	   ssax:uri-string->symbol
	   ssax:skip-internal-dtd
	   ssax:read-pi-body-as-string
	   ssax:reverse-collect-str-drop-ws
	   ssax:read-markup-token
	   ssax:read-cdata-body
	   ssax:read-char-ref
	   ssax:read-attributes
	   ssax:complete-start-tag
	   ssax:read-external-id
	   ssax:read-char-data
	   ssax:make-parser ssax:make-pi-parser ssax:make-elem-parser
	   ssax:xml->sxml
	   ssax:warn-parameter)

  (require (rename (lib "1.ss" "srfi") cons* cons*))
  (require (rename (lib "13.ss" "srfi") string-null? string-null?)
	   (rename (lib "13.ss" "srfi") string-index string-index)
	   (rename (lib "13.ss" "srfi")
		   string-concatenate/shared string-concatenate/shared)
	   (rename (lib "13.ss" "srfi")
		   string-concatenate-reverse/shared string-concatenate-reverse/shared))

  (require "crementing.ss")
  (require "input-parse.ss")
  (require "char-encodings.ss")
  (require "ascii.ss")
  (require "ppretty-prints.ss")
  (require "oleg-utils.ss")
  (require "find-strings.ss")
  (require "assertions.ss")
  (require "coutputs.ss")
  (require "catch-errors.ss")
  (require "oleg-string-ports.ss")

  (define (SSAX:warn-standard port msg . other-msg)
    (apply cerr (cons (string-append (string #\newline) "Warning: ")
		      (cons msg
			    other-msg))))

  
  (define ssax:warn-parameter (make-parameter SSAX:warn-standard))

  (define (ssax:warn port msg . other-msg)
    (apply (ssax:warn-parameter) port msg other-msg))

  (require (lib "include.ss"))
  (include "SSAX-code.scm"))


  



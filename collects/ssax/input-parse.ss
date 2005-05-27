(module input-parse "restricted-mzscheme.ss"

  (provide peek-next-char
	   assert-curr-char
	   skip-until skip-while
	   next-token next-token-of
	   read-text-line
	   read-string
	   parser-error
	   exn:ssax?
	   exn:ssax-port)

  (require (rename (lib "13.ss" "srfi")
		   string-concatenate-reverse string-concatenate-reverse))

  (require "define-opt.ss")
  (require "ascii.ss")
  (require "char-encodings.ss")
  (require "crementing.ss")

  (define-struct (exn:ssax exn) (port))

  (define (format-list list)
    (apply string-append (map format-x list)))
  
  (define (format-x thing)
    (format "~a" thing))

  (define (parser-error port message . rest)
    (raise (make-exn:ssax (string->immutable-string
			   (format-list (cons message rest)))
			  (current-continuation-marks)
			  port)))

  (require (lib "include.ss"))
  (include "input-parse.scm"))
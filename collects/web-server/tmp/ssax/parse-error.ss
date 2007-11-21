; Module header is generated automatically
#cs(module parse-error mzscheme
(require "myenv.ss")

; This code provides informative error messages
;   for SSAX (S)XML parser.
;
;
; NOTE: PLT-specific ! 
; It was tested with SSAX version 4.6 / PLT 103
;

;==============================================================================
; Error handler

; According to the SSAX convention this function
; accepts the port as its first argument which is used for
; location of the error in input file.
; Other parameters are considered as error messages,
;  they are printed to stderr as is.
(define parser-error
  (lambda  args
    (if
      (port? (car args))
      (cerr nl "Error at position " 
	    (file-position (car args)) nl
	    (cdr args))
      (cerr nl "Error in error handler: its first parameter is not a port" 
	    nl args))
    (cerr nl)
    ; (exit -1)  ; this exit makes me completely insane!
    (raise -1)
))

(define SSAX:warn
  (lambda  args
    (if
      (port? (car args))
      (cerr nl "Warning at position " 
	    (file-position (car args)) nl
	    (cdr args) nl)
      #f)
    ))

; Alias
(define ssax:warn SSAX:warn)

(provide (all-defined)))

; Pretty-printer for various Scheme systems
;
; It implements the structure ppretty-prints for Scheme systems
; other than Scheme48 and PLT Scheme.
; Also implement display-circle
;
; $Id: ppretty-prints.scm,v 1.1 2004/10/02 07:50:50 eli Exp $

; If the pretty-printer is available, use it. Otherwise, use 'display'
; If display-circle is available or a regular pretty-printer can handle
; circular lists, use them. Otherwise, refuse to display circular data
; structures

(cond-expand
  (bigloo
    #f)					; pp and display-circle are natively
					; available
  ((or scm gambit)
    					; pp is natively available
    (define (display-circle x)          ; display-circle is not
      (display "Cannot safely display circular datastructures. Use SRFI-38")
      (newline)))
  ((or petite-chez)
    (define pp pretty-print)
    (define display-circle pp))
  (else
    (define pp display)		         ; Fall-back to display
    (define (display-circle x)
      (display "Cannot safely display circular datastructures. Use SRFI-38")
      (newline))))


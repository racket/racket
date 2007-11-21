; Module header is generated automatically
#cs(module char-encoding mzscheme
(require "common.ss")
(require "myenv.ss")

;		Character-encoding module
;
; This module deals with particular character-encoding issues such as
; conversions between characters and their ASCII or UCS2 codes, Scheme
; representations of "Carriage Return" (CR), "tabulation" (TAB) and
; other control characters.
;
; This module by necessity is platform-specific as character encoding
; issues are hardly addressed in R5RS. For example, the result of
; char->integer is generally not an ASCII code of an integer (although
; it is, on many Scheme systems, with the important exception of
; Scheme48 and SCSH). The level of support for character sets other
; than ASCII varies widely among Scheme systems.
;
; This file collects various character-encoding functions that are
; necessary for the SSAX XML parser. The functions are of general use
; and scope.
;
; $Id: char-encoding.scm,v 1.1 2003/04/09 20:34:28 oleg Exp $


;	ascii->char INT -> CHAR
; return a character whose ASCII code is INT
; Note, because ascii->char is injective (there are more characters than
; ASCII characters), the inverse transformation is not defined.
(cond-expand
  (scheme48  #f)		; ascii->char is built into Scheme48
  (scsh #f)			; ascii->char is built into Scheme48
  (else
    (define ascii->char integer->char)
  )
)


;	ucscode->char INT -> CHAR
; Return a character whose UCS (ISO/IEC 10646) code is INT
; Note
; This function is required for processing of XML character entities:
; According to Section "4.1 Character and Entity References"
; of the XML Recommendation:
;  "[Definition: A character reference refers to a specific character
;   in the ISO/IEC 10646 character set, for example one not directly
;   accessible from available input devices.]"

(define (ucscode->char code)
  (cond-expand
    (bigloo
      (ucs2->char (integer->ucs2 code)))
    ((or scheme48 scsh)			; Scheme48 has no support for UCS
      (ascii->char code))
    (else
      (integer->char code))))

; Commonly used control characters

(define char-return (ascii->char 13))
(define char-tab    (ascii->char 9))
(define char-newline (ascii->char 10)) ; a.k.a. #\newline, per R5RS
(define char-space (ascii->char 32))

(provide (all-defined)))

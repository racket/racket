;;; 42.ss  -- Extends SRFI 42 - Eager Comprehensions

; This module provides srfi-42 with some extensions.
; See the "Eager Comprehensions for Black Belts" for examples
; and explanations.

; The extensions are:

;   o  do-ec supports 
;         - let-values
;         - let-match
;         - let-plt-match

;   o  extra comprehensions
;        :combinations
;        :do-until
;        :iterate
;        :let-match
;        :let-values
;        :list-by
;        :match
;        :pairs
;        :pairs-by
;        :plt-match
;        :repeat
;        :vector-combinations

(module |42| mzscheme
  ; Sebastian Egner's original comprehension.ss
  ; patched to handle let-values, let-match, let-plt-match in do-ec
  (require "42-eager-comprehensions/comprehensions.ss")
  (provide (all-from "42-eager-comprehensions/comprehensions.ss"))
  ; Extra generators
  (require "42-eager-comprehensions/extra-generators.scm")
  (provide (all-from "42-eager-comprehensions/extra-generators.scm")))

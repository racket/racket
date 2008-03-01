;;; 78.ss  -- SRFI 78 Draft - Light Weight Testing

; This module provides the reference implementation by
; Sebastian Egner of the srfi-78 draft pr. 18-jan-2006.

(module |78| mzscheme
  (require "78-lightweight-testing/check.scm")
  (provide (all-from "78-lightweight-testing/check.scm")))


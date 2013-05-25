#lang scheme/base
(require mzlib/include
         srfi/23
         srfi/42
         mzlib/pretty)

(include "check-reference.scm")

(provide check
         check-ec
         check-report
         check-set-mode!
         check-reset!
         check-passed?)

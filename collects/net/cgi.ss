
(require-library "cgiu.ss" "net")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:cgi^
  mzlib:cgi@)

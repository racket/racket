
(require-relative-library "dnss.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:dns^
  (require-relative-library "dnsr.ss"))


(require-relative-library "smtps.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:smtp^
  (require-relative-library "smtpr.ss"))


(require-relative-library "imaps.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:imap^
  (require-relative-library "imapr.ss"))

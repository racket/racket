(require-library "nntpu.ss" "net")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:nntp^
  mzlib:nntp@
  nntp)

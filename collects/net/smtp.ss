
(module smtp mzscheme
  (import (lib "unitsig.ss"))

  (import "smtp-sig.ss")
  (import "smtp-unit.ss")

  (define-values/invoke-unit/sig net:smtp^
    net:smtp@)

  (export-signature-elements net:smtp^))

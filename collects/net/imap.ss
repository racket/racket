
(module imap mzscheme
  (import (lib "unitsig.ss"))

  (import "imap-sig.ss")
  (import "imap-unit.ss")

  (define-values/invoke-unit/sig net:imap^
    net:imap@)

  (export-signature-elements net:imap^))

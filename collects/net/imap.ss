
(module imap mzscheme
  (require (lib "unitsig.ss"))

  (require "imap-sig.ss")
  (require "imap-unit.ss")

  (define-values/invoke-unit/sig net:imap^
    net:imap@)

  (provide-signature-elements net:imap^))

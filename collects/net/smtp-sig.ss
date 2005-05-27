
(module smtp-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:smtp^)
  (define-signature net:smtp^
    (smtp-send-message
     smtp-send-message*
     smtp-sending-end-of-message)))


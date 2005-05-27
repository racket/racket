(module uri-codec mzscheme
  (require (lib "unitsig.ss")
           "uri-codec-sig.ss"
           "uri-codec-unit.ss")

  (provide-signature-elements net:uri-codec^)

  (define-values/invoke-unit/sig net:uri-codec^
    uri-codec@))
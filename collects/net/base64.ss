(module base64 mzscheme
  (require mzlib/unit
           "base64-sig.ss"
           "base64-unit.ss")

  (define-values/invoke-unit/infer base64@)

  (provide-signature-elements base64^))

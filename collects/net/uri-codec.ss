(module uri-codec mzscheme
  (require (lib "unit.ss") "uri-codec-sig.ss" "uri-codec-unit.ss")

  (provide-signature-elements uri-codec^)

  (define-values/invoke-unit/infer uri-codec@))

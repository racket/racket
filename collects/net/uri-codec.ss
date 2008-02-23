(module uri-codec mzscheme
  (require mzlib/unit "uri-codec-sig.ss" "uri-codec-unit.ss")

  (provide-signature-elements uri-codec^)

  (define-values/invoke-unit/infer uri-codec@))

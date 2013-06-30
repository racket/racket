(module compiler racket/base
  (require racket/unit)

  (require compiler/sig)

  (require dynext/compile-sig)
  (require dynext/link-sig)
  (require dynext/file-sig)
  ;;
  (require dynext/compile)
  (require dynext/link)
  (require dynext/file)

  (require "option.rkt")

  (require compiler/compiler-unit)

  (define-values/invoke-unit/infer compiler@)

  (provide-signature-elements compiler^))

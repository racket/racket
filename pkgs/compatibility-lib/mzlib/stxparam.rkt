(module stxparam racket/base
  (require racket/stxparam)
  (provide define-syntax-parameter
	   syntax-parameterize)

  (require racket/stxparam-exptime)
  (provide syntax-parameter-value
           make-parameter-rename-transformer))

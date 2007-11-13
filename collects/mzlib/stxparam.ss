
(module stxparam scheme/base
  (require scheme/stxparam)
  (provide define-syntax-parameter
	   syntax-parameterize)

  (require scheme/stxparam-exptime)
  (provide syntax-parameter-value
           make-parameter-rename-transformer))

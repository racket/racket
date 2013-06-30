(module embed racket/base
  (require compiler/embed)
  (define mzc:create-embedding-executable create-embedding-executable)
  (define mzc:embedding-executable-add-suffix embedding-executable-add-suffix)
  (provide mzc:create-embedding-executable
	   mzc:embedding-executable-add-suffix))

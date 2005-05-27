
(module embed mzscheme
  (require (lib "embed.ss" "compiler"))
  (define mzc:make-embedding-executable make-embedding-executable)
  (define mzc:embedding-executable-add-suffix embedding-executable-add-suffix)
  (provide mzc:make-embedding-executable
	   mzc:embedding-executable-add-suffix))

;; module loader for SRFI-17
(module |17| mzscheme
  (require (all-except srfi/17/set set!)
	   (rename srfi/17/set my-set! set!))
  (provide (all-from-except srfi/17/set my-set!)
	   (rename my-set! set!)))

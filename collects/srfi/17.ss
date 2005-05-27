;; module loader for SRFI-17
(module |17| mzscheme
  (require (all-except (lib "set.ss" "srfi" "17") set!)
	   (rename (lib "set.ss" "srfi" "17") my-set! set!))
  (provide (all-from-except (lib "set.ss" "srfi" "17") my-set!)
	   (rename my-set! set!)))

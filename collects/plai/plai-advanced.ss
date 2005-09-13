(module plai-advanced mzscheme
  (require (rename (lib "htdp-advanced.ss" "lang") plai-else else)
           (lib "contract.ss" "mzlib" "private")
	   "private/datatype.ss"
           "test-harness.ss")

  ;; This macro requires & provides bindings without
  ;;  making them locally visible:
  (define-syntax (provide-advanced stx)
    #'(begin
	(require (lib "htdp-advanced.ss" "lang"))
	(provide (all-from-except (lib "htdp-advanced.ss" "lang")
				  plai-else))))
  (provide-advanced)
		   
  (provide (rename advanced-type-case type-case)
	   define-type
	   require provide provide-type
           (all-from-except (lib "contract.ss" "mzlib" "private") contract)
           (all-from "test-harness.ss"))

  (define-type-case advanced-type-case plai-else))

(module plai-intermediate mzscheme
  (require (rename (lib "htdp-intermediate-lambda.ss" "lang") plai-else else)
           (lib "contract.ss" "mzlib" "private")
	   "private/datatype.ss"
           "test-harness.ss")

  ;; This macro requires & provides bindings without
  ;;  making them locally visible:
  (define-syntax (provide-intermediate stx)
    #'(begin
	(require (lib "htdp-intermediate-lambda.ss" "lang"))
	(provide (all-from-except (lib "htdp-intermediate-lambda.ss" "lang")
				  plai-else))))
  (provide-intermediate)
		   
  (provide (rename intermediate-type-case type-case)
	   define-type
	   require provide provide-type
           (all-from (lib "contract.ss" "mzlib" "private"))
           (all-from "test-harness.ss"))

  (define-type-case intermediate-type-case plai-else))

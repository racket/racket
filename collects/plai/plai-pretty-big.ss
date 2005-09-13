(module plai-pretty-big mzscheme
  (require "private/datatype.ss"
           "test-harness.ss"
           (lib "contract.ss" "mzlib" "private"))

  ;; This macro requires & provides bindings without
  ;;  making them locally visible:
  (define-syntax (provide-advanced stx)
    #'(begin
	(require (lib "plt-pretty-big.ss" "lang"))
	(provide (all-from (lib "plt-pretty-big.ss" "lang")))))
  (provide-advanced)
		   
  (provide (rename pretty-big-type-case type-case)
	   define-type
	   provide-type
           (all-from (lib "contract.ss" "mzlib" "private"))
           (all-from "test-harness.ss"))

  (define-type-case pretty-big-type-case else))

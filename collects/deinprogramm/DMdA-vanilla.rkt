#lang deinprogramm/DMdA

(require syntax/docprovide)
(provide #%app #%top (rename-out (DMdA-module-begin #%module-begin)) #%datum #%top-interaction
	 require lib planet provide
	 define let let* letrec lambda cond if else and or
	 define-record-procedures define-record-procedures-parametric
	 .. ... .... ..... ......
	 check-expect check-within check-error check-member-of check-range
	 check-property for-all ==> expect expect-within expect-member-of expect-range
	 signature contract : define-contract -> mixed one-of predicate combined list-of
	 number real rational integer natural boolean true false string empty-list any property)
(provide cons)
(provide-and-document
 procedures
 (all-from-except vanilla: deinprogramm/DMdA procedures
		  quote eq? equal?
		  set!
		  define-record-procedures-2
		  symbol? string->symbol symbol->string
		  apply))


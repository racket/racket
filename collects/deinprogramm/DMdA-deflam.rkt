#lang deinprogramm/DMdA

;; this file exists so there is a single file that exports
;; identifiers named 'define' and 'lambda' that are the 
;; assignments and advanced versions of 'define' and 'lambda',
;; so that we can tell scribble about this file and then it
;; can connect up the re-exports to the documentation properly.

(provide (rename-out (DMdA-advanced-lambda lambda))
	 (rename-out (DMdA-advanced-define define)))

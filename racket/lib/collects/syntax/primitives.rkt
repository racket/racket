
(module primitives racket/base
  
  ;; The following primitives either invoke functions, or
  ;;  install functions that can be used later.
   (define (procedure-calling-prims)
     '(apply map for-each andmap ormap make-promise
	     dynamic-wind thread call-in-nested-thread
	     make-object call-with-values time-apply
	     call-with-output-file call-with-input-file
	     with-output-to-file with-input-from-file
	     exit-handler current-eval initial-exception-handler
	     current-prompt-read current-load
	     call-with-escape-continuation call-with-current-continuation
	     current-print port-display-handler port-write-handler
	     port-print-handler global-port-print-handler
	     error-display-handler error-escape-handler
	     port-read-handler error-value->string-handler
	     call/ec call/cc hash-table-get
	     call-with-continuation-prompt call-with-composable-continuation
	     hash-table-map hash-table-for-each make-input-port make-output-port
	     current-module-name-resolver
	     call-with-semaphore call-with-semaphore/enable-break))

   ;; The following primitives can compute return values by an
   ;;  internal chained tail call (relevant to mzc)
   (define (internal-tail-chain-prims)
     '(call-with-values apply 
	error
	call-with-current-continuation
	call-with-composable-continuation
	call-with-continuation-prompt
	call-with-escape-continuation
	call-with-semaphore
	call-with-semaphore/enable-break
	hash-table-get
	write-image-to-file
	syntax-local-value
        eval eval-syntax
        andmap ormap for-each))

   (provide procedure-calling-prims
	    internal-tail-chain-prims))


;; Poor man's stack-trace-on-exceptions/profiler.
;; See doc.txt for information.

(module errortrace mzscheme
  (require "errortrace-lib.ss")
  
  (provide print-error-trace 
	   error-context-display-depth 
	   
	   instrumenting-enabled 

	   profiling-enabled
	   profiling-record-enabled
	   profile-paths-enabled 
	   get-profile-results
	   output-profile-results

	   execute-counts-enabled
	   get-execute-counts
	   annotate-executed-file)
  
  (current-compile errortrace-compile-handler)
  (error-display-handler errortrace-error-display-handler)
  (use-compiled-file-paths (cons (build-path "compiled" "errortrace")
				 (use-compiled-file-paths))))

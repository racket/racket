#lang racket/base

;; Poor man's stack-trace-on-exceptions/profiler.
;; See manual for information.

(require "errortrace-lib.rkt")

(provide print-error-trace
         error-context-display-depth

         instrumenting-enabled

         profiling-enabled
         profiling-record-enabled
         profile-paths-enabled
         get-profile-results
         output-profile-results
         clear-profile-results

         execute-counts-enabled
         get-execute-counts
         annotate-executed-file

         coverage-counts-enabled
         get-coverage
         test-coverage-info
         annotate-covered-file)

(current-compile errortrace-compile-handler)
(error-display-handler errortrace-error-display-handler)
(use-compiled-file-paths (cons (build-path "compiled" "errortrace")
                               (use-compiled-file-paths)))

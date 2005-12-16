;; SRFI Tests

(load-relative "loadtest.ss")

;; Test that all SRFIs load.  Run this in both DrScheme and
;; MzScheme for maximum coverage.

;; We just require all the SRFIs and hope nothing bombs.
;; Keep an eye out for error messages!

(require (lib "1.ss" "srfi"))
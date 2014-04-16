#lang racket/base
(require racket/runtime-path
         rackunit
         racket/path)

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))

(define-runtime-path here ".")
(define collects 
  (normalize-path (build-path here ".." "..")))
(define (collect-trim bs)
  (regexp-replace* (regexp-quote (path->bytes collects)) bs #"PLTHOME/collects"))

(define (require&catch path)
  (define out-bs (open-output-bytes))
  (define err-bs (open-output-bytes))
  (parameterize ([current-output-port out-bs]
                 [current-error-port err-bs]
                 ;; Don't test context output; it's too fragile.
                 [error-print-context-length 0])
    (dynamic-require path #f))
  (close-output-port out-bs)
  (close-output-port err-bs)
  (values (collect-trim (get-output-bytes out-bs))
          (collect-trim (get-output-bytes err-bs))))

(define-syntax-rule (test-file pth out err)
  (begin
    (define-runtime-module-path mod (file pth))
    (define-values (cout cerr) (require&catch mod))
    (check-equal? cout out)
    (check-equal? cerr err)))

(test-file "standalone-check-test.rkt"
           #"Oh HAI!\nI didn't run\n"
           #"\
--------------------
ERROR
Outta here!

--------------------
--------------------
FAILURE
name:       check
location:   (#<path:PLTHOME/collects/tests/rackunit/standalone-check-test.rkt> 48 0 1450 17)
expression: (check = 1 2)
params:     (#<procedure:=> 1 2)\nmessage:    0.0

Check failure
--------------------
")

(test-file "standalone-test-case-test.rkt"
           #""
           #"\
--------------------
ERROR
First Outta here!

--------------------
--------------------
error
ERROR
Second Outta here!

--------------------
--------------------
FAILURE
actual:     1
expected:   2
name:       check-eq?
location:   (#<path:PLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt> 23 12 626 15)
expression: (check-eq? 1 2)

Check failure
--------------------
--------------------
failure
FAILURE
actual:     1
expected:   2
name:       check-eq?
location:   (#<path:PLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt> 24 21 664 15)
expression: (check-eq? 1 2)

Check failure
--------------------
")

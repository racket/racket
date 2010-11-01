#lang racket/base
(require racket/runtime-path
         rackunit
         racket/path)

(define-runtime-path here ".")
(define collects 
  (normalize-path (build-path here ".." "..")))
(define (collect-trim bs)
  (regexp-replace* (regexp-quote (path->bytes collects)) bs #"PLTHOME/collects"))
  
(define (require&catch path)
  (define out-bs (open-output-bytes))
  (define err-bs (open-output-bytes))
  (parameterize ([current-output-port out-bs]
                 [current-error-port err-bs])
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
           #"--------------------\nERROR\nOutta here!\n\n === context ===\nPLTHOME/collects/rackunit/private/check.rkt:144:29\nPLTHOME/collects/rackunit/private/check.rkt:77:0: top-level-check-around\nPLTHOME/collects/tests/rackunit/standalone-check-test.rkt: [running body]\nPLTHOME/collects/tests/rackunit/standalone.rkt: [running body]\n\n\n--------------------\n--------------------\nFAILURE\nname:       check\nlocation:   (#<path:PLTHOME/collects/tests/rackunit/standalone-check-test.rkt> 44 0 1344 17)\nexpression: (check = 1 2)\nparams:     (#<procedure:=> 1 2)\nmessage:    0.0\n\n--------------------\n")

(test-file "standalone-test-case-test.rkt"
           #"#t\n#t\n"
           #"--------------------\nUnnamed test \nERROR\nOutta here!\n\n === context ===\nPLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt: [running body]\n\n\n--------------------\n--------------------\nerror\nERROR\nOutta here!\n\n--------------------\n--------------------\nUnnamed test \nFAILURE\nname:       check-eq?\nlocation:   (#<path:PLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt> 19 12 507 15)\nexpression: (check-eq? 1 2)\nparams:     (1 2)\nactual:     1\nexpected:   2\n\n--------------------\n--------------------\nfailure\nFAILURE\nname:       check-eq?\nlocation:   (#<path:PLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt> 20 21 545 15)\nexpression: (check-eq? 1 2)\nparams:     (1 2)\nactual:     1\nexpected:   2\n\n--------------------\n")


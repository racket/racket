#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/port
         tests/eli-tester)

(define output
  (with-output-to-string
     (lambda ()
       (parameterize ([current-error-port (current-output-port)])
         (define-check (check3)
           (fail-check))
         
         (run-tests (test-suite "tests" (let ((foo check3)) (foo))))))))

(test
 (regexp-match
  (regexp (format "~a.*~a.*~a"
                 (regexp-quote "--------------------\ntests > #f\nUnnamed test \nFAILURE\nname:       check3\nlocation:   ")                 
                 (regexp-quote "pr10950.rkt:14:51")
                 (regexp-quote "0 success(es) 1 failure(s) 0 error(s) 1 test(s) run\n")))
  output))

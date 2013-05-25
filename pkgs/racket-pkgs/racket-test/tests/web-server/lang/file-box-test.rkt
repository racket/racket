#lang racket/base
(require rackunit
         web-server/lang/file-box
         (only-in mzlib/file make-temporary-file))
(provide file-box-tests)

(define file-box-tests
  (test-suite
   "File Boxes"
   (test-case
    "Creating a file box"
    (check-not-false (file-box (make-temporary-file) 42)))
   
   (test-case
    "Reading a file box"
    (check-equal? (file-unbox (file-box (make-temporary-file) 42)) 42))
   
   (test-case
    "Writing a file box"
    (check-not-false (file-box-set! (file-box (make-temporary-file) 42) 43)))
   
   (test-case
    "Reading and Writing a file box"
    (check-equal? (let ([fb (file-box (make-temporary-file) 42)])
                    (file-box-set! fb 43)
                    (file-unbox fb))
                  43))))

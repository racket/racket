#lang racket/base
(require file/unzip tests/eli-tester)

;; test-me.zip's directory structure is test-zip/1/data.dat
(define (test-with-unzip)
  (with-unzip "unzip-me.zip"
              (lambda (tmp_dir)
                (with-input-from-file (build-path tmp_dir "test-zip" "1" "data.dat")
                  (lambda ()
                    (test (read-line) => "chenxiao"))))))

(define (test-with-unzip-entry)
  (with-unzip-entry "unzip-me.zip" 
                    (build-path "test-zip" "1" "data.dat")
                    (lambda (tmp_file)
                      (with-input-from-file tmp_file
                        (lambda ()
                          (test (read-line) => "chenxiao"))))))

(define (run-tests)
  (test-with-unzip)
  (test-with-unzip-entry))

(provide tests)
(module+ main (tests))
(define (tests) (test do (run-tests)))

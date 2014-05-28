#lang racket/base
(require file/unzip
         racket/runtime-path
         tests/eli-tester)

(define-runtime-path unzip-me.zip "unzip-me.zip")

;; test-me.zip's directory structure is test-zip/1/data.dat
(define (test-with-unzip in)
  (call-with-unzip in
                   (lambda (tmp_dir)
                     (with-input-from-file (build-path tmp_dir "test-zip" "1" "data.dat")
                       (lambda ()
                         (test (read-line) => "chenxiao"))))))

(define (test-with-unzip-entry)
  (call-with-unzip-entry unzip-me.zip
                         (build-path "test-zip" "1" "data.dat")
                         (lambda (tmp_file)
                           (with-input-from-file tmp_file
                             (lambda ()
                               (test (read-line) => "chenxiao"))))))

(define (run-tests)
  (test-with-unzip unzip-me.zip)
  (call-with-input-file* unzip-me.zip test-with-unzip)
  (test-with-unzip-entry))

(provide tests)
(module+ main (tests))
(define (tests) (test do (run-tests)))

#lang errortrace racket/base

(module+ test
  (require
    racket/file
    racket/math
    rackunit
    rackunit/text-ui)

  (run-tests
    (test-suite "Reading stat info"
      (test-case "Writing temporary file and reading stat"
        (define start-time-milliseconds (current-inexact-milliseconds))
        (define temp-file-path (make-temporary-file))
        (define TEST-STRING "stat test")
        (display-to-file TEST-STRING temp-file-path #:exists 'truncate)
        (define stat-result (file-or-directory-stat temp-file-path))
        ; TODO: Make sure the file is removed even if `file-or-directory-stat`
        ; raises an exception.
        (delete-file temp-file-path)
        (check-equal? (hash-ref stat-result 'size) (string-length TEST-STRING))
        (define (positive-fixnum? n) (and (positive-integer? n) (fixnum? n)))
        (check-pred positive-fixnum? (hash-ref stat-result 'inode))
        (check-pred positive-fixnum? (hash-ref stat-result 'device-id)))
  ))
)

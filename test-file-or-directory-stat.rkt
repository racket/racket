#lang errortrace racket/base

(module+ test
  (require
    racket/file
    racket/math
    rackunit
    rackunit/text-ui)

  (define TEST-CONTENT "stat test")

  ;; String [Symbol | Boolean] -> [Values Hash Procedure]
  ;;
  ;; Given a path string and a value for the `as-link?` flag, create a stat
  ;; result for the path and return the stat result and a helper procedure to
  ;; access the stat result by key (without specifying the stat result itself).
  ;; As a special case, if `as-link?` is the symbol `'do-not-use`, the value
  ;; isn't passed to `file-or-directory-stat`. This is used to check if the
  ;; correct default value is used.
  (define (stat-and-stat-ref path as-link?)
    (define stat-result
      (if (eq? as-link? 'do-not-use)
          (file-or-directory-stat path)
          (file-or-directory-stat path as-link?)))
    (define (stat-ref symbol) (hash-ref stat-result symbol))
    (values stat-result stat-ref))

  ;; [Symbol | Boolean] -> [Values String Hash Procedure]
  ;;
  ;; Given a value for the `file-or-directory-stat` argument `as-link?`, create
  ;; a temporary file and return a stat result for it and a helper procedure to
  ;; access the stat result by key (without specifying the stat result itself).
  ;; As a special case, if `as-link?` is the symbol `'do-not-use`, the value
  ;; isn't passed to `file-or-directory-stat`. This is used to check if the
  ;; correct default value is used.
  (define (make-temp-file as-link?)
    (define temp-file-path (path->string (make-temporary-file)))
    (display-to-file TEST-CONTENT temp-file-path #:exists 'truncate)
    (define-values (stat-result stat-ref)
      (stat-and-stat-ref temp-file-path as-link?))
    (values temp-file-path stat-result stat-ref))

  ;; XXX: Expected bitmasks of #o664 depend on the OS being Posix and the umask
  ;; being set to 002.
  ;;
  ;; TODO: Put files and links inside a temporary directory and remove the
  ;; directory at the end of each test case.

  (run-tests
    (test-suite "Reading stat info"
      (test-case "Writing temporary file and reading stat"
        (define-values (temp-file-path stat-result stat-ref) (make-temp-file 'do-not-use))
        ; Check size, inode, hardlink count and device id.
        (check-equal? (stat-ref 'size) (string-length TEST-CONTENT))
        (check-equal? (stat-ref 'hardlink-count) 1)
        (define (positive-fixnum? n) (and (positive-integer? n) (fixnum? n)))
        (check-pred positive-fixnum? (stat-ref 'inode))
        (check-pred positive-fixnum? (stat-ref 'device-id))
        ; Check timestamps.
        (check-equal? (quotient (stat-ref 'modify-time-nanoseconds) #e1e9)
                      (stat-ref 'modify-time-seconds))
        (check-equal? (quotient (stat-ref 'access-time-nanoseconds) #e1e9)
                      (stat-ref 'access-time-seconds))
        (check-equal? (quotient (stat-ref 'change-time-nanoseconds) #e1e9)
                      (stat-ref 'change-time-seconds))
        (check-equal? (stat-ref 'modify-time-seconds)
                      (file-or-directory-modify-seconds temp-file-path))
        (check-equal? (stat-ref 'change-time-nanoseconds)
                      (stat-ref 'modify-time-nanoseconds))
        (check-true (>= (stat-ref 'access-time-nanoseconds)
                        (stat-ref 'modify-time-nanoseconds)))
        ; Check stat data that corresponds to mode bits.
        ;  Read/write/execute
        (check-equal? (bitwise-and (stat-ref 'permission-bits) #o777) #o664)
        ; TODO: Make sure the file is removed even if `file-or-directory-stat`
        ; raises an exception.
        (delete-file temp-file-path))

      (test-case "Comparison with other Racket functions"
        (define-values (temp-file-path stat-result stat-ref) (make-temp-file 'do-not-use))
        (check-equal? (stat-ref 'size) (file-size temp-file-path))
        (check-equal? (stat-ref 'modify-time-seconds)
                      (file-or-directory-modify-seconds temp-file-path))
        (check-equal? (bitwise-and (stat-ref 'permission-bits) #o777)
                      (file-or-directory-permissions temp-file-path 'bits))
        ; TODO: Make sure the file is removed even if `file-or-directory-stat`
        ; raises an exception.
        (delete-file temp-file-path))

      (test-case "Link traversal"
        (define-values (temp-file-path stat-result stat-ref) (make-temp-file 'do-not-use))
        (define link-file-path (string-append temp-file-path "_link"))
        (make-file-or-directory-link temp-file-path link-file-path)
        (for ([test-data `((do-not-use ,(string-length TEST-CONTENT) #o664)
                           (#f         ,(string-length TEST-CONTENT) #o664)
                           (#t         ,(string-length temp-file-path) #o777))])
          (let ([as-link?                 (car test-data)]
                [expected-size            (cadr test-data)]
                [expected-permission-bits (caddr test-data)])
            (define-values (stat-result stat-ref)
              (stat-and-stat-ref link-file-path as-link?))
            (check-equal? (stat-ref 'size) expected-size)
            (check-equal? (bitwise-and (stat-ref 'permission-bits) #o777)
                          expected-permission-bits)))
        ; TODO: Make sure the file is removed even if `file-or-directory-stat`
        ; raises an exception.
        (delete-file temp-file-path))
  ))
)

#lang scheme

(require planet/private/planet-shared)

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ a b)
     (with-syntax ([line (syntax-line stx)]
                   [file (let ([s (syntax-source stx)])
                           (if (string? s)
                               s
                               "<<unknown file>>"))])
       #`(test/proc file line a b))]))

(define (test/proc file line got expected)
  (unless (equal? got expected)
    (error 'test.rkt "FAILED ~a: ~s\n     got ~s\nexpected ~s" file line got expected)))


(test (string->mz-version "372")
      (make-mz-version 372 0))

(test (string->mz-version "372.2")
      (make-mz-version 372 2000))

(test (string->mz-version "4.0")
      (make-mz-version 400 0))

(test (string->mz-version "4.1")
      (make-mz-version 401 0))

(test (string->mz-version "4.0.1")
      (make-mz-version 400 1000))

(test (string->mz-version "4..1")
      #f)

(printf "tests passed\n")

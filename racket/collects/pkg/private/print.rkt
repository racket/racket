#lang racket/base
(require racket/format
         "params.rkt")

;; Output and error helpers

(provide (all-defined-out))

(define-logger pkg)

(define (pkg-error . rest)
  (apply (current-pkg-error) rest))

(define (format-list l)
  (if (null? l)
      " [none]"
      (apply string-append
             (for/list ([v (in-list l)])
               (format "\n   ~a" v)))))

(define (log-exn x what)
  (log-pkg-error (~a "failure ~a\n"
                         "  error: ~s")
                     what
                     (exn-message x)))

(define (printf/flush fmt . args)
  ;; For status reporting, flush immediately after printing
  (apply printf fmt args)
  (flush-output))

(define (complain-about-source s reason)
  (pkg-error (~a "invalid package source;\n"
                 " ~a\n"
                 "  given: ~a")
             reason
             s))

#lang racket/base

(require rackunit
         racket/format
         racket/list
         racket/port)

(module dummy racket/base
  (printf "\n~a" (current-command-line-arguments)))

(module+ test
  ;; Run test in subprocess.
  (define args #("foo" "bar" "1234"))
  (define-values (sp stdout stdin stderr)
   (subprocess #f #f #f (find-executable-path "raco")
               "test" "-s" "dummy" "./test-arg-forwarding.rkt" "--" "foo" "bar" "1234"))
  (thread (lambda () (close-output-port stdin)))
  (subprocess-wait sp)
  ;; Check subprocess returned successfully.
  (define st (subprocess-status sp))
  (check-pred zero? st (port->string stderr #:close? #f))
  (close-input-port stderr)
  ;; Check subprocess output against expected output.
  (define lines (port->lines stdout #:close? #t))
  (define result (and (not (empty? lines)) (last lines)))
  (check-equal? result (~a args) (apply string-append lines)))
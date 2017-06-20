#lang racket/base
(require racket/system
         compiler/find-exe)

(define exe (find-exe))

(define (try mode mod expect . extra-args)
  (printf "trying ~s ~s\n" mod mode)
  (define s (open-output-bytes))
  (parameterize ([current-output-port s])
    (apply system* exe "-l-" "raco" "test"
           mode (append extra-args (list "-l" (string-append "tests/compiler/test/" mod)))))
  (define last-line
    (for/fold ([prev #f]) ([s (in-lines (open-input-bytes (get-output-bytes s)))])
      (if (or (eof-object? s)
              (equal? s "1 test passed"))
          prev
          s)))
  (unless (equal? expect last-line)
    (error 'runtime "test failed\n  module: ~s~a\n  expected: ~s\n  got: ~s"
           mod
           (if (null? extra-args) "" (format "\n  extra args: ~s" extra-args))
           expect
           last-line)))

(for ([mod '("--direct" "--place" "--process")])
  (try mod "racket.rkt" "'(1 2)")
  (try mod "scheme.rkt" "(1 2)")
  (try mod "args.rkt" "1" "-q" "++arg" "1")
  (try mod "args.rkt" "1" "-q" "-s" "multi-test" "++args" "1 2"))





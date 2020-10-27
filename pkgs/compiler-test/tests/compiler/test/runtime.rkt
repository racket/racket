#lang racket/base
(require racket/system
         racket/file
         compiler/find-exe)

(define exe (find-exe))

;; Return last line of the input, or #f if it's empty.
(define (read-last-line input)
  (for/fold ([prev #f]) ([s (in-lines input)])
    (if (or (eof-object? s)
            (equal? s "1 test passed"))
        prev
        s)))

;; 'expect' is either a string or a procedure to check the result
(define (try mode mod expect . extra-args)
  (printf "trying ~s ~s\n" mod mode)
  (define s (open-output-bytes))
  (parameterize ([current-output-port s])
    (apply system* exe "-l-" "raco" "test"
           mode (append extra-args (list "-l" (string-append "tests/compiler/test/" mod)))))
  (define last-line (read-last-line (open-input-bytes (get-output-bytes s))))
  (unless (if (procedure? expect)
              (expect)
              (equal? expect last-line))
    (error 'runtime "test failed\n  module: ~s~a\n  expected: ~s\n  got: ~s"
           mod
           (if (null? extra-args) "" (format "\n  extra args: ~s" extra-args))
           expect
           last-line)))

(define (check-output-file expect file)
  (call-with-input-file* file
    (lambda (in)
      (let* ([last-line (read-last-line in)]
             [result (equal? expect last-line)])
        (unless result
          (eprintf "Output file check, expected: ~s, got: ~s\n" expect last-line))
        result))))

(for ([mod '("--direct" "--place" "--process")])
  (try mod "racket.rkt" "'(1 2)")
  (try mod "scheme.rkt" "(1 2)")
  (try mod "args.rkt" "1" "-q" "++arg" "1")
  (try mod "args.rkt" "1" "-q" "-s" "multi-test" "++args" "1 2")

  ;; Test for 'raco test --output <file>'
  ;; We check the content of the <file> instead of the standard output
  (let ([file (make-temporary-file)])
    (for ([sub-module '("test-stdout" "test-stderr")])
      (try mod "output.rkt"
           (lambda () (check-output-file "1" file))
           "-s" sub-module
           "--output" (path->string file)))
    (delete-file file)))

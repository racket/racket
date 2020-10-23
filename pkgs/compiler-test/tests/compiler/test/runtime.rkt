#lang racket/base
(require racket/system
         racket/file
         compiler/find-exe)

(define exe (find-exe))

;; Return last line of the input, or #f if it's empty.
(define (input-last-line input)
  (for/fold ([prev #f]) ([s (in-lines input)])
      (if (or (eof-object? s)
              (equal? s "1 test passed"))
          prev
          s)))

(define (try mode mod expect post-check . extra-args)
  (printf "trying ~s ~s\n" mod mode)
  (define s (open-output-bytes))
  (parameterize ([current-output-port s])
    (apply system* exe "-l-" "raco" "test"
           mode (append extra-args (list "-l" (string-append "tests/compiler/test/" mod)))))

  (define last-line (input-last-line (open-input-bytes (get-output-bytes s))))
  (define post-check-result (if (procedure? post-check) (post-check) #t))
  (unless (and (equal? expect last-line)
               post-check-result)
    (error 'runtime "test failed\n  module: ~s~a\n  expected: ~s\n  got: ~s\n  post-check: ~s"
           mod
           (if (null? extra-args) "" (format "\n  extra args: ~s" extra-args))
           expect
           last-line
           post-check-result)))

(define (output-file-check except file)
  (equal? except (input-last-line (open-input-file file))))

(for ([mod '("--direct" "--place" "--process")])
  (try mod "racket.rkt" "'(1 2)" #t)
  (try mod "scheme.rkt" "(1 2)" #t)
  (try mod "args.rkt" "1" #t "-q" "++arg" "1")
  (try mod "args.rkt" "1" #t "-q" "-s" "multi-test" "++args" "1 2")

  ;; Test for 'raco test --output <file>'
  ;; We do not except any output to stdout, and will
  ;; check the content of the output file
  (let ([file (make-temporary-file)])
    (try mod "racket.rkt" #f
         (lambda () (output-file-check "'(1 2)" file))
         "--output" (path->string file))
    (delete-file file)))





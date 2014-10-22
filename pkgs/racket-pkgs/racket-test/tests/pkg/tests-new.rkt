#lang racket/base
(require racket/file
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (shelly-case
   "new"

   (define tmp-dir (path->directory-path (make-temporary-file "pkg~a" 'directory)))

   (parameterize ([current-directory tmp-dir])
     (shelly-case
      "new test-blah"
      $ "raco pkg new test-blah"
      $ "raco pkg install test-blah/"
      $ "racket test-blah/main.rkt"
      $ "racket -e \"(require test-blah)\""
      $ "raco pkg remove test-blah")

     (shelly-case
      "modify package"
      $ "raco pkg new test-foo"
      $ "raco pkg install test-foo/"
      $ "echo \"#lang racket/base\n(provide c)\n(define c 5)\" > test-foo/main.rkt"
      $ "racket -e \"(require test-foo)\" -e \"c\"" =stdout> "5\n"
      $ "raco pkg remove test-foo")

     (shelly-case
      "invalid collection name"
      $ "raco pkg new foo/bar" =exit> 1)

     (shelly-case
      "folder already exists"
      $ "raco pkg new repeat"
      $ "raco pkg new repeat" =exit> 1)))))

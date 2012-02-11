#lang racket/base
(require "private/repl-test.rkt" "private/drracket-test-util.rkt")
(fire-up-drscheme-and-run-tests (λ () (run-test '(debug))))


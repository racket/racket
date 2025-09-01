;;; Copyright (c) 2000-2018 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(import (rnrs) (tests compiler-test) (tests helpers) (tests unit-tests) (nanopass helpers))

(printf "Running unit tests\n")
(let* ([succeeded? (run-unit-tests)]
       [succeeded? (and (run-ensure-correct-identifiers) succeeded?)]
       [succeeded? (and (run-maybe-tests) succeeded?)]
       [succeeded? (and (run-maybe-dots-tests) succeeded?)]
       [succeeded? (and (run-maybe-unparse-tests) succeeded?)]
       [succeeded? (and (run-language-dot-support) succeeded?)]
       [succeeded? (and (run-argument-name-matching) succeeded?)]
       [succeeded? (and (run-error-messages) succeeded?)]
       [succeeded? (and (run-pass-parser-unparser) succeeded?)])
  (printf "Compiler loaded, running all tests (quietly)\n")
  (time
    (begin
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)
      (run-all-tests)))
  (exit (if succeeded? 0 1)))

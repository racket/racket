;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(import (rnrs) (tests compiler-test) (tests helpers) (tests unit-tests) (nanopass helpers))

(printf "Running unit tests\n")
(run-unit-tests)
(run-ensure-correct-identifiers)
(run-maybe-tests)
(run-maybe-dots-tests)
(run-maybe-unparse-tests)
(run-language-dot-support)
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
(exit)

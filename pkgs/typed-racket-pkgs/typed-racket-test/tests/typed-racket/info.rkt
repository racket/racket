#lang info

(define test-omit-paths '("fail"
                          "xfail"))
(define test-command-line-arguments
  '(("succeed/priority-queue.scm" ())
    ("succeed/hw01.scm" ())
    ("succeed/foo.scm" ())
    ("succeed/batched-queue.scm" ())))

#lang scheme/base

(require tests/eli-tester lazy)

;; tests for lazy language constructs
;; add tests as needed

(provide test-take)

(define (test-take)
  (define test-lst1 '(1 2 3))
  (test (! (take "nonnum" test-lst1)) =error> "take: expects type <non-negative exact integer> as 1st argument, given: \"nonnum\"; other arguments were: (1 2 3)"
        (! (take -1 test-lst1)) =error> "take: expects type <non-negative exact integer> as 1st argument, given: -1; other arguments were: (1 2 3)"
        (! (take -1 "nonlist")) =error> "take: expects type <non-negative exact integer> as 1st argument, given: -1; other arguments were: \"nonlist\""
        (! (take 0 "nonlist")) => '() ; this is how Racket's take behaves
        (! (take 1 "nonlist")) =error> "take: not a proper list: \"nonlist\""
        (! (take 0 null)) => '()
        (! (take 0 test-lst1)) => '()  ; test for push#22080
        (! (car (take 1 test-lst1))) => 1
        (! (cdr (take 1 test-lst1))) => '()
        (! (first (take 2 test-lst1))) => 1
        (! (second (take 2 test-lst1))) => 2
        (! (cddr (take 2 test-lst1))) => '()
        (! (first (take 4 test-lst1))) => 1
        (! (second (take 4 test-lst1))) => 2
        (! (third (take 4 test-lst1))) => 3
        (! (fourth (take 4 test-lst1))) =error> "take: index 4 too large for input list"))

; not working, only get 1 test passed
#;(define (langimpl-tests)
  (test (test-take)))
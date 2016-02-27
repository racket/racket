#lang racket

(require (submod racket/require for-testing))

;; Tests for multi.
(begin-for-syntax
 (module+ test
   (require tests/eli-tester)
   (test (map syntax-e (multi #'("a" "b" "c")))         => '("a/b/c")
         (map syntax-e (multi #'("a" ("b" "c") "d")))   => '("a/b/d" "a/c/d")
         (map syntax-e (multi #'("a" "b" ("c" "d"))))   => '("a/b/c" "a/b/d")
         (map syntax-e (multi #'(("a" "b") "c" "d")))   => '("a/c/d" "b/c/d")
         (map syntax-e (multi #'(("a" "b") ("c" "d")))) => '("a/c" "a/d" "b/c" "b/d")
         (map syntax-e (multi #'(("a" "b" "c" "d"))))   => '("a" "b" "c" "d")
         (map syntax-e (multi #'(("a" "b" ("c" "d"))))) =error> ""
         (map syntax-e (multi #'(a b c)))       => '(a/b/c)
         (map syntax-e (multi #'(a (b c) d)))   => '(a/b/d a/c/d)
         (map syntax-e (multi #'(a b (c d))))   => '(a/b/c a/b/d)
         (map syntax-e (multi #'((a b) c d)))   => '(a/c/d b/c/d)
         (map syntax-e (multi #'((a b) (c d)))) => '(a/c a/d b/c b/d)
         (map syntax-e (multi #'((a b c d))))   => '(a b c d)
         (map syntax-e (multi #'((a b (c d))))) =error> "")
   ))

#lang racket

(require rackunit rackunit/text-ui unstable/regexp "helpers.rkt")

(define-syntax (regexp-test stx)
  (syntax-case stx ()
    [(_ pattern string result)
     (syntax/loc stx
       (test-suite (format "(regexp-match ~s ~s) = ~s" 'pattern 'string 'result)
         (test-case "regexp"
           (check-equal? (regexp-match (regexp pattern) string) result))
         (test-case "pregexp"
           (check-equal? (regexp-match (pregexp pattern) string) result))))]))

(run-tests
 (test-suite "regexp.ss"
   (test-suite "regexp-sequence"
     (regexp-test (regexp-sequence) "a cat" (list ""))
     (regexp-test (regexp-sequence "cat") "a cat" (list "cat"))
     (regexp-test (regexp-sequence "hot" "dog") "a hotdog" (list "hotdog"))
     (regexp-test (regexp-sequence "cat" "dog") "a cat" #f)
     (regexp-test (regexp-sequence "cat" "dog") "a dog" #f)
     (regexp-test (regexp-sequence "a" "b|c") "c" #f))
   (test-suite "regexp-or"
     (regexp-test (regexp-or "cat") "a cat" (list "cat"))
     (regexp-test (regexp-or "cat" "dog") "a cat" (list "cat"))
     (regexp-test (regexp-or "cat" "dog") "a dog" (list "dog")))
   (test-suite "regexp-maybe"
     (regexp-test (regexp-maybe "cat") "a dog" (list ""))
     (regexp-test (regexp-maybe "cat") "catnap" (list "cat"))
     (regexp-test (regexp-maybe "hot" "dog") "hotdog!" (list "hotdog"))
     (regexp-test (regexp-maybe "hot" "dog") "a dog" (list "")))
   (test-suite "regexp-star"
     (regexp-test (regexp-star "a") "" (list ""))
     (regexp-test (regexp-star "a") "aaa" (list "aaa"))
     (regexp-test (regexp-star "ab") "abab" (list "abab"))
     (regexp-test (regexp-star "a" "b") "abab" (list "abab"))
     (regexp-test (regexp-star "a" "b") "aaaa" (list "")))
   (test-suite "regexp-plus"
     (regexp-test (regexp-plus "a") "" #f)
     (regexp-test (regexp-plus "a") "aaa" (list "aaa"))
     (regexp-test (regexp-plus "ab") "abab" (list "abab"))
     (regexp-test (regexp-plus "a" "b") "abab" (list "abab"))
     (regexp-test (regexp-plus "a" "b") "aaaa" #f))
   (test-suite "regexp-multi"
     (regexp-test (regexp-multi "^cat$") "ant\nbat\ncat\ndog" (list "cat")))
   (test-suite "regexp-save"
     (regexp-test (regexp-save "cat") "a cat" (list "cat" "cat")))))

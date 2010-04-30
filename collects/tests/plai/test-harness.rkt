#lang plai

(define-type WAE
  [num (n number?)]
  [id (s symbol?)])

(define (go)
  (test (num 5) (id 'x))
  
  (test 1 (+ 1 0))
  (test 1 1)
  (test 1 2)
  (test (/ 1 0) 0)
  (test (error "zamboni") 347)
  
  (test 3.4 3.4000001)
  (test +inf.0 +inf.0)
  
  (test/pred 0 zero?)
  (test/pred 1 zero?)
  (test/pred 1 (error 'pred))
  (test/pred 1 (lambda (n) (/ 1 0)))
  (test/pred "a" string->number)
  
  (test/exn (error "zamboni") "zamboni")
  (test/exn (error "samboni") "zamboni")
  (test/exn 5  "zamboni")
  (test/exn (/ 1 0) "division")
  
  (test/regexp (error "zamboni") "zam")
  (test/regexp (error "samboni") "zam")
  (test/regexp 5  "zam")
  (test/regexp (/ 1 0) "divis")
  )

(for ([catch? (in-list (list #t #f))])
  (plai-catch-test-exn catch?)
  (for ([errors? (in-list (list #t #f))])
    (print-only-errors errors?)
    (for ([abridged? (in-list (list #t #f))])
      (abridged-test-output abridged?)
      (with-handlers ([exn? (lambda (x) (printf "~S~n" x))])
        (go))
      (newline))))
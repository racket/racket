;; Change the lang to scheme for untyped version
#lang typed-scheme

(define tests (list (list (λ() 1) 1 "test 1")
                   (list (λ() 2) 2 "test 2")))

; Comment out the type signature when running untyped
(: check-all (All (A ...) ((List (-> A) A String) ... A -> Void)))
(define (check-all . tests)
 (let aux ([tests tests]
           [num-passed 0])
   (if (null? tests)
       (printf "~a tests passed.\n" num-passed)
       (let ((test (car tests)))
         (let ((actual ((car test)))
               (expected (cadr test))
               (msg (caddr test)))
           (if (equal? actual expected)
               (aux (cdr tests) (+ num-passed 1))
               (printf "Test failed: ~a. Expected ~a, got ~a.\n"
                       msg expected actual)))))))

(apply check-all tests) ; Works in untyped, but not in typed
(check-all (car tests) (cadr tests)) ; Works in typed or untyped

#lang racket

(require (only-in stepper/private/model stepper-model-debug?)
         (prefix-in m: "language-level-model.rkt")
         "test-engine.rkt"
         "test-cases.rkt"
         
         ;; for xml testing:
         ;; mzlib/class
         ;; (all-except xml/xml-snipclass snip-class)
         ;; (all-except xml/scheme-snipclass snip-class)
         ;; mred

         )


(provide run-test run-tests run-all-tests run-all-tests-except)


(define list-of-tests null)

(define (add-test test)
  (match test
    [(list name models string expected-steps extra-files)
     (unless (symbol? name)
       (error 'add-test "expected name to be a symbol, got: ~e" name))
     (unless (or (m:ll-model? models)
                 (and (list? models) (andmap m:ll-model? models)))
       (error 'add-test "expected models to be a list of models, got: ~e" models))
     (unless (string? string)
       (error 'add-test "expected string to be a string, got: ~e" string))
     (unless (list? expected-steps)
       (error 'add-test "expected expected-steps to be a list, got: ~e" expected-steps))
     (match extra-files
       [(list (list (? string? filename) (? string? content)) ...) #t]
       [other (error 'add-test 
                     "expected list of extra file specifications, got: ~e" 
                     other)])
     (when (assq name list-of-tests)
       (error 'add-test "name ~v is already in the list of tests" name))
     (set! list-of-tests 
           (append list-of-tests 
                   (list (list name
                               (rest test)))))]))

;; add all the tests imported from the test cases file(s):
(for-each add-test the-test-cases)



;; run a test : (list symbol test-thunk) -> boolean
;; run the named test, return #t if a failure occurred during the test
(define (run-one-test/helper test-pair)
  (apply run-one-test (car test-pair) (cadr test-pair)))

(define (run-all-tests)
  (andmap/no-shortcut 
   run-one-test/helper
   list-of-tests))

(define (run-all-tests-except nix-list)
  (andmap/no-shortcut 
   run-one-test/helper
   (filter (lambda (pr) (not (member (car pr) nix-list)))
           list-of-tests)))

(define (run-test name)
  (let ([maybe-test (assq name list-of-tests)])
    (if maybe-test
        (run-one-test/helper maybe-test)
        (error 'run-test "test not found: ~.s" name))))

(define (run-tests names)
  (ormap/no-shortcut run-test names))


;; like an ormap, but without short-cutting
(define (ormap/no-shortcut f args)
  (foldl (lambda (a b) (or a b)) #f (map f args)))

(define (andmap/no-shortcut f args)
  (foldl (lambda (a b) (and a b)) #t (map f args)))


  
  (provide ggg)
  ;; run whatever tests are enabled (intended for interactive use):
  (define (ggg)
    ;; NB: unlike standard linux config-file convention, the values
    ;; associated with the commented-out parameters are *not* the 
    ;; default ones, but rather the ones you're likely to want
    ;; to use instead of the default.
    (parameterize (#;[disable-stepper-error-handling #t]
                   #;[display-only-errors #t]
                   #;[store-steps #f]
                   #;[show-all-steps #t]
                   #;[stepper-model-debug? #t])
      #;(run-tests '(check-expect forward-ref check-within check-within-bad
                                  check-error check-error-bad))
      #;(run-tests '(teachpack-universe))
      (run-test 'let*-deriv)
      #;(run-test 'letrec1)
      #;(run-test 'require-test)

      #;(string->expanded-syntax-list m:mz "(if true 3 4)"
                                    #;"(define (a3 x) (if true x x))")
      #;(string->expanded-syntax-list m:intermediate "(letrec ([z 19] [a (lambda (x) (a x))] [b 4]) (+ (a 4) b))")
      
      #;(syntax-case
          (first (string->expanded-syntax-list m:intermediate 
                                               "(if true 3 4)"
                                               #;"(letrec ([z 19] [a (lambda (x) (a x))] [b 4]) (+ (a 4) b))"))
        ()
        [(_ _ _ 
            (_ _ (_ _ (_ _ it) _))) #'it])
      ))
  
  
  
  
  

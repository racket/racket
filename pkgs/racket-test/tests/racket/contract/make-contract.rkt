#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
(contract-eval
 '(define proj:add1->sub1
    (make-contract
     #:name 'proj:add1->sub1
     #:projection
     (lambda (blame)
       (lambda (f)
         (unless (and (procedure? f) (procedure-arity-includes? f 1))
           (raise-blame-error blame f "expected a unary function, got: ~e" f))
         (lambda (x)
           (unless (and (integer? x) (exact? x))
             (raise-blame-error (blame-swap blame) x
                                "expected an integer, got: ~e" x))
           (let* ([y (f (add1 x))])
             (unless (and (integer? y) (exact? y))
               (raise-blame-error blame y "expected an integer, got: ~e" y))
             (sub1 y)))))
     #:first-order
     (lambda (f)
       (and (procedure? f) (procedure-arity-includes? f 1))))))

(test/spec-passed/result
 'make-contract-1
 '((contract proj:add1->sub1 sqrt 'pos 'neg) 15)
 3)

(test/pos-blame
 'make-contract-2
 '(contract proj:add1->sub1 'dummy 'pos 'neg))

(test/pos-blame
 'make-contract-3
 '((contract proj:add1->sub1 (lambda (x) 'dummy) 'pos 'neg) 2))

(test/neg-blame
 'make-contract-4
 '((contract proj:add1->sub1 sqrt 'pos 'neg) 'dummy))

(ctest #t contract? proj:add1->sub1)
(ctest #f flat-contract? proj:add1->sub1)
(ctest #f chaperone-contract? proj:add1->sub1)
(ctest #t impersonator-contract? proj:add1->sub1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  make-chaperone-contract
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(contract-eval
 '(define proj:prime-box-list/c
    (let* ([prime? (λ (n)
                     (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                       (not (= (remainder n m) 0))))]
           [wrap-box (λ (blame b)
                       (chaperone-box
                        b
                        (λ (b v)
                          (unless (prime? v)
                            (raise-blame-error blame v
                                               "expected prime, got ~v" v))
                          v)
                        (λ (b v)
                          (unless (prime? v)
                            (raise-blame-error (blame-swap blame) v
                                               "expected prime, got ~v" v))
                          v)))])
      (make-chaperone-contract
       #:name 'prime-box-list/c
       #:first-order (λ (v) (and (list? v) (andmap box? v)))
       #:projection (λ (blame)
                      (λ (v)
                        (unless (and (list? v) (andmap box? v))
                          (raise-blame-error blame v
                                             "expected list of boxes, got ~v" v))
                        (map (λ (b) (wrap-box blame b)) v)))))))

(test/spec-passed/result
 'make-chaperone-contract-1
 '(contract proj:prime-box-list/c
            (list (box 2) (box 3) (box 5) (box 7))
            'pos 'neg)
 (list (box 2) (box 3) (box 5) (box 7)))

(test/pos-blame
 'make-chaperone-contract-2
 '(let ([boxes (contract proj:prime-box-list/c
                         (list (box 2) (box 3) (box 4) (box 5))
                         'pos 'neg)])
    (unbox (caddr boxes))))

(test/neg-blame
 'make-chaperone-contract-3
 '(let ([boxes (contract proj:prime-box-list/c
                         (list (box 2) (box 3) (box 4) (box 5))
                         'pos 'neg)])
    (set-box! (caddr boxes) 6)))

(ctest #t contract? proj:prime-box-list/c)
(ctest #f flat-contract? proj:prime-box-list/c)
(ctest #t chaperone-contract? proj:prime-box-list/c)
(ctest #f impersonator-contract? proj:prime-box-list/c)

(contract-eval
 '(define proj:bad-prime-box-list/c
    (let* ([prime? (λ (n)
                     (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                       (not (= (remainder n m) 0))))]
           [wrap-box (λ (blame b) (box (unbox b)))])
      (make-chaperone-contract
       #:name 'bad-prime-box-list/c
       #:first-order (λ (v) (and (list? v) (andmap box? v)))
       #:projection (λ (blame)
                      (λ (v)
                        (unless (and (list? v) (andmap box? v))
                          (raise-blame-error blame v
                                             "expected list of boxes, got ~v" v))
                        (map (λ (b) (wrap-box blame b)) v)))))))

(ctest #t contract? proj:bad-prime-box-list/c)
(ctest #f flat-contract? proj:bad-prime-box-list/c)
(ctest #t chaperone-contract? proj:bad-prime-box-list/c)
(ctest #f impersonator-contract? proj:bad-prime-box-list/c)

(contract-error-test
 'contract-error-test5
 '(contract proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
 exn:fail?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  make-flat-contract
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(contract-eval
 '(define proj:prime/c
    (let ([prime? (λ (n)
                    (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                      (not (= (remainder n m) 0))))])
      (make-flat-contract
       #:name 'prime/c
       #:first-order prime?))))

(test/spec-passed/result
 'make-flat-contract-1
 '(contract proj:prime/c 2 'pos 'neg)
 2)

(test/pos-blame
 'make-flat-contract-2
 '(contract proj:prime/c 4 'pos 'neg))

(ctest #t contract? proj:prime/c)
(ctest #t flat-contract? proj:prime/c)

(test/spec-passed/result
 'make-flat-contract-5
 '(chaperone-contract? proj:prime/c)
 #t)

;; Check to make sure that flat contracts always return the original value,
;; even if the projection is written badly.
(contract-eval
 '(define proj:prime-list/c
    (let ([prime? (λ (n)
                    (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                      (not (= (remainder n m) 0))))])
      (make-flat-contract
       #:name 'prime-list/c
       #:first-order (λ (v) (and (list? v) (andmap prime? v)))
       #:projection (λ (b)
                      (λ (v)
                        (unless (and (list? v) (andmap prime? v))
                          (raise-blame-error b v "expected prime list, got ~v" v))
                        (map values v)))))))

(test/spec-passed/result
 'make-flat-contract-bad-1
 '(contract proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
 (list 2 3 5 7))

(test/pos-blame
 'make-flat-contract-bad-2
 '(contract proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

(test/spec-passed/result
 'make-flat-contract-bad-3
 '(let ([l (list 2 3 5 7)])
    (eq? l (contract proj:prime-list/c l 'pos 'neg)))
 #t)

(ctest #t contract? proj:prime-list/c)
(ctest #t flat-contract? proj:prime-list/c)

(test/spec-passed/result
 'make-flat-contract-bad-6
 '(chaperone-contract? proj:prime-list/c)
 #t))
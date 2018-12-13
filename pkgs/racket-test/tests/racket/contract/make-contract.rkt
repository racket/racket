#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract 
                                               'racket/contract/private/blame)])
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
 3
 do-not-double-wrap)

(test/pos-blame
 'make-contract-2
 '(contract proj:add1->sub1 'dummy 'pos 'neg))

(test/pos-blame
 'make-contract-3
 '((contract proj:add1->sub1 (lambda (x) 'dummy) 'pos 'neg) 2))

(test/neg-blame
 'make-contract-4
 '((contract proj:add1->sub1 sqrt 'pos 'neg) 'dummy))

(test/spec-passed/result
 'make-contract-5
 '(list-contract?
   (make-contract #:late-neg-projection (λ (b) (λ (v) (λ (neg-party) v)))))
 #f)

  (test/spec-passed/result
   'make-contract-6
   '(list-contract?
     (make-contract #:late-neg-projection
                    (λ (b) (λ (v) (λ (neg-party) v)))
                    #:list-contract? "a true value"))
   #t)

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

  (contract-eval
   '(define val-first-proj:bad-prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b) (box (unbox b)))])
        (make-chaperone-contract
         #:name 'bad-prime-box-list/c
         #:first-order (λ (v) (and (list? v) (andmap box? v)))
         #:val-first-projection
         (λ (blame)
           (λ (v)
             (λ (neg-party)
               (unless (and (list? v) (andmap box? v))
                 (raise-blame-error blame v
                                    #:missing-party neg-party
                                    "expected list of boxes, got ~v" v))
               (map (λ (b) (wrap-box blame b)) v))))))))
  
  (contract-error-test
   'contract-error-test6
   '(contract val-first-proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
   exn:fail?)

  (contract-eval
   '(define late-neg-proj:bad-prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b) (box (unbox b)))])
        (make-chaperone-contract
         #:name 'bad-prime-box-list/c
         #:first-order (λ (v) (and (list? v) (andmap box? v)))
         #:late-neg-projection
         (λ (blame)
           (λ (v neg-party)
             (unless (and (list? v) (andmap box? v))
               (raise-blame-error blame v
                                  "expected list of boxes, got ~v" v))
             (map (λ (b) (wrap-box blame b)) v)))))))
  
  (contract-error-test
   'contract-error-test7
   '(contract late-neg-proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
   exn:fail?)
  
  (test/pos-blame
   'build-chaperone-contract-property1
   '(let ()
      (struct val-first-none ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:val-first-projection (λ (me)
                                  (λ (blame)
                                    (λ (val)
                                      (λ (neg-party)
                                        (raise-blame-error
                                         blame
                                         val
                                         "bad")))))
         #:name (λ (x) 'the-name)
         ;; make a very aproximate first-order check
         #:first-order (λ (c) (λ (x) #t))
         #:stronger (λ (x y) #f)))
      
      (((contract-projection (val-first-none))
        (make-blame (srcloc #f #f #f #f #f) 5 (λ () 'the-name) 'pos 'neg #t))
       5)))

  (test/spec-passed/result
   'build-chaperone-contract-property2
   '(let ()
      (struct odd-length-list-of-integers ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:val-first-projection (λ (me)
                                  (λ (blame)
                                    (λ (val)
                                      (λ (neg-party)
                                        (cond
                                          [(and (list? val)
                                                (odd? (length val))
                                                (andmap integer? val))
                                           val]
                                          [else
                                           (raise-blame-error
                                            blame
                                            val
                                            "bad")])))))
         #:list-contract? (λ (c) #t)
         #:name (λ (x) 'the-name)
         ;; make a very aproximate first-order check
         #:first-order (λ (c) (λ (x) #t))
         #:stronger (λ (x y) #f)))
      (list-contract? (odd-length-list-of-integers)))
   #t)

  (test/pos-blame
   'build-chaperone-contract-property-s-e1
   '(let ()
      (struct s-e-late-neg-none ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:collapsible-late-neg-projection
         (λ (ctc)
           (λ (blame)
             (values
              (λ (val neg-party)
                (raise-blame-error blame val "bad"))
              #f)))
         #:name (λ (x) 'the-name)
         #:first-order (λ (c) (λ (x) #t))
         #:stronger (λ (x y) #f)))

      (((contract-projection (s-e-late-neg-none))
        (make-blame (srcloc #f #f #f #f #f) 5 (λ () 'the-name) 'pos 'neg #t))
       5)))

  (test/pos-blame
   'build-chaperone-contract-property-s-e2
   '(let ()
      (struct s-e-late-neg-none ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:collapsible-late-neg-projection
         (λ (ctc)
           (λ (blame)
             (values
              (λ (val neg-party)
                (raise-blame-error blame val "bad"))
              #f)))
         #:name (λ (x) 'the-name)
         #:first-order (λ (c) (λ (x) #t))
         #:stronger (λ (x y) #f)))

      (((contract-projection (listof (s-e-late-neg-none)))
        (make-blame (srcloc #f #f #f #f #f) 5 (λ () 'the-name) 'pos 'neg #t))
       (list 1 2 3))))

  (contract-eval
   '(define prop:late-neg-proj:bad-prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b) (box (unbox b)))])
        (struct ctc ()
          #:property
          prop:chaperone-contract
          (build-chaperone-contract-property
           #:name (λ (c) 'bad-prime-box-list/c)
           #:first-order (λ (c) (λ (v) (and (list? v) (andmap box? v))))
           #:late-neg-projection
           (λ (c)
             (λ (blame)
               (λ (v neg-party)
                 (unless (and (list? v) (andmap box? v))
                   (raise-blame-error blame v #:missing-party neg-party
                                      "expected list of boxes, got ~v" v))
                 (map (λ (b) (wrap-box blame b)) v))))))
        (ctc))))
  
  (contract-error-test
   'contract-error-test8
   '(contract prop:late-neg-proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
   exn:fail?)

  (contract-eval
   '(define prop:val-first-proj:bad-prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b) (box (unbox b)))])
        (struct ctc ()
          #:property
          prop:chaperone-contract
          (build-chaperone-contract-property
           #:name (λ (c) 'bad-prime-box-list/c)
           #:first-order (λ (c) (λ (v) (and (list? v) (andmap box? v))))
           #:val-first-projection
           (λ (c)
             (λ (blame)
               (λ (v)
                 (λ (neg-party)
                   (unless (and (list? v) (andmap box? v))
                     (raise-blame-error blame v #:missing-party neg-party
                                        "expected list of boxes, got ~v" v))
                   (map (λ (b) (wrap-box blame b)) v)))))))
        (ctc))))
  
  (contract-error-test
   'contract-error-test9
   '(contract prop:val-first-proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
   exn:fail?)

  (contract-eval
   '(define prop:proj:bad-prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b) (box (unbox b)))])
        (struct ctc ()
          #:property
          prop:chaperone-contract
          (build-chaperone-contract-property
           #:name (λ (c) 'bad-prime-box-list/c)
           #:first-order (λ (c) (λ (v) (and (list? v) (andmap box? v))))
           #:projection
           (λ (c)
             (λ (blame)
               (λ (v)
                 (unless (and (list? v) (andmap box? v))
                   (raise-blame-error blame v
                                      "expected list of boxes, got ~v" v))
                 (map (λ (b) (wrap-box blame b)) v))))))
        (ctc))))
  
  (contract-error-test
   'contract-error-test10
   '(contract prop:proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
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
 #t)

  (contract-eval
   '(define val-first-proj:prime-list/c
      (let ([prime? (λ (n)
                      (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                        (not (= (remainder n m) 0))))])
        (make-flat-contract
         #:name 'prime-list/c
         #:first-order (λ (v) (and (list? v) (andmap prime? v)))
         #:val-first-projection
         (λ (b)
           (λ (v)
             (λ (neg-party)
               (unless (and (list? v) (andmap prime? v))
                 (raise-blame-error b v #:missing-party neg-party
                                    "expected prime list, got ~v" v))
               (map values v))))))))


  

(test/spec-passed/result
 'make-flat-contract-bad-7
 '(contract val-first-proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
 (list 2 3 5 7))

(test/pos-blame
 'make-flat-contract-bad-8
 '(contract val-first-proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

(test/spec-passed/result
 'make-flat-contract-bad-9
 '(let ([l (list 2 3 5 7)])
    (eq? l (contract val-first-proj:prime-list/c l 'pos 'neg)))
 #t)

(ctest #t contract? val-first-proj:prime-list/c)
(ctest #t flat-contract? val-first-proj:prime-list/c)

(test/spec-passed/result
 'make-flat-contract-bad-10
 '(chaperone-contract? val-first-proj:prime-list/c)
 #t)

  
  (contract-eval
   '(define late-neg-proj:prime-list/c
      (let ([prime? (λ (n)
                      (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                        (not (= (remainder n m) 0))))])
        (make-flat-contract
         #:name 'prime-list/c
         #:first-order (λ (v) (and (list? v) (andmap prime? v)))
         #:late-neg-projection
         (λ (b)
           (λ (v neg-party)
             (unless (and (list? v) (andmap prime? v))
               (raise-blame-error b v #:missing-party neg-party "expected prime list, got ~v" v))
             (map values v)))))))


  

(test/spec-passed/result
 'make-flat-contract-bad-11
 '(contract late-neg-proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
 (list 2 3 5 7))

(test/pos-blame
 'make-flat-contract-bad-12
 '(contract late-neg-proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

(test/spec-passed/result
 'make-flat-contract-bad-13
 '(let ([l (list 2 3 5 7)])
    (eq? l (contract late-neg-proj:prime-list/c l 'pos 'neg)))
 #t)

(ctest #t contract? late-neg-proj:prime-list/c)
(ctest #t flat-contract? late-neg-proj:prime-list/c)

(test/spec-passed/result
 'make-flat-contract-bad-14
 '(chaperone-contract? late-neg-proj:prime-list/c)
 #t)


(contract-eval
 '(define prop:proj:prime-list/c
    (let ([prime? (λ (n)
                    (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                      (not (= (remainder n m) 0))))])
      (struct ctc ()
        #:property
        prop:flat-contract
        (build-flat-contract-property
         #:name (λ (c) 'prime-list/c)
         #:first-order (λ (c) (λ (v) (and (list? v) (andmap prime? v))))
         #:projection
         (λ (c)
           (λ (b)
             (λ (v)
               (unless (and (list? v) (andmap prime? v))
                 (raise-blame-error b v "expected prime list, got ~v" v))
               (map values v))))))
      
      (ctc))))

(test/spec-passed/result
 'make-flat-contract-bad-15
 '(contract prop:proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
 (list 2 3 5 7))

(test/pos-blame
 'make-flat-contract-bad-16
 '(contract prop:proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

(test/spec-passed/result
 'make-flat-contract-bad-17
 '(let ([l (list 2 3 5 7)])
    (eq? l (contract prop:proj:prime-list/c l 'pos 'neg)))
 #t)

(ctest #t contract? prop:proj:prime-list/c)
(ctest #t flat-contract? prop:proj:prime-list/c)

(test/spec-passed/result
 'make-flat-contract-bad-18
 '(chaperone-contract? prop:proj:prime-list/c)
 #t)


(contract-eval
 '(define prop:val-first-proj:prime-list/c
    (let ([prime? (λ (n)
                    (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                      (not (= (remainder n m) 0))))])
      (struct ctc ()
        #:property
        prop:flat-contract
        (build-flat-contract-property
         #:name (λ (c) 'prime-list/c)
         #:first-order (λ (c) (λ (v) (and (list? v) (andmap prime? v))))
         #:val-first-projection
         (λ (c)
           (λ (b)
             (λ (v)
               (λ (neg-party)
                 (unless (and (list? v) (andmap prime? v))
                   (raise-blame-error b v
                                      #:missing-party neg-party
                                      "expected prime list, got ~v" v))
                 (map values v)))))))
      
      (ctc))))

(test/spec-passed/result
 'make-flat-contract-bad-19
 '(contract prop:val-first-proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
 (list 2 3 5 7))

(test/pos-blame
 'make-flat-contract-bad-20
 '(contract prop:val-first-proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

(test/spec-passed/result
 'make-flat-contract-bad-21
 '(let ([l (list 2 3 5 7)])
    (eq? l (contract prop:val-first-proj:prime-list/c l 'pos 'neg)))
 #t)

(ctest #t contract? prop:val-first-proj:prime-list/c)
(ctest #t flat-contract? prop:val-first-proj:prime-list/c)

(test/spec-passed/result
 'make-flat-contract-bad-22
 '(chaperone-contract? prop:val-first-proj:prime-list/c)
 #t)

  (contract-eval
 '(define prop:late-neg-proj:prime-list/c
    (let ([prime? (λ (n)
                    (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                      (not (= (remainder n m) 0))))])
      (struct ctc ()
        #:property
        prop:flat-contract
        (build-flat-contract-property
         #:name (λ (c) 'prime-list/c)
         #:first-order (λ (c) (λ (v) (and (list? v) (andmap prime? v))))
         #:late-neg-projection
         (λ (c)
           (λ (b)
             (λ (v neg-party)
               (unless (and (list? v) (andmap prime? v))
                 (raise-blame-error b v
                                    #:missing-party neg-party
                                    "expected prime list, got ~v" v))
               (map values v))))))
      
      (ctc))))

(test/spec-passed/result
 'make-flat-contract-bad-23
 '(contract prop:late-neg-proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
 (list 2 3 5 7))

(test/pos-blame
 'make-flat-contract-bad-24
 '(contract prop:late-neg-proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

(test/spec-passed/result
 'make-flat-contract-bad-25
 '(let ([l (list 2 3 5 7)])
    (eq? l (contract prop:late-neg-proj:prime-list/c l 'pos 'neg)))
 #t)

(ctest #t contract? prop:late-neg-proj:prime-list/c)
(ctest #t flat-contract? prop:late-neg-proj:prime-list/c)

(test/spec-passed/result
 'make-flat-contract-bad-26
 '(chaperone-contract? prop:late-neg-proj:prime-list/c)
 #t))

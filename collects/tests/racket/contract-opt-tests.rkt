#lang racket/base
(require racket/contract
         rackunit
         rackunit/text-ui)

(define (exn:fail:contract-violation? exn)
  (if (regexp-match #rx"contract violation" (exn-message exn)) #t #f))

(define ((blame-to whom) exn)
  (and (exn:fail:contract-violation? exn)
       (regexp-match (regexp-quote (format "blaming ~a" whom))
                     (exn-message exn))))

(define ((match-msg msg) exn)
  (regexp-match (regexp msg) (exn-message exn)))

(define-simple-check (check-pred2 func thunk)
  (let-values ([(a b) (thunk)])
    (func a b)))

(define-simple-check (check-name expected ctc)
  (let ((got (contract-name ctc)))
    (equal? expected got)))

(define opt-tests
  (test-suite
   "Tests for opt/c"
   
   (test-case
    "or 1"
    (check-pred (λ (x) (= x 1))
                (contract (opt/c (or/c number? boolean?)) 1 'pos 'neg)))
   
   (test-case
    "or 2"
    (check-pred (λ (x) (eq? x #t))
                (contract (opt/c (or/c number? boolean?)) #t 'pos 'neg)))
   
   (test-exn
    "or 3"
    (blame-to 'pos)
    (λ ()
      (contract (opt/c (or/c number? boolean?)) "string" 'pos 'neg)))
   
   (test-case
    "or 4"
    (check-pred (λ (x) (= x 1))
                ((contract (opt/c (or/c number? (-> boolean? number?)))
                           (λ (x) 1) 'pos 'neg) #t)))
   
   (test-case
    "or 5"
    (check-pred (λ (x) (= x 1))
                ((contract (opt/c (or/c (-> boolean? boolean? number?) (-> boolean? number?)))
                           (λ (x y) 1) 'pos 'neg) #t #f)))
   
   (test-case
    "lifting 1"
    (check-pred (λ (x) (= x 1))
                (let ((volatile 0))
                  (contract (opt/c (between/c (begin (set! volatile 1) 3) 5)) 4 'pos 'neg)
                  volatile)))
   
   (test-case
    "arrow 1"
    (check-pred (λ (x) (= x 1))
                ((contract (opt/c (-> boolean? number?)) (λ (x) 1) 'pos 'neg) #t)))
   
   (test-case
    "arrow 2"
    (check-pred2 (λ (x y) (and (= x 1) (= y 2)))
                 (λ ()
                   ((contract (opt/c (-> boolean? (values number? number?)))
                              (λ (x) (values 1 2)) 'pos 'neg) #t))))
   
   (test-case
    "arrow 3"
    (check-pred2 (λ (x y) (and (= x 1) (= y 2)))
                 (λ ()
                   ((contract (opt/c (-> boolean? any)) (λ (x) (values 1 2)) 'pos 'neg) #t))))
   
   (test-case
    "arrow 4"
    (check-pred (λ (x) (= x 1))
                ((contract (opt/c (-> boolean? any)) (λ (x) 1) 'pos 'neg) #t)))
   
   (test-exn
    "arrow 5"
    (blame-to 'neg)
    (λ ()
      ((contract (opt/c (-> boolean? number?)) (λ (x) #t) 'pos 'neg) 1)))
   
   (test-exn
    "arrow 6"
    (blame-to 'pos)
    (λ ()
      ((contract (opt/c (-> boolean? number?)) (λ (x) #t) 'pos 'neg) #t)))
   
   (test-case
    "flat-contract 1"
    (check-pred (λ (x) (= x 1))
                (contract (opt/c (flat-contract (λ (x) (= x 1)))) 1 'pos 'neg)))
   
   (test-exn
    "flat-contract 2"
    (match-msg "expected a flat")
    (λ ()
      (contract (opt/c (flat-contract (λ (x y) #f))) 1 'pos 'neg)))
   
   (test-case
    "cons/c 1"
    (check-pred (λ (x) (and (= (car x) 1) (= (cdr x) 2)))
                (contract (opt/c (cons/c number? (flat-contract (λ (x) (= x 2)))))
                          (cons 1 2) 'pos 'neg)))
   
   (test-case
    "cons/c 1"
    (check-pred (λ (x) (and (= (car x) 1) (= (cdr x) 2)))
                (contract (opt/c (cons/c number? (flat-contract (λ (x) (= x 2)))))
                          (cons 1 2) 'pos 'neg)))
   
   (test-case
    "cons/c 2"
    (check-pred (λ (x) (and (= (car x) 1) (= ((cdr x) 1) 2)))
                (contract (opt/c (cons/c number? (-> number? any)))
                          (cons 1 (λ (x) 2)) 'pos 'neg)))
   
   (test-case
    "between/c 1"
    (check-pred (λ (x) (= x 1))
                (contract (opt/c (between/c 1 2)) 1 'pos 'neg)))
   
   (test-case
    "between/c 2"
    (blame-to 'pos)
    (λ ()
      (contract (opt/c (between/c 1 2)) 3 'pos 'neg)))
   
   (test-exn
    "between/c 2"
    (match-msg "expected a real number as first")
    (λ ()
      (contract (opt/c (between/c 'x 'b)) 1 'pos 'neg)))
   
   (test-exn
    "between/c 3"
    (match-msg "expected a real number as second")
    (λ ()
      (contract (opt/c (between/c 1 'b)) 1 'pos 'neg)))
   
   ;;
   ;; name tests
   ;;
   
   (test-case
    "integer? name"
    (check-name 'integer? (opt/c (flat-contract integer?))))
   
   (test-case
    "boolean? name"
    (check-name 'boolean? (opt/c (flat-contract boolean?))))
   
   (test-case
    "char? name"
    (check-name 'char? (opt/c (flat-contract char?))))
   
   (test-case
    "any/c name"
    (check-name 'any/c (opt/c any/c)))
   
   (test-case
    "-> name 1"
    (check-name '(-> integer? integer?) (opt/c (-> integer? integer?))))
   
   (test-case
    "-> name 2"
    (check-name '(-> integer? any) (opt/c (-> integer? any))))
   
   (test-case
    "-> name 3"
    (check-name '(-> integer? (values boolean? char?)) (opt/c (-> integer? (values boolean? char?)))))
   
   (test-case
    "or/c name 1"
    (check-name '(or/c) (opt/c (or/c))))
   
   (test-case
    "or/c name 2"
    (check-name '(or/c integer? gt0?) (opt/c (or/c integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))))
   
   (test-case
    "or/c name 3"
    (check-name '(or/c integer? boolean?)
                (opt/c (or/c (flat-contract integer?)
                             (flat-contract boolean?)))))
   
   (test-case
    "or/c name 4"
    (check-name '(or/c integer? boolean?)
                (opt/c (or/c integer? boolean?))))
   
   (test-case
    "or/c name 5"
    (check-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
                (opt/c (or/c (-> (>=/c 5) (>=/c 5)) boolean?))))
   
   (test-case
    "or/c name 6"
    (check-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
                (opt/c (or/c boolean? (-> (>=/c 5) (>=/c 5))))))
   
   (test-case
    "or/c name 7"
    (check-name '(or/c (-> (>=/c 5) (>=/c 5))
                       (-> (<=/c 5) (<=/c 5) (<=/c 5)))
                (opt/c (or/c (-> (>=/c 5) (>=/c 5))
                             (-> (<=/c 5) (<=/c 5) (<=/c 5))))))
   
   (test-case
    "or/c name 8"
    (check-name '(or/c boolean?
                       (-> (>=/c 5) (>=/c 5))
                       (-> (<=/c 5) (<=/c 5) (<=/c 5)))
                (opt/c (or/c boolean?
                             (-> (>=/c 5) (>=/c 5))
                             (-> (<=/c 5) (<=/c 5) (<=/c 5))))))
   
   (test-case
    "=/c name 1"
    (check-name '(=/c 5) (opt/c (=/c 5))))
   
   (test-case
    ">=/c name 1"
    (check-name '(>=/c 5) (opt/c (>=/c 5))))
   
   (test-case
    "<=/c name 1"
    (check-name '(<=/c 5) (opt/c (<=/c 5))))
   
   (test-case
    "</c name 1"
    (check-name '(</c 5) (opt/c (</c 5))))
   
   (test-case
    ">/c name 1"
    (check-name '(>/c 5) (opt/c (>/c 5))))
   
   (test-case
    "between/c name 1"
    (check-name '(between/c 5 6) (opt/c (between/c 5 6))))
   
   (test-case
    "cons/c name 1"
    (check-name '(cons/c boolean? integer?) 
                (opt/c (cons/c boolean? (flat-contract integer?)))))
   
   (test-case
    "cons/c name 2"
    (check-name '(cons/c boolean? integer?) 
                (opt/c (cons/c boolean? (flat-contract integer?)))))
   
   (test-case
    "cons/c name 1"
    (check-name '(cons/c boolean? integer?)
                (opt/c (cons/c boolean? (flat-contract integer?)))))
   
   (test-case
    "cons/c name 2"
    (check-name '(cons/c boolean? integer?) 
                (opt/c (cons/c boolean? (flat-contract integer?)))))
   
   (test-case
    "cons/c name 3"
    (check-name '(cons/c boolean? integer?) 
                (opt/c (cons/c boolean? (flat-contract integer?)))))
   
   (test-case
    "cons/c name 4"
    (check-name '(cons/c (-> boolean? boolean?) integer?)
                (opt/c (cons/c (-> boolean? boolean?) integer?))))))

(unless (zero? (run-tests opt-tests))
  (error 'contract-opt-tests.rkt "tests failed"))

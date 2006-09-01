(module contract-opt-tests mzscheme
  (require (lib "private/contract-opt.scm")
           (lib "private/contract-opters.scm")
           (lib "contract.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 1)))
    
  (define (exn:fail:contract-violation? exn)
    (if (regexp-match #rx"broke" (exn-message exn)) #t #f))
  
  (define ((blame-to whom) exn)
    (and (exn:fail:contract-violation? exn)
         (regexp-match (format "~a broke" whom)
                       (exn-message exn))))
  
  (define ((match-msg msg) exn)
    (regexp-match (regexp msg) (exn-message exn)))
  
  (define-check (check-pred2 func thunk)
    (let-values ([(a b) (thunk)])
      (func a b)))
  
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
      (match-msg "expected procedure")
      (λ ()
        (contract (opt/c (flat-contract (λ (x y) #f))) 1 'pos 'neg)))
     
     (test-case
      "cons/c 1"
      (check-pred (λ (x) (and (= (car x) 1) (= (cdr x) 2)))
                  (contract (opt/c (cons/c number? (flat-contract (λ (x) (= x 2)))))
                            (cons 1 2) 'pos 'neg)))
     
     (test-exn
      "cons/c 2"
      (match-msg "expected two flat")
      (λ ()
        (contract (opt/c (cons/c number? (-> number? any))) (cons 1 2) 'pos 'neg)))
     
     (test-case
      "cons-immutable/c 1"
      (check-pred (λ (x) (and (= (car x) 1) (= (cdr x) 2)))
                  (contract (opt/c (cons-immutable/c number? (flat-contract (λ (x) (= x 2)))))
                            (cons-immutable 1 2) 'pos 'neg)))
     
     (test-case
      "cons-immutable/c 2"
      (check-pred (λ (x) (and (= (car x) 1) (= ((cdr x) 1) 2)))
                  (contract (opt/c (cons-immutable/c number? (-> number? any)))
                            (cons-immutable 1 (λ (x) 2)) 'pos 'neg)))
     
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
      (match-msg "expected two numbers")
      (λ ()
        (contract (opt/c (between/c 'x 'b)) 1 'pos 'neg)))
      
     ))
  
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 1)))
  (test/text-ui opt-tests))

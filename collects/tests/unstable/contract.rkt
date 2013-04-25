#lang racket

(require rackunit rackunit/text-ui unstable/contract "helpers.rkt")

(run-tests
 (test-suite "contract.rkt"
   (test-suite "Flat Contracts"
     (test-suite "truth/c"
       (test-ok (with/c truth/c #t))
       (test-ok (with/c truth/c #f))
       (test-ok (with/c truth/c '(x)))))
   (test-suite "Higher Order Contracts"
     (test-suite "predicate/c"
       (test-ok ([with/c predicate/c integer?] 1))
       (test-ok ([with/c predicate/c integer?] 1/2))
       (test-bad ([with/c predicate/c values] 'x))))
   (test-suite "Collection Contracts"
     (test-suite "sequence/c"
       (test-ok
        (for ([x (with/c (sequence/c integer?) (list 1 2 3 4))])
          (void)))
       (test-bad
        (for ([x (with/c (sequence/c integer?) (list 1 2 'c 4))])
          (void)))
       (test-bad
        (for ([x (with/c (sequence/c integer? symbol?) (list 1 2 3 4))])
          (void)))
       (test-ok
         (for ([(x y) (with/c (sequence/c integer? symbol?)
                              (in-dict (list (cons 1 'one) (cons 2 'two))))])
           (void)))
       (test-bad
         (for ([(x y) (with/c (sequence/c integer? symbol?)
                              (in-dict (list (cons 1 'one) (cons 2 "two"))))])
           (void)))
       (test-bad
         (for ([(x y) (with/c (sequence/c integer?)
                              (in-dict (list (cons 1 'one) (cons 2 'two))))])
           (void)))
       (let ([s (sequence->stream (contract (sequence/c #:min-count 2 any/c) "x" 'pos 'neg))])
         (check-equal? (stream-first s) #\x)
         (check-exn (λ (x) (and (exn:fail? x)
                                (regexp-match #rx"blaming: pos" (exn-message x))))
                    (lambda () (stream-first (stream-rest s)))))))
   (test-suite "Data structure contracts"
     (test-suite "maybe/c"
       (test-true "flat" (flat-contract? (maybe/c number?)))
       (test-true "chaperone" (chaperone-contract? (maybe/c (box/c number?))))
       (test-true "impersonator" (impersonator-contract? (maybe/c (object/c))))
       (test-ok (with/c (maybe/c number?) 0))
       (test-ok (with/c (maybe/c number?) #f))
       (test-ok (with/c (maybe/c (-> number? number?)) #f))
       (test-ok (with/c (maybe/c (-> number? number?)) +))
       (test-ok (with/c (maybe/c (class/c (field [x number?])))
                        (class object% (super-new) (field [x 0]))))
       (test-ok (with/c (maybe/c (class/c (field [x number?]))) #f))
       (test-ok (with/c (class/c (field [c (maybe/c string?)]))
                        (class object% (super-new) (field [c #f]))))
       (test-bad (with/c (maybe/c number?) "string"))
       (test-bad (with/c (maybe/c (-> number? number?))
                         (lambda (x y) x)))
       (test-bad
         ([with/c (maybe/c (-> number? number?))
                  (lambda (x) (void))]
          0))
       (test-bad (with/c (maybe/c (class/c (field [x number?])))
                        (class object% (super-new))))
       (test-bad (with/c (maybe/c (class/c (field [x number?]))) 5))
       (test-bad
         (get-field c (with/c (class/c (field [c (maybe/c string?)]))
                              (class object%
                                (super-new)
                                (field [c 70])))))))))

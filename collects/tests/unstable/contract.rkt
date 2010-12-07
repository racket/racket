#lang racket

(require rackunit rackunit/text-ui unstable/contract "helpers.rkt")

(run-tests
 (test-suite "contract.ss"
   (test-suite "Flat Contracts"
     (test-suite "nat/c"
       (test-ok (with/c nat/c 1))
       (test-ok (with/c nat/c 0))
       (test-bad (with/c nat/c -1))
       (test-bad (with/c nat/c 'non-numeric)))
     (test-suite "pos/c"
       (test-ok (with/c pos/c 1))
       (test-bad (with/c pos/c 0))
       (test-bad (with/c pos/c -1))
       (test-bad (with/c pos/c 'non-numeric)))
     (test-suite "truth/c"
       (test-ok (with/c truth/c #t))
       (test-ok (with/c truth/c #f))
       (test-ok (with/c truth/c '(x)))))

   (test-suite "Syntax Object Contracts"

     (test-suite "syntax-datum/c"
       (test-ok (with/c (syntax-datum/c (listof (listof natural-number/c)))
                        #'((0 1 2) () (3 4) (5))))
       (test-bad (with/c (syntax-datum/c (listof (listof natural-number/c)))
                         #'((x y z))))
       (test-bad (with/c (syntax-datum/c string?) "xyz")))

     (test-suite "syntax-listof/c"
       (test-ok (with/c (syntax-listof/c identifier?) #'(a b c)))
       (test-bad (with/c (syntax-listof/c identifier?) #'(1 2 3)))
       (test-bad (with/c (syntax-listof/c identifier?) #'(a b . c)))
       (test-bad (with/c (syntax-listof/c identifier?) (list #'a #'b #'c))))

     (test-suite "syntax-list/c"
       (test-ok (with/c (syntax-list/c identifier? (syntax/c string?))
                        #'(a "b")))
       (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                         #'(a "b" #:c)))
       (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                         #'(a b)))
       (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                         #'(a "b" . c)))
       (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                         '(#'a #'"b")))))
   (test-suite "Higher Order Contracts"
     (test-suite "thunk/c"
       (test-ok ([with/c thunk/c gensym]))
       (test-bad ([with/c thunk/c gensym] 'x))
       (test-bad ([with/c thunk/c cons])))
     (test-suite "unary/c"
       (test-ok ([with/c unary/c list] 'x))
       (test-bad ([with/c unary/c list] 'x 'y))
       (test-bad ([with/c unary/c cons] 1)))
     (test-suite "binary/c"
       (test-ok ([with/c binary/c +] 1 2))
       (test-bad ([with/c binary/c +] 1 2 3))
       (test-bad ([with/c binary/c symbol->string] 'x 'y)))
     (test-suite "predicate/c"
       (test-ok ([with/c predicate/c integer?] 1))
       (test-ok ([with/c predicate/c integer?] 1/2))
       (test-bad ([with/c predicate/c values] 'x)))
     (test-suite "predicate-like/c"
       (test-ok ([with/c predicate-like/c integer?] 1))
       (test-ok ([with/c predicate-like/c integer?] 1/2))
       (test-ok ([with/c predicate-like/c values] 'x)))
     (test-suite "comparison/c"
       (test-ok ([with/c comparison/c equal?] 1 1))
       (test-ok ([with/c comparison/c equal?] 1 2))
       (test-bad ([with/c comparison/c list] 1 2)))
     (test-suite "comparison-like/c"
       (test-ok ([with/c comparison-like/c equal?] 1 1))
       (test-ok ([with/c comparison-like/c equal?] 1 2))
       (test-ok ([with/c comparison-like/c list] 1 2))))
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
          (void))))
     (test-suite "dict/c"
       (test-ok
        (for ([(x y)
               (in-dict
                (with/c (dict/c integer? symbol?)
                        #hash([1 . a] [2 . b])))])
          (void)))
       (test-bad
        (for ([(x y)
               (in-dict
                (with/c (dict/c integer? symbol?)
                        #hash([1 . a] [three . b])))])
          (void)))
       (test-bad
        (for ([(x y)
               (in-dict
                (with/c (dict/c integer? symbol?)
                        #hash([1 . a] [2 . "b"])))])
          (void)))))))

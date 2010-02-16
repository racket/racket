#lang scheme/base
(require schemeunit
         plai/random-mutator)

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/mutator\n'x"))
 (list 'x))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\ntrue"))
 (list #t))
(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\n1"))
 (list 1))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\n'(x y 1)"))
 (list 1 'x 'y))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\n(error 'x \"hm\")(test 'y 'z) (test/exn 'w 'q)"))
 (list))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang scheme/base\n(error 'x \"hm\")(test 'y 'z) (test/exn 'w 'q)"))
 (list 'q 'w 'x 'y 'z))

(check-equal?
 (find-heap-values
  (open-input-string
   "((error 'x \"hm\")(test 'y 'z) (test/exn 'w 'q))"))
 (list 'q 'w 'x 'y 'z))

(check-equal?
 (find-heap-values
  (open-input-string
   "(true false null)"))
 (list #f #t'()))

(check-equal?
 (find-heap-values
  (open-input-string
   "empty"))
 (list '()))

(check-equal?
 (find-heap-values
  (open-input-string
   "`x"))
 (list 'x))

(check-equal?
 (find-heap-values
  (open-input-string
   "`(())"))
 (list '()))

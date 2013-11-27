#lang racket/base

(require rackunit
         "file.rkt")

(check-equal? (my-+ 1 1) 2)
(check-equal? (my-* 1 2) 2)

(test-begin
 (let ((lst (list 2 4 6 9)))
   (check = (length lst) 4)
   (for-each
    (lambda (elt)
      (check-pred even? elt))
    lst)))

(test-case
  "List has length 4 and all elements even"
  (let ((lst (list 2 4 6 9)))
    (check = (length lst) 4)
    (for-each
     (lambda (elt)
       (check-pred even? elt))
     lst)))

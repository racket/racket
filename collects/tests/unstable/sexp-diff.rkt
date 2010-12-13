#lang scheme
(require unstable/sexp-diff)
(require tests/eli-tester)
(test

 (sexp-diff 1 2) => '(#:old 1 #:new 2)

 (sexp-diff '(1 2 3) '(4 2 3)) => '((#:new 4 #:old 1 2 3))

 (sexp-diff '(0 (1 2 3)) '(0 (4 2 3))) => '((0 (#:new 4 #:old 1 2 3)))

 (sexp-diff '(defun f (x) (+ (* x 2) 1)) 
            '(defun f (x) (- (* x 2) 3 1)))
 => '((defun f (x) (#:new - #:old + (* x 2) #:new 3 1)))
 
 (sexp-diff '(defun f (x) (+ (* x 2) 4 1))
            '(defun f (x) (- (* x 2) 5 3 1)))
 => '((defun f (x) (#:new - #:old + (* x 2) #:new 5 #:new 3 #:old 4 1)))

 (sexp-diff '(defun f (x) (+ (* x 2) 4 4 1))
            '(defun f (x) (- (* x 2) 5 5 3 1)))
 => '((defun f (x) (#:new - #:old + (* x 2) #:new 5 #:new 5 #:new 3 #:old 4 #:old 4 1)))
 
 (sexp-diff (list 1 (list 2) 3) (list 1 (list 4) 3))
 => '((1 (#:new 4 #:old 2) 3))
 )

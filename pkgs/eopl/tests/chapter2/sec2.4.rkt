#lang eopl
(require eopl/tests/private/utils)

(define identifier? symbol?)

(define-datatype lc-exp lc-exp? 
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; occurs-free? : Sym * Lcexp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and
                   (not (eqv? search-var bound-var))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

;; test items
(check-equal? (occurs-free? 'x (var-exp 'x)) #t)

(check-equal? (occurs-free? 'x (var-exp 'y)) #f)

(check-equal? (occurs-free? 'x (lambda-exp 'x
                                           (app-exp (var-exp 'x) (var-exp 'y))))
              #f)

(check-equal?
 (occurs-free? 'x (lambda-exp 'y
                              (app-exp (var-exp 'x) (var-exp 'y))))
 #t)

(check-equal?
 (occurs-free? 'x 
               (app-exp
                (lambda-exp 'x (var-exp 'x))
                (app-exp (var-exp 'x) (var-exp 'y))))
 #t)

(check-equal? 
 (occurs-free? 'x
               (lambda-exp 'y
                           (lambda-exp 'z
                                       (app-exp (var-exp 'x)
                                                (app-exp (var-exp 'y) (var-exp 'z))))))
 #t)

(define-datatype s-list s-list? 
  (empty-s-list)
  (non-empty-s-list 
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp? 
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))

;; page 48: alternate definition 
(define-datatype s-list-alt s-list-alt? 
  (an-s-list
   (sexps (list-of s-exp?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

;; For exercises 2.24-2.25
(define-datatype bintree bintree? 
  (leaf-node 
   (num integer?))
  (interior-node
   (key symbol?) 
   (left bintree?)
   (right bintree?)))

;;   > (bintree-to-list
;;       (interior-node
;;         'a
;;         (leaf-node 3)
;;         (leaf-node 4)))
;;   (interior-node a (leaf-node 3) (leaf-node 4)))

;;   > (define tree-1
;;       (interior-node 'foo (leaf-node 2) (leaf-node 3)))
;;   > (define tree-2
;;       (interior-node 'bar (leaf-node -1) tree-1))
;;   > (define tree-3
;;       (interior-node 'baz tree-2 (leaf-node 1)))
;;   > (max-interior tree-2)
;;   foo
;;   > (max-interior tree-3)
;;   baz

;(eopl:printf "unit tests completed successfully.~%")

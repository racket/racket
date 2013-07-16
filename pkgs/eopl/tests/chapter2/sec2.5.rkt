#lang eopl
(require eopl/tests/private/utils)

;; data definitions
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

;; parse-expression : Schemeval -> Lcexp
;; page 53
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error "invalid concrete syntax ~s" datum)))

;; unit tests
(check-equal?
 (parse-expression 'x)
 (var-exp 'x))

(check-equal?
 (parse-expression 'y)
 (var-exp 'y))

(check-equal?
 (parse-expression '(lambda (x) (x y)))
 (lambda-exp 'x
             (app-exp (var-exp 'x) (var-exp 'y))))

(check-equal?
 (parse-expression '(lambda (y) (x y)))
 (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))

(check-equal?
 (parse-expression '((lambda (x) x) (x y))) 
 (app-exp
  (lambda-exp 'x (var-exp 'x))
  (app-exp (var-exp 'x) (var-exp 'y))))

(check-equal? 
 (parse-expression '(lambda (y) (lambda (z) (x (y z)))))
 (lambda-exp 'y
             (lambda-exp 'z
                         (app-exp (var-exp 'x)
                                  (app-exp (var-exp 'y) (var-exp 'z))))))

;(report-unit-tests-completed 'parse-expression)

;; unparse-lc-exp : Lcexp -> Schemeval
;; page 53
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body) 
                  (list 'lambda (list bound-var)
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list 
                (unparse-lc-exp rator) (unparse-lc-exp rand))))))


;; unit tests
(check-equal?
 (unparse-lc-exp (var-exp 'x))
 'x)

(check-equal?
 (unparse-lc-exp (var-exp 'y))
 'y)

(check-equal?
 (unparse-lc-exp
  (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
 '(lambda (x) (x y)))

(check-equal?
 (unparse-lc-exp
  (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))
 '(lambda (y) (x y)))

(check-equal?
 (unparse-lc-exp 
  (app-exp
   (lambda-exp 'x (var-exp 'x))
   (app-exp (var-exp 'x) (var-exp 'y))))
 '((lambda (x) x) (x y)))


(check-equal? 
 (unparse-lc-exp 
  (lambda-exp 'y
              (lambda-exp 'z
                          (app-exp (var-exp 'x)
                                   (app-exp (var-exp 'y) (var-exp 'z))))))
 '(lambda (y) (lambda (z) (x (y z)))))

;(report-unit-tests-completed 'unparse-lc-exp)


;; Exercise 2.27
;;   ((lambda (a) (a b)) c)

;;   (lambda (x)
;;     (lambda (y)
;;       ((lambda (x)
;;          (x y))
;;        x)))

;; Exercise 2.31
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

;;   (diff-exp
;;     (diff-exp
;;       (const-exp 3)
;;       (const-exp 2))
;;     (diff-exp
;;       (const-exp 4)
;;       (diff-exp
;;         (const-exp 12)
;;         (const-exp 7))))

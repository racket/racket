#lang scheme
(require redex)

(reduction-steps-cutoff 10)

(define-language lang
  (e (e e)
     (abort e)
     x
     v)
  (x (variable-except lambda call/cc abort))
  (c (v c)
     (c e)
     hole)
  (v call/cc
     number
     (lambda (x t) e))
  (t num
     (t -> t)))

(define reductions
  (reduction-relation
   lang
   (--> (in-hole c_1 (call/cc v_arg))
        ,(term-let ([v (variable-not-in (term c_1) 'x)])
           (term
            (in-hole c_1 (v_arg (lambda (v) (abort (in-hole c_1 v)))))))
        call/cc)
   (--> (in-hole c (abort e_1))
        e_1
        abort)
   
   ;; this rules calls subst with the wrong arguments, which is caught by the example below.
   (--> (in-hole c_1 ((lambda (x_format t_1) e_body) v_actual))
        (in-hole c_1 (subst x_format v_actual e_body))
        βv)))

(define (type-check term)
  (let/ec k
    (let loop ([term term]
               [env '()])
      (match term
        [(? symbol?) 
         (let ([l (assoc term env)])
           (if l
               (cdr l)
               (k #f)))]
        [(? number?) 'num]
        [`(lambda (,x ,t) ,b)
         (let ([body (loop b (cons (cons x t) env))])
           `(,t -> ,body))]
        [`(,e1 ,e2)
         (let ([t1 (loop e1 env)]
               [t2 (loop e2 env)])
           (match t1
             [`(,td -> ,tr)
              (if (equal? td t2)
                  tr
                  (k #f))]
             [else (k #f)]))]))))

(define (pred term1)
  (let ([t1 (type-check term1)])
    (lambda (term2)
      (and t1
           (equal? (type-check term2) t1)))))

(define (show term)
  (traces reductions term #:pred (pred term)))

(show '((lambda (x (num -> num)) 1) ((lambda (x (num -> num)) x) (lambda (x num) x))))
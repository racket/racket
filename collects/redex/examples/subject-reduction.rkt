#lang racket
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
        (in-hole c_1 (subst (x_format e_body v_actual)))
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

(define-language subst-lang
  (x variable))

(define-metafunction subst-lang
  [(subst-n ((x_1 any_1) (x_2 any_2) ... any_3))
   (subst (x_1 any_1 (subst-n ((x_2 any_2) ... any_3))))]
  [(subst-n (any_3)) any_3])

(define-metafunction subst-lang
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst (x_1 any_1 (λ (x_1 t) any_2)))
   (λ (x_1 t) any_2)]
  ;; 2. general purpose capture avoiding case
  [(subst (x_1 any_1 (λ (x_2 t) any_2)))
   ,(term-let ([x_new
                (variable-not-in (term (x_1 any_1 any_2)) 
                                 (term x_2))])
              (term 
               (λ (x_new t) 
                 (subst (x_1 any_1 (subst-vars ((x_2 x_new) any_2)))))))]
  ;; 3. replace x_1 with e_1
  [(subst (x_1 any_1 x_1)) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst (x_1 any_1 x_2)) x_2]
  ;; the last two cases cover all other expression forms
  [(subst (x_1 any_1 (any_2 ...)))
   ((subst (x_1 any_1 any_2)) ...)]
  [(subst (x_1 any_1 any_2)) any_2])

(define-metafunction subst-lang
  [(subst-vars ((x_1 any_1) x_1)) any_1]
  [(subst-vars ((x_1 any_1) (any_2 ...))) ((subst-vars ((x_1 any_1) any_2)) ...)]
  [(subst-vars ((x_1 any_1) any_2)) any_2]
  [(subst-vars ((x_1 any_1) (x_2 any_2) ... any_3)) 
   (subst-vars ((x_1 any_1) (subst-vars ((x_2 any_2) ... any_3))))]
  [(subst-vars (any)) any])

(define (show term)
  (traces reductions term #:pred (pred term)))

(show '((lambda (x (num -> num)) 1) ((lambda (x (num -> num)) x) (lambda (x num) x))))

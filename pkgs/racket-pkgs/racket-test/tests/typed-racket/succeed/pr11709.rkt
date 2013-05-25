#lang typed/racket

(define stop-value (gensym))

(: an-alist : (All (A B) (Listof (Pair A B)) -> (Rec R (-> (values (-> (values A B))  (-> R))))))
(define (an-alist lst)
  (lambda: ()
    (if (null? lst) (raise stop-value)
        (let ([first (car lst)]
              [rest  (cdr lst)])
          (values
           (lambda: () (values (car first) (cdr first)))
           (lambda: () (an-alist rest)))))))

(define alist (an-alist (list (cons 1 2) (cons 3 4) (cons 5 6))))

(call-with-values alist values) ; this works

(call-with-values alist (λ (e a) a)) 

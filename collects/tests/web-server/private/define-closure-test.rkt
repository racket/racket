#lang racket/base
(require rackunit
         mzlib/serialize
         mzlib/match
         web-server/private/define-closure)
(provide define-closure-tests)

(define-closure id (x) () x)

(define-closure add-y (x) (y) (+ x y))
(define-closure even-p (n) (odd-p) (or (zero? n)
                                       (odd-p (sub1 n))))
(define-closure odd-p (n) (even-p) (or (= 1 n)
                                       (even-p (sub1 n))))

(define even-p (make-even-p (lambda () odd-p)))
(define odd-p (make-odd-p (lambda () even-p)))

;; an interpreter

(define-closure the-empty-env (var) ()
  (error "unbound symbol: " var))

(define-closure extended-env (v) (env var val)
  (if (eqv? v var) val
      (env v)))

(define-serializable-struct proc (var body env))

(define-closure clsr:evaluate (expr env) (evaluate eval-app)
  (match expr
    [('lambda (var) body) (make-proc var body env)]
    [(expr1 expr2)
     (let ([val1 (evaluate expr1 env)]
           [val2 (evaluate expr2 env)])
       (eval-app val1 val2))]
    [(? number? n) n]
    [var (env var)]))

(define-closure clsr:eval-app (val1 val2) (evaluate)
  (cond
    [(proc? val1)
     (evaluate (proc-body val1)
               (make-extended-env
                (lambda () (values (proc-env val1) (proc-var val1) val2))))]
    [else 
     (error "stuck term: " (list val1 val2))]))

(define evaluate (make-clsr:evaluate (lambda () (values evaluate eval-app))))
(define eval-app (make-clsr:eval-app (lambda () evaluate)))

(define define-closure-tests
  (test-suite
   "Define Closure"
   
   (test-case
    "serialize id procedure"
    (check = 7 ((deserialize (serialize (make-id (lambda () (values))))) 7)))
   
   (test-case
    "id procedure"
    (check = 7 ((make-id (lambda () (values))) 7)))
   
   (test-case
    "add-y procedure"
    (check = 2 ((make-add-y (lambda () 1)) 1)))
   
   (test-case
    "serialize the add-y procedure"
    (check = 2 ((deserialize (serialize (make-add-y (lambda () 1)))) 1)))
   
   (test-case
    "even-p procedure"
    (check-true (even-p 8)))
   
   (test-case
    "serialize the even-p procedure"
    (check-true ((deserialize (serialize even-p)) 64)))
   
   (test-case
    "simple interpreter case"
    (check = 3 (evaluate 3 (make-the-empty-env (lambda () (values))))))
   
   (test-case
    "serialize simple interpreter case"
    (check = 3 ((deserialize (serialize evaluate))
                3
                (deserialize (serialize (make-the-empty-env (lambda () (values))))))))
   
   (test-case
    "apply identity"
    (check = 3 (evaluate '((lambda (x) x) 3) (make-the-empty-env (lambda () (values))))))
   
   (test-case
    "serialize environments"
    (let* ([e0 (make-the-empty-env (lambda () (values)))]
           [e1 (make-extended-env (lambda () (values e0 'x 1)))]
           [e2 (make-extended-env (lambda () (values e1 'y 2)))]
           [e3 (make-extended-env (lambda () (values e2 'z 3)))]
           [e4 (make-extended-env (lambda () (values e3 'x 4)))]
           [e5 (make-extended-env (lambda () (values e4 'y 5)))]
           [e6 (make-extended-env (lambda () (values e5 'z 6)))]
           [env3 (deserialize (serialize e3))]
           [env5 (deserialize (serialize e5))]
           [env6 (deserialize (serialize e6))])
      (check = 1 (env3 'x))
      (check = 2 (env3 'y))
      (check = 3 (env3 'z))
      (check = 4 (env5 'x))
      (check = 5 (env5 'y))
      (check = 3 (env5 'z))
      (check = 4 (env6 'x))
      (check = 5 (env6 'y))
      (check = 6 (env6 'z))))))

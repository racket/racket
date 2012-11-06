;; The optimizing compiler from
;;  "Using closures for code generation", Feeley & Lapalme,
;;  Computer Languages, 12(1):47-66, 1987.

(define (compile expr)
  ((gen expr '() #f)))

(define (gen expr env term)
  (cond ((symbol? expr)
         (ref (variable expr env) term))
        ((not (pair? expr))
         (cst expr term))
        ((eq? (car expr) 'quote)
         (cst (cadr expr) term))
        ((eq? (car expr) 'set!)
         (set (variable (cadr expr) env) (gen (caddr expr) env #f) term))
        ((eq? (car expr) 'if)
         (gen-tst (gen (cadr expr) env #f)
                   (gen (caddr expr) env term)
                   (gen (cadddr expr) env term)))
        ((eq? (car expr) 'lambda)
         (let ((p (cadr expr)))
            (prc p (gen (caddr expr) (allocate p env) #t) term)))
        (else
         (let ((args (map (lambda (x) (gen x env #f)) (cdr expr))))
            (let ((var (and (symbol? (car expr)) (variable (car expr) env))))
              (if (global? var)
                (app (cons var args) #t term)
                (app (cons (gen (car expr) env #f) args) #f term)))))))

(define (allocate parms env)
  (cond ((null? parms)         env)
        ((symbol? parms) (cons parms env))
        (else (cons               (car parms) (allocate (cdr parms) env)))))

(define (variable symb env)
  (let ((x (memq symb env)))
    (if x
      (- (length env) (length x))
      (begin
        (if (not (assq symb *glo-env*)) (define-global symb 'undefined))
        (assq symb *glo-env*)))))

(define (global? var)
  (pair? var))

(define (cst val term)
  (cond ((eqv? val 1)         ((if term gen-1*  gen-1 )       ))
        ((eqv? val 2)         ((if term gen-2*  gen-2 )       ))
        ((eqv? val '()) ((if term gen-null* gen-null)            ))
        (else ((if              term gen-cst*      gen-cst ) val))))

(define (ref var term)
  (cond ((global? var) ((if term gen-ref-glo*        gen-ref-glo )  var))
        ((= var 0)               ((if term gen-ref-loc-1* gen-ref-loc-1)            ))
        ((= var 1)               ((if term gen-ref-loc-2* gen-ref-loc-2)            ))
        ((= var 2)               ((if term gen-ref-loc-3* gen-ref-loc-3)            ))
        (else ((if            term gen-ref*          gen-ref )       var))))

(define (set var val term)
  (cond ((global? var) ((if term gen-set-glo*        gen-set-glo )  var val))
        ((= var 0)               ((if term gen-set-loc-1* gen-set-loc-1)               val))
        ((= var 1)               ((if term gen-set-loc-2* gen-set-loc-2)               val))
        ((= var 2)               ((if term gen-set-loc-3* gen-set-loc-3)               val))
        (else ((if            term gen-set*          gen-set )       var val))))

(define (prc parms body term)
  ((cond ((null? parms)                              (if term gen-prc0* gen-prc0 ))
         ((symbol? parms)                        (if term gen-prc1/rest* gen-prc1/rest))
         ((null? (cdr parms))            (if term gen-prc1*   gen-prc1 ))
         ((symbol? (cdr parms))      (if term gen-prc2/rest* gen-prc2/rest))
         ((null? (cddr parms))         (if term gen-prc2*     gen-prc2 ))
         ((symbol? (cddr parms)) (if term gen-prc3/rest* gen-prc3/rest))
         ((null? (cdddr parms))      (if term gen-prc3*       gen-prc3 ))
         (else (error                       "too many parameters")))
   body))

(define (app vals glo term)
  (apply (case (length vals)
            ((1) (if glo (if term gen-ap0-glo* gen-ap0-glo)
                          (if term gen-ap0*               gen-ap0)))
            ((2) (if glo (if term gen-ap1-glo* gen-ap1-glo)
                          (if term gen-ap1*               gen-ap1)))
            ((3) (if glo (if term gen-ap2-glo* gen-ap2-glo)
                          (if term gen-ap2*               gen-ap2)))
            ((4) (if glo (if term gen-ap3-glo* gen-ap3-glo)
                          (if term gen-ap3*               gen-ap3)))
            (else (error "too many arguments")))
         vals))

;- -- code generation procedures for non-terminal evaluations ---

;- -- code generation for constants ---

(define (gen-cst a) ; any constant
  (lambda () a))

(define (gen-1)       ;f or constant 1
  (lambda () 1))

(define (gen-2)       ;f or constant 2
  (lambda () 2))

(define (gen-null)  ;f or constant ()
  (lambda () '()))

;- -- code generation for variable references ---

(define (gen-ref-glo a) ; for a global variable
  (lambda () (cdr a)))

(define (gen-ref a)     ;f or any non-global variable
  (lambda () (do ((i 0 (+ i 1)) (env (cdr *env*) (cdr env)))
                 ((= i a) (car env)))))

(define (gen-ref-loc-1) ; for first local variable
  (lambda () (cadr *env*)))

(define (gen-ref-loc-2) ; for second local variable
  (lambda () (caddr *env*)))

(define (gen-ref-loc-3) ; for third local variable
  (lambda () (cadddr *env*)))

;- -- code generation for assignments ---

(define (gen-set-glo a b) ; for a global variable
  (lambda () (set-cdr! a (b))))

(define (gen-set a b)     ;f or any non-global variable
  (lambda () (do ((i 0 (+ i 1)) (env (cdr *env*) (cdr env)))
                 ((= i a) (set-car! env (b))))))

(define (gen-set-loc-1 a) ; for first local variable
  (lambda () (set-car! (cdr *env*) (a))))

(define (gen-set-loc-2 a) ; for second local variable
  (lambda () (set-car! (cddr *env*) (a))))

(define (gen-set-loc-3 a) ; for third local variable
  (lambda () (set-car! (cdddr *env*) (a))))

;- -- code generation for 'if' special form ---

(define (gen-tst a b c)
  (lambda () (if (a) (b) (c))))

;- -- code generation for procedure application ---

(define (gen-ap0 a)     ;a ny application (of 0 to 3 arguments)
  (lambda () ((a))))

(define (gen-ap1 a b)
  (lambda () ((a) (b))))

(define (gen-ap2 a b c)
  (lambda () ((a) (b) (c))))

(define (gen-ap3 a b c d)
  (lambda () ((a) (b) (c) (d))))

(define (gen-ap0-glo a) ; application with global variable as operator
  (lambda () ((cdr a))))

(define (gen-ap1-glo a b)
  (lambda () ((cdr a) (b))))

(define (gen-ap2-glo a b c)
  (lambda () ((cdr a) (b) (c))))

(define (gen-ap3-glo a b c d)
  (lambda () ((cdr a) (b) (c) (d))))

;- -- code generation for 'lambda' special form ---


(define (gen-prc0 a)      ;nor  est parameter (0 to 3 parameters)
  (lambda () (let ((def (cdr *env*)))
               (lambda ()
                 (set! *env* (cons *env* def))
                 (a)))))

(define (gen-prc1 a)
  (lambda () (let ((def (cdr *env*)))
               (lambda (x)
                 (set! *env* (cons *env* (cons x def)))
                 (a)))))

(define (gen-prc2 a)
  (lambda () (let ((def (cdr *env*)))
               (lambda (x y)
                 (set! *env* (cons *env* (cons x (cons y def))))
                 (a)))))

(define (gen-prc3 a)
  (lambda () (let ((def (cdr *env*)))
               (lambda (x y z)
                 (set! *env* (cons *env* (cons x (cons y (cons z def)))))
                 (a)))))

(define (gen-prc1/rest a) ; when a rest parameter is present
  (lambda () (let ((def (cdr *env*)))
               (lambda x
                 (set! *env* (cons *env* (cons x def)))
                 (a)))))

(define (gen-prc2/rest a)
  (lambda () (let ((def (cdr *env*)))
               (lambda (x . y)
                 (set! *env* (cons *env* (cons x (cons y def))))
                 (a)))))

(define (gen-prc3/rest a)
  (lambda () (let ((def (cdr *env*)))
               (lambda (x y . z)
                 (set! *env* (cons *env* (cons x (cons y (cons z def)))))
                 (a)))))

;- -- code generation procedures for terminal evaluations ---

;- -- code generation for constants ---

(define (gen-cst* a) ; any constant
  (lambda () (set! *env* (car *env*)) a))

(define (gen-1*)     ;f or constant 1
  (lambda () (set! *env* (car *env*)) 1))

(define (gen-2*)     ;f or constant 2
  (lambda () (set! *env* (car *env*)) 2))

(define (gen-null*)  ;f or constant ()
  (lambda () (set! *env* (car *env*)) '()))

;- -- code generation for variable references ---

(define (gen-ref-glo* a) ; for a global variable
  (lambda () (set! *env* (car *env*)) (cdr a)))

(define (gen-ref* a)     ;f or any non-global variable
  (lambda () (do ((i 0 (+ i 1)) (env (cdr *env*) (cdr env)))
                 ((= i a) (set! *env* (car *env*)) (car env)))))

(define (gen-ref-loc-1*) ; for first local variable
  (lambda () (let ((val (cadr *env*))) (set! *env* (car *env*)) val)))

(define (gen-ref-loc-2*) ; for second local variable
  (lambda () (let ((val (caddr *env*))) (set! *env* (car *env*)) val)))

(define (gen-ref-loc-3*) ; for third local variable
  (lambda () (let ((val (cadddr *env*))) (set! *env* (car *env*)) val)))

;- -- code generation for assignments ---

(define (gen-set-glo* a b) ; for a global variable
  (lambda () (set! *env* (car *env*)) (set-cdr! a (b))))

(define (gen-set* a b)     ;f or any non-global variable
  (lambda () (do ((i 0 (+ i 1)) (env (cdr *env*) (cdr env)))
                 ((= i a) (set-car! env (b)) (set! *env* (car *env*))))))

(define (gen-set-loc-1* a) ; for first local variable
  (lambda () (set-car! (cdr *env*) (a)) (set! *env* (car *env*))))

(define (gen-set-loc-2* a) ; for second local variable
  (lambda () (set-car! (cddr *env*) (a)) (set! *env* (car *env*))))

(define (gen-set-loc-3* a) ; for third local variable
  (lambda () (set-car! (cdddr *env*) (a)) (set! *env* (car *env*))))

;- -- code generation for procedure application ---

(define (gen-ap0* a)     ;a ny application (of 0 to 3 arguments)
  (lambda () (let ((w (a)))
               (set! *env* (car *env*))
               (w))))

(define (gen-ap1* a b)
  (lambda () (let ((w (a)) (x (b)))
               (set! *env* (car *env*))
               (w x))))

(define (gen-ap2* a b c)
  (lambda () (let ((w (a)) (x (b)) (y (c)))
               (set! *env* (car *env*))
               (w x y))))

(define (gen-ap3* a b c d)
  (lambda () (let ((w (a)) (x (b)) (y (c)) (z (d)))
               (set! *env* (car *env*))
               (w x y z))))

(define (gen-ap0-glo* a) ; application with global variable as operator
  (lambda ()
    (set! *env* (car *env*))
    ((cdr a))))

(define (gen-ap1-glo* a b)
  (lambda () (let ((x (b)))
               (set! *env* (car *env*))
               ((cdr a) x))))

(define (gen-ap2-glo* a b c)
  (lambda () (let ((x (b)) (y (c)))
               (set! *env* (car *env*))
               ((cdr a) x y))))

(define (gen-ap3-glo* a b c d)
  (lambda () (let ((x (b)) (y (c)) (z (d)))
               (set! *env* (car *env*))
               ((cdr a) x y z))))

;- -- code generation for 'lambda' special form ---

(define (gen-prc0* a)      ;nor  est parameter (0 to 3 parameters)
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda ()
                 (set! *env* (cons *env* def))
                 (a)))))

(define (gen-prc1* a)
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda (x)
                 (set! *env* (cons *env* (cons x def)))
                 (a)))))

(define (gen-prc2* a)
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda (x y)
                 (set! *env* (cons *env* (cons x (cons y def))))
                 (a)))))

(define (gen-prc3* a)
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda (x y z)
                 (set! *env* (cons *env* (cons x (cons y (cons z def)))))
                 (a)))))

(define (gen-prc1/rest* a) ; when a rest parameter is present
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda x
                 (set! *env* (cons *env* (cons x def)))
                 (a)))))

(define (gen-prc2/rest* a)
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda (x . y)
                 (set! *env* (cons *env* (cons x (cons y def))))
                 (a)))))

(define (gen-prc3/rest* a)
  (lambda () (let ((def (cdr *env*)))
               (set! *env* (car *env*))
               (lambda (x y . z)
                 (set! *env* (cons *env* (cons x (cons y (cons z def)))))
                 (a)))))

;- -- evaluator ---

(define (evaluate expr)
  ((compile (list 'lambda '() expr))))

(define *env* '(dummy)) ; current environment

;- -- global variable definition ---

(define (define-global var val)
  (if (assq var *glo-env*)
    (set-cdr! (assq var *glo-env*) val)
    (set! *glo-env* (cons (cons var val) *glo-env*))))

(define *glo-env* (list (cons 'define define-global)))
(define-global 'cons      cons )
(define-global 'car   car )
(define-global 'cdr   cdr )
(define-global 'null? null?)
(define-global 'not   not )
(define-global '<     <)
(define-global '+     +)
(define-global '-     -)

;- -- to evaluate an expression we compile it and then call the result ---

(evaluate '(define 'fib
             (lambda (x)
               (if (< x 2)
                   1
                   (+ (fib (- x 1))
                      (fib (- x 2)))))))
(display (time (evaluate '(fib 30))))
(newline)

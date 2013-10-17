;; The interpreter from
;;  "Using closures for code generation", Feeley & Lapalme,
;;  Computer Languages, 12(1):47-66, 1987.

(define (interpret expr)
  (int expr *glo-env*))

(define (int expr env)
  (cond ((symbol? expr)
         (int-ref expr env))
        ((not (pair? expr))
         (int-cst expr env))
        ((eq? (car expr) 'quote)
         (int-cst (cadr expr) env))
        ((eq? (car expr) 'set!)
         (int-set (cadr expr) (caddr expr) env))
        ((eq? (car expr) 'if)
         (int-tst (cadr expr) (caddr expr) (cadddr expr) env))
        ((eq? (car expr) 'lambda)
         (let ((p (cadr expr)))
           (cond ((null? p)
                  (int-prc0 (caddr       expr) env))
                 ((symbol? p)
                  (int-prc1/rest (caddr expr) p env))
                 ((null? (cdr p))
                  (int-prc1 (caddr       expr) (car p) env))
                 ((symbol? (cdr p))
                  (int-prc2/rest (caddr expr) (car p) (cdr p) env))
                 ((null? (cddr p))
                  (int-prc2 (caddr       expr) (car p) (cadr p) env))
                 ((symbol? (cddr p))
                  (int-prc3/rest (caddr expr) (car p) (cadr p) (cddr p) env))
                 ((null? (cdddr p))
                  (int-prc3 (caddr       expr) (car p) (cadr p) (caddr p) env))
                 (else
                  (error "too many parameters")))))
        ((null? (cdr expr))
         (int-ap0 (car expr) env))
        ((null? (cddr expr))
         (int-ap1 (car expr) (cadr expr) env))
        ((null? (cdddr expr))
         (int-ap2 (car expr) (cadr expr) (caddr expr) env))
        ((null? (cddddr expr))
         (int-ap3 (car expr) (cadr expr) (caddr expr) (cadddr expr) env))
        (else
         (error "too many arguments"))))

;- -- interpretation of constants ---

(define (int-cst a env)
  a)

;- -- interpretation of variable references ---

(define (int-ref a env)
  (cdr (assq a env)))

;- -- interpretation of assignments ---

(define (int-set a b env)
  (set-cdr! (assq a env) (int b env)))

;- -- interpretation of 'if' special form ---

(define (int-tst a b c env)
  (if (int a env) (int b env) (int c env)))

;- -- interpretation of procedure application ---

(define (int-ap0 a env)
  ((int a env)))

(define (int-ap1 a b env)
  ((int a env) (int b env)))

(define (int-ap2 a b c env)
  ((int a env) (int b env) (int c env)))

(define (int-ap3 a b c d env)
  ((int a env) (int b env) (int c env) (int d env)))

;- -- interpretation of 'lambda' special form ---

(define (int-prc0 a env)
  (lambda ()
    (int a env)))

(define (int-prc1 a b env)
  (lambda (x)
    (int a (cons (cons b x) env))))

(define (int-prc2 a b c env)
  (lambda (x y)
    (int a (cons (cons b x) (cons (cons c y) env)))))

(define (int-prc3 a b c d env)
  (lambda (x y z)
    (int a (cons (cons b x) (cons (cons c y) (cons (cons d z) env))))))

(define (int-prc1/rest a b env)
  (lambda x
    (int a (cons (cons b x) env))))

(define (int-prc2/rest a b c env)
  (lambda (x . y)
    (int a (cons (cons b x) (cons (cons c y) env)))))

(define (int-prc3/rest a b c d env)
  (lambda (x y . z)
    (int a (cons (cons b x) (cons (cons c y) (cons (cons d z) env))))))

;- -- evaluator ---

(define (evaluate expr)
  (interpret expr))

;- -- global variable definition ---

(define (define-global var val)
  (if (assq var *glo-env*)
    (set-cdr! (assq var *glo-env*) val)
    (begin
      (set-cdr! *glo-env* (cons (car *glo-env*) (cdr *glo-env*)))
      (set-car! *glo-env* (cons var val)))))

(define *glo-env* (list (cons 'define define-global)))
(define-global 'cons      cons )
(define-global 'car   car )
(define-global 'cdr   cdr )
(define-global 'null? null?)
(define-global 'not   not )
(define-global '<     <)
(define-global '+     +)
(define-global '-     -)

;- -- to evaluate an expression we call the interpreter ---

(evaluate '(define 'fib
             (lambda (x)
               (if (< x 2)
                   1
                   (+ (fib (- x 1))
                      (fib (- x 2)))))))
(display (time (evaluate '(fib 30))))
(newline)




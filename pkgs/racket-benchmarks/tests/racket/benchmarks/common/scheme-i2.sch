;; Yet another Scheme interpreter, this time running
;;  a Y-combinator countdown

(define (make-closure arg body env) (vector arg body env))
(define (closure-arg v) (vector-ref v 0))
(define (closure-body v) (vector-ref v 1))
(define (closure-env v) (vector-ref v 2))

(define (interp expr env)
  (cond
    [(number? expr) expr]
    [(symbol? expr)
     (cdr (assq expr env))]
    [else
     (case (car expr)
       [(+)
        (+ (interp (cadr expr) env)
           (interp (caddr expr) env))]
       [(-)
        (- (interp (cadr expr) env)
           (interp (caddr expr) env))]
       [(zero?)
        (zero? (interp (cadr expr) env))]
       [(if)
        (if (interp (cadr expr) env)
            (interp (caddr expr) env)
            (interp (cadddr expr) env))]
       [(let)
        (let ([rhs-val
               (interp (cadr (caadr expr)) env)])
          (interp (caddr expr)
                  (cons (cons (car (caadr expr))
                              rhs-val)
                        env)))]
       [(lambda)
        (make-closure (caadr expr)
                      (caddr expr)
                      env)]
       [else
        (let ([clos (interp (car expr) env)]
              [arg-val (interp (cadr expr) env)])
          (interp (closure-body clos)
                (cons
                 (cons (closure-arg clos)
                       arg-val)
                 (closure-env clos))))])]))

(time (interp '(let ([Y (lambda (m)
                          ((lambda (f) (m (lambda (a) ((f f) a))))
                           (lambda (f) (m (lambda (a) ((f f) a))))))])
                 (let ([count
                        (Y (lambda (count)
                             (lambda (n)
                               (if (zero? n)
                                   0
                                   (+ 1 (count (- n 1)))))))])
                   (count 500000)))
              '()))

#lang typed/scheme

#;#;
(: g (Any -> Boolean : (U 'r 's)))
(define (g x)
  (let ([q x])
    (let ([op2 (eq? 'r x)])
      (if op2 op2 (eq? 's x)))))


(define: f? : (Any -> Boolean : (U 'q 'r 's))
  (lambda (x)
    (let ([op1 (eq? 'q x)])
      (if op1 op1
        (let ([op2 (eq? 'r x)])
               (if op2
                   ;; !#f_op2
                   op2
                   (eq? 's x)))))))

(define: f2? : (Any -> Boolean : (U 'q 'r 's))
  (lambda (x)
    (or (eq? 'q x)
        (eq? 'r x)
        (eq? 's x))))

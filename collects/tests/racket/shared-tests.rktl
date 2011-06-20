;; This file has to work for both "shared.rkt" and "advanced.rkt"

;; this writes values to strings and compares the strings
;; to implements an equal? predicate that works for cyclic
;; structures.

(define (gs v)
  (let ([p (open-output-string)])
    (begin
      (write v p)
      (get-output-string p))))
(define (x s)
  (read (open-input-string s)))
(define (stest expect expression)
  (test
   (gs expect)
   (let ([shared-tester (lambda (x) (gs (eval x)))]) shared-tester)
   expression))

(stest '(1 2) '(shared ([x (list 1 2)]) x))
(stest #(1 2) '(shared ([x (vector 1 2)]) x))
(stest (box 1) '(shared ([x (box 1)]) x))
(stest '(1) '(shared ([x (cons 1 null)]) x))
(stest (mcons 1 null) '(shared ([x (mcons 1 null)]) x))

(stest (x "#1=(#1# 1)") '(shared ([x (list x 1)]) x))
(stest (x "#2=#(#2# 1)") '(shared ([x (vector x 1)]) x))
(stest (x "#2=#(#2# 1)") '(shared ([x (vector-immutable x 1)]) x))
(stest (x "#3=#&#3#") '(shared ([x (box x)]) x))
(stest (x "#3=#&#3#") '(shared ([x (box-immutable x)]) x))
(stest (x "#4=(#4#)") '(shared ([x (cons x null)]) x))
(stest (x "#5=(1 . #5#)") '(shared ([x (cons 1 x)]) x))
(stest (let ([x (mcons 1 #f)]) 
         (begin (set-mcdr! x x) 
                x))
       '(shared ([x (mcons 1 x)]) x))

(stest (x "#11=(#11#)") '(shared ([x `(,x)]) x))

(stest 1 '(shared ([x (list 1 x p)]
                   [p (lambda () x)])
             (car ((caddr x)))))

(define-struct s (a b))
(shared ([x (make-s 17 x)])
  (begin
    (test #t s? x)
    (test 17 s-a x)
    (test #t eq? x (s-b x))))

(define-struct shared-test-ds-no-fields ())
(stest (make-shared-test-ds-no-fields)
       '(shared ((-4- 11) 
                 (-8- (make-shared-test-ds-no-fields)))
          -8-))

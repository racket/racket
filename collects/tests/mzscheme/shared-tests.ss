
;; this writes values to strings and compares the strings
;; to implements an equal? predicate that works for cyclic
;; structures.

(define (gs v)
  (let ([p (open-output-string)])
    (begin
      (write v p)
      (get-output-string p))))
(define (stest expect expression)
  (test
   (gs expect)
   (let ([shared-tester (lambda (x) (gs (eval x)))]) shared-tester)
   expression))

(stest '(1 2) '(shared ([x (list 1 2)]) x))
(stest #(1 2) '(shared ([x (vector 1 2)]) x))
(stest (box 1) '(shared ([x (box 1)]) x))
(stest '(1) '(shared ([x (cons 1 null)]) x))

(stest '#1=(#1# 1) '(shared ([x (list x 1)]) x))
(stest '#2=#(#2# 1) '(shared ([x (vector x 1)]) x))
(stest '#3=#&#3# '(shared ([x (box x)]) x))
(stest '#4=(#4#) '(shared ([x (cons x null)]) x))
(stest '#5=(1 . #5#) '(shared ([x (cons 1 x)]) x))

(stest '#11=(#11#) '(shared ([x `(,x)]) x))


(define-struct s (a b))
(shared ([x (make-s 17 x)])
  (begin
    (test #t s? x)
    (test 17 s-a x)
    (test #t eq? x (s-b x))))



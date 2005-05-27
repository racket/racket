
(load-relative "loadtest.ss")

(SECTION 'optimization)

;; For some comparison, ignore the stack-depth
;;  part of the compilation result (since it's
;;  an approximation, anyway).
(define maybe-different-depths? #f)

(define (comp=? c1 c2)
  (let ([s1 (open-output-string)]
	[s2 (open-output-string)])
    (write c1 s1)
    (write c2 s2)
    (let ([t1 (get-output-string s1)]
	  [t2 (get-output-string s2)])
      (or (string=? t1 t2)
	  (and maybe-different-depths?
	       (string=? (substring t1 5 (string-length t1))
			 (substring t2 5 (string-length t2))))))))

(define test-comp
  (case-lambda
   [(expr1 expr2) (test-comp expr1 expr2 #t)]
   [(expr1 expr2 same?)
    (test same? `(compile ,same? ,expr2) (comp=? (compile expr1) (compile expr2)))]))

(let ([x (compile '(lambda (x) x))])
  (test #t 'fixpt (eq? x (compile x))))

(test-comp 5 '(if #t 5 (cons 1 2)))
(test-comp 5 '(if #f (cons 1 2) 5))

(test-comp 5 '(begin0 5 'hi "apple" 1.5))
(test-comp 5 '(begin0 5 (begin0 'hi "apple" 1.5)))
(test-comp 5 '(begin0 5 (begin0 'hi "apple") 1.5))
(test-comp 5 '(begin0 5 (begin 'hi "apple" 1.5)))
(test-comp 5 '(begin0 5 (begin 'hi "apple") 1.5))
(test-comp 5 '(begin0 (begin0 5 'hi "apple" 1.5)))
(test-comp 5 '(begin0 (begin0 5 'hi "apple") 1.5))

; Can't drop `begin0' if the first expresson is not valueable:
(test-comp '(begin0 (begin0 (+ 1 2) 0) 0) '(begin0 (begin0 (+ 1 2) 'hi "apple") 1.5))

(test-comp 5 '(begin 'hi "apple" 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin0 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin0 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5 5)))
(test-comp 5 '(begin 'hi (begin "apple" 1.5 5)))

(test-comp '(let ([x 8][y 9]) (lambda () x))
	   '(let ([x 8][y 9]) (lambda () (if #f y x))))
(test-comp '(let ([x 8][y 9]) (lambda () (+ x y)))
	   '(let ([x 8][y 9]) (lambda () (if #f y (+ x y)))))

(test-comp '(let ([x 5]) (set! x 2)) '(let ([x 5]) (set! x x) (set! x 2)))

(test-comp '(let* () (f 5))
	   '(f 5))
(test-comp '(letrec () (f 5))
	   '(f 5))
(test-comp '(with-handlers () (f 5))
	   '(f 5))
(test-comp '(parameterize () (f 5))
	   '(f 5))
(test-comp '(fluid-let () (f 5))
	   '(f 5))

(test-comp '(let ([i (cons 0 1)]) (let ([j i]) j))
	   '(let ([i (cons 0 1)]) i))

(define (normalize-depth s)
  `(let ([a ,s]
	 [b (let-values ([(a b c d e f) (values 1 2 3 4 5 6)])
	      (list a b c d e f))])
     10))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][j i]) j))
	   (normalize-depth '(let* ([i (cons 0 1)]) i)))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)][g i]) g))
	   (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)]) i)))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)][g i][h g]) h))
	   (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)]) i)))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][g i][h (car g)][m h]) m))
	   (normalize-depth '(let* ([i (cons 0 1)][h (car i)]) h)))

(set! maybe-different-depths? #t)

(require #%kernel) ; 

(test-comp (void) '(void))
(test-comp 3 '(+ 1 2))
(test-comp 65 '(char->integer #\A))
(test-comp (expt 5 30)
	   '(expt 5 (* 5 6)))
(test-comp 88
	   '(if (pair? null) 89 88))

(test-comp '(let ([x 3]) x)
	   '((lambda (x) x) 3))
(test-comp '(let ([x 3][y 4]) (+ x y))
	   '((lambda (x y) (+ x y)) 3 4))

(report-errs)

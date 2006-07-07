
(load-relative "loadtest.ss")
(require (lib "package.ss"))


(Section 'packages)

(define expand-test-use-toplevel? #t)

;; syntax
(syntax-test #'(dot))
(syntax-test #'(dot 1))
(syntax-test #'(dot 1 2))
(syntax-test #'(dot 1 x))

(syntax-test #'(open))
(syntax-test #'(open 1))
(syntax-test #'(open 1 2))
(syntax-test #'(open 1 x))

(syntax-test #'(define-dot))
(syntax-test #'(define-dot 1))
(syntax-test #'(define-dot x))
(syntax-test #'(define-dot 1 2))
(syntax-test #'(define-dot 1 x))
(syntax-test #'(define-dot x 1))
(syntax-test #'(define-dot x y))
(syntax-test #'(define-dot 1 x y))
(syntax-test #'(define-dot x y 3))
(syntax-test #'(define-dot x 2 y))

(syntax-test #'(package))
(syntax-test #'(package x))
(syntax-test #'(package 1))
(syntax-test #'(package x 1))
(syntax-test #'(package x x))
(syntax-test #'(package x (1)))



;; Providing
(package p1 all-defined
  (define x 10)
  (package y all-defined
    (define x 12)))

(package p2 ()
  (define x 10))

(package p3 (x)
  (package x all-defined
    (define x 10)))

(package p4 all-defined
  (package x (x)
    (define x 10)
    (define y 11)))

(define exn:variable? exn:fail:contract:variable?)

(err/rt-test xxxx exn:variable?)
(test 10 "" (dot p1 x))
(test 12 "" (dot p1 y x))
(syntax-test #'(dot p2 x))
(test 10 "" (dot p3 x x))
(test 10 "" (dot p4 x x))
(syntax-test #'(dot p4 x y))
(syntax-test #'(package p (x)))
(syntax-test #'(package p (x) (package y (x) (define x 10))))

;; Internal-defines
(let ((p1 1)
      (x 2))
  (define x 1111)
  (package p1 all-defined
    (define x 10)
    (package y all-defined
      (define x 12)))
  
  (package p2 ()
    (define x 10))
  
  (package p3 (x)
    (package x all-defined
      (define x 10)))
  
  (package p4 all-defined
    (package x (x)
      (define x 10)
      (define y 11)))
  
  (test 10 "" (dot p1 x))
  (test 12 "" (dot p1 y x))
  (syntax-test #'(dot p2 x))
  (test 10 "" (dot p3 x x))
  (test 10 "" (dot p4 x x))
  (syntax-test #'(dot p4 x y)))
(syntax-test #'(let () (package p (x)) 1))
(syntax-test #'(let () (package p (x) (package y (x) (define x 10))) 1))
(syntax-test #'(let ((all-defined 1)) (package p all-defined (define s 1)) 1))

;; starred defines
(package p5 all-defined
  (define*-values (x) 10)
  (define*-values (f) (lambda () x))
  (define*-values (x) 12))
(test 12 "" (dot p5 x))
(test 10 "" ((dot p5 f)))

;; mutual references
(package p99 all-defined
  (define (f) x)
  (define x 77))
(test 77 "" (dot p99 x))
(test 77 "" ((dot p99 f)))
(let ()
  (package p99. all-defined
    (define (f) x)
    (define x 177))
  (test 177 "" (dot p99. x))
  (test 177 "" ((dot p99. f))))
;;
(package p98 all-defined
  (define (f) x)
  (define* y 11)
  (define x 78))
(test 78 "" (dot p98 x))
(test 11 "" (dot p98 y))
(test 78 "" ((dot p98 f)))
(let ()
  (package p98. all-defined
    (define (f) x)
    (define* y 111)
    (define x 178))
  (test 178 "" (dot p98. x))
  (test 111 "" (dot p98. y))
  (test 178 "" ((dot p98. f))))

;; nesting
(package p6 all-defined
  (package xx all-defined
    (define x 10))
  (package z all-defined
    (package a all-defined
      (define z 111)))
  (define y (dot xx x))
  (define x 11))

(test 11 "" (dot p6 x))
(test 10 "" (dot p6 y))
(syntax-test #'(dot p6 x x))
(test 111 "" (dot p6 z a z))

;; open
(let ()
  (package p7 all-defined
    (define a 1)
    (define b 2)
    (define c 3))
  (let ()
    (package p8 all-defined
      (open* p7)
      (define* c 4))
    (test 1 "" (dot p8 a))
    (test 1 "" (dot p7 a))
    (test 2 "" (dot p8 b))
    (test 2 "" (dot p7 b))
    (test 4 "" (dot p8 c))
    (test 3 "" (dot p7 c))))

(let ()
  ;; Same test as above, but without nested lets:
  (package p7. all-defined
    (define a 10)
    (define b 20)
    (define c 30))
  (package p8. all-defined
    (open* p7.)
    (define* c 40))
  (test 10 "" (dot p8. a))
  (test 10 "" (dot p7. a))
  (test 20 "" (dot p8. b))
  (test 20 "" (dot p7. b))
  (test 40 "" (dot p8. c))
  (test 30 "" (dot p7. c)))

(let ()
  (package p9 all-defined
    (package x all-defined
      (define x 1)))
  (let ()
    (open p9)
    (test 1 "" (dot x x))))

(let ()
  (package p9 all-defined
    (package x all-defined
      (define x 1)))
  (let ()
    (open p9 x)
    (test 1 "" x)))

(syntax-test #'(open x))
(syntax-test #'(let () (package y all-defined (package z ())) (let () (open y a))))
(syntax-test #'(let () (package y all-defined (package z ())) (let () (open y z a))))

;; open* after use => no capture
(let ([x 99])
  (package yyy () 
    (package p (x) (define x 8)) 
    (define (f) x) 
    (open* p) 
    (test 99 f))
  'ok)
;; open after use => capture
(package yyy () 
  (package p (x) 
    (define x 88)) 
  (define (f) x) 
  (open p) 
  (test 88 f))

;; Mutually referential packages:
(let ()
  (package o (odd)
    (define (odd x) (if (zero? x) #f (even (sub1 x)))))
  (package e (even)
    (define (even x) (if (zero? x) #t (odd (sub1 x)))))
  (open o)
  (open e)
  (test #t odd 17)
  (test #f even 19))
(err/rt-test
 ;; Like above, but omit an open:
 (let ()
   (package o (odd)
     (define (odd x) (if (zero? x) #f (even (sub1 x)))))
   (package e (even)
     (define (even x) (if (zero? x) #t (odd (sub1 x)))))
   (open o)
   (odd 17))
 exn:variable?)
(err/rt-test
 ;; Omit the other open:
 (let ()
   (package o (odd)
     (define (odd x) (if (zero? x) #f (even (sub1 x)))))
   (package e (even)
     (define (even x) (if (zero? x) #t (odd (sub1 x)))))
   (open e)
   (even 17))
 exn:variable?)
;; Same as working, but in a package:
(package yyy ()
  (package o (odd)
    (define (odd x) (if (zero? x) #f (even (sub1 x)))))
  (package e (even)
    (define (even x) (if (zero? x) #t (odd (sub1 x)))))
  (open o)
  (open e)
  (test #t odd 17)
  (test #f even 19))
(err/rt-test
 ;; open* shouldn't work:
 (let ()
   (package yyy ()
     (package o (odd)
       (define (odd x) (if (zero? x) #f (even (sub1 x)))))
     (package e (even)
       (define (even x) (if (zero? x) #t (odd (sub1 x)))))
     (open* o)
     (open e)
     (odd 17))
   'ok)
 exn:variable?)

;; define-dot
(let ()
  (package x all-defined
    (define z 10))
  (define-dot a x z)
  (test 10 "" a))
(let ()
  (package x all-defined
    (package y all-defined
      (define z 10)))
  (define-dot a x y)
  (define-dot b a z)
  (test 10 "" b))

(syntax-test #'(let () (package x ()) (define-dot a x c) 1))

;; dot
(let ()
  (package x all-defined
    (define z 10))
  (test 10 "" (dot x z)))
(let ()
  (package x all-defined
    (package y all-defined
      (define z 10)))
  (define-dot a x y)
  (test 10 "" (dot a z)))
(syntax-test #'(let () (package x ()) (dot x c)))


(define expand-test-use-toplevel? #f)

(report-errs)

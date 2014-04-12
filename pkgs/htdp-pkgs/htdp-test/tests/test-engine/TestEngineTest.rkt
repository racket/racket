;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TestEngineTest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;;Expect 42 checks, 19 failures

(define (count f)
  (cond
    [(zero? f) 1]
    [else (add1 (count (sub1 f)))]))

(check-expect (count 3) 3) ; fails
(check-expect (count 3) 4)

(check-within 1.345 1.3 .05)
(check-within 1.345 1.3 .005) ; fails

(check-expect (cons 1 (cons 2 (cons 3 empty))) (cons 2 (cons 2 (cons 2 empty)))) ;fails
(check-expect (cons 1 (cons 2 (cons 3 empty))) empty) ;fails
(check-expect (cons 1 (cons 2 (cons 3 empty))) (cons 1 (cons 2 (cons 3 empty))))
(check-within (cons 1 (cons 2 (cons 3 empty))) (cons 1.1 (cons 2.1 (cons 3.1 empty))) .2)
(check-within (cons 1 (cons 2 (cons 3 empty))) (cons 1.1 (cons 2.1 (cons 3.1 empty))) .01) ;fails

(check-expect 'red 'blue) ;fails
(check-expect 'red 'red)
(check-within 'red 'red .002)
(check-expect 'red "red") ;fails

(check-expect "red" "red")
(check-expect "red " "red") ;fails
(check-expect "Hello" "red") ;fails
(check-within "hello" "Hello" .03) ;fails

(define-struct ball (point rad color))
(define blue-ball (make-ball (make-posn 1 3) 3.4 "blue"))

(check-expect (make-ball 4 5 'blue) (make-ball 4 5 'blue))
(check-expect (make-ball (make-posn 1 2) 3.5 'blue) (make-ball (make-posn 1 2) 3.5 'blue))
(check-expect (make-ball 3 (make-posn 1 2) "blue") (make-ball (make-posn 1 2) 3.3 "blue")) ;fails
(check-within blue-ball (make-ball (make-posn 1 3) 3.3 "blue") .1)
(check-within blue-ball (make-ball (make-posn 1 3) 3.3 "blue") .01) ;fails

(check-error (error 'test "hi") "test: hi")
(check-error (/ 1 0) "/: division by zero")
(check-error 3 "some message") ;fails
(check-error (first empty) "another message") ;fails

(define (create n)
  (make-ball n n 'blue))

(check-member-of (create 1) (make-ball 1 2 'blue) (make-ball 1 1 'blue) (make-ball 1 2 'red) 'red)
(check-member-of 1 1 1 1 1)
(check-member-of (create 2) (make-ball 1 2 'blue) (make-ball 1 1 'blue) (make-ball 1 2 'red) 'fails)

(check-range 5 0 10)
(check-range 0 0 10)
(check-range 10 0 10)
(check-range 11 0 10) ;fails
(check-range 5.01 0 10.5)
(check-range 0.0 0 10.5)
(check-range 10.5 0 10.5)
(check-range 10.5001 0 10.5) ;fails

;; ---------------------------------------------------------------------------------------------------
;; MF: from DVH 

(define (random-elt ls) (list-ref ls (random (length ls))))

(check-random (random-elt (build-list 100 identity))
              (list-ref (build-list 100 identity) (random 100)))


(define (f _x)
  (list (random 10) (random 20)))

(define (g _x)
  (list (random 10) (random 20)))

(check-random (f 0) (list (random 10) (random 20)))

(check-random (g 0)
              (let* ((x2 (random 20))
                     (x1 (random 10)))
                (list x1 x2))) ;; fails 

(define (h _x) (first (list (random 50) (random 20))))

(check-random (h 0) (begin0 (random 50) (random 20)))

(check-random (h 0) (begin (random 20) (random 50))) ;; fails 
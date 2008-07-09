;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TestEngineTest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Expect 29 checks, 17 failures

(define (count f)
  (cond
    [(zero? f) 1]
    [else (add1 (count (sub1 f)))]))

(check-expect (count 3) 3) ; fails
(check-expect (count 3) 4)
(check-expect (count (/ 1 0)) 2) ; fails

(check-within 1.345 1.3 .05)
(check-within 1.345 1.3 .005) ; fails

(check-expect (cons 1 (cons 2 (cons 3 empty))) (cons 2 (cons 2 (cons 2 empty)))) ;fails
(check-expect (cons 1 (cons 2 (cons 3 empty))) empty) ;fails
(check-expect (cons 1 (cons 2 (cons 3 empty))) (cons 1 (cons 2 (cons 3 empty))))
(check-expect (first empty) empty) ;fails
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

(check-expect (make-ball 4 5 'blue) (make-ball 4 5 'blue))
(check-expect (make-ball (make-posn 1 2) 3.5 'blue) (make-ball (make-posn 1 2) 3.5 'blue))
(check-expect (make-ball 3 (make-posn 1 2) "blue") (make-ball (make-posn 1 2) 3.3 "blue")) ;fails
(check-within (make-ball (make-posn 1 3) 3.4 "blue") (make-ball (make-posn 1 3) 3.3 "blue") .1)
(check-within (make-ball (make-posn 1 3) 3.4 "blue") (make-ball (make-posn 1 3) 3.3 "blue") .01) ;fails
(check-expect (posn-x (ball-point 3)) 3) ;fails

(check-error (error 'test "hi") "test: hi")
(check-error (/ 1 0) "/: division by zero")
(check-error 3 "some message") ;fails
(check-error (first empty) "another message") ;fails

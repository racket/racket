;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(check-error (error) "")
(check-error (error 1) "1")
(check-error (error 'a) "a: ")
(check-error (error 'a "bad input") "a: bad input")
(check-error (error 'a "bad input: " 1) "a: bad input: 1")
(check-error (error 'a "bad input: " 1 " and " "hello") "a: bad input: 1 and hello")
(check-error (error 'a "bad input: " 1 " and " false) "a: bad input: 1 and false")
(check-error (error 'a "uhoh " (list 1 2 3)) "a: uhoh (cons 1 (cons 2 (cons 3 empty)))")

(define-struct err (str))

(check-error (error 'a "bad input: " 1 " and " (make-err "hello")) 
             "a: bad input: 1 and (make-err \"hello\")")

;; test errors by hand in GUI
(load "tester.ss")

;; f2c : num -> num
;; to convert a Fahrenheit temperature into a Celsius temperature 
(define (f2c f)
  (* 5/9 (- f 32)))

(define (fx y) 
  'xyz)

#| Tests: with teach-pack convert.ss |#

(= (f2c 212) 100)
(= (f2c 32) 0)
(= (f2c -40) -40)

;; ----------------------------------------------------------------------------
(define IN "convert-in.dat")
(define OUT "convert-out.dat")

(define (create-convert-in)
  (printf "212 32~n-40~n"))

(define (check-convert-out)
  (and (= (read) 100)
       (= (read) 0)
       (= (read) -40)))

(when (file-exists? IN) (delete-file IN))
(with-output-to-file IN create-convert-in)
(when (file-exists? OUT) (delete-file OUT))
(convert-file IN f2c OUT)
(with-input-from-file OUT check-convert-out)
    
(test-error (convert-file IN list OUT))
;; convert-file: procedure of one argument expected as "convert-gui" argument;
;; given procedure ...

(test-error (convert-file IN first OUT))
;; first: expects argument of type <non-empty list>; given 212

(test-error (convert-file IN fx OUT))
;; convert: The conversion function must produce a number; result: 'xyz

(test-error (convert-file IN f2c 10))
;; convert-file: expected <string> as third argument, given: 10

;; ----------------------------------------------------------------------------
;; convert by repl: 
(convert-repl f2c) 
;; type in 0 212 40 into the repl 

(test-error (convert-repl first))
;; hilight first 

(test-error (convert-repl fx))
;; signal an error about not returning a number

;; ----------------------------------------------------------------------------
;; convert by GUI 

(convert-gui f2c) 

(define (f2x x) (* x x x x))
;; (convert-gui f2x)
;; must result in an error 

;; TEST BY HAND: (convert-gui first)
;; first is blamed but not nicely 

;; TEST BY HAND: (convert-gui fx)
;; signal an error about not returning a number

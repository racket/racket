#lang scheme/base

(require tests/eli-tester lazy/force)

(test

;; lazy/force behavior
(test
 (! 1) => 1
 (! (! 1)) => 1
 (! (~ 1)) => 1
 (! (~ (~ (~ 1)))) => 1)

;; !list
(test
 (!list (list 1 2 3)) => '(1 2 3)
 (!list (~ (list 1 2 3))) => '(1 2 3)
 (!list (~ (cons 1 (~ (cons 2 (~ (cons 3 (~ null)))))))) => '(1 2 3)
 (!list 1) => 1 ; works on dotted lists
 (!list (cons 1 2)) => '(1 . 2))

;; !!list
(test
 (!!list (list 1 2 3)) => '(1 2 3)
 (!!list (list (~ 1) (~ 2) (~ 3))) => '(1 2 3)
 (!!list (list* (~ 1) (~ 2) (~ 3))) => '(1 2 . 3)
 (!!list (~ (cons (~ 1) (~ (cons (~ 2) (~ (cons (~ 3) (~ null)))))))) => '(1 2 3)
 (!!list (~ (cons (~ 1) (~ (list 2 3))))) => '(1 2 3)
 (!!list (~ (cons (~ 1) (~ (list 2 (~ 3)))))) => '(1 2 3))

;; !!
(parameterize ([print-graph #t])
  (test
   (!! (~ (cons (~ 1) (~ (cons (~ 2) (~ (cons (~ 3) (~ null)))))))) => '(1 2 3)
   (format "~s" (!! (letrec ([ones (~ (cons 1 (~ ones)))]) ones)))
   => "#0=(1 . #0#)"
   (format "~s" (!! (letrec ([ones (~ (cons 1 (~ ones)))]) (list ones ones))))
   => "(#0=(1 . #0#) #0#)"
   (format "~s" (!! (letrec ([x (vector 1 (~ x))]) x)))
   => "#0=#(1 #0#)"
   (format "~s" (!! (letrec ([x (vector-immutable 1 (~ x))]) x)))
   => "#0=#(1 #0#)"
   (format "~s" (!! (letrec ([x (box (~ x))]) x)))
   => "#0=#&#0#"
   (format "~s" (!! (letrec ([x (box-immutable (~ x))]) x)))
   => "#0=#&#0#"
   (format "~s" (!! (letrec ([x (make-prefab-struct 'foo 1 (~ x))]) x)))
   => "#0=#s(foo 1 #0#)"
   ))

)

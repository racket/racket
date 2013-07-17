#lang racket
(require unstable/markparam
         racket/serialize
         tests/eli-tester)

(define x (mark-parameter))
(define y (mark-parameter))
(test
 (mark-parameter? x)
 (mark-parameter? (deserialize (serialize x)))
 (deserialize (serialize x)) => x
 
 (apply eq? (deserialize (serialize (list x x))))
 
 #:failure-prefix "empty"
 (mark-parameterize 
  ()
  (test 
   (x) => #f
   (mark-parameter-first x) => #f
   (mark-parameter-first y) => #f
   (mark-parameter-all x) => empty
   (mark-parameter-all y) => empty
   (mark-parameters-all (list x)) => empty
   (mark-parameters-all (list y)) => empty
   (mark-parameters-all (list x y)) => empty))
 
 #:failure-prefix "x 1"
 (mark-parameterize 
  ([x 1])
  (test
   (x) => 1
   (mark-parameter-first x) => 1
   (mark-parameter-first y) => #f
   (mark-parameter-all x) => (list 1)
   (mark-parameter-all y) => empty
   (mark-parameters-all (list x)) => (list (vector 1))
   (mark-parameters-all (list y)) => empty
   (mark-parameters-all (list x y)) => (list (vector 1 #f))
   (mark-parameters-all (list x y) 20) => (list (vector 1 20))))
 
 #:failure-prefix "x 2 > x 1"
 (mark-parameterize 
  ([x 2])
  (mark-parameterize 
   ([x 1])
   (test
    (mark-parameter-first x) => 1
    (mark-parameter-first y) => #f
    (mark-parameter-all x) => (list 1)
    (mark-parameter-all y) => empty
    (mark-parameters-all (list x)) => (list (vector 1))
    (mark-parameters-all (list y)) => empty
    (mark-parameters-all (list x y)) => (list (vector 1 #f))
    (mark-parameters-all (list x y) 20) => (list (vector 1 20)))))
 
 #:failure-prefix "x 2 > list > x 1"
 (mark-parameterize 
  ([x 2])
  (list
   (mark-parameterize 
    ([x 1])
    (test
     (mark-parameter-first x) => 1
     (mark-parameter-first y) => #f
     (mark-parameter-all x) => (list 1 2)
     (mark-parameter-all y) => empty
     (mark-parameters-all (list x)) => (list (vector 1) (vector 2))
     (mark-parameters-all (list y)) => empty
     (mark-parameters-all (list x y)) => (list (vector 1 #f) (vector 2 #f))
     (mark-parameters-all (list x y) 20) => (list (vector 1 20) (vector 2 20))))))
 
 #:failure-prefix "x 2 > list > x 1"
 (mark-parameterize 
  ([x 2])
  (list
   (mark-parameterize 
    ([x 1])
    (test
     (mark-parameter-first x) => 1
     (mark-parameter-first y) => #f
     (mark-parameter-all x) => (list 1 2)
     (mark-parameter-all y) => empty
     (mark-parameters-all (list x)) => (list (vector 1) (vector 2))
     (mark-parameters-all (list y)) => empty
     (mark-parameters-all (list x y)) => (list (vector 1 #f) (vector 2 #f))
     (mark-parameters-all (list x y) 20) => (list (vector 1 20) (vector 2 20))))))
 
  #:failure-prefix "x 2 > list > y 1"
 (mark-parameterize 
  ([x 2])
  (list
   (mark-parameterize 
    ([y 1])
    (test
     (mark-parameter-first x) => 2
     (mark-parameter-first y) => 1
     (mark-parameter-all x) => (list 2)
     (mark-parameter-all y) => (list 1)
     (mark-parameters-all (list x)) => (list (vector 2))
     (mark-parameters-all (list y)) => (list (vector 1))
     (mark-parameters-all (list x y)) => (list (vector #f 1) (vector 2 #f))
     (mark-parameters-all (list x y) 20) => (list (vector 20 1) (vector 2 20))))))
 
 #:failure-prefix "x 1 y 2"
 (mark-parameterize 
  ([x 1] [y 2])
  (test
   (mark-parameter-first x) => 1
   (mark-parameter-first y) => 2
   (mark-parameter-all x) => (list 1)
   (mark-parameter-all y) => (list 2)
   (mark-parameters-all (list x)) => (list (vector 1))
   (mark-parameters-all (list y)) => (list (vector 2))
   (mark-parameters-all (list x y)) => (list (vector 1 2))
   (mark-parameters-all (list x y) 20) => (list (vector 1 2))))
 
 #:failure-prefix "x 1 y x"
 (mark-parameterize 
  ([x 1] [y (mark-parameter-first x)])
  (test
   (mark-parameter-first x) => 1
   (mark-parameter-first y) => #f
   (mark-parameter-all x) => (list 1)
   (mark-parameter-all y) => (list #f)
   (mark-parameters-all (list x)) => (list (vector 1))
   (mark-parameters-all (list y)) => (list (vector #f))
   (mark-parameters-all (list x y)) => (list (vector 1 #f))
   (mark-parameters-all (list x y) 20) => (list (vector 1 #f)))))

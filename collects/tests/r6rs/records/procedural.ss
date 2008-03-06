#!r6rs

(library (tests r6rs records procedural)
  (export run-records-procedural-tests)
  (import (rnrs)
          (tests r6rs test))

  ;; ----------------------------------------

  (define rtd1
    (make-record-type-descriptor
     'rtd1 #f #f #f #f
     '#((immutable x1) (immutable x2))))
  
  (define rtd2
    (make-record-type-descriptor
     'rtd2 rtd1 #f #f #f
     '#((immutable x3) (immutable x4))))
  
  (define rtd3
    (make-record-type-descriptor
     'rtd3 rtd2 #f #f #f
     '#((immutable x5) (immutable x6))))
  
  (define protocol1
    (lambda (p)
      (lambda (a b c)
        (p (+ a b) (+ b c)))))
  
  (define protocol2
    (lambda (n)
      (lambda (a b c d e f)
        (let ((p (n a b c)))
          (p (+ d e) (+ e f))))))
  
  (define protocol3
    (lambda (n)
      (lambda (a b c d e f g h i)
        (let ((p (n a b c d e f)))
          (p (+ g h) (+ h i))))))
  
  (define cd1
    (make-record-constructor-descriptor
     rtd1 #f protocol1))
  
  (define cd2
    (make-record-constructor-descriptor
     rtd2 cd1 protocol2))
  
  (define cd3
    (make-record-constructor-descriptor
     rtd3 cd2 protocol3))
  
  (define make-rtd1 (record-constructor cd1))
  
  (define make-rtd2 (record-constructor cd2))
  
  (define make-rtd3 (record-constructor cd3))
  
  
  (define :point
    (make-record-type-descriptor
     'point #f
     #f #f #f 
     '#((mutable x) (mutable y))))

  (define :point-cd
    (make-record-constructor-descriptor :point #f #f))
  
  (define make-point (record-constructor :point-cd))
  
  (define point? (record-predicate :point))
  (define point-x (record-accessor :point 0))
  (define point-y (record-accessor :point 1))
  (define point-x-set! (record-mutator :point 0))
  (define point-y-set! (record-mutator :point 1))
  
  (define p1 (make-point 1 2))

  (define :point2
    (make-record-type-descriptor
     'point2 :point 
     #f #f #f '#((mutable x) (mutable y))))
  
  (define make-point2
    (record-constructor
     (make-record-constructor-descriptor :point2
                                         #f #f)))
  (define point2? (record-predicate :point2))
  (define point2-xx (record-accessor :point2 0))
  (define point2-yy (record-accessor :point2 1))

  (define p2 (make-point2 1 2 3 4))

  (define :point-cd/abs
    (make-record-constructor-descriptor
     :point #f
     (lambda (new)
       (lambda (x y)
         (new (abs x) (abs y))))))
  
  (define make-point/abs
    (record-constructor :point-cd/abs))
  
  (define :cpoint
    (make-record-type-descriptor
     'cpoint :point
     #f #f #f
     '#((mutable rgb))))
  
  (define make-cpoint
    (record-constructor
     (make-record-constructor-descriptor
      :cpoint :point-cd
      (lambda (p)
        (lambda (x y c)
          ((p x y) (color->rgb c)))))))

  (define make-cpoint/abs
    (record-constructor
     (make-record-constructor-descriptor
      :cpoint :point-cd/abs
      (lambda (p)
        (lambda (x y c)
          ((p x y) (color->rgb c)))))))
  
  (define cpoint-rgb
    (record-accessor :cpoint 0))
  
  (define (color->rgb c)
    (cons 'rgb c))
  
  ;; ----------------------------------------

  (define (run-records-procedural-tests)

    (let ([r (make-rtd3 1 2 3 4 5 6 7 8 9)])
      (test ((record-accessor rtd1 0) r) 3)
      (test ((record-accessor rtd1 1) r) 5)
      (test ((record-accessor rtd2 0) r) 9)
      (test ((record-accessor rtd2 1) r) 11)
      (test ((record-accessor rtd3 0) r) 15)
      (test ((record-accessor rtd3 1) r) 17))
    
    (test (point? p1) #t)
    (test (point-x p1) 1)
    (test (point-y p1) 2)
    (test/unspec (point-x-set! p1 5))
    (test (point-x p1) 5)
    
    (test (point? p2) #t)
    (test (point-x p2) 1)
    (test (point-y p2) 2)
    (test (point2-xx p2) 3)
    (test (point2-yy p2) 4)
    
    (test (point-x (make-point/abs -1 -2)) 1)
    (test (point-y (make-point/abs -1 -2)) 2)
    
    (test (cpoint-rgb (make-cpoint -1 -3 'red)) '(rgb . red))
    (test (point-x (make-cpoint -1 -3 'red)) -1)
    (test (point-x (make-cpoint/abs -1 -3 'red)) 1)
    
    ;;
    ))


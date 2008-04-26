#!r6rs

(library (tests r6rs records syntactic)
  (export run-records-syntactic-tests)
  (import (rnrs)
          (tests r6rs test))

  ;; ----------------------------------------

  (define-record-type (point make-point point?)
    (fields (immutable x point-x)
            (mutable y point-y set-point-y!))
    (nongenerative
     point-4893d957-e00b-11d9-817f-00111175eb9e))
  
  (define-record-type (cpoint make-cpoint cpoint?)
    (parent point)
    (protocol
     (lambda (n)
       (lambda (x y c) 
         ((n x y) (color->rgb c)))))
    (fields
     (mutable rgb cpoint-rgb cpoint-rgb-set!)))
  
  (define-record-type (cpoint2 make-cpoint2 cpoint2?)
    (parent-rtd (record-type-descriptor point)
                (record-constructor-descriptor point))
    (fields rgb)
    (opaque #f) (sealed #f))
  
  (define (color->rgb c)
    (cons 'rgb c))
  
  (define p1 (make-point 1 2))
  (define p2 (make-cpoint 3 4 'red))

  (define-record-type (ex1 make-ex1 ex1?)
    (protocol (lambda (p) (lambda a (p a))))
    (fields (immutable f ex1-f)))
  
  (define ex1-i1 (make-ex1 1 2 3))

  (define-record-type (ex2 make-ex2 ex2?)
    (protocol
     (lambda (p) (lambda (a . b) (p a b))))
    (fields (immutable a ex2-a)
            (immutable b ex2-b)))
  
  (define ex2-i1 (make-ex2 1 2 3))

  (define-record-type (unit-vector
                       make-unit-vector
                       unit-vector?)
    (protocol
     (lambda (p)
       (lambda (x y z)
         (let ((length 
                (sqrt (+ (* x x)
                         (* y y)
                         (* z z)))))
           (p (/ x length)
              (/ y length)
              (/ z length))))))
    (fields (immutable x unit-vector-x)
            (immutable y unit-vector-y)
            (immutable z unit-vector-z)))
  
  (define *ex3-instance* #f)
  
  (define-record-type ex3
    (parent cpoint)
    (protocol
     (lambda (n)
       (lambda (x y t)
         (let ((r ((n x y 'red) t)))
           (set! *ex3-instance* r)
           r))))
    (fields 
     (mutable thickness))
    (sealed #t) (opaque #t))
  
  (define ex3-i1 (make-ex3 1 2 17))
  
  ;; ----------------------------------------

  (define (run-records-syntactic-tests)
    (test (point? p1) #t)
    (test (point? p2) #t)
    (test (point? (vector)) #f)
    (test (point? (cons 'a 'b)) #f)
    (test (cpoint? p1) #f)
    (test (cpoint? p2) #t)
    (test (point-x p1) 1)
    (test (point-y p1) 2)
    (test (point-x p2) 3)
    (test (point-y p2) 4)
    (test (cpoint-rgb p2) '(rgb . red))

    (test/unspec (set-point-y! p1 17))
    (test (point-y p1) 17)

    (test (record-rtd p1) (record-type-descriptor point))
    (test point (record-type-descriptor point))
    
    (test (ex1-f ex1-i1) '(1 2 3))

    (test (ex2-a ex2-i1) 1)
    (test (ex2-b ex2-i1) '(2 3))

    (test (ex3? ex3-i1) #t)
    (test (cpoint-rgb ex3-i1) '(rgb . red))
    (test (ex3-thickness ex3-i1) 17)
    (test/unspec (ex3-thickness-set! ex3-i1 18))
    (test (ex3-thickness ex3-i1) 18)
    (test *ex3-instance* ex3-i1)

    (test (record? p1) #t)
    (test (record? ex3-i1) #f)

    (test (record-type-name point) 'point)
    (test (record-type-name cpoint2) 'cpoint2)
    (test (record-type-name ex1) 'ex1)

    (test (record-type-parent point) #f)
    (test (record-type-parent cpoint2) point)

    (test (record-type-uid point) 'point-4893d957-e00b-11d9-817f-00111175eb9e)
    (test/unspec (record-type-uid cpoint2))
    (test/unspec (record-type-uid ex1))

    (test (record-type-generative? point) #f)
    (test (record-type-generative? cpoint2) #t)
    (test (record-type-generative? ex1) #t)

    (test (record-type-sealed? point) #f)
    (test (record-type-sealed? ex3) #t)
      
    (test (record-type-opaque? point) #f)
    (test (record-type-opaque? ex3) #t)
      
    (test (record-type-field-names point) '#(x y))
    (test (record-type-field-names cpoint2) '#(rgb))
    
    (test (record-field-mutable? point 0) #f)
    (test (record-field-mutable? point 1) #t)
    (test (record-field-mutable? cpoint 0) #t)

    ;;
    ))


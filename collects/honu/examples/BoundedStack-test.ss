(define (push stack num)
  (send stack BoundedStack<%>-push (new IntegerC% [value num])))
  
(define s0 (emptyBoundedStack 5))
(define s1 (push s0 5))
(define s2 (push s1 3))
(define s3 (push s2 10))
(define s4 (push s3 20))
(define s5 (push s4 40))

(append (map interface? (list List<%>
                              Stack<%>
                              BoundedStack<%>
                              Integer<%>))
        (map class? (list ConsList%
                          BoundedStackC%
                          ListStackC%
                          IntegerC%))
        (map
         (lambda (object)
           (andmap
            (lambda (spec) (is-a? object spec))
            (list Stack<%> BoundedStack<%> BoundedStack<%>)))
         (list s0 s1 s2 s3 s4 s5))
        (list
         (not (send s0 BoundedStack<%>-isFull '()))
         (not (send s1 BoundedStack<%>-isFull '()))
         (not (send s2 BoundedStack<%>-isFull '()))
         (not (send s3 BoundedStack<%>-isFull '()))
         (not (send s4 BoundedStack<%>-isFull '()))
         (send s5 BoundedStack<%>-isFull '()))
        (list
         (let* ([expected (list 5 3 10 20 40)]
                [actual (list)])
           (send s5 BoundedStack<%>-foreach
                 (lambda (int)
                   (set! actual (cons (send int Integer<%>-value-get 'Dummy) actual))))
           (equal? expected actual))
         (with-handlers ([exn:fail? (lambda (exn) #t)])
           (push s5 50)
           #f)))

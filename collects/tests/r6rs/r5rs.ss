#!r6rs

(library (tests r6rs r5rs)
  (export run-r5rs-tests)
  (import (rnrs)
          (rnrs r5rs)
          (tests r6rs test))

  ;; ----------------------------------------

  (define a-stream
    (letrec ((next
              (lambda (n)
                (cons n (delay (next (+ n 1)))))))
      (next 0)))
  (define head car)
  (define tail
    (lambda (stream) (force (cdr stream))))

  (define count 0)
  (define p
    (delay (begin (set! count (+ count 1))
                  (if (> count x)
                      count
                      (force p)))))
  (define x 5)

  ;; ----------------------------------------

  (define (run-r5rs-tests)

    (test (modulo 13 4)            1)
    (test (remainder 13 4)         1)

    (test (modulo -13 4)           3)
    (test (remainder -13 4)        -1)
    
    (test (modulo 13 -4)           -3)
    (test (remainder 13 -4)        1)

    (test (modulo -13 -4)          -1)
    (test (remainder -13 -4)       -1)

    (test (remainder -13 -4.0)     -1.0)

    (test (force (delay (+ 1 2)))    3)
    
    (test (let ((p (delay (+ 1 2))))
            (list (force p) (force p)))  
          '(3 3))
    

    (test (head (tail (tail a-stream))) 2)

    (test/unspec p)
    (test (force p) 6)
    (test/unspec p)
    (test (begin (set! x 10)
                 (force p))      
          6)
    ;;
    ))


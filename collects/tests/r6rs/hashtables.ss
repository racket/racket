#!r6rs

(library (tests r6rs hashtables)
  (export run-hashtables-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-hashtables-tests)

    (let-values ([(kv vv)
                  (let ((h (make-eqv-hashtable)))
                    (hashtable-set! h 1 'one)
                    (hashtable-set! h 2 'two)
                    (hashtable-set! h 3 'three)
                    (hashtable-entries h))])
      (test (or (equal? (cons kv vv)
                        '(#(1 2 3)  . #(one two three)))
                (equal? (cons kv vv)
                        '(#(1 3 2)  . #(one three two)))
                (equal? (cons kv vv)
                        '(#(2 1 3)  . #(two one three)))
                (equal? (cons kv vv)
                        '(#(2 3 1)  . #(two three one)))
                (equal? (cons kv vv)
                        '(#(3 1 2)  . #(three one two)))
                (equal? (cons kv vv)
                        '(#(3 2 1)  . #(three two one))))
            #t))
    ;;
    ))


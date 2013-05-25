#!r6rs

(library (tests r6rs hashtables)
  (export run-hashtables-tests)
  (import (rnrs)
          (tests r6rs test))

  (define-syntax test-ht
    (syntax-rules ()
      [(_ mk key=? ([key val] ...)
          key/r orig-val new-val
          key/a a-val
          key/rm)
       (let ([h mk])
         (test (hashtable? h) #t)
         (test (hashtable-size h) 0)
         (test (hashtable-ref h key/r 'nope) 'nope)
         (test/unspec (hashtable-delete! h key)) ...
         (test (hashtable-size h) 0)

         (test (hashtable-ref h key/r 'nope) 'nope)
         (test (hashtable-contains? h key/r) #f)
         (test/unspec (hashtable-set! h key/r orig-val))
         (test (hashtable-ref h key/r 'nope) orig-val)
         (test (hashtable-contains? h key/r) #t)
         (test (hashtable-size h) 1)

         (test/unspec (hashtable-set! h key val)) ...
         (test (hashtable-size h) (length '(key ...)))
         (test (hashtable-ref h key/r 'nope) orig-val)
         (test (hashtable-ref h key 'nope) val) ...

         (let ([h1 (hashtable-copy h #t)]
               [h1i (hashtable-copy h)])
           (test (hashtable-mutable? h) #t)
           (test (hashtable-mutable? h1) #t)
           (test (hashtable-mutable? h1i) #f)
           
           (test (vector-length (hashtable-keys h))
                 (hashtable-size h))
           (test (vector-length (let-values ([(k e) (hashtable-entries h)])
                                  e))
                 (hashtable-size h))
           (test (exists (lambda (v) (key=? v key/r))
                         (vector->list (hashtable-keys h)))
                 #t)

           (test/unspec (hashtable-set! h key/r new-val))
           (test (hashtable-contains? h key/r) #t)
           (test (hashtable-ref h key/r 'nope) new-val)

           (test/unspec (hashtable-update! h key/r (lambda (v)
                                                     (test v new-val)
                                                     orig-val)
                                           'nope))
           (test (hashtable-ref h key/r 'nope) orig-val)
           (test/unspec (hashtable-update! h key/r (lambda (v)
                                                     (test v orig-val)
                                                     new-val)
                                           'nope))
           (test (hashtable-ref h key/r 'nope) new-val)

           (test/unspec (hashtable-update! h key/a (lambda (v)
                                                     (test v 'nope)
                                                     a-val)
                                           'nope))
           (test (hashtable-ref h key/a 'nope) a-val)
           (test/unspec (hashtable-delete! h key/a))

           (test (hashtable-contains? h key/rm) #t)
           (hashtable-delete! h key/rm)
           (test (hashtable-contains? h key/rm) #f)
           (test (hashtable-ref h key/rm 'nope) 'nope)

           (test (hashtable-ref h1 key 'nope) val) ...
           (test (hashtable-ref h1i key 'nope) val) ...
           (test (hashtable-contains? h1 key/rm) #t)
           (test (hashtable-contains? h1i key/rm) #t)

           (hashtable-clear! h)
           (test (hashtable-contains? h key) #f) ...
           (test (hashtable-contains? h1 key) #t) ...
           (test (hashtable-contains? h1i key) #t) ...

           (test/unspec (hashtable-clear! h1))
           
           (test/exn (hashtable-set! h1i key/r #f) &violation)
           (test/exn (hashtable-delete! h1i key/r) &violation)
           (test/exn (hashtable-update! h1i key/r (lambda (q) q) 'none) &violation)
           (test/exn (hashtable-clear! h1i) &violation)))]))
  
  ;; ----------------------------------------

  (define (run-hashtables-tests)

    (let-values ([(kv vv)
                  (let ((h (make-eqv-hashtable)))
                    (hashtable-set! h 1 'one)
                    (hashtable-set! h 2 'two)
                    (hashtable-set! h 3 'three)
                    (hashtable-entries h))])
      (test/alts (cons kv vv)
                 '(#(1 2 3)  . #(one two three))
                 '(#(1 3 2)  . #(one three two))
                 '(#(2 1 3)  . #(two one three))
                 '(#(2 3 1)  . #(two three one))
                 '(#(3 1 2)  . #(three one two))
                 '(#(3 2 1)  . #(three two one))))
    
    (test-ht (make-eq-hashtable) eq?
             (['a 7] ['b "bee"]
              [#t 8] [#f 9]
              ['c 123456789101112])
             'b "bee" "bumble"
             'd 12
             'c)
    
    (test-ht (make-eqv-hashtable) eqv?
             (['a 7] [#\b "bee"]
              [#t 8] [0.0 85]
              [123456789101112 'c])
             #\b "bee" "bumble"
             'd 12
             123456789101112)

    (let ([val-of (lambda (a)
                    (if (number? a) 
                        a 
                        (string->number a)))])
      (test-ht (make-hashtable val-of
                               (lambda (a b)
                                 (= (val-of a) (val-of b)))) 
               equal?
               ([1 'one]["2" 'two]
                [3 'three]["4" 'four])
               2 'two 'er
               5 'five
               4))

    (test (hashtable? (make-eq-hashtable 10)) #t)
    (test (hashtable? (make-eqv-hashtable 10)) #t)
    (test (hashtable? (make-hashtable (lambda (x) 0) equal? 10)) #t)
    
    (let ([zero (lambda (a) 0)]
          [same? (lambda (a b) #t)])
      (let ([ht (make-hashtable zero same?)])
        (test (hashtable-equivalence-function ht) same?)
        (test (hashtable-hash-function ht) zero)))

    (test (equal-hash "a") (equal-hash (make-string 1 #\a)))
    (test (equal-hash 1024) (equal-hash (expt 2 10)))
    (test (equal-hash '(1 2 3)) (equal-hash (list 1 2 3)))

    (test (string-hash "a") (string-hash (make-string 1 #\a)))
    (test (string-hash "aaaaa") (string-hash (make-string 5 #\a)))
    (test (string-ci-hash "aAaAA") (string-ci-hash (make-string 5 #\a)))
    (test (string-ci-hash "aAaAA") (string-ci-hash (make-string 5 #\A)))

    (test (symbol-hash 'a) (symbol-hash 'a))

    ;;
    ))


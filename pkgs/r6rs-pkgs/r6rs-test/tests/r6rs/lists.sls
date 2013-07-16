#!r6rs

(library (tests r6rs lists)
  (export run-lists-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-lists-tests)
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Tests originally from R6RS

    (test (find even? '(3 1 4 1 5 9)) 4)
    (test (find even? '(3 1 5 1 5 9)) #f)
    
    (test (for-all even? '()) #t)
    (test (for-all even? '(3 1 4 1 5 9)) #f)
    ;; (test (for-all even? '(3 1 4 1 5 9 . 2)) #f) ; removed from R6RS
    (test (for-all even? '(2 4 14)) #t)
    (test/exn (for-all even? '(2 4 14 . 9)) &assertion)
    (test (for-all (lambda (n) (and (even? n) n))
                   '(2 4 14)) 
          14)
    (test (for-all < '(1 2 3) '(2 3 4)) #t)
    (test (for-all < '(1 2 4) '(2 3 4)) #f)
    
    (test (exists even? '(3 1 4 1 5 9)) #t)
    (test (exists even? '(3 1 1 5 9)) #f)
    (test (exists even? '()) #f)
    (test/exn (exists even? '(3 1 1 5 9 . 2)) &assertion)
    (test (exists (lambda (n) (and (even? n) n)) '(2 1 4 14)) 2)
    (test (exists < '(1 2 4) '(2 3 4)) #t)
    (test (exists > '(1 2 3) '(2 3 4)) #f)

    (test (filter even? '(3 1 4 1 5 9 2 6)) '(4 2 6))

    (test/values (partition even? '(3 1 4 1 5 9 2 6)) '(4 2 6) '(3 1 1 5 9))

    (test (fold-left + 0 '(1 2 3 4 5)) 15)

    (test (fold-left (lambda (a e) (cons e a)) '()
                     '(1 2 3 4 5)) 
          '(5 4 3 2 1))

    (test (fold-left (lambda (count x)
                       (if (odd? x) (+ count 1) count))
                     0
                     '(3 1 4 1 5 9 2 6 5 3)) 
          7)
    (test (fold-left (lambda (max-len s)
                       (max max-len (string-length s)))
                     0
                     '("longest" "long" "longer")) 
          7)

    (test (fold-left cons '(q) '(a b c)) '((((q) . a) . b) . c))
    
    (test (fold-left + 0 '(1 2 3) '(4 5 6)) 21)
    
    (test (fold-right + 0 '(1 2 3 4 5)) 15)
    
    (test (fold-right cons '() '(1 2 3 4 5)) '(1 2 3 4 5))
    
    (test (fold-right (lambda (x l)
                        (if (odd? x) (cons x l) l))
                      '()
                      '(3 1 4 1 5 9 2 6 5))
          '(3 1 1 5 9 5))

    (test (fold-right cons '(q) '(a b c)) '(a b c q))

    (test (fold-right + 0 '(1 2 3) '(4 5 6)) 21)
    
    (test (remp even? '(3 1 4 1 5 9 2 6 5)) '(3 1 1 5 9 5))

    (test (remove 1 '(3 1 4 1 5 9 2 6 5)) '(3 4 5 9 2 6 5))
    
    (test (remv 1 '(3 1 4 1 5 9 2 6 5)) '(3 4 5 9 2 6 5))
    
    (test (remq 'foo '(bar foo baz)) '(bar baz))
    
    (test (memp even? '(3 1 4 1 5 9 2 6 5)) '(4 1 5 9 2 6 5))
    
    (test (memq 'a '(a b c))               '(a b c))
    (test (memq 'b '(a b c))               '(b c))
    (test (memq 'a '(b c d))               #f)
    (test (memq (list 'a) '(b (a) c))      #f)
    (test (member (list 'a) '(b (a) c))   '((a) c))
    (test/unspec (memq 101 '(100 101 102)))
    (test (memv 101 '(100 101 102)) '(101 102))

    (let ([d '((3 a) (1 b) (4 c))])
      (test (assp even? d) '(4 c))
      (test (assp odd? d) '(3 a)))

    (let ([e '((a 1) (b 2) (c 3))])
      (test (assq 'a e)      '(a 1))
      (test (assq 'b e)      '(b 2))
      (test (assq 'd e)      #f))


    (test (assq (list 'a) '(((a)) ((b)) ((c))))
          #f)
    (test (assoc (list 'a) '(((a)) ((b)) ((c))))   
          '((a)))
    (test/unspec (assq 5 '((2 3) (5 7) (11 13))))
    (test (assv 5 '((2 3) (5 7) (11 13))) '(5 7))

    (test (cons* 1 2 '(3 4 5)) '(1 2 3 4 5))
    (test (cons* 1 2 3) '(1 2 . 3))
    (test (cons* 1) 1)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Tests originally from Ikarus

    (test (for-all even? '(1 2 3 4)) #f)
    (test (for-all even? '(10 12 14 16)) #t)
    (test (for-all even? '(2 3 4)) #f)
    (test (for-all even? '(12 14 16)) #t)
    (test (for-all (lambda (x) x) '(12 14 16)) 16)
    (test (for-all (lambda (x) x) '(12 14)) 14)
    (test (for-all (lambda (x) x) '(12)) 12)
    (test (for-all (lambda (x) x) '()) #t)
    ;; (test (for-all even? '(13 . 14)) #f) ; removed from R6RS
    (test (for-all cons '(1 2 3) '(a b c)) '(3 . c))
    (test (for-all (lambda (a b) (= a 1)) '(1 2 3) '(a b c)) #f)
    ;; R6RS merely says that this *should* work, but not must:
    ;; (test (for-all (lambda (a b) (= a 1)) '(1 2) '(a b c)) #f)
    (test (fold-left + 0 '(1 2 3 4 5)) 15)
    (test (fold-left (lambda (a b) (cons b a)) '() '(1 2 3 4 5))
          '(5 4 3 2 1))
    (test (fold-left (lambda (count x)
                       (if (odd? x) 
                           (+ count 1) 
                           count))
                     0 
                     '(3 1 4 1 5 9 2 6 5 3))
          7)
    (test (fold-left cons '(q) '(a b c)) '((((q) . a) . b) . c))
    (test (fold-left + 0 '(1 2 3) '(4 5 6)) 21)
    (test (fold-right + 0 '(1 2 3 4 5)) 15)
    (test (fold-right cons '() '(1 2 3 4 5))
          '(1 2 3 4 5))
    (test (fold-right (lambda (x l)
                        (if (odd? x) 
                            (cons x l)
                            l))
                      '()
                      '(3 1 4 1 5 9 2 6 5 3))
          '(3 1 1 5 9 5 3))
    (test (fold-right + 0 '(1 2 3) '(4 5 6)) 21)

    ;;
    ))

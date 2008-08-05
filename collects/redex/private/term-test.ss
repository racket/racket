(module term-test mzscheme
  (require "term.ss"
           "matcher.ss"
           "test-util.ss")
  
  (reset-count)
  (test (term 1) 1)
  (test (term (1 2)) (list 1 2))
  (test (term (1 ,(+ 1 1))) (list 1 2))
  (test (term-let ([x 1]) (term (x x))) (list 1 1))
  (test (term-let ([(x ...) (list 1 2 3)]) (term ((y x) ...))) '((y 1) (y 2) (y 3)))
  
  (test (term (in-hole (1 hole) 2)) (term (1 2)))
  (test (term (in-hole (1 hole (hole x)) 2)) (term (1 2 (hole x))))
  
  (test (equal? (term hole) (term hole)) #t)
  (test (hole? (term hole)) #t)
  (test (hole? (term (hole #f))) #f)
  (test (hole? (term (hole the-name))) #f)
  
  (test (term-let-fn ((f (lambda (q) q)))
                     (term (f 1 2 3)))
        (term (1 2 3)))
  
  (test (term-let-fn ((f (lambda (q) `(y ,(car q)))))
                     (term (f (zzzz))))
        (term (y (zzzz))))
  
  (test (term-let-fn ((f (λ (x) (add1 (car x)))))
                     (term (f 2)))
        (term 3))
  
  (test (with-syntax ([((x ...) ...) (list (list 1 1) (list 2 2) (list 3 3))])
          (term-let-fn ((f (λ (x) (car x))))
                       (term ((qq (f x) ...) ...))))
        (term ((qq 1 1) (qq 2 2) (qq 3 3))))
  
  (test (term-let-fn ((f (lambda (x) (car x))))
                     (term (f hole)))
        (term hole))
  
  (test (term-let-fn ((f (lambda (q) `(y ,(car q)))))
                     (term-let-fn ((g (lambda (x) `(ff ,(car x)))))
                                  (term (g (f (zzzz))))))
        (term (ff (y (zzzz)))))
  
  (test (term-let-fn ((f (lambda (q) `(y ,(car q)))))
                     (term-let-fn ((g (lambda (x) `(ff ,(car x)))))
                                  (term (f (g (f (zzzz)))))))
        (term (y (ff (y (zzzz))))))
  
  (test (term-let ([x 1])
          (term (x . y)))
        (term (1 . y)))
  
  (test (term-let ([(x ...) (list 3 2 1)])
          (term (x ... . y)))
        (term (3 2 1 . y)))
  
  (test (term-let ([(x . y) (cons 1 2)])
          (term (x y)))
        (term (1 2)))
  
  ;; test that the implicit `plug' inserted by `in-hole' 
  ;; deals with ellipses properly
  (test (term-let ([(E ...) '(1 2 3)])
          (term ((in-hole E x) ...)))
        (term (1 2 3)))
  
  (fprintf (current-error-port) "term-test.ss commented out test that fails; matches PR 8765\n")
  
  #;
  (test (term-let-fn ((metafun (λ (x) x)))
                     (term-let ((x 'whatever)
                                ((y ...) '(4 5 6)))
                       (term (((metafun x) y) ...))))
        '((whatever 4) (whatever 5) (whatever 6)))
  
  (print-tests-passed 'term-test.ss))
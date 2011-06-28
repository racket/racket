#lang racket
(require rackunit
         datalog/ast
         datalog/private/variant)
(require/expose datalog/private/variant
                (variant-terms variant-term variant-var variant? term-hash mk-literal-hash))

(provide variant-tests)

(define (test-not-equal? n v1 v2)
  (test-case n (check-not-equal? v1 v2)))

(define variant-tests
  (test-suite
   "variant"
   
   (test-suite 
    "variant?"
    (test-not-false "same" (variant? (make-literal #f 'lit1 empty) (make-literal #f 'lit1 empty)))
    (test-false "dif lit" (variant? (make-literal #f 'lit1 empty) (make-literal #f 'lit2 empty)))
    (test-not-false "same" (variant? (make-literal #f 'lit1 (list (make-constant #f 'k1))) 
                                     (make-literal #f 'lit1 (list (make-constant #f 'k1)))))
    (test-false "dif con" (variant? (make-literal #f 'lit1 (list (make-constant #f 'k1))) 
                                    (make-literal #f 'lit1 (list (make-constant #f 'k2)))))
    (test-false "dif var/con" (variant? (make-literal #f 'lit1 (list (make-variable #f 'v1))) 
                                        (make-literal #f 'lit1 (list (make-constant #f 'k1)))))
    (test-false "dif con/var" (variant? (make-literal #f 'lit1 (list (make-constant #f 'k1))) 
                                        (make-literal #f 'lit1 (list (make-variable #f 'v1)))))
    (test-not-false "same" (variant? (make-literal #f 'lit1 (list (make-variable #f 'v1))) 
                                     (make-literal #f 'lit1 (list (make-variable #f 'v1)))))
    
    (test-not-false "var (dif name)" (variant? (make-literal #f 'lit1 (list (make-variable #f 'v2))) 
                                               (make-literal #f 'lit1 (list (make-variable #f 'v1))))))
   
   (test-suite
    "mem-literal"
    (test-false "mt" (mem-literal (make-literal #f 'lit1 empty) empty))
    (test-not-false "in" (mem-literal (make-literal #f 'lit1 empty) (list (make-literal #f 'lit1 empty))))
    (test-not-false "var" (mem-literal (make-literal #f 'lit1 (list (make-variable #f 'v2))) 
                                       (list (make-literal #f 'lit1 (list (make-variable #f 'v1)))))))
   
   (test-suite
    "term-hash"
    (test-equal? "var" (term-hash (make-variable #f (gensym)) equal-hash-code) 101)
    (test-equal? "con" (term-hash (make-constant #f 'v2) equal-hash-code) (equal-hash-code 'v2)))
   
   (local [(define literal-hash (mk-literal-hash equal-hash-code))
           (define (literal-hash-equal? l1 l2)
             (equal? (literal-hash l1) (literal-hash l2)))]
     (test-suite
      "mk-literal-hash"
      (test-not-false "same" (literal-hash-equal? (make-literal #f 'lit1 empty) (make-literal #f 'lit1 empty)))
      (test-not-false "same" (literal-hash-equal? (make-literal #f 'lit1 (list (make-constant #f 'k1))) 
                                                  (make-literal #f 'lit1 (list (make-constant #f 'k1)))))
      (test-not-false "same" (literal-hash-equal? (make-literal #f 'lit1 (list (make-variable #f 'v1))) 
                                                  (make-literal #f 'lit1 (list (make-variable #f 'v1)))))
      
      (test-not-false "var (dif name)" (literal-hash-equal? (make-literal #f 'lit1 (list (make-variable #f 'v2))) 
                                                            (make-literal #f 'lit1 (list (make-variable #f 'v1)))))))))

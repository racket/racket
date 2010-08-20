#lang plai
(require (prefix-in eli: tests/eli-tester))

(define-type A
  [mta]
  [a (b B?)])

(define-type B
  [mtb]
  [b (a A?)])

(define-type T
  [i (f number?)])

(define-type T1
  [i1 (f (car 1))])

(define-type DefrdSub
  [mtSub]
  [aSub (value boolean?)])

(define (lookup ds the-name)
  (type-case DefrdSub ds
             [mtSub () 1]
             [aSub (a-name) 2]))

(define-type t (c))

(define-type t1 (c1 (n number?)))


(eli:test
 (i 4)
 
 (regexp-match "\\(exception \\(make-i #f\\) \".+/collects/tests/plai/datatype\\.rkt:13\\.3: use broke the contract \\(-> number\\? i\\?\\) on make-i given to \\\\n  \\(file\\\\n   .+/collects/tests/plai/datatype\\.rkt\\)\\\\n; expected <number\\?>, given: #f\" '<no-expected-value> \"at line 36\"\\)"
               (with-output-to-string (λ () (test/exn (make-i #f) "contract"))))
 
 (regexp-match "\\(exception \\(i-f #f\\) \".+/collects/tests/plai/datatype\\.rkt:13\\.6: use broke the contract \\(-> i\\? number\\?\\) on i-f given to \\\\n  \\(file\\\\n   .+/collects/tests/plai/datatype\\.rkt\\)\\\\n; expected <i\\?>, given: #f\" '<no-expected-value> \"at line 39\"\\)"
               (with-output-to-string (λ () (test/exn (i-f #f) "contract"))))
 
 
 (type-case A (mta)
            [mta () 1]
            [a (x) 2])
 =>
 1
 
 (regexp-match "\\(exception \\(c1 \\(quote not-a-number\\)\\) \".+/collects/tests/plai/datatype\\.rkt:29\\.17: use broke the contract \\(-> number\\? c1\\?\\) on c1 given to \\\\n  \\(file\\\\n   .+/plt/collects/tests/plai/datatype\\.rkt\\)\\\\n; expected <number\\?>, given: 'not-a-number\" '<no-expected-value> \"at line 49\"\\)"
               (with-output-to-string (λ () (test (c1 'not-a-number) (list 5)))))
 
 (regexp-match (regexp-quote "(exception (type-case t (list 1) (c () 1)) \"type-case: expected a value from type t, got: (1)\" '<no-expected-value> \"at line 53\")")
               (with-output-to-string (λ ()
                                        (test/exn
                                         (type-case t (list 1) (c () 1))
                                         "expected")))))
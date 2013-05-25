#lang plai
(require (prefix-in eli: tests/eli-tester)
         "util.rkt")

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
 
 (regexp-match "\\(exception \\(make-i #f\\) \"make-i.+\" '<no-expected-value> \"at line 36\"\\)"
               (with-both-output-to-string (λ () (test/exn (make-i #f) "contract"))))
 
 (regexp-match "\\(exception \\(i-f #f\\) \"i-f.+\" '<no-expected-value> \"at line 39\"\\)"
               (with-both-output-to-string (λ () (test/exn (i-f #f) "contract"))))
 
 
 (type-case A (mta)
            [mta () 1]
            [a (x) 2])
 =>
 1
 
 (regexp-match "\\(exception \\(c1 \\(quote not-a-number\\)\\) \"c1.+\" '<no-expected-value> \"at line 49\"\\)"
               (with-both-output-to-string (λ () (test (c1 'not-a-number) (list 5)))))
 
 (regexp-match (regexp-quote "(exception (type-case t (list 1) (c () 1)) \"type-case: expected a value from type t, got: (1)\" '<no-expected-value> \"at line 53\")")
               (with-both-output-to-string (λ ()
                                             (test/exn
                                              (type-case t (list 1) (c () 1))
                                              "expected"))))
 
 (type-case "foo" "bar") =error> "this must be a type defined with define-type"
 
 (type-case + "bar") =error> "this must be a type defined with define-type"

 (type-case #f [x () 1]) =error> "this must be a type defined with define-type"
 )

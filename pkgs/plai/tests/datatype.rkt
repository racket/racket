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

(define ((ERR f) line#)
  (format f (+ line# THIS-LINE#)))

(define ERR1
  (ERR "\\(exception \\(make-i #f\\) \"make-i.+\" '<no-expected-value> \"at line ~a\"\\)"))

(define ERR2
  (ERR "\\(exception \\(i-f #f\\) \"i-f.+\" '<no-expected-value> \"at line ~a\"\\)"))

(define ERR3 
  (ERR
   "\\(exception \\(c1 \\(quote not-a-number\\)\\) \"c1.+\" '<no-expected-value> \"at line ~a\"\\)"))

(define (ERR4 line#)
  (string-append 
   (regexp-quote "(exception (type-case t (list 1) (c () 1)) \"")
   (regexp-quote "type-case: expected a value from type t, got: '(1)\"")
   (regexp-quote " '<no-expected-value> \"at line ")
   ((ERR "~a") line#)
   (regexp-quote "\")")))

(define THIS-LINE# 53)

(eli:test
 (i 4)
 (regexp-match (ERR1 4) (with-both-output-to-string (λ () (test/exn (make-i #f) "contract"))))
 (regexp-match (ERR2 5) (with-both-output-to-string (λ () (test/exn (i-f #f) "contract"))))
 (type-case A (mta) [mta () 1] [a (x) 2])
 =>
 1
 (regexp-match (ERR3 9) (with-both-output-to-string (λ () (test (c1 'not-a-number) (list 5)))))
 
 (regexp-match 
  (ERR4 (+ 12 1))
  (with-both-output-to-string (λ () (test/exn (type-case t (list 1) (c () 1)) "expected"))))

 (type-case "foo" "bar") =error> "this must be a type defined with define-type"
 (type-case + "bar") =error> "this must be a type defined with define-type"
 (type-case #f [x () 1]) =error> "this must be a type defined with define-type")

#lang racket/base
(require (only-in plai define-type)
         racket/contract
         mzlib/pconvert
         racket/pretty)

(define-type Foo
  [bar (v any/c)
       (v2 any/c)])

(define (to-string print v)
  (let ([s (open-output-string)])
    (print v s)
    (get-output-string s)))

(define success 0)
(define (check a b) 
  (if (equal? a b) 
      (set! success (add1 success))
      (error 'check "failed: ~s vs. ~s" a b)))

(check (to-string print Foo?) "#<procedure:Foo?>")
(check (to-string write Foo?) "#<procedure:Foo?>")
(check (to-string display Foo?) "#<procedure:Foo?>")

(check (to-string print bar?) "#<procedure:bar?>")
(check (to-string write bar?) "#<procedure:bar?>")
(check (to-string display bar?) "#<procedure:bar?>")

(check (to-string print (bar "a" 'b)) "(bar \"a\" 'b)")
(check (to-string write (bar "a" 'b)) "#(struct:bar \"a\" b)")
(check (to-string display (bar "a" 'b)) "#(struct:bar a b)")

(check (to-string print (list (bar "a" (list 'b)))) "(list (bar \"a\" '(b)))")
(check (to-string write (list (bar "a" (list 'b)))) "(#(struct:bar \"a\" (b)))")
(check (to-string display (list (bar "a" (list 'b)))) "(#(struct:bar a (b)))")

;; Check `print-convert' plus `pretty-write'
;; as used by DrRacket's "constructor" printing mode:
(constructor-style-printing #t)
(check (to-string pretty-write (print-convert '(a b))) "(list 'a 'b)\n")
(check (to-string pretty-write (print-convert (bar "a" 'b))) "(bar \"a\" 'b)\n")
;; "quasiquote" printing mode:
(constructor-style-printing #f)
(check (to-string pretty-write (print-convert '(a b))) "`(a b)\n")
(check (to-string pretty-write (print-convert (bar "a" 'b))) "(bar \"a\" 'b)\n")

(printf "~a tests passed.\n" success)

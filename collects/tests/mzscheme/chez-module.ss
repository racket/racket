
;; Run this file with -r, and inspect the
;; printouts.

(module helpers mzscheme
  (require (lib "package.ss"))

  (provide identifier-syntax with-implicit
	   (rename package module)
	   (rename open import))

  (define-syntax (identifier-syntax stx)
    (syntax-case stx ()
      [(_ id) (if (identifier? #'id)
		  #'(make-rename-transformer (quote-syntax id))
		  ;; Cheating in this case... examples only
		  ;; use this in a non-applied position
		  #'(lambda (stx) (quote-syntax id)))]))

  (define-syntax with-implicit
    (syntax-rules ()
      [(_ (orig id ...) body)
       (with-syntax ([id (datum->syntax-object #'orig (syntax-e #'id))]
		     ...)
	 body)])))
    

(require helpers)
(require-for-syntax helpers)

;; Make evaluation print the result, for testing
(let ([eh (current-eval)])
  (current-eval (lambda (x)
		  (let ([v (eh x)])
		    (unless (void? v)
		      (printf "~s~n" v))
		    v))))

(read-case-sensitive #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From "Extending the Scope of Syntactic Abstraction"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((x 1))
  (module M (x setter)
    (define-syntax x (identifier-syntax z))
    (define setter (lambda (x) (set! z x)))
    (define z 5))
  (let ((y x) (z 0))
    (import M)
    (setter 3) 
    (list x y z)))
"(3 1 0) is correct"


(define-syntax from
  (syntax-rules ()
    ((_ M id) (let () (import M) id))))
(let ((x 10))
  (module m1 (x) (define x 1))
  (module m2 (x) (define x 2))
  (list (from m1 x) (from m2 x)))
"(1 2) is correct"

(define-syntax module*
  (syntax-rules ()
    [(_ (id ...) form ...)
     (begin
       (module tmp (id ...) form ...)
       (import tmp))]
    [(_ name (id ...) form ...)
     (module name (id ...) form ...)]))
(let ([w 88])
  (module* (f)
   (define (f) w)
   (define w 17))
  (list (f) w))
"(17 88) is correct"

(define-syntax define-alias
  (syntax-rules ()
    [(_ x y)
     (define-syntax x 
       (identifier-syntax y))]))

(define-syntax import*
  (syntax-rules ()
    [(_ M) (begin)]
    [(_ M (new old))
     (module* (new)
      (define-alias new tmp)
      (module* (tmp)
       (import M)
       (define-alias tmp old)))]
    [(_ M id) (module* (id) (import M))]
    [(_ M spec0 spec1 ...)
     (begin (import* M spec0)
            (import* M spec1 ...))]))
(module m (x y z)
  (define x 'x)
  (define y 'y)
  (define z 'z))
(import* m x (y z) (z y))
(list x y z)
"(x z y) is correct"

(module A (x y)
  (define x 1)
  (define y 2))
(module B (y z)
  (define y 3) 
  (define z 4))
(module C (a b c d)
  (import* A (a x) (b y))
  (import* B (c y) (d z)))
(module D (a c) 
  (import C)) 
(module E (b d) 
  (import C))
(let ([a 'a]
      [b 'b]
      [c 'c]
      [d 'd])
  (import D)
  (list a b c d))
"(1 b 3 d) is correct"
(let ([a 'a]
      [b 'b]
      [c 'c]
      [d 'd])
  (import E)
  (list a b c d))
"(a 2 c 4) is correct"

(module* (A B)
 (module A (x) 
   (define x (lambda () y)))
 (module B (y)
   (define y (lambda () x)))
 (import A)
 (import B))
(import A)
(import B)
(and (eq? x (y)) (eq? (y) x))
"#t is correct"


(define-syntax rec-modules
  (syntax-rules ()
    ((_ (module N (id ...) form ...) ...)
     (module* (N ...)
      (module N (id ...) form ...) ...
      (import N) ...))))
(rec-modules
 (module O (odd)
   (define (odd x)
     (if (zero? x) #f (even (sub1 x)))))
 (module E (even)
   (define (even x)
     (if (zero? x) #t (odd (sub1 x))))))
(import O)
(list (odd 17) (odd 32))
"(#t #f) is correct"


(define-syntax define-interface
  (syntax-rules ()
    [(_ name (export ...))
     (define-syntax name
       (lambda (x)
	 (syntax-case x ()
	   [(_ n defs)
	    (with-implicit (n export ...)
	     #'(module n (export ...) . defs))])))]))
(define-syntax define-module
  (syntax-rules ()
    [(_ name interface defn ...)
     (interface name (defn ...))]))
(define-interface simple (a b))
(define-module M simple
  (define-syntax a (identifier-syntax 1))
  (define b (lambda () c))
  (define c 2))
(let ()
  (import M)
  (list a (b)))
"(1 2) is right"

(define-syntax define-interface
  (syntax-rules (compound-interface)
    [(_ name (compound-interface i0 i1 ...))
     (d-i-help name (i0 i1 ...) ())]
    [(_ name (export ...))
     (define-syntax name
       (lambda (x)
	 (syntax-case x (expand-exports)
	   [(_ n defs)
	    (with-implicit (n export ...)
	     #'(module n (export ...) . defs))]
	   [(_ (expand-exports i-name mac)) 
	    (with-implicit (i-name export ...)
	     #'(mac i-name export ...))])))]))
(define-syntax d-i-help
  (syntax-rules ()
    [(_ name () (export ...))
     (define-interface name (export ...))]
    [(_ name (i0 i1 ...) (e ...))
     (begin
       (define-syntax tmp
	 (syntax-rules ()
	   [(_ name expt (... ...))
	    (d-i-help name (i1 ...)
		      (e ... expt (... ...)))]))
       (i0 (expand-exports name tmp)))]))
(define-syntax define-module
  (syntax-rules (compound-interface)
    [(_ name (compound-interface i ...) defn ...)
     (begin
       (define-interface tmp(compound-interface i ...))
       (define-module name tmp defn ...))]
    [(_ name interface defn ...) (interface name (defn ...))])) 
(define-interface one (a b))
(define-interface two (c d))
(define-interface both
  (compound-interface one two))
(define-module M (compound-interface one two)
  (define a 1)
  (define b 2)
  (define c 3) 
  (define d 4))
(let () 
  (import M) 
  (list a b c d))
"(1 2 3 4) is correct"

(define-syntax declare
  (syntax-rules ()
    [(_ id) (define id (void))]))
(define-syntax satisfy
  (syntax-rules ()
    [(_ id val) (set! id val)]))

(define-syntax abstract-module
  (syntax-rules ()
    ((_ name (ex ...) (mac ...) defn ...)
     (module name (ex ... mac ...)
       (declare ex) 
       ... defn ...))))
(define-syntax implement
  (syntax-rules ()
    ((_ name form ...)
     (module* () 
      (import name)
      form ...))))
(abstract-module E (even?) ())
(abstract-module 
 O (odd?) (pred)
 (define-syntax pred
   (syntax-rules () ((_ exp) (- exp 1)))))
(implement E
 (import O)
 (satisfy even? 
  (lambda (x) (or (zero? x) (odd? (pred x))))))
(implement O
 (import E)
 (satisfy 
  odd?
  (lambda (x) (not (even? x))))) 
(import O)
(list (odd? 10) (odd? 13))
"(#f #t) is correct"


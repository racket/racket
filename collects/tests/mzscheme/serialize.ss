

(load-relative "loadtest.ss")

(Section 'serialization)

(require (lib "serialize.ss"))

;; ----------------------------------------

(define insp (current-inspector))

(define-serializable-struct a () insp)
(define-serializable-struct b (x y) insp)
(define-serializable-struct (c a) (z) insp)
(define-serializable-struct (d b) (w) insp)

(define (same? v1 v2)
  (define ht (make-hash-table))
  (let loop ([v1 v1][v2 v2])
    (cond
     [(hash-table-get ht v1 (lambda () #f))
      => (lambda (x) (eq? x v2))]
     [(and (a? v1)
	   (a? v2)
	   (not (c? v1))
	   (not (c? v2)))
      #t]
     [(and (b? v1)
	   (b? v2)
	   (not (d? v1))
	   (not (d? v2)))
      (hash-table-put! ht v1 v2)
      (and (loop (b-x v1) (b-x v2))
	   (loop (b-y v1) (b-y v2)))]
     [(and (c? v1) (c? v2))
      (hash-table-put! ht v1 v2)
      (loop (c-z v1) (c-z v2))]
     [(and (d? v1) (d? v2))
      (hash-table-put! ht v1 v2)
      (and (loop (b-x v1) (b-x v2))
	   (loop (b-y v1) (b-y v2))
	   (loop (d-w v1) (d-w v2)))]
     [(and (pair? v1)
	   (pair? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (loop (car v1) (car v2))
	   (loop (cdr v1) (cdr v2)))]
     [(and (vector? v1)
	   (vector? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (= (vector-length v1) (vector-length v2))
	   (andmap loop
		   (vector->list v1)
		   (vector->list v2)))]
     [(and (box? v1) (box? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (loop (unbox v1) (unbox v2)))]
     [(and (hash-table? v1) (hash-table? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (eq? (hash-table? v1 'equal) (hash-table? v2 'equal))
	   (eq? (hash-table? v1 'weak) (hash-table? v2 'weak))
	   (= (hash-table-count v1) (hash-table-count v2))
	   (let ([ok? #t])
	     (hash-table-for-each
	      v1
	      (lambda (k v)
		(set! ok?
		      (and ok?
			   (loop v (hash-table-get v2 k (lambda () 'not-found)))))))
	     ok?))]
     [(and (date? v1) (date? v2))
      (hash-table-put! ht v1 v2)
      (andmap loop 
	      (vector->list (struct->vector v1))
	      (vector->list (struct->vector v2)))]
     [(and (arity-at-least? v1) (arity-at-least? v2))
      (hash-table-put! ht v1 v2)
      (loop (arity-at-least-value v1)
	    (arity-at-least-value v2))]
     [else
      (and (equal? v1 v2)
	   (eq? (immutable? v1) (immutable? v2)))])))
      

(define (test-ser v)
  (parameterize ([print-graph #t])
    (test #t serializable? v)
    (test #t same? v v)
    (test #t same? v (deserialize (serialize v)))))

(define (mk-ht . args)
  (let ([ht (apply make-hash-table args)])
    (hash-table-put! ht 'apple 'ok)
    (hash-table-put! ht 'banana 'better)
    ht))

;; ----------------------------------------

(test-ser 1)
(test-ser "apple")
(test-ser (string-copy "apple"))
(test-ser #"apple")
(test-ser (bytes-copy #"apple"))
(test-ser #\c)
(test-ser 145.79)
(test-ser 2/3)
(test-ser #t)
(test-ser #f)
(test-ser (void))
(test-ser 'ok)
(test-ser null)
(test-ser (current-directory))
(test-ser (seconds->date (current-seconds)))
(test-ser (procedure-arity (lambda (x . y) 10)))
(test-ser (make-immutable-hash-table '((1 . a) (2 . b))))
(test-ser (make-immutable-hash-table '(("x" . a) ("y" . b)) 'equal))
(test-ser (mk-ht))
(test-ser (mk-ht 'equal))
(test-ser (mk-ht 'weak))
(test-ser (mk-ht 'equal 'weak))

(test-ser '(1))
(test-ser '#(1))
(test-ser '#&1)

(test-ser (cons 1 2))
(test-ser (cons-immutable 1 2))
(test-ser (vector))
(test-ser (vector 1 2))
(test-ser (vector-immutable))
(test-ser (vector-immutable 1 2))
(test-ser (box 10))
(test-ser (box-immutable 10))

(test-ser (make-a))
(test-ser (make-b 1 2))
(test-ser (make-c 30))
(test-ser (make-d 100 200 300))

;; Simple sharing
(let ([p (cons 1 2)])
  (test-ser (cons p p))
  (test-ser (vector p p))
  (test-ser (make-b p p))
  (test-ser (make-d p 1 p)))
(let ([p (vector 1 2 3)])
  (test-ser (cons p p))
  (test-ser (vector p p))
  (test-ser (make-b p p))
  (test-ser (make-d p 1 p)))
(let ([p (box 1)])
  (test-ser (cons p p))
  (test-ser (vector p p))
  (test-ser (make-b p p))
  (test-ser (make-d p 1 p)))
(let ([p (make-a)])
  (test-ser (cons p p)))
(let ([p (make-d 1 2 3)])
  (test-ser (vector p p p)))
(let ([p (seconds->date (current-seconds))])
  (test-ser (cons p p)))
(let ([p (make-arity-at-least 10)])
  (test-ser (cons p p)))
(let ([p (mk-ht)])
  (test-ser (cons p p)))

;; Cycles
(let ([p (cons 1 2)])
  (set-car! p p)
  (test-ser p)
  (set-cdr! p p)
  (test-ser p)
  (test-ser (make-c p))
  (test-ser (make-b p p)))
(let ([p (vector 1 2 3)])
  (vector-set! p 1 p)
  (test-ser p)
  (vector-set! p 2 p)
  (test-ser p)
  (test-ser (make-c p))
  (test-ser (make-b p p)))
(let ([p (box 1)])
  (set-box! p p)
  (test-ser p)
  (test-ser (make-c p))
  (test-ser (make-b p p)))
(let ([p (make-c 1)])
  (set-c-z! p p)
  (test-ser p)
  (test-ser (make-c p)))
(let ([p (make-b 1 2)])
  (set-b-x! p p)
  (test-ser p)
  (set-b-y! p p)
  (test-ser p)
  (set-b-x! p 1)
  (test-ser p)
  (test-ser (make-c p)))
(let ([p (make-d 1 2 3)])
  (set-b-x! p p)
  (test-ser p)
  (set-b-y! p p)
  (test-ser p)
  (set-d-w! p p)
  (test-ser p)
  (set-b-x! p 1)
  (test-ser p)
  (set-b-y! p 2)
  (test-ser p)
  (test-ser (make-c p)))
(let ([p (seconds->date (current-seconds))])
  (set-date-second! p p)
  (test-ser p)
  (test-ser (cons p p)))
(let ([p (make-arity-at-least 10)])
  (set-arity-at-least-value! p p)
  (test-ser p)
  (test-ser (cons p p)))
(let ([p (mk-ht)])
  (hash-table-put! p 'banana p)
  (test-ser p)
  (test-ser (cons p p)))

;; Cycles with immutable parts
(let* ([p1 (cons 1 2)]
       [p2 (cons-immutable 0 p1)])
  (set-cdr! p1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (cons p1 p2))
  (test-ser (cons p2 p1))
  (test-ser (make-c p1))
  (test-ser (make-b p1 p2))
  (test-ser (make-b p2 p1)))
(let* ([p1 (vector 1 2 3)]
       [p2 (vector-immutable 0 p1 4)])
  (vector-set! p1 1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (make-c p1))
  (test-ser (make-b p1 p2))
  (test-ser (make-b p2 p1)))
(let* ([p1 (box 1)]
       [p2 (box-immutable p1)])
  (set-box! p1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (make-c p1))
  (test-ser (make-b p1 p2))
  (test-ser (make-b p2 p1)))
(let* ([p1 (cons 1 2)]
       [p2 (make-immutable-hash-table
	    (cons (cons 'x p1) '((a . 2) (b . 4))))])
  (set-cdr! p1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (cons p1 p2))
  (test-ser (cons p2 p1)))
      
;; ----------------------------------------

(module ser-mod mzscheme
   (require (lib "serialize.ss"))
   (provide ser-mod-test)

   (define-serializable-struct foo (a b))

   (define (ser-mod-test)
     (foo-a (deserialize (serialize (make-foo 1 2))))))

(require ser-mod)
(test 1 ser-mod-test)

;; ----------------------------------------
;; Classes

(require (lib "class.ss"))

(define-serializable-class s:c% object%
  (init-field [one 0])
  (define f1 one)
  (define f2 1)
  (define/public (get-f1) f1)
  (define/public (get-f2) f2)
  (define/public (set-f1 v) (set! f1 v))
  (define/public (set-f2 v) (set! f2 v))
  (super-new))

(let ([o (new s:c% [one 17])])
  (test 17 'o (send (deserialize (serialize o)) get-f1))
  (test 1 'o (send (deserialize (serialize o)) get-f2)))

(define-serializable-class s:d% s:c% object%
  (define f3 3)
  (define/public (get-f3) f3)
  (define/public (set-f3 v) (set! f3 v))
  (super-new [one (+ f3 4)]))

(let ([o (new s:d%)])
  (test 3 'o (send (deserialize (serialize o)) get-f3))
  (test 7 'o (send (deserialize (serialize o)) get-f1))
  (test 1 'o (send (deserialize (serialize o)) get-f2)))

(let* ([e% (class s:d% (define goo 12) (super-new))]
       [o (new e%)])
  (test #f is-a? (deserialize (serialize o)) e%)
  (test #t is-a? (deserialize (serialize o)) s:d%))

;; Can't define serializable from non-transparent, non-externalizable<%>:
(test 'right-error 'dsc
      (with-handlers ([exn:fail:object? (lambda (x) 'right-error)])
	(eval #'(define-serializable-class foo% (class s:d% (super-new))))))

(define s:transparent%
  (class object%
    (inspect #f)
    (define f1 12)
    (define/public (get-f1) f1)
    (define/public (set-f1 v) (set! f1 v))
    (super-new)))

(err/rt-test (serialize (new s:transparent%)))

(define-serializable-class s:g% s:transparent%
  (define f2 18)
  (define/public (get-f2) f2)
  (define/public (set-f2 v) (set! f2 v))
  (super-new))

;; Adding more transperant was ok, and check cycle:
(let ([o (new s:g%)])
  (send o set-f1 6)
  (send o set-f2 16)
  (test #t is-a? (deserialize (serialize o)) s:g%)
  (test 6 'o (send (deserialize (serialize o)) get-f1))
  (test 16 'o (send (deserialize (serialize o)) get-f2))
  (test #(struct:object:s:g% 6 ...) struct->vector o))

(define-serializable-class s:h% (class s:g%
				  (inspect #f)
				  (define hoo 34)
				  (super-new))
  (define f3 80)
  (define/public (get-f3) f3)
  (define/public (set-f3 v) (set! f3 v))
  (super-new))

(let ([o (new s:h%)])
  (send o set-f1 6)
  (send o set-f2 16)
  (test #t is-a? (deserialize (serialize o)) s:h%)
  (test 6 'o (send (deserialize (serialize o)) get-f1))
  (test 16 'o (send (deserialize (serialize o)) get-f2))
  (test #(struct:object:s:h% 6 ... 34 ...) struct->vector o)
  (send o set-f3 o)
  (let ([o2 (deserialize (serialize o))])
    (test o2 'cycle (send o2 get-f3))))

(define-serializable-class* s:k% object% (externalizable<%>)
  (define z 12)
  (define/public (externalize) '(ok))
  (define/public (internalize v) (set! z 13))
  (define/public (get-z) z)
  (super-new))

(let ([o (new s:k%)])
  (test 12 'z (send o get-z))
  (let ([o2 (deserialize (serialize o))])
    (test #t is-a? o2 s:k%)
    (test 13 'z (send o2 get-z))))

(define s:m%
  (class object%
    (define x 12)
    (define/public (mm v) (begin0 x (set! x v)))
    (super-new)))

(define s:n%
  (class* s:m% (externalizable<%>)
    (inherit mm)
    (define x 18)
    (define/public (nm v) (begin0 x (set! x v)))
    (define/public (externalize) (list x (let ([v (mm 0)]) (mm v) v)))
    (define/public (internalize v) 
      (set! x (car v))
      (mm (cadr v)))
    (super-new)))

;; Just implementing externalize<%> isn't enough
(err/rt-test (serialize (new s:n%)))

;; Derive from externalizable class
(define-serializable-class s:p% s:n%
  (define x 0)
  (define/public (pm v) (begin0 x (set! x v)))
  (super-new))

(let ([o (new s:p%)])
  (test 12 'n (send o mm 12))
  (test 18 'n (send o nm 17))
  (test 0 'n (send o pm 10))
  (test 10 'n (send o pm 10))
  (let ([o2 (deserialize (serialize o))])
    (test #t is-a? o2 s:p%)
    (test 12 'n (send o2 mm 14))
    (test 14 'n (send o2 mm 15))
    (test 12 'n (send o mm 12))
    (test 17 'n (send o2 nm 0))
    (test 17 'n (send o nm 17))
    (test 0 'n (send o2 pm 0))))

;; Override doesn't matter until it's made serializable again:
(let ([s:q% (class s:p% 
	      (define/override (externalize) (error "ack"))
	      (super-new))])
  (test #t is-a? (deserialize (serialize (new s:q%))) s:p%)
  (test #f is-a? (deserialize (serialize (new s:q%))) s:q%))

;; override externalize
(define-serializable-class s:r% s:n%
  (inherit nm)
  (define/override (externalize) 10)
  (define/override (internalize v) (nm 77))
  (super-new))

(let ([o (new s:r%)])
  (send o mm 1)
  (send o nm 2)
  (let ([o2 (deserialize (serialize o))])
    (test #t is-a? o2 s:r%)
    (test 12 'n (send o2 mm 14))
    (test 77 'n (send o2 nm 15))))

(define-serializable-class s:bad% s:n%
  (init foo)
  (super-new))

(test #t pair? (serialize (new s:bad% [foo 10])))
(err/rt-test (deserialize (serialize (new s:bad% [foo 10]))) exn:fail:object?)

;; ----------------------------------------

(report-errs)

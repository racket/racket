
(load-relative "loadtest.rktl")

(Section 'serialization)

(require racket/serialize
         racket/file
         racket/flonum
         racket/fixnum)

;; ----------------------------------------

(define insp (current-inspector))

(define-serializable-struct a () #:inspector insp #:mutable)
(define-serializable-struct b (x y) #:inspector insp #:mutable)
(define-serializable-struct (c a) (z) #:inspector insp #:mutable)
(define-serializable-struct (d b) (w) #:inspector insp #:mutable)

(define (same? v1 v2)
  ;; This is not quite the same as `equal?', veuase it knows
  ;; about the structure types a, b, etc.
  (define ht (make-hasheq))
  (let loop ([v1 v1][v2 v2])
    (cond
     [(hash-ref ht v1 (lambda () #f))
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
      (hash-set! ht v1 v2)
      (and (loop (b-x v1) (b-x v2))
	   (loop (b-y v1) (b-y v2)))]
     [(and (c? v1) (c? v2))
      (hash-set! ht v1 v2)
      (loop (c-z v1) (c-z v2))]
     [(and (d? v1) (d? v2))
      (hash-set! ht v1 v2)
      (and (loop (b-x v1) (b-x v2))
	   (loop (b-y v1) (b-y v2))
	   (loop (d-w v1) (d-w v2)))]
     [(and (pair? v1)
	   (pair? v2))
      (hash-set! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (loop (car v1) (car v2))
	   (loop (cdr v1) (cdr v2)))]
     [(and (vector? v1)
	   (vector? v2))
      (hash-set! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (= (vector-length v1) (vector-length v2))
	   (andmap loop
		   (vector->list v1)
		   (vector->list v2)))]
     [(and (box? v1) (box? v2))
      (hash-set! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (loop (unbox v1) (unbox v2)))]
     [(and (hash? v1) (hash? v2))
      (hash-set! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (eq? (hash-eq? v1) (hash-eq? v2))
           (eq? (hash-eqv? v1) (hash-eqv? v2))
	   (eq? (hash-weak? v1) (hash-weak? v2))
	   (= (hash-count v1) (hash-count v2))
	   (let ([ok? #t])
	     (hash-for-each
	      v1
	      (lambda (k v)
		(set! ok?
		      (and ok?
			   (loop v (hash-ref v2 k (lambda () 'not-found)))))))
	     ok?))]
     [(and (date? v1) (date? v2))
      (hash-set! ht v1 v2)
      (andmap loop 
	      (vector->list (struct->vector v1))
	      (vector->list (struct->vector v2)))]
     [(and (arity-at-least? v1) (arity-at-least? v2))
      (hash-set! ht v1 v2)
      (loop (arity-at-least-value v1)
	    (arity-at-least-value v2))]
     [(and (struct? v1) (prefab-struct-key v1)
           (struct? v2) (prefab-struct-key v2))
      (equal? (struct->vector v1) (struct->vector v2))]
     [else
      (and (equal? v1 v2)
	   (eq? (immutable? v1) (immutable? v2)))])))

(define (test-ser v)
  (parameterize ([print-graph #t])
    (test #t serializable? v)
    (test #t same? v v)
    (test #t same? v (deserialize (serialize v)))
    (test #t serialized=? (serialize v) (serialize v))
    (test #f serialized=? (serialize v) (serialize (not v)))))

(define (mk-ht mk)
  (let ([ht (mk)])
    (hash-set! ht 'apple 'ok)
    (hash-set! ht 'banana 'better)
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
(test-ser (struct-copy date (seconds->date (current-seconds)))) ; not date*
(test-ser (procedure-arity (lambda (x . y) 10)))
(test-ser (make-immutable-hasheq '((1 . a) (2 . b))))
(test-ser (make-immutable-hasheqv '((1 . a) (2 . b))))
(test-ser (make-immutable-hash '(("x" . a) ("y" . b))))
(test-ser (mk-ht make-hasheq))
(test-ser (mk-ht make-hasheqv))
(test-ser (mk-ht make-hash))
(test-ser (mk-ht make-weak-hasheq))
(test-ser (mk-ht make-weak-hasheqv))
(test-ser (mk-ht make-weak-hash))
(test-ser #s(a 0 1 2))
(test-ser #s((a q 2) 0 1 2))
(test-ser (fxvector 1 2 30))
(test-ser (flvector 0.1 2.0 30e3))

(test-ser (set 'set 0 1 2))
(test-ser (seteqv 'seteqv 0 1 2))
(test-ser (seteq 'seteq 0 1 2))

(test-ser '(1))
(test-ser '#(1))
(test-ser '#&1)

(test-ser (mcons 1 2))
(test-ser (cons 1 2))
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

(test-ser (make-srcloc 1 2 3 4 5))
(test-ser (make-srcloc (string->path "/tmp/test.rkt") 2 3 4 5))

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
(let ([p (mk-ht make-hasheq)])
  (test-ser (cons p p)))
(let ([p #s(a 1 2 3)])
  (test-ser (list p p)))

;; Cycles
(let ([p (mcons 1 2)])
  (set-mcar! p p)
  (test-ser p)
  (set-mcdr! p p)
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
  (test-ser p)
  (test-ser (cons p p)))
(let ([p (make-arity-at-least 10)])
  (test-ser p)
  (test-ser (cons p p)))
(let ([p (mk-ht make-hasheq)])
  (hash-set! p 'banana p)
  (test-ser p)
  (test-ser (cons p p)))

;; Cycles with immutable parts
(let* ([p1 (mcons 1 2)]
       [p2 (cons 0 p1)])
  (set-mcdr! p1 p2)
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
(let* ([p1 (mcons 1 2)]
       [p2 (make-immutable-hasheq
	    (cons (cons 'x p1) '((a . 2) (b . 4))))])
  (set-mcdr! p1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (cons p1 p2))
  (test-ser (cons p2 p1)))

(let ()
  (struct a ([b #:mutable] [c #:mutable]) #:prefab)
  (struct z a ([b #:mutable] [c #:mutable]) #:prefab)
  
  (let ([z0 (z 1 2 3 4)])    
    (test-ser z0)
    (set-z-b! z0 z0)
    (test-ser z0)))
      
;; ----------------------------------------

(module ser-mod mzscheme
   (require racket/serialize)
   (provide ser-mod-test)

   (define-serializable-struct foo (a b))

   (define (ser-mod-test)
     (foo-a (deserialize (serialize (make-foo 1 2))))))

(require 'ser-mod)
(test 1 ser-mod-test)

;; ----------------------------------------
;; Classes

(require racket/class)

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

;; Custom deserialize:
(module my-own-deserialize racket/base
  (require racket/serialize)
  (provide a
           deserialize-info)
  (struct a ()
    #:property prop:serializable
    (make-serialize-info (lambda (v) #())
                         #'deserialize-info
                         #f
                         (or (current-load-relative-directory)
                             (current-directory))))
  (define deserialize-info
    (make-deserialize-info (lambda () 'a)
                           (lambda () (values 'a void)))))

(require 'my-own-deserialize)
(test 'a deserialize (serialize (a)))

;; Same thing, but with submodule:
(module my-own-deserialize/sub racket/base
  (require racket/serialize)
  (provide b)
  (module+ deserialize-info
    (provide deserialize-info))
  (struct b ()
    #:property prop:serializable
    (make-serialize-info (lambda (v) #())
                         #'deserialize-info
                         #f
                         (or (current-load-relative-directory)
                             (current-directory))))
  (define deserialize-info
    (make-deserialize-info (lambda () 'b)
                           (lambda () (values 'b void)))))

(require 'my-own-deserialize/sub)
(test 'b deserialize (serialize (b)))

;; ----------------------------------------

(let ([fn (make-temporary-file)])
  (with-output-to-file fn
    #:exists 'truncate
    (lambda () (display
                (string-append "#lang racket/base\n"
                               "(require racket/serialize)\n"
                               "(module+ main\n"
                               "   (provide s)\n"
                               "   (serializable-struct foo (bar))\n"
                               "   (define s (serialize (foo 35))))\n"))))
  (define s (dynamic-require `(submod ,fn main) 's))
  (let ([o (open-output-bytes)])
    (write s o)
    (test s read (open-input-string (get-output-string o))))
  (delete-file fn))


;; ----------------------------------------

(report-errs)

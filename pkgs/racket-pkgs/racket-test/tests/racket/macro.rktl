
(load-relative "loadtest.rktl")

(Section 'macro)

(error-test #'(define-syntaxes () (values 1)) exn:application:arity?)
(error-test #'(define-syntaxes () (values 1 2)) exn:application:arity?)
(error-test #'(define-syntaxes (x) (values 1 2)) exn:application:arity?)
(error-test #'(define-syntaxes (x y) (values 1)) exn:application:arity?)
(error-test #'(define-syntaxes (x y) (values 1 2 3)) exn:application:arity?)

;; Declarations:
(define-syntaxes (x) (values))
(define-syntaxes (x y) (values))

(define-syntax mx
  (lambda (stx)
    (syntax-case stx ()
      [(_ x)
       (syntax (x 1 8))])))
(test 9 'macro (mx +))
(test -7 'macro (mx -))
(test 18 'macro (let ([mx (lambda (x) (x 1 8 9))]) (mx +)))

(define-syntax (m stx)
  (syntax-case stx ()
    [(_) #'(m2 x)]))

(define-syntax (m2 stx)
  (syntax-case stx ()
    [(_ y) #'(let ([y 10]
		   [x 8])
	       y)]))

(test 10 'hygiene (m))
(test 10 'hygiene (m2 y))
(test 10 'hygiene (m2 x))
(test 10 'hygiene (eval #'(m)))
(test 10 'hygiene (eval (expand #'(m))))
(test 10 'hygiene (eval (expand (expand #'(m)))))

(define-syntax define-foo
  (syntax-rules ()
    ((_ expr) (define foo expr))))

(test 'o 'hygiene (let ()
		    (define-foo 'f)
		    (define-foo 'o)
		    (define o 'o)
		    o))
(test 'o 'hygiene (eval (expand (expand #'(let ()
					    (define-foo 'f)
					    (define-foo 'o)
					    (define o 'o)
					    o)))))


(test 13 'let-macro (let-syntax ([mx (lambda (stx)
				       (syntax-case stx ()
					 [(_ x) (syntax (x 6 7))]))])
		      (mx +)))
(test -7 'let-macro (let-syntax ([mx2 (lambda (stx)
					(syntax-case stx ()
					  [(_ x y) (syntax (mx y))]))])
		      (mx2 + -)))
(test '(10) 'let-macro ((lambda () (let-syntax ([x (lambda (stx)
						     (syntax-case stx ()
						       [(_ v) (syntax (list v))]))])
				     (x 10)))))

(test '(10) 'let-macro (let () 
			 (define-syntax x
			   (lambda (stx)
			     (syntax-case stx ()
			       [(_ v) (syntax (list v))])))
			 (x 10)))
(test '(10) 'let-macro ((lambda () 
			  (define-syntax x
			    (lambda (stx)
			      (syntax-case stx ()
				[(_ v) (syntax (list v))])))
			  (x 10))))

(test 55 (let ()
	   (define-syntax (g x) #'f)
	   (define (f x) x)
	   (define h g)
	   h)
      55)

(test 66 (let ()
	   (define (f x) x)
	   (define-syntax (g x) #'f)
	   (define h g)
	   h)
      66)

(test 77 (let ()
	   (define (f x) x)
	   (define h g)
	   (define-syntax (g x) #'f)
	   h)
      77)

(test 55 (letrec-syntaxes+values ([(g) (lambda (x) #'f)])
				 ([(f) (lambda (x) x)]
				  [(h) f])
	   h)
      55)

(test 7 'lrsv (letrec-syntaxes+values () () 7))

(syntax-test #'(set! lambda 5))
(syntax-test #'(lambda () (define-syntax x 10) (set! x 5)))
(syntax-test #'(lambda () (define-syntax (x) 10) (set! x 5)))

(syntax-test #'letrec-syntaxes+values)
(syntax-test #'(letrec-syntaxes+values))
(syntax-test #'(letrec-syntaxes+values . 1))
(syntax-test #'(letrec-syntaxes+values ()))
(syntax-test #'(letrec-syntaxes+values () . 1))
(syntax-test #'(letrec-syntaxes+values () ()))
(syntax-test #'(letrec-syntaxes+values () () . 1))
(syntax-test #'(letrec-syntaxes+values x () 1))
(syntax-test #'(letrec-syntaxes+values (x) () 1))
(syntax-test #'(letrec-syntaxes+values ([x]) () 1))
(syntax-test #'(letrec-syntaxes+values ([(x)]) () 1))
(syntax-test #'(letrec-syntaxes+values ([(x) 1 2]) () 1))
(syntax-test #'(letrec-syntaxes+values ([(x) 1] . y) () 1))
(syntax-test #'(letrec-syntaxes+values () x 1))
(syntax-test #'(letrec-syntaxes+values () (x) 1))
(syntax-test #'(letrec-syntaxes+values () ([x]) 1))
(syntax-test #'(letrec-syntaxes+values () ([(x)]) 1))
(syntax-test #'(letrec-syntaxes+values () ([(x) 1 2]) 1))
(syntax-test #'(letrec-syntaxes+values () ([(x) 1] . y) 1))

(test 7 'set!-transformer
      (let ([x 3])
	(let-syntax ([f (make-set!-transformer
			 (lambda (stx)
			   (syntax-case stx ()
			     [(_ __ val)
			      #'(set! x val)])))])
	  (set! f 7)
	  x)))

(test 77 'set!-transformer-prop
      (let ([x 3])
	(let-syntax ([f (let ()
                          (define-struct s!t (proc)
                            #:property prop:set!-transformer 0)
                          (make-s!t
                           (lambda (stx)
                             (syntax-case stx ()
                               [(_ __ val)
                                #'(set! x val)]))))])
	  (set! f 77)
	  x)))

(test 777 'set!-transformer-prop2
      (let ([x 3])
	(let-syntax ([f (let ()
                          (define-struct s!t ()
                            #:property prop:set!-transformer
                            (lambda (stx)
                              (syntax-case stx ()
                                [(_ __ val)
                                 #'(set! x val)])))
                          (make-s!t))])
	  (set! f 777)
	  x)))

(test 7 'rename-transformer
      (let ([x 3])
	(let-syntax ([f (make-rename-transformer #'x)])
	  (set! f 6)
	  (set! x (add1 x))
	  f)))

(test 5 'rename-with-non-hygiene
      (let-syntax ([f (lambda (stx) (datum->syntax stx 'foo))])
	(let-syntax ([g (make-rename-transformer #'f)])
	  (let ([foo 5])
	    g))))

(test 12 'rename-with-non-hygiene/app
      (let-syntax ([f (lambda (stx) 
			   (syntax-case stx ()
			     [(_ arg)
			      #`(#,(datum->syntax stx 'foo) arg)]))])
	(let-syntax ([g (make-rename-transformer #'f)])
	  (let ([foo (lambda (x) (sub1 x))])
	    (g 13)))))

(test 43 'rename-with-non-hygiene/set
      (let-syntax ([f (make-set!-transformer
			  (lambda (stx) 
			    (syntax-case stx ()
			      [(set! _ arg)
			       #`(set! #,(datum->syntax stx 'foo) arg)])))])
	(let-syntax ([g (make-rename-transformer #'f)])
	  (let ([foo 45])
	    (set! g 43)
	    foo))))

(define foo 88)
(test 88 'rename-with-hygiene
      (let-syntax ([g (make-rename-transformer #'foo)])
	(let ([foo 5])
	  g)))

(define (foox w) (+ w 88))
(test 99 'rename-with-hygiene/app
      (let-syntax ([g (make-rename-transformer #'foox)])
	(let ([foox 5])
	  (g 11))))

(define fooy 12)
(test '(5 11) 'rename-with-hygiene/set!
      (list (let-syntax ([g (make-rename-transformer #'fooy)])
	      (let ([fooy 5])
		(set! g 11)
		fooy))
	    fooy))

(test 12 'rename-internal-define
      (let-syntax ([fooz (syntax-rules ()
			   [(_ id) (define id 12)])])
	(let-syntax ([foozzz (make-rename-transformer #'fooz)])
	  (foozzz foozz)
	  foozz)))

(test #t 'free-identifier=?-of-rename-via-shadower
      (let ([y 5])
        (let-syntax ([m (lambda (stx)
                          #`(quote-syntax #,(syntax-local-get-shadower #'x)))])
          (let-syntax ([x (make-rename-transformer #'y)])
            (free-identifier=? (m) #'y)))))


(test #t set!-transformer? (make-set!-transformer void))
(test #t rename-transformer? (make-rename-transformer #'void))

(err/rt-test (make-set!-transformer 5))
(err/rt-test (make-set!-transformer #'x))
(err/rt-test (make-rename-transformer 5))
(err/rt-test (make-rename-transformer void))

(arity-test make-set!-transformer 1 1)
(arity-test set!-transformer? 1 1)
(arity-test make-rename-transformer 1 2)
(arity-test rename-transformer? 1 1)

;; Test inheritance of context when . is used in a pattern

(define-syntax keep-context
  (syntax-rules () [(a . b) b]))
(define-syntax (discard-context stx) 
  (syntax-case stx () 
    [(v . a) (datum->syntax #f (syntax-e #'a))]))

(test 6 'plus (keep-context + 1 2 3))
(test 6 'plus (keep-context . (+ 1 2 3)))

(unless building-flat-tests?
  (eval-syntax
   #'(test 6 'plus (discard-context keep-context . (+ 1 2 3)))))

(syntax-test #'(discard-context + 1 2 3))
(syntax-test #'(discard-context . (+ 1 2 3)))
(syntax-test #'(discard-context keep-context + 1 2 3))

;; ----------------------------------------

(require (for-syntax racket/struct-info))

(define-syntax (et-struct-info stx)
  (syntax-case stx ()
    [(_ id) #`(quote #,(extract-struct-info (syntax-local-value #'id)))]))

(let ([v (et-struct-info exn)])
  (test '(struct:exn make-exn exn? (exn-continuation-marks exn-message) (#f #f) #t) values v))

(let ([v (et-struct-info exn:break)])
  (test '(struct:exn:break make-exn:break exn:break? (exn:break-continuation exn-continuation-marks exn-message) (#f #f #f) exn) values v))

(let ([v (et-struct-info arity-at-least)])
  (test '(struct:arity-at-least make-arity-at-least arity-at-least? 
				(arity-at-least-value) (#f) #t) 
	values v))

(let ()
  (define-struct a (x y) #:mutable)
  (let ([v (et-struct-info a)])
    (test '(struct:a make-a a? (a-y a-x) (set-a-y! set-a-x!) #t) values v)
    (let ()
      (define-struct (b a) (z))
      (let ([v (et-struct-info b)])
	(test '(struct:b make-b b? (b-z a-y a-x) (#f set-a-y! set-a-x!) a) values v)))
    (let ()
      (define-struct (b exn) (z) #:mutable)
      (let ([v (et-struct-info b)])
	(test '(struct:b make-b b? (b-z exn-continuation-marks exn-message) (set-b-z! #f #f) exn) values v)))))
    
;; ----------------------------------------

(let () 
  (define-syntax (goo stx) 
    (syntax-case stx () 
      [(_ foo) #'(define-syntax (foo stx) (syntax-case stx () [(_ x) #'(define x 120)]))]))
  (goo foo)
  (foo x)
  (test 120 'intdef x))

(let-syntax ([goo (lambda (stx) #'(begin (define z 131) (test 131 'intdef z)))])
  (let-syntax ([y (lambda (stx) #'goo)])
    (let () 
      (define-syntax (goo stx) 
	(syntax-case stx () 
	  [(_ foo) #'(define-syntax (foo stx) (syntax-case stx () [(_ x) #'(define x 121)]))]))
      (goo foo)
      (foo x)
      y
      (test 121 'intdef x))))

(syntax-test #'(let ()
		 (define-syntax goo 10)
		 (define goo 10)
		 12))

(test 128 apply (lambda ()
                  (let-syntax ([ohno (lambda (stx) #'(define z -10))])
                    (let ()
                      (define ohno 128)
                      ohno
                      (define-syntax (goo stx) #'ohno)
                      ohno)))
      null)

(define-syntax (def-it stx)
  (syntax-case stx ()
    [(_ eid id) 
     #'(define-syntax eid #'id)]))

(define-syntax (get-it stx)
  (syntax-case stx ()
    [(_ id eid) 
     #`(define id #,(syntax-local-value #'eid))]))

(let ()
  (define x1 90)
  (def-it y1 x1)
  (get-it z1 y1)
  (test 90 'intdef z1))

(let ()
  (define-struct a (b c))
  (test 2 'intdef (a-c (make-a 1 2))))

(let ()
  (define-struct a (b c))
  (let ()
    (define-struct (d a) (e))
    (test 3 'intdef (d-e (make-d 1 2 3)))))

(let ()
  (define-struct a (b c))
  (define-struct (d a) (e))
  (test 3 'intdef (d-e (make-d 1 2 3))))

;; ----------------------------------------

(test 10 'vector-pattern (syntax-case #() () [#() 10]))
(test 'b 'vector-pattern (syntax-case #(a b c) () [#(x y z) (syntax-e #'y)]))

;; ----------------------------------------

(require racket/block)

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([x:id (datum->syntax #'id 'x)])
       #'(begin
           (define x:id 50)
           (define-syntax id #'x:id)))]))
(define-syntax (look stx)
  (syntax-case stx ()
    [(_ id) (syntax-local-value #'id)]))

(test 50 'look
      (let ()
        (def foo)
        (look foo)))

(test 50 'look
      (block
       (def foo)
       (look foo)))

(test #t 'bwd-struct
      (let ()
        (block
         (define-struct a (x y))
         (define-struct (b a) (z))
         (b? (make-b 1 2 3)))))

(test 5 'intdef
      (let ()
        (define-syntax foo
          (syntax-rules ()
            [(_ id) (begin
                      (define x 5)
                      (define id x))]))
        (foo x)
        x))

(test 6 'intdef-values
      (let ()
        (define-syntax foo
          (syntax-rules ()
            [(_ id) (define-values (x id) 
                      (values 6 (lambda () x)))]))
        (foo x)
        (x)))

(test 75 'bwd
      (block
       (define-syntax foo
         (syntax-rules ()
           [(_ id) (begin
                     (define x 75)
                     (define id x))]))
       (foo x)
       x))

;; ----------------------------------------

(define-syntax (bind stx)
  (syntax-case stx ()
    [(_ handle def)
     (let ([def-ctx (syntax-local-make-definition-context)]
           [ctx (cons (gensym 'intdef)
                      (let ([orig-ctx (syntax-local-context)])
                        (if (pair? orig-ctx)
                            orig-ctx
                            null)))]
           [kernel-forms (list #'define-values)])
       (let ([def (local-expand #'def ctx kernel-forms def-ctx)])
         (syntax-case def ()
           [(define-values (id) rhs)
            (begin
              (syntax-local-bind-syntaxes (list #'id) #f def-ctx)
              (internal-definition-context-seal def-ctx)
              #'(begin
                  (define-values (id) rhs)
                  (define-syntax handle (quote-syntax id))))]
           [_ (error "no")])))]))

(define-syntax (nab stx)
  (syntax-case stx ()
    [(_ handle)
     (syntax-local-value #'handle)]))

(let ()
  (bind h (define q 5))
  (define q 8)
  (nab h))

;; #'module* in stop list shouldn't add all the rest:
(let ()
  (define-syntax (m stx) (syntax-case stx () 
                           [(_ e) 
                            (let ([e (local-expand #'e 'expression (list #'module*))])
                              (syntax-case e  (#%plain-app quote)
                                [(#%plain-app + (quote 1) (quote 2)) 'ok]
                                [else (error 'test "bad local-expand result: ~e" e)])
                              #'(void))]))
  (m (+ 1 2)))

;; #'module* in stop list should stop:
(module m1-for-local-expand racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [mb #%module-begin])
           (except-out (all-from-out racket/base) #%module-begin))
  (define-syntax (mb stx)
    (syntax-case stx ()
      [(_ 10) #'(#%plain-module-begin 10)]
      [(_ 11) #'(#%plain-module-begin 11)]
      [(_ form ...)
       (let ([e (local-expand #'(#%plain-module-begin form ...)
                              'module-begin
                              (list #'module*))])
         (syntax-case e (module module* quote #%plain-app)
           [(mod-beg
             (#%plain-app + (quote 1) (quote 2))
             (module* q #f 10)
             (module* z #f 11))
            'ok]
           [else (error 'test "bad local-expand result: ~s" (syntax->datum e))])
         e)])))
(module m2-for-local-expand 'm1-for-local-expand
  (+ 1 2)
  (module* q #f 10) (module* z #f 11))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module rename-transformer-tests racket/base
  (require (for-syntax racket/base))

  (define x 12)
  (define-syntax bar (let ([x 10])
                       (make-rename-transformer #'x)))
  (define-syntax foo (make-rename-transformer #'x))
  (list foo
        (identifier-binding #'foo)
        (free-identifier=? #'x #'foo))
  (identifier-binding #'bar)

  (begin-for-syntax 
   (define-struct rt (id)
     #:property prop:rename-transformer 0
     #:omit-define-syntaxes))

  (let-syntax ([q (make-rt #'x)])
    (list q
          (identifier-binding #'q)
          (free-identifier=? #'q #'x)))

  (let ([w 11])
    (letrec-syntax ([q (let ()
                         (define-struct rt ()
                           #:property prop:rename-transformer #'w)
                         (make-rt))])
      (list q
            (identifier-binding #'q)
            (free-identifier=? #'q #'w))))

  (letrec-syntax ([n (make-rename-transformer #'glob)])
    (list (identifier-binding #'n)
          (free-identifier=? #'n #'glob)))

  (letrec-syntax ([i (make-rename-transformer #'glob)])
    (letrec-syntax ([n (make-rename-transformer (syntax-property #'i 'not-free-identifier=? #f))])
      (list (identifier-binding #'n)
            (free-identifier=? #'n #'glob)))))

(let ([accum null])
  (parameterize ([current-print (lambda (v)
                                  (set! accum (cons (let loop ([v v])
                                                      (cond
                                                       [(module-path-index? v) 'mpi]
                                                       [(pair? v) (cons (loop (car v))
                                                                        (loop (cdr v)))]
                                                       [else v]))
                                                    accum)))])
    (dynamic-require ''rename-transformer-tests #f))
  (test '((#f #t) 
          (#f #t)
          (11 lexical #t)
          (12 (mpi x mpi x 0 0 0) #t)
          lexical
          (12 (mpi x mpi x 0 0 0) #t))
        values accum))

(module rename-transformer-tests:m racket/base
  (require (for-syntax racket/base))
  (define-syntax x 1)
  (define-syntax x* (make-rename-transformer #'x))
  (define-syntax x** (make-rename-transformer (syntax-property #'x 'not-free-identifier=? #t)))
  (define-syntax (get stx)
    (syntax-case stx ()
      [(_ i)
       #`#,(free-identifier=? #'i #'x)]))
  (provide get x* x**))

(module rename-transformer-tests:n racket
  (require 'rename-transformer-tests:m)
  (provide go)
  (define (go)
    (list (get x*) (get x**))))

(test '(#t #f) (dynamic-require ''rename-transformer-tests:n 'go))

;; ----------------------------------------

(let ()
  (define-syntax (foo stx)
    (define context (syntax-local-make-definition-context))
    (with-handlers ([exn:fail:contract? (lambda (x) #''ok)])
      (syntax-local-bind-syntaxes (list 'q) #'1 context)))
  (test 'ok 'ok (foo)))

;; ----------------------------------------

(test '(+ 1 2)
      'local-transformer-expand/expr
      (let-syntax ([m (lambda (stx)
                        (define e (local-transformer-expand #'(+ 1 2) 'expression (list #'#%app)))
                        #`(quote #,e))])
        (m)))

(test '(define-values (x) '3)
      'local-transformer-expand/top-level
      (let-syntax ([m (lambda (stx)
                        (define e (local-transformer-expand #'(define x 3) 'top-level null))
                        #`(quote #,e))])
        (m)))

(module check-transformer-lift racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base)))
  (provide e d)
  (begin-for-syntax
   (define-syntax (n stx)
     (syntax-local-lift-expression #'5)
     #'ok))
  (define e
    (let-syntax ([m (lambda (stx)
                      (define e (local-transformer-expand #'(n) 'expression (list #'#%app)))
                      #`(quote #,e))])
      (m)))
  (define d
    (let-syntax ([m (lambda (stx)
                      (define e (local-transformer-expand/capture-lifts #'(n) 'top-level (list #'#%app)))
                      #`(quote #,e))])
      (m))))

(require syntax/datum)
(test #t
      'local-transformer-expand/lift
      (datum-case (dynamic-require ''check-transformer-lift 'e) (let-values ok)
        [(let-values (((lifted) 5)) ok) #t]
        [x (datum x)]))
(test #t
      'local-transformer-expand/lift
      (datum-case (dynamic-require ''check-transformer-lift 'd) (begin define-values ok)
        [(begin (define-values (lifted) 5) ok) #t]
        [x (datum x)]))

;; ----------------------------------------
;; Check `#%variable-reference' expansion to make sure
;;  a lexically bound identifier is made consistent with
;;  its binding

(module m-check-varref-expand racket
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ e) 
       ;; use `local-expand' to trigger re-expansion:
       (local-expand #'e 'expression null)]))
  
  (m
   (let ([x 10])
     (define-syntax-rule (q) (#%variable-reference x))
     ;; `q' introduces a marked `x' under `#%variable-reference':
     (q))))
(require 'm-check-varref-expand)

;; ----------------------------------------
;; Check that a modul-level binding with 0 marks
;;  but lexical context is found correctly with
;;  1 and 2 marks (test case by Carl):

(module check-macro-introduced-via-defctx racket/base
  (require (for-syntax racket/base racket/syntax))

  (begin-for-syntax
   (define env (box #false)))
  
  (define-syntax (one stx)
    (define ctx (syntax-local-make-definition-context #false))
    (define id
      (internal-definition-context-apply 
       ctx
       (syntax-local-introduce (datum->syntax #false 'private))))
    (syntax-local-bind-syntaxes (list id) #false ctx)
    (internal-definition-context-seal ctx)
    #`(begin
        (begin-for-syntax (set-box! env #'#,id))
        (define #,id #false)))
  (one)
  
  (define-syntax (two stx)
    (define id ((make-syntax-introducer) (unbox env)))
    (unless (free-identifier=? id (syntax-local-introduce id))
      (raise-syntax-error 
       #false
       (format "mark changes identifier's binding: ~v / ~v"
               (identifier-binding id)
               (identifier-binding (syntax-local-introduce id)))
       id))
    #'#f)
  (two))

;; ----------------------------------------
;; Related: check that matching no marks is considered less
;; of a match than matching with marks:

(module another-test-empty-marks-with-context racket/base
  (require (for-syntax racket/base))

  (define-for-syntax count 0)
  (define-for-syntax (inc-count!) (set! count (add1 count)))
  
  (define-syntax (foo stx)
    (syntax-case stx ()
      [(_ x e)
       (let ([cid-marker (make-syntax-introducer)])
         (with-syntax ([y (cid-marker #'x)])
           #'(begin
               (define y e)
               (+ y 1)
               (define-syntax x
                 (lambda (stx) 
                   (inc-count!)
                   (when (= count 5) (error "stop"))
                   #'y)))))]))
  
  (module* test #f
    (foo eX 3)))

;; ----------------------------------------
;; Check `free-identifier=?' propagation,
;; definition contexts, and `syntax-local-bind-syntaxes'

(let ()
  (define-syntax (compare stx)
    (define id0 (cadr (syntax->list stx)))
    (unless (free-identifier=? id0 #'compare)
      (error "`free-identifier=? test failed on int-def binding"))
    #'10)
  
  (define-syntax (invoke stx)
    (define id1 (cadr (syntax->list stx)))
    (define id2 ((make-syntax-introducer) (datum->syntax #false 'dummy)))
    (define d-ctx (syntax-local-make-definition-context #false))
    (define e-ctx (list (gensym)))
    (syntax-local-bind-syntaxes
     (list id2)
     #`(make-rename-transformer #'#,id1)
     d-ctx)
    (internal-definition-context-seal d-ctx)
    (local-expand #`(#,id2 #,id2) e-ctx (list #'quote) d-ctx))

  (test 10 'ten (invoke compare)))

;; ----------------------------------------
;; Check zo marshaling of prefab in a list:

(let ([s #'(quote-syntax (#s(foo bar)))])
  (define-values (i o) (make-pipe))
  (parameterize ([current-namespace (make-base-namespace)]
                 [read-accept-compiled #t])
    (write (compile s) o)
    (test (syntax->datum (eval (read i))) values '(#s(foo bar)))))

;; ----------------------------------------
;; Check require & provide of marked names in various phases:

(module phase-providing-check racket/base
  (define-syntax-rule (bounce phase phase+1)
    (begin
      (#%require (for-meta phase racket/base))
      (#%provide (for-meta phase printf)
                 (for-meta phase+1 syntax-rules))
      (define (expect f v) 
        (unless (f v)
          (error 'marks "failure at phase ~s: ~s vs. ~s"
                 phase f v)))
      (expect list? (identifier-binding #'printf phase))
      (expect list? (identifier-binding #'syntax-rules phase+1))
      (unless (or (eq? phase phase+1)
                  (eq? phase+1 0))
        (expect not (identifier-binding #'printf phase+1)))))

  (bounce 0 1)
  (bounce 1 2)
  (bounce 2 3)
  (bounce -2 -1)
  (bounce -1 0)
  (bounce #f #f)
  (define printf 'ok!))
(require 'phase-providing-check)

;; ----------------------------------------
;; Check reconstruction of `provide' forms:

(test #t
      'provide
      (syntax-case (expand '(module m racket 
                              (define-for-syntax x 8) 
                              (provide (for-meta 1 x)))) ()
        [(module m racket
           (#%module-begin
            config-runtime
            defn
            (#%provide (for-meta 1 x))))
         #t]
        [else #f]))

;; ----------------------------------------
;; Check that `syntax-local-bind-syntaxes' and others 
;; in module for-syntax top level don't crash due to
;; the lack of a mark:

(for ([e (list
          '(syntax-local-bind-syntaxes
            '()
            #'(syntax-local-make-delta-introducer #'dummy)
            (syntax-local-make-definition-context))
          '(let-values ([(x y) (syntax-local-expand-expression #'1)])
             (eval y)))])
  (err/rt-test (eval `(module m racket/base
                        (require (for-syntax racket/base))
                        (begin-for-syntax
                         ,e)))
               exn:fail?))

;; ----------------------------------------
;; Check that the `syntax' macro adds a
;; 'disappeared-use property that is
;; original (if it should be):

(parameterize ([current-namespace (make-base-namespace)])
  (define stx (expand #'(with-syntax ([x 1]) #'x)))
  (let sloop ([stx stx]
              [in-prop? #f])
    (cond
     [(and (identifier? stx)
           (eq? 'x (syntax-e stx)))
      (when in-prop?
        (test #t syntax-original? stx))]
     [(syntax? stx)
      (sloop (syntax-property stx 'disappeared-use) #t)
      (sloop (syntax-e stx) in-prop?)]
     [(pair? stx)
      (sloop (car stx) in-prop?)
      (sloop (cdr stx) in-prop?)])))

;; ----------------------------------------

(err/rt-test (syntax-local-lift-require 'abc #'def))

;; ----------------------------------------

(let ()
  (define stx
    (expand #'(module m racket
                (require (only-in racket/list first)))))
  
  (define r/ls
    (syntax-case stx ()
      [(_mod _m _racket
             (_mod-begin config-runtime
                         (_req (_just-meta _0 (_rename rl1 . _whatever))
                               (_only rl2))))
       (list #'rl1 #'rl2)]))
  
  (test (list #t #t) map syntax-original? r/ls)
  (test (list #t #t) map number? (map syntax-position r/ls)))

;; ----------------------------------------

(module check-shadower-in-submodule racket/base
  (require (for-syntax racket/base))
  
  (define-syntax (define-2 stx)
    (syntax-case stx () 
      [(_ id)
       (with-syntax ([new-id
                      ((make-syntax-introducer)
                       (datum->syntax #f
                                      (string->symbol
                                       (format "~a2" (syntax-e #'id)))))])
         #'(begin
             (define new-id 5)
             (define-syntax (id stx)
               (syntax-local-get-shadower #'new-id))))]))
  
  (module* main #f
    (provide out)
    (define-2 f)
    (define f2 6)
    (define out f)))

(test 5 dynamic-require '(submod 'check-shadower-in-submodule main) 'out)

;; ----------------------------------------

(parameterize ([current-namespace (make-base-namespace)])
  (define m '(module m racket/base
               (require racket/splicing
                        (for-syntax racket/base))
               
               (define-syntax-rule (def id)
                 (splicing-let ([x +])
                               (define-syntax id (let ([v #'(x)])
                                                   (lambda (stx)
                                                     v)))))
               
               (provide def)))

  (eval (if #t
            (expand m)
            m))
  (namespace-require ''m)
  (eval '(def t))
  (eval '(t)))

;; ----------------------------------------
;; Check that a `free-identifier=?' mapping via a rename transformer
;; doesn't mess up a `free-identifier=?' test where the relevant
;; identifier is shadowed with a lexical binding; this test was
;; provided by Carl Eastlund.

(module lang-for-identifier=?-test racket/base
  (#%module-begin

    (provide
      #%module-begin
      (all-from-out racket/base)
      (for-syntax
        (all-from-out racket/base)
        (all-from-out syntax/parse)))

    (require
      (for-syntax
        racket/base
        syntax/parse))

    (define-syntax (#%module-begin stx)
      (syntax-parse stx
        [(_ before ...)
         (syntax-parse (local-expand
                         #'(#%plain-module-begin before ...)
                         'module-begin
                         '())
           #:literal-sets {kernel-literals}
           [(#%plain-module-begin
              _
              _
              (#%plain-lambda {one:id}
                (letrec-syntaxes+values _ _ two:id)))
            
            (let ()
              (when (bound-identifier=? #'one #'two)
                (unless (free-identifier=? #'one #'two)
                  (error 'bug
                         "{bound,free}-identifier=? inconsistency")))
              
              #'(#%plain-module-begin))])]))))

(module consistency-of-identifier=?-test 'lang-for-identifier=?-test
  (#%module-begin

    (define-syntaxes {lam}
      (lambda (stx)
        (syntax-parse stx
          [(_ unmarked . body)
           (define/syntax-parse marked
             (syntax-local-introduce (attribute unmarked)))
           #'(#%plain-lambda {marked}
               (define-syntaxes {unmarked}
                 (make-rename-transformer #'marked))
               . body)])))

    (define-syntaxes {x}
      (make-rename-transformer #'dummy))

    (lam x x)))

;; ----------------------------------------
;; Check consistency of `free-identifier=?' and binding

(module consistency-free-id-A racket
  (provide g (rename-out [*a a]))
  (define *a 10)
  (define a 10)
  (define-syntax g #'a))

(module consistency-free-id-B racket
  (require 'consistency-free-id-A)
  (provide consistency-free-id)
  (define-syntax (n stx)
    (syntax-case stx ()
      [(_ ref)
       (with-syntax ([in (syntax-local-introduce
                          (syntax-local-value #'g))])
         #'(let ([in 10]) ; BINDING
             (list (free-identifier=? #'in #'ref)
                   in
                   ref)))])) ; REFERENCE
  (define (consistency-free-id) (n a)))

(require 'consistency-free-id-B)

(test (list #t 10 10) consistency-free-id)

;; ----------------------------------------
;; Check `syntax-local-lift...` outside of macro:

(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module m racket/base
           (require (for-syntax racket/base))
           (let-syntax ([x (syntax-local-lift-expression #'(display "hi\n"))])
             (void))))
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (eval `(require 'm)))
  (test "hi\n" get-output-string o))

(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module m racket/base
           (require (for-syntax racket/base))
           (define x 10)
           (let-syntax ([x (syntax-local-lift-provide #'x)])
             (void))))
  (test 10 eval `(dynamic-require ''m 'x)))

(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module m racket/base
           (require (for-syntax racket/base))
           (let-syntax ([x (let ([x (syntax-local-lift-require #'racket/fixnum
                                                               #'(displayln (fx+ 1 1)))])
                             (syntax-local-lift-expression x)
                             (void))])
             (void))))
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (eval `(require 'm)))
  (test "2\n" get-output-string o))

;; ----------------------------------------
;; Lifting should not introduce `#%top` around
;; the reference to the lifted identifier:

(module lifting-doesnt-introduce-top-wrapper racket
  ;; do the lifting
  (define-syntax (m stx)
    (syntax-local-lift-expression #'(+ 1 1)))

  ;; catch the lift, try to put definitions in a let
  (define-syntax (catch stx)
    (syntax-case stx ()
      [(_ body)
       (syntax-case (local-expand/capture-lifts #'body (syntax-local-context) null)
           (define-values begin)
         [(begin (define-values (x ...) e ...) ... exp)
          #'(let () (define-values (x ...) e ...) ... exp)])]))

  (define z (catch (+ 1 (m))))

  (provide z))

(test 3 dynamic-require ''lifting-doesnt-introduce-top-wrapper 'z)


(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax (m stx)
           (syntax-local-lift-expression #'(+ 1 1))))
  (eval '(define-syntax (catch stx)
           (syntax-case stx ()
             [(_ body)
              (syntax-case (local-expand/capture-lifts #'body (syntax-local-context) null)
                  (define-values begin)
                [(begin (define-values (x ...) e ...) ... exp)
                 #'(let () (define-values (x ...) e ...) ... exp)])])))
  (test 3 eval '(catch (+ 1 (m)))))

(let-syntax ([m (lambda (stx)
                  (define e (local-expand #'nonsuch 'expression null))
                  (unless (identifier? e)
                    (error 'test "bad expansion: ~e" e))
                  #'(void))])
  (m))
(let-syntax ([m (lambda (stx)
                  (define e (local-expand #'(#%top . nonsuch) 'expression null))
                  (syntax-case e (#%top)
                    [(#%top . id)
                     (identifier? #'id)
                     #'(void)]
                    [else
                     (error 'test "bad expansion: ~e" e)]))])
  (m))

(test 10 values
      (let-syntax ([#%top (lambda (stx)
                            (syntax-case stx ()
                              [(_ . id) #'10]))])
        (let-syntax ([m (lambda (stx)
                          (local-expand #'nonsuch 'expression null))])
          (m))))

;; ----------------------------------------

;; Check that `expand` produces valied syntax
(let ()
  (define (check mod)
    (define e
      (syntax->datum
       (parameterize ([current-namespace (make-base-namespace)])
         (expand mod))))
    (let loop ([e e])
      (cond
       [(syntax? e) (loop (syntax-e e))]
       [(pair? e) (loop (car e)) (loop (cdr e))]
       [(null? e) (void)]
       [(symbol? e) (void)]
       [(keyword? e) (void)]
       [(number? e) (void)]
       [(boolean? e) (void)]
       [else (error 'expand-test "unexpected value: ~e" e)])))
  (check '(module m '#%kernel
            (#%declare #:cross-phase-persistent)))
  (check '(module m '#%kernel
            (define-values (x) 10)))
  (check '(module m racket/base
            (require (for-syntax racket/base))
            (define-syntax (m stx)
              (syntax-local-lift-expression #'(+ 1 2)))
            (list (m))))
  (check '(module m racket/base
            (module+ main 10))))

;; ----------------------------------------

(let ()
 (struct foo (id)
         #:property prop:rename-transformer 0)

 (test 'x syntax-e
       (rename-transformer-target
        (chaperone-struct (foo #'x) foo-id (lambda (f x) x)))))

;; ----------------------------------------

(report-errs)

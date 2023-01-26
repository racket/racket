
(load-relative "loadtest.rktl")

(Section 'macro)

(test #f struct-predicate-procedure? syntax?)

(test #t struct-predicate-procedure? exn:fail:syntax?)

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
        (let-syntax ([m (lambda ()
                          (syntax-local-get-shadower #'x))])
          (let-syntax ([x (make-rename-transformer #'y)])
            (let-syntax ([n (lambda (stx)
                              #`#,(free-identifier=? ((syntax-local-value #'m)) #'y))])
              (n))))))

(test #t set!-transformer? (make-set!-transformer void))
(test #t rename-transformer? (make-rename-transformer #'void))

(err/rt-test (make-set!-transformer 5))
(err/rt-test (make-set!-transformer #'x))
(err/rt-test (make-rename-transformer 5))
(err/rt-test (make-rename-transformer void))

(arity-test make-set!-transformer 1 1)
(arity-test set!-transformer? 1 1)
(arity-test make-rename-transformer 1 1)
(arity-test rename-transformer? 1 1)

;; Test inheritance of context when . is used in a pattern

(define-syntax keep-context
  (syntax-rules () [(a . b) b]))
(define-syntax (discard-context stx) 
  (syntax-case stx () 
    [(v . a) (datum->syntax #f (syntax-e #'a))]))

(test 6 'plus (keep-context + 1 2 3))
(test 6 'plus (keep-context . (+ 1 2 3)))

(eval-syntax
 #'(test 6 'plus (discard-context keep-context . (+ 1 2 3))))

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


(module uses-internal-definition-context-around-id racket/base
  (require (for-syntax racket/base
                       racket/block))
  
  (define-syntax (m stx)
    (let ([x1 #'x]
          [x2 (block
               (define x3 #'x)
               x3)])
      #`(let ([#,x2 1]) #,x1)))
  
  (define v (m))
  (provide v))
(test 1 dynamic-require ''uses-internal-definition-context-around-id 'v)

;; Make sure `syntax-local-make-definition-context` can be called
;; at unusual times, where the scope that is otherwise captured
;; for `quote-syntax` isn't or can't be recorded
(let-syntax ([x (syntax-local-make-definition-context)])
  (void))
(module makes-definition-context-at-compile-time-begin racket
  (begin-for-syntax
    (syntax-local-make-definition-context)))
(require 'makes-definition-context-at-compile-time-begin)


(module create-definition-context-during-visit racket/base
  (require (for-syntax racket/base))
  (provide (for-syntax ds))
  ;; won't be stipped for `quote-syntax`
  (define-for-syntax ds (syntax-local-make-definition-context)))

(module create-definition-context-during-expand racket/base
  (require (for-syntax racket/base)
           'create-definition-context-during-visit)
  (provide results
           get-results)

  ;; will be stipped for `quote-syntax`
  (define-for-syntax ds2 (syntax-local-make-definition-context))

  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ body)
       (internal-definition-context-introduce ds #'body)]))

  (define-syntax (m2 stx)
    (syntax-case stx ()
      [(_ body)
       (internal-definition-context-introduce ds2 #'body)]))

  (define-syntax (m3 stx)
    (syntax-case stx ()
      [(_ body)
       (let ([ds3 (syntax-local-make-definition-context)])
         (internal-definition-context-introduce ds3 #'body))]))

  (define results
    (list
     (bound-identifier=? (m #'x)
                         #'x)
     (bound-identifier=? (m2 #'x)
                         #'x)
     (bound-identifier=? (m3 #'x)
                         #'x)))

  (define (get-results)
    (list
     (bound-identifier=? (m #'x)
                         #'x)
     (bound-identifier=? (m2 #'x)
                         #'x)
     (bound-identifier=? (m3 #'x)
                         #'x))))

(test '(#f #t #t) dynamic-require ''create-definition-context-during-expand 'results)
(test '(#f #t #t) (dynamic-require ''create-definition-context-during-expand 'get-results))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module local-expand-begin-for-syntax-test racket/base
  (require (for-syntax racket/base)
           (for-meta 2 racket/base))

  (provide result)

  (begin-for-syntax
    (define-syntax (z stx)
      (syntax-local-lift-expression #'(+ 1 2))))

  (define-syntax (m stx)
    #`'#,(local-expand #'(begin-for-syntax (define x 10) (z)) 'top-level null))

  (define result (m)))

(let ([r (dynamic-require ''local-expand-begin-for-syntax-test 'result)])
  (define lifted-id (and (list? r) (last r)))
  (test `(begin-for-syntax (define-values (,lifted-id) (#%app + '1 '2)) (define-values (x) '10) ,lifted-id)
        'local-expand-begin-for-syntax
        r))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module rename-transformer-tests racket/base
  (require (for-syntax racket/base))

  (define x 12)
  (define-syntax foo (make-rename-transformer #'x))
  (list foo
        (identifier-binding #'foo)
        (free-identifier=? #'x #'foo))

  (begin-for-syntax 
   (define-struct rt (id)
     #:property prop:rename-transformer 0
     #:omit-define-syntaxes))

  (define-syntax v (make-rt #'x))
  (list v
        (identifier-binding #'v)
        (free-identifier=? #'v #'x))

  (define w 11)
  (define-syntax q (let ()
                     (define-struct rt ()
                       #:property prop:rename-transformer #'w)
                     (make-rt)))
  (list q
        (identifier-binding #'q)
        (free-identifier=? #'q #'w))

  (define-syntax n1 (make-rename-transformer #'glob))
  (list (identifier-binding #'n1)
        (free-identifier=? #'n1 #'glob))
  
  (define-syntax i (make-rename-transformer #'glob))
  (define-syntax n2 (make-rename-transformer (syntax-property #'i 'not-free-identifier=? #f)))
  (list (identifier-binding #'n2)
        (free-identifier=? #'n2 #'glob)))

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
          (11 (mpi w mpi w 0 0 0) #t)
          (12 (mpi x mpi x 0 0 0) #t)
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

(test #f portal-syntax? 0)
(test #t portal-syntax? (make-portal-syntax #'hi))
(err/rt-test (make-portal-syntax 'hi))

(module root-constant-transformer-tests racket/base
  (provide x)
  (define x 12))

(module constant-transformer-tests racket/base
  (require (for-syntax racket/base))
  (define-syntax-rule (portal foo)
    (begin
      (require 'root-constant-transformer-tests)
      (#%require (portal foo any))))
  (portal foo)

  (provide foo))

(define (namespace-identifier-constant-syntax id [phase 0])
  (define stx (identifier-binding-portal-syntax id phase))
  (and stx (datum->syntax stx 'x)))

(require (only-in 'constant-transformer-tests
                  [foo constant-transformer-tests:foo]))
(test 'x syntax-e (namespace-identifier-constant-syntax #'constant-transformer-tests:foo))
(test (module-path-index-join ''root-constant-transformer-tests #f)
      car
      (identifier-binding (namespace-identifier-constant-syntax #'constant-transformer-tests:foo)))

(require (for-meta 5 (only-in 'constant-transformer-tests
                              [foo constant-transformer-tests5:foo])))
(test 'x syntax-e (namespace-identifier-constant-syntax #'constant-transformer-tests5:foo 5))
(test #f namespace-identifier-constant-syntax #'constant-transformer-tests:foo 5)
(test #f namespace-identifier-constant-syntax #'constant-transformer-testsL:foo)
(test (module-path-index-join ''root-constant-transformer-tests #f)
      car
      (identifier-binding (namespace-identifier-constant-syntax #'constant-transformer-tests5:foo 5) 5))

(require (for-label (only-in 'constant-transformer-tests
                             [foo constant-transformer-testsL:foo])))
(test 'x syntax-e (namespace-identifier-constant-syntax #'constant-transformer-testsL:foo #f))
(test #f namespace-identifier-constant-syntax #'constant-transformer-tests:foo #f)
(test #f namespace-identifier-constant-syntax #'constant-transformer-tests5:foo #f)
(test #f namespace-identifier-constant-syntax #'constant-transformer-testsL:foo)
(test (module-path-index-join ''root-constant-transformer-tests #f)
      car
      (identifier-binding (namespace-identifier-constant-syntax #'constant-transformer-testsL:foo #f) #f))

(module bread-and-butter-and-jam-module racket/base
  (provide bread butter jam)
  (define bread 'Bread)
  (define butter 'Butter)
  (define jam 'Jam))

(module bins-bread-and-butter racket/base
  (define-syntax-rule (portal id meta)
    (begin
      (require (for-meta meta 'bread-and-butter-and-jam-module))
      (#%require (for-meta meta (portal id [any id intro])))))
  (portal bread-and-butter 0)
  (portal bread-and-butter 5)
  (require (only-in 'bread-and-butter-and-jam-module jam))
  (provide result result5 result-j bread-and-butter)
  (define bread (quote bound))
  (define result (identifier-binding-portal-syntax
                  (quote-syntax bread-and-butter)))
  (define result5 (identifier-binding-portal-syntax
                   (quote-syntax bread-and-butter)
                   5))
  (define result-j jam))

(define bread-and-butter1 (dynamic-require ''bins-bread-and-butter 'result))
(test 'bread-and-butter syntax-e (cadr (syntax-e bread-and-butter1)))
(test #t list? (identifier-binding (cadr (syntax-e bread-and-butter1))))
(test #f identifier-binding (datum->syntax (cadr (syntax-e bread-and-butter1)) 'butter))
(test #t list? (identifier-binding (datum->syntax (caddr (syntax-e bread-and-butter1)) 'bread)))
(test (module-path-index-join ''bread-and-butter-and-jam-module #f)
      car (identifier-binding (datum->syntax (caddr (syntax-e bread-and-butter1)) 'bread)))
(test (module-path-index-join ''bins-bread-and-butter #f)
      car (identifier-binding (datum->syntax (cadr (syntax-e bread-and-butter1)) 'bread)))
(test 'Jam dynamic-require ''bins-bread-and-butter 'result-j)

(define bread-and-butter5 (dynamic-require ''bins-bread-and-butter 'result5))
(test 'any syntax-e (car (syntax-e bread-and-butter5)))
(test 'bread-and-butter syntax-e (cadr (syntax-e bread-and-butter5)))

(module imports-constant-syntax racket/base
  (require 'bins-bread-and-butter)
  (provide result)
  (define result (identifier-binding-portal-syntax
                  (quote-syntax bread-and-butter))))

(define bread-and-butter2 (dynamic-require ''imports-constant-syntax 'result))
(test 'bread-and-butter syntax-e (cadr (syntax-e bread-and-butter2)))
(test #t list? (identifier-binding (cadr (syntax-e bread-and-butter2))))
(test #t list? (identifier-binding (datum->syntax (caddr (syntax-e bread-and-butter2)) 'bread)))
(test (module-path-index-join ''bread-and-butter-and-jam-module #f)
      car (identifier-binding (datum->syntax (caddr (syntax-e bread-and-butter2)) 'bread)))

(module uses-syntax-local-constant-syntax racket/base
  (require (for-syntax racket/base)
           'bread-and-butter-and-jam-module)
  (#%require (portal bread-and-butter any))
  (provide macro result bread-and-butter)
  (define-syntax (macro stx)
    (define stx (portal-syntax-content (syntax-local-value (quote-syntax bread-and-butter))))
    #`(list #,(datum->syntax stx 'bread) #,(datum->syntax stx 'butter)))
  (define result macro))

(test '(Bread Butter) dynamic-require ''uses-syntax-local-constant-syntax 'result)
(test '(Bread Butter) dynamic-require ''uses-syntax-local-constant-syntax 'macro)

(module uses-uses-syntax-local-constant-syntax racket/base
  (require (for-syntax racket/base)
           'uses-syntax-local-constant-syntax)
  (provide macro result)
  (define-syntax (macro stx)
    (define stx (portal-syntax-content (syntax-local-value (quote-syntax bread-and-butter))))
    #`(list #,(datum->syntax stx 'bread) #,(datum->syntax stx 'butter)))
  (define result macro))

(test '(Bread Butter) dynamic-require ''uses-uses-syntax-local-constant-syntax 'result)
(test '(Bread Butter) dynamic-require ''uses-uses-syntax-local-constant-syntax 'macro)

(#%require (portal check-top-level-portal (yes ok)))
(test '(yes ok) syntax->datum (identifier-binding-portal-syntax (quote-syntax check-top-level-portal)))
(test '(yes ok) 'top-level-portal (let-syntax ([m (lambda (stx)
                                                    #`(quote #,(portal-syntax-content
                                                                (syntax-local-value (quote-syntax check-top-level-portal)))))])
                                    (m)))

(#%require (for-meta 17 (portal check-top-level-portal (yes ok 17))))
(test '(yes ok 17) syntax->datum (identifier-binding-portal-syntax (quote-syntax check-top-level-portal) 17))
(test #f identifier-binding-portal-syntax (quote-syntax check-top-level-portal) 16)

(module interactively-define-portal-syntax-in-here racket/base)
(dynamic-require ''interactively-define-portal-syntax-in-here #f)
(parameterize ([current-namespace (module->namespace ''interactively-define-portal-syntax-in-here)])
  (eval '(#%require (portal check-entered (inside))))
  (test '(inside) syntax->datum (identifier-binding-portal-syntax (eval '(quote-syntax check-entered)))))

(#%require (for-meta #f (portal check-top-level-portal (yes ok #f))))

(module bins-bread-and-butter-for-label racket/base
  (#%require (for-meta #f (portal check-top-level-portal (yes ok #f)))))

(module check-syntax-local-lift-portal racket/base
  (require (for-syntax racket/base))
  (define-syntax (lift stx)
    (define x-id (syntax-local-lift-require #'(portal x #t) #'x))
    (unless (portal-syntax? (syntax-local-value x-id))
      (error "portal lift failed"))
    #'(void))
  (lift)
  (#%expression (lift))
  (let ()
    (lift)
    (void)))

;; make sure top-level portals with distinct scopes are distinct
(parameterize ([current-namespace (make-base-namespace)])
  (define intro (make-syntax-introducer))
  (define id (namespace-syntax-introduce (datum->syntax #f 'alpha)))
  (eval #`(#%require (portal #,id 1)))
  (eval #`(#%require (portal #,(intro id) 2)))
  (define (extract s)
    (syntax-case s ()
      [v (syntax-e #'v)]))
  (test 1 extract (identifier-binding-portal-syntax id))
  (test 2 extract (identifier-binding-portal-syntax (intro id))))

;; ----------------------------------------

(module distinct-binding-tests racket/base
  (require (for-syntax racket/base))
  (provide result)

  (define-syntax-rule (go get)
    (begin
      (require racket/base)
      (define (get wrt) (identifier-distinct-binding #'cons wrt))))
  (go get)
  
  (define result
    (list (identifier-distinct-binding #'cons (datum->syntax #f 'cons))
          (identifier-distinct-binding #'cons (datum->syntax #f 'cons) 1)
          (get #'cons)
          ;; #f results:
          (identifier-distinct-binding #'cons #'cons)
          (identifier-distinct-binding #'kons (datum->syntax #f 'kons))
          (identifier-distinct-binding #'cons #'cons 2))))

(test #t list? (car (dynamic-require ''distinct-binding-tests 'result)))
(test #t list? (cadr (dynamic-require ''distinct-binding-tests 'result)))
(test #t list? (caddr (dynamic-require ''distinct-binding-tests 'result)))
(test '(#f #f #f) cdddr (dynamic-require ''distinct-binding-tests 'result))

;; ----------------------------------------

(let ()
  (define (go contexts
              wrap
              prop:macro
              #:sub? [sub? #f]
              #:trim-error [error-rx #f])
    (define o (open-output-bytes))
    (define e (open-output-bytes))
    (parameterize ([current-output-port o]
                   [current-error-port e]
                   [current-namespace (make-base-namespace)])
      ;; these tests rely on errors printing to the current-error-port
      (with-handlers ([exn:fail? (lambda (e) ((error-display-handler) (exn-message e) e))])
        (call-with-continuation-prompt
         (lambda ()
           (eval
            `(module m racket/base
               (require (for-syntax racket/base))
               
               (define v 0)
               
               (begin-for-syntax
                 (struct e (p)
                   #:property ,prop:macro ,(if (eq? prop:macro 'prop:procedure)
                                               0
                                               (list #'quote-syntax
                                                     (syntax-property #'v
                                                                      'not-free-identifier=?
                                                                      #t)))
                   #:property prop:expansion-contexts ',contexts))
               
               (define-syntax m (e (lambda (stx)
                                     (displayln (syntax-local-context))
                                     #'10)))
               
               ,(wrap 'm)))
           (dynamic-require (if sub? '(submod 'm sub) ''m) #f)))))
    (list (get-output-string o)
          (if error-rx
              (let ([m (regexp-match error-rx (get-output-string e))])
                (or (and m (car m))
                    (get-output-string e)))
              (get-output-string e))))
  
  (test '("module\n10\n" "") go '(module expression) list 'prop:procedure)
  (test '("module\n10\n" "") go '(module expression) values 'prop:procedure)
  (test '("expression\n10\n" "") go '(expression) list 'prop:procedure)
  (test '("expression\n10\n" "") go '(expression) values 'prop:procedure)
  (test '("" "m: not allowed in context\n  expansion context: module") go '() values 'prop:procedure
        #:trim-error #rx"^[^\n]*\n[^\n]*")
  (test '("0\n" "") go '(module expression) values 'prop:rename-transformer)
  (test '("" "application: not a procedure") go '(module expression) list 'prop:rename-transformer
        #:trim-error #rx"^[^;]*")
  (test '("0\n" "") go '(expression) values 'prop:rename-transformer)
  (test '("" "application: not a procedure") go '(expression) list 'prop:rename-transformer
        #:trim-error #rx"^[^;]*")  
  (test '("" "m: not allowed in context\n  expansion context: module") go '() values 'prop:rename-transformer
        #:trim-error #rx"^[^\n]*\n[^\n]*")
  
  (define (in-submodule s) `(module* sub #f ,s))
  (test '("module-begin\n10\n" "") go '(module-begin expression) in-submodule 'prop:procedure #:sub? #t)
  (test '("module-begin\n10\n" "") go '(module-begin) in-submodule 'prop:procedure #:sub? #t)
  (test '("module\n10\n" "") go '(module expression) in-submodule 'prop:procedure #:sub? #t)
  (test '("module\n10\n" "") go '(module) in-submodule 'prop:procedure #:sub? #t)
  (test '("expression\n10\n" "") go '(expression) in-submodule 'prop:procedure #:sub? #t)
  (test '("" "m: not allowed in context\n  expansion context: module") go '() in-submodule 'prop:procedure
        #:trim-error #rx"^[^\n]*\n[^\n]*")
  (test '("0\n" "") go '(module-begin expression) in-submodule 'prop:rename-transformer #:sub? #t)
  (test '("0\n" "") go '(module-begin) in-submodule 'prop:rename-transformer #:sub? #t)
  (test '("0\n" "") go '(module expression) in-submodule 'prop:rename-transformer #:sub? #t)
  (test '("0\n" "") go '(module) in-submodule 'prop:rename-transformer #:sub? #t)
  (test '("0\n" "") go '(expression) in-submodule 'prop:rename-transformer #:sub? #t)
  (test '("" "m: not allowed in context\n  expansion context: module") go '() in-submodule 'prop:rename-transformer
        #:trim-error #rx"^[^\n]*\n[^\n]*")
  
  (define (in-defctx s) `(let () ,s))
  (test '("(#<liberal-define-context>)\n10\n" "") go '(definition-context expression) in-defctx 'prop:procedure)
  (test '("(#<liberal-define-context>)\n10\n" "") go '(definition-context) in-defctx 'prop:procedure)
  (test '("expression\n10\n" "") go '(expression) in-defctx 'prop:procedure)
  (test '("" "m: not allowed in context\n  expansion context: definition-context") go '() in-defctx 'prop:procedure
        #:trim-error #rx"^[^\n]*\n[^\n]*")
  (test '("0\n" "") go '(definition-context expression) in-defctx 'prop:rename-transformer)
  (test '("0\n" "") go '(definition-context) in-defctx 'prop:rename-transformer)
  (test '("0\n" "") go '(expression) in-defctx 'prop:rename-transformer)
  (test '("" "m: not allowed in context\n  expansion context: definition-context") go '() in-defctx 'prop:rename-transformer
        #:trim-error #rx"^[^\n]*\n[^\n]*")
  
  (void))
  
  

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
(syntax-test #'(datum-case '(1 "x" -> y) (->) [(a b -> c) (define q 1)])
             #rx"macro.rktl:.*the last form is not an expression")

(let ()
  (define-syntax-rule (check-error clause)
    (err/rt-test
     (datum-case '((1 2) (3)) () clause)
     (lambda (exn)
       (and (regexp-match? #rx"incompatible ellipsis match counts for template"
                           (exn-message exn))))))
  (check-error [((a ...) (b ...)) (datum ((a b) ...))])
  (check-error [((a ...) (b ...)) (datum ((a 0 b) ...))])
  (check-error [((a ...) (b ...)) (datum (((a) (b)) ...))]))

(syntax-test (quote-syntax (syntax-case #f () [#s(a ... b) 'ok])))

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
;; Check that expand of reference to rename transformer
;; applies the transformer

(test #t eval (expand '(let ([x 10])
                         (let-syntax ([y (make-rename-transformer #'x)])
                           (variable-reference-constant?
                            (#%variable-reference y))))))

;; ----------------------------------------
;; Check that a module-level binding with 0 marks
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
;; Check that an implicitly introduced #%app has the same
;; `syntax-original?` as its parenthesized form

(let ([find (lambda (e sym)
              (let loop ([s (syntax-property e 'origin)])
                (cond
                  [(and (identifier? s)
                        (eq? sym (syntax-e s)))
                   s]
                  [(pair? s) (or (loop (car s)) (loop (cdr s)))]
                  [else #f])))])
  ;; expecting `#%app` from `racket/base` to reqrite to core `#%app`
  (test #t syntax-original? (find (expand #'(+ 1 2)) '#%app))
  (test #t syntax-property (find (expand #'(+ 1 2)) '#%app) 'implicit-made-explicit)
  (test #t syntax-original? (find (expand #'100) '#%datum))
  (test #t syntax-property (find (expand #'100) '#%datum) 'implicit-made-explicit)

  (test #f syntax-original? (find (expand (datum->syntax #'here '(+ 1 2))) '#%app))
  (test #t syntax-property (find (expand (datum->syntax #'here '(+ 1 2))) '#%app) 'implicit-made-explicit)
  (test #f syntax-original? (find (expand (datum->syntax #'here '100)) '#%datum))
  (test #t syntax-property (find (expand (datum->syntax #'here '100)) '#%datum) 'implicit-made-explicit))

;; ----------------------------------------

(err/rt-test (syntax-local-lift-require 'abc #'def))

(for ([lift-attempt+rx:expected-error
       (in-list
        (list (cons '(syntax-local-lift-expression #'body)
                    #rx"no lift target")
              (cons '(syntax-local-lift-module  #'(module m racket/base))
                    #rx"not currently transforming within a module declaration or top level")
              (cons '(syntax-local-lift-module-end-declaration  #'(define done #t))
                    #rx"not currently transforming an expression within a module declaration")
              (cons '(syntax-local-lift-provide #'cons)
                    #rx"not expanding in a module run-time body")))])
  (define lift-attempt (car lift-attempt+rx:expected-error))
  (define rx:expected-error (cdr lift-attempt+rx:expected-error))
  (err/rt-test
   (expand `(module m racket/base
              (require (for-syntax racket/base))
              
              (provide #%module-begin)
              
              (define-syntax (#%module-begin stx)
                (syntax-case stx ()
                  [(_ body) ,lift-attempt]))
              
              (module* test (submod "..")
              body)))
   (lambda (exn) (regexp-match? rx:expected-error (exn-message exn)))))

;; ----------------------------------------

(let ()
  (define stx
    (expand #'(module m racket
                (require (only-in racket/list first)))))
  
  (define r/ls
    (syntax-case stx ()
      [(_mod _m _racket
             (_mod-begin config-runtime
                         (_req (_just-meta _0 (_just-space #f (_rename rl1 . _whatever)))
                               (_only rl2))))
       (list #'rl1 #'rl2)]))
  
  (test (list #t #t) map syntax-original? r/ls)
  (test (list #t #t) map number? (map syntax-position r/ls)))

;; ----------------------------------------

(module binds-in-friend-spaces racket/base
  (require (for-syntax racket/base)
           (for-space friend1 racket/base))

  (provide spaces)
  
  (define-syntax (def stx)
    (syntax-case stx ()
      [(_ id)
       #`(begin
           (define #,((make-interned-syntax-introducer 'friend2) #'id) 'ok)
           (define id (quote #,(syntax-local-module-interned-scope-symbols))))]))
  
  (def spaces))

(let ([spaces (dynamic-require ''binds-in-friend-spaces 'spaces)])
  (test #t pair? (memq 'friend1 spaces))
  (test #t pair? (memq 'friend2 spaces)))

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
                (let-values _ two:id)))
            
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
             (datum->syntax #f (syntax->datum (attribute unmarked))))
           #'(#%plain-lambda {marked}
               (define-syntaxes {unmarked}
                 (make-rename-transformer #'marked))
               . body)])))

    (define-syntaxes {x}
      (make-rename-transformer #'dummy))

    (lam x x)))

;; ----------------------------------------
;; Check consistency of `free-identifier=?' and binding;
;;  the result changed with the new macro system, so
;;  it's consistent the other way around

(module consistency-free-id-A racket
  (provide g (rename-out [*a a]))
  (define *a 10)
  (define a 11)
  (define-syntax g #'a))

(module consistency-free-id-B racket
  (require 'consistency-free-id-A)
  (provide consistency-free-id)
  (define-syntax (n stx)
    (syntax-case stx ()
      [(_ ref)
       (with-syntax ([in (syntax-local-introduce
                          (syntax-local-value #'g))])
         #'(let ([in 12]) ; BINDING
             (list (free-identifier=? #'in #'ref)
                   in
                   ref)))])) ; REFERENCE
  (define (consistency-free-id) (n a)))

(require 'consistency-free-id-B)

(test (list #f 12 10) consistency-free-id)

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

(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module m racket/base
           (require (for-syntax racket/base))
           (define x 10)
           (let-syntax ([x (syntax-local-lift-module #'(module m racket/base 
                                                         (provide x)
                                                         (define x 10)))])
             (void))))
  (test 10 eval `(dynamic-require '(submod 'm m) 'x)))

;; ----------------------------------------
;; Check bad lift provide

(err/rt-test
 (eval '(module a '#%kernel
          (#%require (for-syntax '#%kernel))
          (define-syntaxes (m)
            (lambda (stx)
              (let-values ([(ctx) (syntax-local-make-definition-context)])
                (syntax-local-bind-syntaxes (list (quote-syntax x)) (quote-syntax 5) ctx)
                (syntax-local-lift-provide (internal-definition-context-introduce ctx (quote-syntax x) 'add))
                (quote-syntax (void)))))
          (m)))
 exn:fail:syntax?
 #rx"provided identifier is not defined or required")

;; ----------------------------------------
;; Check require lifting without adding a new scope

(module uses-a-no-scope-lifted-require racket/base
  (require (for-syntax racket/base))
  (provide also-sub)
  (module sub racket/base
    (provide sub)
    (define sub "sub"))
  (define-syntax (lift stx)
    (syntax-local-lift-require #'(submod "." sub) #'(void) #f))
  (lift)
  (define also-sub sub))

(test "sub" dynamic-require ''uses-a-no-scope-lifted-require 'also-sub)


;; ----------------------------------------
;; Check portal lifting to the top level

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax (lift stx)
           (define id (syntax-local-lift-require
                       #'(portal ptl 5)
                       #'ptl))
           #`(portal-lookup #,id)))
  (eval '(define-syntax (portal-lookup stx)
           (syntax-case stx ()
             [(_ id)
              (datum->syntax
               #'id
               (portal-syntax? (syntax-local-value #'id #f)))])))
 (test #t eval '(lift)))

;; ----------------------------------------
;; Check module lifting in a top-level context

(define-syntax (do-lift-example-1 stx)
  (syntax-local-lift-module
   #'(module lift-example-1 racket/base
       (provide x)
       (define x 10)))
  #'(void))
(do-lift-example-1)
(test 10 dynamic-require ''lift-example-1 'x)

(test '(begin
        (module lift-example-1 racket/base
          (provide x)
          (define x 10))
        (#%app void))
      'local-expand/capture-lifts
      (let-syntax ([quote-local-expand
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ e)
                         #`(quote #,(local-expand/capture-lifts #'e 'top-level null))]))])
        (quote-local-expand (do-lift-example-1))))

(define-syntax (do-lift-example-1* stx)
  (syntax-local-lift-module
   #'(module* lift-example-1* racket/base
       (provide x)
       (define x 10)))
  #'(void))

(err/rt-test (expand '(do-lift-example-1*))
             (lambda (exn)
               (and (exn:fail:contract? exn)
                    (regexp-match #rx"cannot lift.*module[*]" (exn-message exn)))))

(test '(begin
        (module* lift-example-1* racket/base
          (provide x)
          (define x 10))
        (#%app void))
      'local-expand/capture-lifts
      (let-syntax ([quote-local-expand
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ e)
                         #`(quote #,(local-expand/capture-lifts #'e 'module null))]))])
        (quote-local-expand (do-lift-example-1*))))

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
;; Check that new binding scopes are introduced even for
;; empty `let` bindings:

(test 1 'empty-let-intro
      (let ()
        (define-syntax (m stx)
          (syntax-case stx ()
            [(_ def-id id)
             #`(define-syntax def-id
                 (make-rename-transformer (quote-syntax #,(syntax-local-introduce
                                                           (syntax-local-value #'id)))))]))
        (define-syntax (n stx)
          (syntax-case stx ()
            [(_ def-id id)
             #`(define-syntax def-id (quote-syntax #,(syntax-local-introduce
                                                      (syntax-local-value #'id))))]))

        (let ()
          (define x 1)
          (define-syntax id #'x)
          (let ()
            (n id2 id)
            (define x 2)
            (let ()
              (m z id2)
              z)))))

;; ----------------------------------------
;; Check that expansion works right for a rename transformer
;; that redirects to an imported binding

(parameterize ([current-namespace (make-base-namespace)])
  (void
   (expand
    '(module m racket/base
      (#%plain-module-begin
       (require (for-syntax racket/base
                            syntax/parse))
       
       (define-syntax (mylam stx)
         (syntax-parse stx
           [(_ (xx) body)
            #'(#%plain-lambda (xx) (letrec-syntaxes+values ([(xx) (make-rename-transformer #'+)])
                                                           ()
                                                           body))]))
       
       ((mylam (x) (x 1 2)) 'any))))))

;; ----------------------------------------
;; Make sure an internal-definition context expansion
;; propagates the context's scope while expanding an expression:
;; (Test from Spencer Florence)

(module check-intfdef-expansion-scope racket/base
  (require (for-syntax syntax/parse
                       racket/base))

  ;; a standard context for identifiers
  (define-for-syntax ctx #'ctx)

  ;; create an ID with the context `ctx`, and the current
  ;; expander mark (so that the mark is canceled later),
  ;; and the location loc
  (define-for-syntax (make-id loc)
    (syntax-local-introduce
     (datum->syntax ctx 'id loc)))

  ;; This introduces a binding
  (define-syntax (def stx)
    (syntax-parse stx
      [(def)
       (with-syntax ([id (make-id #'here)])
         #'(define id 5))]))

  ;; this attempts to use the binding introduced by `def`
  (define-syntax (use stx)
    (syntax-parse stx
      [(use)
       (with-syntax ([id (make-id #'here)])
         #'id)]))

  (let ()
    (def)
    (use)))

;; ----------------------------------------
;; Similar to preceding, but at module level using an initialy
;; empty scope set:

(module check-module-recur-expansion-scope racket/kernel
  (#%require racket/base)
  (require (for-syntax racket/base
                       syntax/parse))
  ;; empty context:
  (define-for-syntax ctx (datum->syntax #f 'ctx))
  (define-for-syntax (make-id loc)
    (syntax-local-introduce
     (datum->syntax ctx 'id loc)))
  (define-syntax (def stx)
    (syntax-parse stx
      [(def)
       (with-syntax ([id (make-id #'here)])
         #'(define-syntax-rule (id) (define x 1)))]))
  (define-syntax (use stx)
    (syntax-parse stx
      [(use)
       (with-syntax ([id (make-id #'here)])
         #'(id))]))
  (begin
    (def)
    (use)))

;; Module body is expanded with `local-expand`:
(module check-module-local-expand-recur-expansion-scope racket/base
  (require (for-syntax racket/base
                       syntax/parse))
  ;; empty context:
  (define-for-syntax ctx (datum->syntax #f 'ctx))
  (define-for-syntax (make-id loc)
    (syntax-local-introduce
     (datum->syntax ctx 'id loc)))
  (define-syntax (def stx)
    (syntax-parse stx
      [(def)
       (with-syntax ([id (make-id #'here)])
         #'(define-syntax-rule (id) (define x 1)))]))
  (define-syntax (use stx)
    (syntax-parse stx
      [(use)
       (with-syntax ([id (make-id #'here)])
         #'(id))]))
  (begin
    (def)
    (use)))

;; ----------------------------------------
;; Check `local-expand` argument checking

(let-syntax ([m
              (lambda (stx)
                (define (le . args)
                  (with-handlers ([exn:fail:contract? void])
                    (apply local-expand args)
                    (error "fail")))
                (le #'1 'xpression null)
                (le #'1 'expression 1)
                (le #'1 'expression #'1)
                (le #'1 'expression #'(x))
                (le #'1 'expression (list 'x))
                #'#t)])
  (void (m)))

;; ----------------------------------------
;; Check that `expand-syntax` attaches the namespace's 

(test 573
      'expand
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(require (for-syntax racket/base)))
        (eval '(define-syntax (m stx)
                (with-syntax ([id (datum->syntax #f 'gen-id)])
                  #`(begin
                      (define id 573)
                      id))))
        (define stx (namespace-syntax-introduce (datum->syntax #f '(m))))
        (syntax-case (expand-syntax-to-top-form stx) (begin)
          [(begin a b)
           (begin
             (eval-syntax #'a)
             (eval-syntax (expand-syntax #'b)))])))

;; ----------------------------------------
;; Basic use-site scope example

(module define-n-as-ten-not-five racket/base
  (define x 10)
  
  (define-syntax-rule (use-x misc-id)
    (let ([misc-id 5])
      x))
  
  (define n (use-x x))
  
  (provide n))

(test 10 dynamic-require ''define-n-as-ten-not-five 'n)

;; ----------------------------------------
;; Check that use-site scopes are not pruned too eagerly
;;  (based on examples from Brian Mastenbrook)

(module should-be-inner-1 racket/base
  (define x 'outer)
  
  (define-syntax-rule (def-m m given-x)
    (define-syntax-rule (m d)
      (begin
        (define given-x 'inner)
        (define d x))))
  
  (def-m m x)
  (m d)
  (provide d))

(test 'inner dynamic-require ''should-be-inner-1 'd)

(module should-be-inner-2 racket/base
  (define x 'outer)
  
  (define d
    (let ()
      (define-syntax-rule (def-m m given-x)
        (define-syntax-rule (m)
          (begin
            (define given-x 'inner)
            x)))
      
      (def-m m x)
      (m)))
  
  (provide d))

(test 'inner dynamic-require ''should-be-inner-2 'd)

(module should-be-outer-1 racket/base
  (define x 'outer)
  
  (define-syntax-rule (def-m m given-x)
    (define-syntax-rule (m d)
      (define d
        (let ()
          (define given-x 'inner)
          x))))
  
  (def-m m x)
  (m d)
  (provide d))

(test 'outer dynamic-require ''should-be-outer-1 'd)

(module should-be-outer-2 racket/base
  (define x 'outer)
  
  (define-syntax-rule (def-m m given-x)
    (define-syntax-rule (m)
      (begin
        (define given-x 'inner)
        x)))
  
  (define d
    (let ()
      (def-m m x)
      (m)))
  
  (provide d))

(test 'outer dynamic-require ''should-be-outer-2 'd)

(module should-be-outer-3 racket/base
  (define x 'outer)
  
  (define-syntax-rule (def-m m given-x)
    (define-syntax-rule (m)
      (begin
        (define given-x 'inner)
        x)))
  
  (def-m m x)
  (define d
    (let ()
      (m)))
  
  (provide d))

(test 'outer dynamic-require ''should-be-outer-3 'd)

(module should-be-outer-4 racket/base
  (define x 'outer)
  
  (define d
    (let ()
      (define-syntax-rule (def-m m given-x)
        (define-syntax-rule (m)
          (begin
            (define given-x 'inner)
            x)))
      
      (def-m m x)
      (let ()
        (m))))
  
  (provide d))

(test 'outer dynamic-require ''should-be-outer-4 'd)

;; ----------------------------------------
;; Check that taint check precedes bound-with-binding substitution:

(err/rt-test (expand '(let ([x 1])
                       (let-syntax ([m (lambda (stx)
                                         #`(list #,(syntax-taint #'x)))])
                         (m))))
             (lambda (exn)
               (regexp-match? #rx"cannot use identifier tainted by macro transformation"
                              (exn-message exn))))

;; ----------------------------------------
;; Check that lifting works right at the top level:

(module macro-that-introduces-a-lifted-one racket
  (define-syntax (m stx)
    (syntax-local-lift-expression #'1))
  (m))
(dynamic-require ''macro-that-introduces-a-lifted-one #f)

(test 1 values (parameterize ([current-namespace
                               (module->namespace ''macro-that-introduces-a-lifted-one)])
                 (eval '(values m))))

;; ----------------------------------------
;; Check that expanded references in submodule
;; have the right binding info for 'origin

(let ()
  (define m
    '(module m racket/base
      (define-syntax-rule (m) '1)
      (module+ main
        (m))))

  (define m-expanded
    (parameterize ([current-namespace (make-base-namespace)])
      (expand m)))

  (define-values (bind-m ref-m)
    (syntax-case m-expanded ()
      [(module _ racket/base
         (#%module-begin
          _
          (define-syntaxes
            (m)
            _)
          (module* main #f
            (#%module-begin-2
             _
             (#%app1 call-with-values (lambda () ONE) print-values)))))
       (values #'m (car (syntax-property #'ONE 'origin)))]))

  (test #t free-identifier=? bind-m ref-m))

;; ----------------------------------------

(module check-lift-during-local-transformer-expand racket/base
  (require (for-syntax racket/base))
  
  (begin-for-syntax
    (require (for-syntax racket/base))
    (define-syntax (check stx)
      (unless (syntax-transforming-with-lifts?)
        (error "expected lifts to be allowed"))
      (syntax-local-lift-expression #'foo)
      #'1)
    (define-syntax (check2 stx)
      (unless (syntax-transforming-with-lifts?)
        (error "expected lifts to be allowed"))
      #'2))

  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ e f)
       (begin
         (unless (eq? 'begin
                      (syntax-e (car (syntax-e (local-transformer-expand #'e 'top-level null)))))
           (error "lift failed"))
         (when (eq? 'begin
                    (syntax-e (car (syntax-e (local-transformer-expand #'f 'top-level null)))))
           (error "lift introduced unexpected `begin`"))
         #'(void))]))
       
  (m (check) (check2)))


;; ----------------------------------------
;; Check that `syntax-local-lift-values-expression` works rigth when lifts
;; are converted to `let`; in particular, make sure the order is
;; right

(module uses-local-lift-values-at-expansion-time racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (require (for-syntax racket/base))
    
    (define-syntax (m stx)
      #`(values #,@(syntax-local-lift-values-expression 3 #'(values 1 2 3)))))
  
  (define-syntax (n stx)
    (define-values (a b c) (m))
    #`(list #,a #,b #,c))
  
  (provide l)
  (define l (n)))

(test '(1 2 3) dynamic-require ''uses-local-lift-values-at-expansion-time 'l)

;; ----------------------------------------
;; Check that `local-expand` tentatively allows out-of-context identifiers

(module tentatively-out-of-context racket/base
  (require (for-syntax racket/base))

  (define-syntax (new-lam stx)
    (syntax-case stx ()
      [(_ x body)
       (with-syntax ([(_ (x+) body+)
                      (local-expand #'(lambda (x) body) 'expression null)])
         (with-syntax ([body++ ;; double-expand body
                        (local-expand #'body+ 'expression null)])
           #'(lambda (x+) body++)))]))

  ((new-lam X X) 100))

;; ----------------------------------------
;; Check that properties interact properly with the rename transformer
;; that is used to implement `let-syntax` [example from Stephen Chang]

(define-syntax (test-key-property-as-val stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([x/prop (syntax-property #'x 'key 'val)])
       (with-syntax ([(lam _ (lv1 _ (lv2 _ x+)))
                      (local-expand
                       #'(lambda (x)
                           (let-syntax ([x (lambda (stx) #'x)])
                             x/prop))
                       'expression null)])
         #`'#,(syntax-property #'x+ 'key)))]))

(test 'val 'let-syntax-rename-transformer-property (test-key-property-as-val stx))

;; ----------------------------------------
;; Check that a chain of rename transformers maintains properties correctly

(module chains-properties-through-two-rename-transformer racket/base
  (require (for-syntax racket/base))
  
  (define a 'a)
  
  (define-syntax b (make-rename-transformer (syntax-property #'a 'ids 'b)))
  (define-syntax c (make-rename-transformer (syntax-property #'b 'ids 'c)))
  
  (define-syntax (inspect stx)
    (syntax-case stx ()
      [(_ e)
       (let ([e (local-expand #'e 'expression null)])
         #`(quote #,(syntax-property e 'ids)))]))
  
  (provide prop-val)
  (define prop-val (inspect c)))

(test '(b . c) dynamic-require ''chains-properties-through-two-rename-transformer 'prop-val)

;; ----------------------------------------
;; Check that the wrong properties are *not* added when a rename transformer is involed

(module inner-and-outer-properties-around-rename-transformers racket/base
  (require (for-syntax racket/base))

  (define-syntax (some-define stx)
    (syntax-case stx ()
      [(_ x)
       #'(define-syntax x
           (make-rename-transformer
            (syntax-property #'void 'prop 'inner)))]))

  (some-define x)

  (define-syntax (wrapper stx)
    (syntax-case stx ()
      [(_ e)
       (local-expand
        (syntax-property #'e 'prop 'outer)
        'expression null)]))

  (define-syntax (#%app stx)
    (syntax-case stx ()
      [(_ f)
       #`(quote #,(syntax-property #'f 'prop))]))

  (provide prop-val)
  (define prop-val (wrapper (x))))

(test 'inner dynamic-require ''inner-and-outer-properties-around-rename-transformers 'prop-val)

;; ----------------------------------------
;; Check that a `prop:rename-transformer` procedure is called in a
;; `syntax-transforming?` mode when used as an expression

(let ([x 'one]
      [bad 'bad])
  (let-syntax ([also-x (let ()
                         (struct ax ()
                                 #:property
                                 prop:rename-transformer
                                 (lambda (an-ax)
                                   (make-will-executor)
                                   (if (syntax-transforming?)
                                       #'x
                                       #'bad)))
                         (ax))])
    (test 'one values also-x)))

(let ([x 'two]
      [bad 'bad])
  (let-syntax ([also-x (let ()
                         (struct ax ()
                                 #:property
                                 prop:rename-transformer
                                 (lambda (an-ax)
                                   (make-will-executor)
                                   (if (syntax-transforming?)
                                       (syntax-property #'x 'not-free-identifier=? #t)
                                       #'bad)))
                         (ax))])
    (test 'two values also-x)))

;; ----------------------------------------
;; Make sure top-level definition replaces a macro binding

(define-syntax-rule (something-previously-bound-as-syntax) 1)
(define something-previously-bound-as-syntax 5)
(test 5 values something-previously-bound-as-syntax)

;; ----------------------------------------
;; Check that ellipsis-counts errors are reported when a single
;; pattern variable is used at different depths

(err/rt-test (syntax->datum
              (with-syntax ([((b ...) ...) #'((1 2) (3) ())])
                #'([(b (b ...)) ...] ...)))
             (lambda (exn) (regexp-match? #rx"incompatible ellipsis" (exn-message exn))))

;; ----------------------------------------
;; Check `local-expand` for a `#%module-begin` that
;; routes `require`s through a macro (which involves use-site
;; scopes)

(module module-begin-check/mb racket/base
  (require (for-syntax racket/base))
  
  (provide (except-out (all-from-out racket/base)
                       #%module-begin)
           (rename-out [mb #%module-begin]))
  
  (define-syntax (mb stx)
    (syntax-case stx ()
      [(_ f ... last)
       (local-expand #'(#%module-begin f ... last)
                     'module-begin 
                     (list #'module*))])))

(module module-begin-check/y racket/base
  (provide y)
  (define y 'y))

(module x 'module-begin-check/mb
  (define-syntax-rule (req mod ...)
    (require mod ...))
  (req 'module-begin-check/y)
  (void y))

;; ----------------------------------------
;; Check that expansion to `#%module-begin` is prepared to handle
;; definition contexts

(module make-definition-context-during-module-begin racket/base
  (require (for-syntax racket/base))
  
  (define-syntax (bind-and-expand stx)
    (syntax-case stx ()
      [(_ x form ...)
       (let ()
         (define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (list #'x) #'(λ (stx) (println stx) #'(void)) ctx)
         (local-expand #'(#%plain-module-begin form ...) 'module-begin '() ctx))]))
  
  (module* a #f (bind-and-expand x x))
  (module* b #f (bind-and-expand x (list x))))

;; ----------------------------------------
;; Make sure it's ok to produce an already-expanded
;; expression in a definition context

(module macro-with-syntax-local-expand-expression-in-a-definition-context racket/base
  (require (for-syntax racket/base))

  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ e)
       (let ()
         (define-values (new-stx new-exp)
           (syntax-local-expand-expression #'e #t))
         new-exp)]))

  (m 5)

  (let ()
    (define whatever 10)
    (m (+ 1 2))
    'ok))

;; ----------------------------------------
;; Error (instead of looping) when an implicit is not bound as syntax

(syntax-test #'(module m racket/base
                 (require (for-syntax racket/base))
                 (let-syntax ([#%app (make-rename-transformer #'unbound)])
                   (+ 1 2))))

(syntax-test #'(module m racket/base
                 (require (for-syntax racket/base))
                 (let-syntax ([#%app (make-rename-transformer #'cons)])
                   (+ 1 2))))

(syntax-test #'(module m racket/base
                 (require (for-syntax racket/base))
                 (let-syntax ([#%datum (make-rename-transformer #'unbound)])
                   (+ 1 2))))

(module make-sure-not-bound-as-syntax-is-not-propagated-to-reexpansion racket/base
  (module new-top racket/base
    (provide #%top)
    (define (helper sym) sym)
    (define-syntax-rule (#%top . ID)
      (helper 'ID)))
  
  (local-require (submod 'new-top))
  foobar)

;; ----------------------------------------
;; Check that definition context bindings are made available when the context is provided as fourth
;; argument of syntax-local-bind-syntaxes

(module syntax-local-bind-syntaxes-local-value-in-other-context racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (define intdef-ctx-a (syntax-local-make-definition-context))
    (syntax-local-bind-syntaxes (list #'x) #'42 intdef-ctx-a)

    (define intdef-ctx-b (syntax-local-make-definition-context intdef-ctx-a))
    (syntax-local-bind-syntaxes (list #'y) #'(syntax-local-value #'x) intdef-ctx-b intdef-ctx-a)))

;; ----------------------------------------
;; internal-definition-context-introduce always adds scope

(module internal-definition-context-introduce-always-adds-scope racket/base
  (require (for-syntax racket/base))
  (provide result)
  (define-syntax (m stx)
    (define intdef-ctx (syntax-local-make-definition-context #f #f))
    (syntax-local-bind-syntaxes (list #'x) #''value intdef-ctx)
    #`(list '#,(syntax-local-value #'x (λ () #f) intdef-ctx)
            '#,(syntax-local-value (internal-definition-context-introduce intdef-ctx #'x)
                                   (λ () #f) intdef-ctx)))
  (define result (m)))

(test '(#f value) dynamic-require ''internal-definition-context-introduce-always-adds-scope 'result)


;; ----------------------------------------
;; Make sure `#%expression` doesn't appear in fully
;; expanded forms

(let ([stx (expand '(module m racket/base
                      (require racket/class)
                      (define c
                        (class object%
                          (super-new)))))])
  (test #f 'any-#%expression? (let loop ([e stx])
                                (cond
                                  [(eq? e '#%expression) #t]
                                  [(syntax? e) (loop (syntax-e e))]
                                  [(pair? e) (or (loop (car e)) (loop (cdr e)))]
                                  [else #f]))))

;; ----------------------------------------
;; parent definition contexts’ bindings, but not scopes, are added implicitly during expansion

(err/rt-test
 (eval #'(module parent-internal-definition-contexts-do-not-add-scopes racket/base
           (require (for-syntax racket/base))
           (begin-for-syntax
             (define intdef-ctx-a (syntax-local-make-definition-context))
             (syntax-local-bind-syntaxes (list #'x) #'42 intdef-ctx-a)

             (define intdef-ctx-b (syntax-local-make-definition-context intdef-ctx-a))
             (syntax-local-bind-syntaxes (list #'y) #'(make-rename-transformer #'x) intdef-ctx-b)

             (syntax-local-value (internal-definition-context-introduce intdef-ctx-b #'y)
                                 #f intdef-ctx-b))))
 exn:fail?)

(module parent-internal-definition-contexts-do-add-bindings racket/base
  (require (for-syntax racket/base))
  (provide result)
  (define-syntax (m stx)
    (define intdef-ctx-a (syntax-local-make-definition-context))
    (syntax-local-bind-syntaxes (list #'x) #'42 intdef-ctx-a)

    (define intdef-ctx-b (syntax-local-make-definition-context intdef-ctx-a))
    (define x_a-id (internal-definition-context-introduce intdef-ctx-a #'x))
    (syntax-local-bind-syntaxes (list #'y) #`(make-rename-transformer #'#,x_a-id) intdef-ctx-b)

    #`(quote #,(syntax-local-value (internal-definition-context-introduce intdef-ctx-b #'y)
                                   #f intdef-ctx-b)))
  (define result (m)))

(test 42 dynamic-require ''parent-internal-definition-contexts-do-add-bindings 'result)

;; ----------------------------------------
;; Make sure `local-expand` doesn't flip the use-site
;; scope in addition to the introduction scope

(module local-expand-result-depends-on-use-site-scope racket/base
  (require (for-syntax racket/base))
  (define x 1)
  
  (define-syntax (a stx)
    (syntax-case stx ()
      [(a id)
       (local-expand #'(let ([id 3]) x) 'expression '())]))
  
  (define result (a x))
  (provide result))

(test 1 dynamic-require ''local-expand-result-depends-on-use-site-scope 'result)

;; ----------------------------------------
;; Make sure `local-expand` doesn't get confused about the
;; post-expansion scope needed to make a binding phase-specific

(module test-for-scope-specific-binding racket/base

  (module l racket/base
    (require (for-syntax racket/base racket/syntax)
             syntax/parse/define)

    (provide #%module-begin m
             (all-from-out racket/base))

    (define-syntax-parser #%module-begin
      [(_ form ...)
       (let ([intdef-ctx (syntax-local-make-definition-context #f #f)])
         (local-expand #'(#%plain-module-begin form ...)
                       'module-begin
                       '()
                       intdef-ctx))])

    (define-syntax-parser m
      [(_)
       #:with x (generate-temporary)
       #'(begin
           (define x #f)
           ;; The #' here triggers a `syntax-local-value` call:
           (begin-for-syntax #'x))]))

  (module c (submod ".." l)
    (#%module-begin
     (m))))

(module test-for-scope-specific-binding/nested racket/base

  (module l racket/base
    (require (for-syntax racket/base racket/syntax)
             syntax/parse/define)

    (provide expand-in-modbeg m
             (all-from-out racket/base))

    (define-syntax-parser expand-in-modbeg
      [(_ form ...)
       (local-expand #'(#%plain-module-begin form ...)
                     'module-begin
                     '())
       #'(void)])

    (define-syntax-parser m
      [(_)
       #:with x (generate-temporary)
       #'(begin
           (define x #f)
           (begin-for-syntax #'x))]))

  (module c (submod ".." l)
    (let ()
      (expand-in-modbeg
       (m)))))

;; ----------------------------------------
;; Make sure `syntax-local-bind-syntaxes` binds variables in a way
;; that `local-expand` replaces a use with the binding scopes.

(module uses-definition-context-and-local-expand-to-replace racket/base
  (require (for-syntax racket/base))

  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ id)
       (let ()
         (define intdef (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (list #'id)
                                     #f ; => local variable
                                     intdef)
         (define raw-bind-id (internal-definition-context-introduce intdef #'id))
         (define raw-ex-id (local-expand ((make-syntax-introducer) #'id) 'expression null intdef))
         (define bind-id (syntax-local-identifier-as-binding raw-bind-id))
         (define ex-id (syntax-local-identifier-as-binding raw-ex-id))
         #`(list #,(free-identifier=? bind-id ex-id)
                 #,(bound-identifier=? bind-id ex-id)))]))

  (define result (m x))
  (provide result))

(test '(#t #t) dynamic-require ''uses-definition-context-and-local-expand-to-replace 'result)

;; ----------------------------------------
;; Check that a rename transformer mapped to a
;; primitive is ok in the result of a local expansion

(module module-begin-with-a-rename-transformer racket/base
  (require (for-syntax racket/base))

  (define-syntax m (make-rename-transformer #'#%expression))
  
  (begin-for-syntax
    (local-expand #'(#%plain-module-begin (m #f)) 'module-begin '())))

;; ----------------------------------------
;; Check order of complaints for unbound identifiers

(define (check-complain-first m)
  (err/rt-test (eval m)
               (lambda (x)
                 (regexp-match? #rx"complain-about-this-one" (exn-message x)))))

(check-complain-first '(module m racket/base
                         (define (f a)
                           (complain-about-this-one (not-about-this-one)))))
(check-complain-first '(module m racket/base
                         (require (for-syntax racket/base))
                         (define-syntax (f a)
                           (complain-about-this-one (not-about-this-one)))))

;; ----------------------------------------
;; Make sure "currently expanding" is not propagated to threads

(let ()
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ e)
       (let ([ok? #t])
         (sync (thread (lambda ()
                         (local-expand #'e 'expression null)
                         (set! ok? #f))))
         (if ok? #''ok #''oops))]))
  
  (test 'ok values (m 1)))

;; ----------------------------------------

(test 'ok 'scope-for-letrec
      (let ([x 'ok])
        (letrec-syntax ([foo (lambda (stx)
                               #'x)])
          (define x 2)
          (foo))))


(test 1 'scope-for-letrec
      (let ()
        (define-syntaxes (stash restore)
          (let ([stash #f])
            (values
             ;; stash
             (lambda (stx)
               (syntax-case stx ()
                 [(_ arg)
                  (begin (set! stash (syntax-local-introduce #'arg))
                         #'arg)]))
             ;; restore
             (lambda (stx)
               (syntax-local-introduce stash)))))

        (define x 1)

        (letrec-values ([(foo) (stash x)])
          (define x 2)
          (restore))))

;; ----------------------------------------
;; Make sure something reasonable happens when a `for-syntax` `define`
;; is seen via `local-expand` but is not preserved in the expansion

(module module-compiles-but-does-not-visit racket/base
  (require (for-syntax racket/base))

  (begin-for-syntax
    (when (eq? (syntax-local-context) 'module)
      (local-expand
       #'(#%plain-module-begin
          (begin-for-syntax
            (define x 42)))
       'module-begin
       '())))

  (begin-for-syntax
    ;; Weird: can be 42 at compile time, but since the for-syntax
    ;; `define` did not survive in the fully expanded form, it
    ;; turns into a reference to an undefined variable.
    x))

(err/rt-test/once (begin
                    (eval '(require 'module-compiles-but-does-not-visit))
                    ;; triggers visit:
                    (eval #t))
                  exn:fail:contract:variable?)

;; ----------------------------------------
;; Make sure a reasonable exceptoion happens when `local-expand`
;; is misused under `begin-for-syntax`

(module module-also-compiles-but-does-not-visit racket/base
  (require (for-syntax racket/base))

  (begin-for-syntax
    (local-expand
     #'(#%plain-module-begin
        (begin-for-syntax
          (define x 42)))
     'module-begin
     '())))

(err/rt-test/once (begin
                    (eval '(require 'module-also-compiles-but-does-not-visit))
                    ;; triggers visit:
                    (eval #t))
                  (lambda (exn)
                    (and (exn:fail:syntax? exn) ; the error is from `#%plain-module-begin`
                         (regexp-match? #rx"not currently transforming a module" (exn-message exn)))))

;; ----------------------------------------
;; Make sure `syntax-local-bind-syntaxes` binds installs `free-identifier=?`
;; equivalences in a context in which previous definitions in the context are bound

(module syntax-local-bind-syntaxes-free-id-context racket/base
  (require (for-syntax racket/base))
  (provide result)
  (define-syntax (letrec-syntax/intdef stx)
    (syntax-case stx ()
      [(_ ([x rhs] ...) e)
       (let ()
         (define intdef (syntax-local-make-definition-context))
         (for ([x (in-list (syntax->list #'(x ...)))]
               [rhs (in-list (syntax->list #'(rhs ...)))])
           (syntax-local-bind-syntaxes (list x) rhs intdef))
         (local-expand #'e 'expression '() intdef))]))
  (begin-for-syntax
    (struct indirect-rename-transformer (target-holder)
      #:property prop:rename-transformer
      (lambda (self) (syntax-local-value (indirect-rename-transformer-target-holder self)))))
  (define result
    (letrec-syntax/intdef ([holder #'add1]
                           [add1-indirect (indirect-rename-transformer #'holder)])
      (add1-indirect 10))))

(test 11 dynamic-require ''syntax-local-bind-syntaxes-free-id-context 'result)

;; ----------------------------------------
;; Related to the above, make sure `syntax-local-value/immediate` resolves rename
;; transformers in a context with first-class definition context bindings

(module syntax-local-value-free-id-context racket/base
  (require (for-syntax racket/base))
  (provide result)
  (begin-for-syntax
    (struct indirect-rename-transformer (target-holder)
      #:property prop:rename-transformer
      (lambda (self) (syntax-local-value (indirect-rename-transformer-target-holder self)))))
  (define-syntax (m stx)
    (define intdef (syntax-local-make-definition-context))
    (syntax-local-bind-syntaxes (list (syntax-local-introduce #'holder)) #'#'add1 intdef)
    (syntax-local-bind-syntaxes (list (syntax-local-introduce #'add1-indirect))
                                (syntax-local-introduce #'(indirect-rename-transformer #'holder))
                                intdef)
    (define-values [value target]
      (syntax-local-value/immediate
       (internal-definition-context-introduce intdef (syntax-local-introduce #'add1-indirect) 'add)
       #f
       (list intdef)))
    #`'#,(indirect-rename-transformer? value))
  (define result (m)))

(test #t dynamic-require ''syntax-local-value-free-id-context 'result)

;; ----------------------------------------
;; Ensure the expansion of a rename transformer is not `syntax-original?`

(module rename-transformer-introduction-scope racket/base
  (require (for-syntax racket/base))
  (provide sym free-id=? original? sameloc?
           set-sym set-free-id=? set-original? set-sameloc?)
  (define x #f)
  (define-syntax y (make-rename-transformer #'x))
  (define-syntax (m stx)
    (define orig-y #'y)
    (define expanded-y (syntax-local-introduce (local-expand orig-y 'expression '())))
    (define expanded-set-y (syntax-local-introduce (local-expand #`(set! #,orig-y 5) 'expression '())))
    (define (same-srcloc? a b) (and (equal? (syntax-source a) (syntax-source b))
                                    (equal? (syntax-line a) (syntax-line b))
                                    (equal? (syntax-column a) (syntax-column b))))
    (syntax-case expanded-set-y ()
      [(_ set!ed-y _)
       #`(values '#,(syntax-e expanded-y)
                 '#,(free-identifier=? expanded-y #'x)
                 '#,(syntax-original? expanded-y)
                 '#,(same-srcloc? expanded-y orig-y)
                 '#,(syntax-e #'set!ed-y)
                 '#,(free-identifier=? #'set!ed-y #'x)
                 '#,(syntax-original? #'set!ed-y)
                 '#,(same-srcloc? #'set!ed-y orig-y))]))
  (define-values (sym free-id=? original? sameloc?
                      set-sym set-free-id=? set-original? set-sameloc?)
    (m)))

(test 'x dynamic-require ''rename-transformer-introduction-scope 'sym)
(test #t dynamic-require ''rename-transformer-introduction-scope 'free-id=?)
(test #f dynamic-require ''rename-transformer-introduction-scope 'original?)
(test #t dynamic-require ''rename-transformer-introduction-scope 'sameloc?)

(test 'x dynamic-require ''rename-transformer-introduction-scope 'set-sym)
(test #t dynamic-require ''rename-transformer-introduction-scope 'set-free-id=?)
(test #f dynamic-require ''rename-transformer-introduction-scope 'set-original?)
(test #t dynamic-require ''rename-transformer-introduction-scope 'set-sameloc?)

;; ----------------------------------------
;; Make sure replacing scopes of binding on reference does not
;; turn a non-`syntax-original?` identifier into a `syntax-original?`
;; one

(let ([m #'(module m racket/base
             (let ()
               (define x 10)
               (define-syntax y
                 (syntax-rules ()
                   [(_) x]))
               (+ (y)
                  x)))])
  (define found-it? #f)
  (define (check s)
    (cond
      [(syntax? s)
       (when (and (syntax-original? s)
                  (eq? (syntax-e s) 'x))
         (test #f = (syntax-line s) (+ (syntax-line m) 6))
         (when (= (syntax-line s) (+ (syntax-line m) 7))
           (set! found-it? #t)))
       (check (syntax-e s))]
    [(pair? s)
     (check (car s))
     (check (cdr s))]))
  (check (expand m))
  (test #t values found-it?))

;; ----------------------------------------
;; Make sure that `block` forces an expression context with #%expression

(let ()
  (define-syntax-rule (m x) (set! x 'outer))
  (define res #f)
  (let ()
    (block
      (m res))
    (define-syntax-rule (m x) (set! x 'inner))
    (void))
  (test 'inner values res))

;; ----------------------------------------
;; Make sure that `block` works normally in a non-expression context
(let ()
  (block
    ; ensure forward references work
    (define (f x) (g x))
    ; ensure definition splices
    (define-syntax-rule (def-g name)
      (define (name x) (h x)))
    ; ensure use-site binder doesn't capture
    (define-syntax-rule (def-h name arg)
      (define (name x)
        (let ([arg 'bad])
          x)))
    (def-g g)
    (def-h h x)
    (test 'ok values (f 'ok))))

;; ----------------------------------------
;; Make sure that `local` works normally in a non-expression context

(require racket/local)
(let ()
  (local [; ensure forward references work
          (define (f x) (g x))
          ; ensure definition splices
          (define-syntax-rule (def-g name)
            (define (name x) x))
          ; ensure use-site binder doesn't capture
          (define-syntax-rule (def-h name arg)
            (define (name x)
              (let ([arg 'bad])
                x)))
          (def-g g)
          (def-h h x)]
    (test 'ok values (f 'ok))))

;; ----------------------------------------
;; Make sure that `block` does not evaluate phase 1 expressions multiple times

(module block-define-syntax-evaluation racket/base
  (require (for-syntax racket/base) racket/block)
  (provide final-counter)

  (define-for-syntax counter 0)
  (define-syntax (get-counter stx) #`'#,counter)
  (block (define-syntax m (set! counter (add1 counter))))
  (define final-counter (get-counter)))

(test 1 dynamic-require ''block-define-syntax-evaluation 'final-counter)

;; ----------------------------------------
;; Allow `#%app` at the top level to expand to a definition

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(define-syntax-rule (#%app id) (begin (define id 'id) id)))
  (test 'v eval '(v))
  (test 'v eval 'v))

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax-rule (#%app id) (begin (define-syntax (id stx) #''id) (begin))))
  (eval '(v))
  (test 'v eval 'v))

;; ----------------------------------------
;; Check definition context outside-edge scope behavior

(module definition-context-outside-edge-macro-defs racket/base
  (require (for-syntax racket/base))
  (provide simple-let-syntax partial-expand-in-ctx)

  (define-syntax (simple-let-syntax stx)
    (syntax-case stx ()
      [(_ [v e] b)
       (let ([ctx (syntax-local-make-definition-context)])
         (syntax-local-bind-syntaxes
           (list (internal-definition-context-add-scopes ctx #'v))
           #'e ctx)
         (local-expand
           (internal-definition-context-add-scopes ctx #'b)
           'expression '() ctx))]))

  (define-syntax (partial-expand-in-ctx stx)
    (syntax-case stx ()
      [(_ b)
       (let ([ctx (syntax-local-make-definition-context)])
         (local-expand
           (internal-definition-context-add-scopes ctx #'b)
           'expression (list #'#%expression) ctx))])))

;; The scope should prevent introduced reference capture
(module definition-context-outside-edge-introduced-reference racket/base
  (require 'definition-context-outside-edge-macro-defs (for-syntax racket/base))
  (provide res)
  (define-syntax-rule (x) 'outside)
  (define-syntax-rule (m) (x))
  (define res
    (simple-let-syntax [x (lambda (stx) #''captured)]
      (m))))
(test 'outside dynamic-require ''definition-context-outside-edge-introduced-reference 'res)

;; The scope should be removed at quote-syntax, either within local-expand
;; or when expanded later
(module definition-context-outside-edge-quote-syntax racket/base
  (require (for-syntax racket/base 'definition-context-outside-edge-macro-defs) (for-meta 2 racket/base))
  (provide res1 res2)
  (define-syntax (m1 stx)
    (define id (simple-let-syntax [m (lambda (stx) #''ignore)]
                                  #'x))
    #`(let ([#,id 'bound])
        x))
  (define res1 (m1))
  (define-syntax (m2 stx)
    (define id (partial-expand-in-ctx (#%expression #'x)))
    #`(let ([#,id 'bound])
        x))
  (define res2 (m2)))
(test 'bound dynamic-require ''definition-context-outside-edge-quote-syntax 'res1)
(test 'bound dynamic-require ''definition-context-outside-edge-quote-syntax 'res2)

;; The scope should be added by syntax-local-get-shadower
(module definition-context-outside-edge-get-shadower racket/base
  (require 'definition-context-outside-edge-macro-defs (for-syntax racket/base))
  (provide res)
  (define-for-syntax id (car (generate-temporaries '(x))))
  (define-syntax (ref stx)
    (syntax-local-get-shadower (syntax-local-introduce id)))
  (define-syntax (binding stx)
    #`(simple-let-syntax [#,(syntax-local-identifier-as-binding
                              (syntax-local-introduce id))
                          (lambda (stx)
                            #''bound)]
        (ref)))
  (define res (binding)))
(test 'bound dynamic-require ''definition-context-outside-edge-get-shadower 'res)

;; internal-definition-context-splice-binding-identifier should remove the scope
(module definition-context-outside-edge-splice racket/base
  (require (for-syntax racket/base))
  (provide res)
  (define-syntax (two-level-block stx)
    (syntax-case stx (splicing-let-syntax define-syntax)
      [(_ (splicing-let-syntax ([v1 e1]) (define-syntax v2 e2)) e3)
       (let* ([outer-ctx (syntax-local-make-definition-context)]
              [inner-ctx (syntax-local-make-definition-context outer-ctx)])
         (with-syntax ([(v1 e1 v2 e2 e3) (internal-definition-context-add-scopes outer-ctx #'(v1 e1 v2 e2 e3))])
           (with-syntax ([(v1 e1 v2 e2) (internal-definition-context-add-scopes inner-ctx #'(v1 e1 v2 e2))])
             (syntax-local-bind-syntaxes
               (list #'v1)
               #'e1 inner-ctx)
             (syntax-local-bind-syntaxes
               (list (internal-definition-context-splice-binding-identifier inner-ctx #'v2))
               #'e2 outer-ctx)
             (local-expand #'e3 'expression '() inner-ctx))))]))
  (define-syntax-rule (m1) 'outer)
  (define res (two-level-block
                (splicing-let-syntax ([m1 (lambda (stx) #''inner)])
                  (define-syntax m2 (lambda (stx) #'(m1))))
                (m2))))
(test 'inner dynamic-require ''definition-context-outside-edge-splice 'res)

;; ----------------------------------------
;; Check that `syntax-local-bind-syntaxes` returns identifiers with intdef
;; scopes attached and use-site scopes removed.

(let ()
  (define res
    (let ()
      ; define and use in same definition context to trigger use-site scope
      (define-syntax m
        (lambda (stx)
          (syntax-case stx ()
            [(_ arg)
             (let ()
               (define ctx (syntax-local-make-definition-context))
               (define orig-id #'arg)
               (define bind-id (car (syntax-local-bind-syntaxes (list orig-id) #f ctx)))
               (define scoped-id
                 (syntax-local-identifier-as-binding
                   (internal-definition-context-introduce ctx orig-id 'add) ctx))
               #`#,(bound-identifier=? bind-id scoped-id))])))
      (m x)))
  (test #t values res))

;; ----------------------------------------
;; Check that definitions in `block` don't capture introduced references

(let ()
  (define x 'outer)
  (define-syntax-rule (m) x)
  (define res
   (block
     (define x 'inner)
     (m)))
  (test 'outer values res))

;; ----------------------------------------
;; Test the behavior of the definition context scope operations
;; in the context of define*-like definitions. The easist way to
;; do this is via a block macro that supports define*.

(module define-*-definition-context racket/base
  (require (for-syntax racket/base syntax/transformer syntax/context racket/syntax))
  (provide do-test)

  (define-syntax define-values*
    (lambda (stx) (raise-syntax-error #f "define-values* may only be used in block*" stx)))
  (define-syntax (define* stx)
    (syntax-case stx ()
      [(_ a b) (syntax-property #'(define-values* (a) b) 'taint-mode 'transparent-binding)]))

  (define-syntax block*
    (make-expression-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ body ...)
          (let ()
            (define stop-list (list #'begin #'define-values #'define-values* #'define-syntaxes))
            (define ctx-id (list (generate-expand-context #t)))
            (define top-def-ctx (syntax-local-make-definition-context))

            ; mutated
            (define stxs '())
            (define vals '())
            (define def-ctx top-def-ctx)
            (define inner-def-ctxs '())
            (define last-expr #'(void))
            (define worklist (syntax->list (internal-definition-context-add-scopes top-def-ctx #'(body ...))))
          
            (define (pop!)
              (let ([r (car worklist)])
                (set! worklist (cdr worklist))
                r))
          
            (define (new-scope!)
              (set! def-ctx (syntax-local-make-definition-context def-ctx))
              (set! inner-def-ctxs (cons def-ctx inner-def-ctxs))
              (set! worklist
                    (for/list ([s worklist])
                      (internal-definition-context-add-scopes def-ctx s))))
      
            (define (splice id)
              (for/fold ([id id])
                        ([def-ctx inner-def-ctxs])
                (internal-definition-context-splice-binding-identifier def-ctx id)))

            (define (drop-use-sites stx)
              (for/fold ([stx stx])
                        ([def-ctx (cons top-def-ctx inner-def-ctxs)])
                (syntax-local-identifier-as-binding stx def-ctx)))

            (let loop ()
              (define form^ (local-expand (pop!) ctx-id stop-list def-ctx))
            
              (syntax-case form^ (begin define-values define-values* define-syntaxes)
                [(begin . rest)
                 (set! worklist (append (syntax->list #'rest) worklist))]
                [(define-syntaxes (v ...) rhs)
                 (with-syntax* ([rhs^ (local-transformer-expand #'rhs
                                                                'expression '() def-ctx)]
                                [(v^ ...) (syntax-local-bind-syntaxes
                                           (map splice (syntax->list #'(v ...)))
                                           #'rhs^ def-ctx)])
                   (set! stxs (cons #'[(v^ ...) rhs^] stxs)))]
                [(define-values (v ...) rhs)
                 (with-syntax ([(v^ ...) (syntax-local-bind-syntaxes
                                          (map splice (syntax->list #'(v ...)))
                                          #f top-def-ctx)])
                   (set! vals (cons #'[(v^ ...) rhs] vals)))]
                [(define-values* (v ...) rhs)
                 (begin
                   (new-scope!)
                   (with-syntax* ([(v^ ...)
                                   (syntax-local-bind-syntaxes
                                    (map drop-use-sites (syntax->list (internal-definition-context-add-scopes def-ctx #'(v ...))))
                                    #f def-ctx)])
                     (set! vals (cons #'[(v^ ...) rhs] vals))))]
                [other-stx
                 (with-syntax ([tmp (generate-temporary #'tmp)])
                   (set! vals (cons #'[(tmp) other-stx] vals))
                   (set! last-expr #'tmp))])

              (unless (null? worklist) (loop)))

            #`(letrec-syntaxes+values #,(reverse stxs)
                #,(reverse vals)
                #,last-expr))]))))

  (define (do-test test)
    ; basic functionality
    (test
     11
     'define-*-definition-context
     (block*
      (define f (lambda () y))
      (define x 5)
      (set! x (+ x 1))
      (define* x (+ x 2))
      (define y (+ x 3))
      (f)))

    ; Analgue of test in compatibility-test/tests/racket/package.rkt
    (test
     10
     'define-*-definition-context
     (block*
       (define* x 10)
       (define-syntax (y stx)
         (syntax-case stx ()
           [(_ z) #'(begin (define z x))]))
       (define* x 12)
       (define* z 13)
       (y q)
       q))

    ; correct use-site binder behavior
    (test
     '(new new)
     'define-*-definition-context
     (let ()
       (define x 'old)
       (define y 'old)
       (block*
        (define-syntax-rule (m arg1 arg2)
          (begin
            (define* arg1 'new)
            (define arg2 'new)
            (list x y)))
        (m x y))))

    ; correct inside-edge scope behavior
    (test
     '(inner inner)
     'define-*-definition-context
     (let ()
       (define x 'outer)

       (define-syntax (m1 stx)
         (with-syntax ([id (syntax-local-introduce #'x)])
           #'(begin
               (define id 'inner)
               id)))
     
       (define-syntax (m2 stx)
         (with-syntax ([id (syntax-local-introduce #'x)])
           #'(begin
               (define* id 'inner)
               id)))
       (list
        (block*
         (m1)
         (m2)
         (m2))
        (block*
         (m1)
         (m2)
         (m2)))))

    ; correct quote-syntax scope stripping
    (test
     #t
     'define-*-definition-context
     (let ([id1 (quote-syntax x)])
       (block*
        (define* x 5)
        (define id2 (quote-syntax x))
        (bound-identifier=? id1 id2))))))

((dynamic-require ''define-*-definition-context 'do-test) test)

;; ----------------------------------------
;; Check that backwards compatibility behavior for local-expand
;; with a list of definition contexts minimizes breakage via precise
;; frame id

(module defctx-list racket
  (provide res)
  
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ d e)
       (let ()
         (define def-ctx (syntax-local-make-definition-context))
         (syntax-case (local-expand #'d (list (gensym)) (list #'define-values) (list def-ctx)) (define-values)
           [(define-values (v) rhs)
            (syntax-local-bind-syntaxes (list #'v) #f def-ctx)])
         (define e^ (local-expand #'e 'expression '() (list def-ctx)))
         #''success)]))

  (define res
    (m
     (define x 5)
     (+ x x))))

(test 'success dynamic-require ''defctx-list 'res)

;; ----------------------------------------
;; regression test

(test (void) eval '(require (combine-in)))

;; ----------------------------------------
;; regression test

(err/rt-test (eval '(module m racket/base
                      (require (for-syntax racket/base))
                      (define-for-syntax (f)
                        (values 1 2))
                      (define-for-syntax (g)
                        (define x (f))
                        (displayln 1)
                        1)
                      (begin-for-syntax
                        (g))))
             exn:fail:contract:arity?
             #rx"received: 2")


;; ----------------------------------------
;; regression test

(err/rt-test
 (eval
  '(module m '#%kernel
     (#%require (for-syntax '#%kernel))
     (begin-for-syntax
       (define-values (ctx) (syntax-local-make-definition-context))
       (define-values (x-id) (internal-definition-context-add-scopes ctx (quote-syntax deadbeef-x)))
       (define-values (y-id) (internal-definition-context-add-scopes ctx (quote-syntax deadbeef-y)))
       (syntax-local-bind-syntaxes (list (syntax-shift-phase-level x-id -1)) (quote-syntax (lambda (s) (quote-syntax 1000))) ctx)
       (syntax-local-bind-syntaxes (list y-id) (datum->syntax x-id '(+ 1 (deadbeef-x))) ctx))))
 exn:fail:syntax?
 #rx"deadbeef-x: identifier used out of context")

;; ----------------------------------------
;; regression test for local-expand and out-of-context variables

(err/rt-test
 (eval
  '(module m racket/base
     (require (for-syntax racket/base))
     (define-syntax (foo stx)
       (define id (datum->syntax #f 'id))
       (local-expand
        #`(let ([#,id "ok"])
            (let-syntax ([other #,id])
              'done))
        'expression
        '()))
     (foo)))
 exn:fail:syntax?
 #rx"id: identifier used out of context")

;; ----------------------------------------
;; check for `syntax-original?` of `module+`

(for ([stx (list #'(module m racket/base (module+ m))
                 #'(module m racket/base (module+ m) 0)
                 #'(module m racket/base (module+ m 1))
                 #'(module m racket/base (module+ m 1) (module+ m 2))
                 #'(module m racket/base (module+ m 1) (module+ m 2) 0))])
  (test #t 'module+original?
        (let loop ([stx (expand stx)])
          (cond
            [(pair? stx)
             (or (loop (car stx))
                 (loop (cdr stx)))]
            [(identifier? stx)
             (and (syntax-original? stx)
                  (eq? (syntax-e stx) 'module+))]
            [(syntax? stx)
             (or (loop (syntax-e stx))
                 (loop (syntax-property stx 'origin)))]
            [else #f]))))


;; ----------------------------------------
;; check that conversion of `defines` to nested `let-synatx`
;; re-expands correctly

(module reexpand-should-not-be-confused-by-internal-definition-to-nested-lets racket/base
  (require (for-syntax racket/base))

  (define-syntax (re-expand stx)
    (syntax-case stx ()
      [(_ e)
       (local-expand #'e 'expression null)]))

  (#%expression
   (re-expand
    (let ()
      (define-syntax-rule (m y)
        (begin
          (define x 'a)
          (define y 'b)
          (println x)))
      (m x)
      (println x)))))

(module reexpand-should-not-be-confused-by-internal-definition-to-nested-letrec racket/base
  (require (for-syntax racket/base))

  (define-syntax (re-expand stx)
    (syntax-case stx ()
      [(_ e)
       (local-expand #'e 'expression null)]))

  (#%expression
   (re-expand
    (let ()
      (define (call) 'ok)
      (define (step) (return))
      (define (return) 'done)
      step))))

(module reexpand-should-not-be-confused-by-keyword-arguments-either racket/base
  (require (for-syntax racket/base))

  (define-syntax (re-expand stx)
    (syntax-case stx ()
      [(_ e)
       (local-expand #'e 'expression null)]))

  (#%expression
   (re-expand
    (let ()
      (define (call) 'ok)
      (define (step) (return #:arg 1))
      (define (return #:arg x) 'done)
      step))))

;; ----------------------------------------

(report-errs)

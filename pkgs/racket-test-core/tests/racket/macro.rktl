
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
  (test '("(#(struct:liberal-define-context))\n10\n" "") go '(definition-context expression) in-defctx 'prop:procedure)
  (test '("(#(struct:liberal-define-context))\n10\n" "") go '(definition-context) in-defctx 'prop:procedure)
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
             #rx"macro.rktl:.*no expression after a sequence of internal definitions")

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

(for ([lift-attempt+rx:expected-error
       (in-list
        (list (cons '(syntax-local-lift-require 'racket #'body)
                    #rx"could not find target context")
              (cons '(syntax-local-lift-expression #'body)
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
                         (_req (_just-meta _0 (_rename rl1 . _whatever))
                               (_only rl2))))
       (list #'rl1 #'rl2)]))
  
  (test (list #t #t) map syntax-original? r/ls)
  (test (list #t #t) map number? (map syntax-position r/ls)))

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
      (define-syntax-rule (m) 1)
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
;; Make sure that changing the code expander disables syntax
;; disarming when it should

(parameterize ([current-namespace (make-base-namespace)]
               [current-code-inspector (current-code-inspector)])

  (eval
   '(module disarm racket/base
      (require (for-template racket/base))
      (define (disarm s)
        (unless (syntax-tainted? (car (syntax-e (syntax-disarm s #f))))
          (error "disarm suceeded!"))
        #''ok)
      (provide disarm)))

  (eval
   '(module mapply racket/base
      (require (for-syntax racket/base))
      (provide mapply)
      (define-syntax (mapply stx)
        (syntax-case stx ()
          [(_ m) #`(m #,(syntax-protect #'(x)))]))))

  (current-code-inspector (make-inspector (current-code-inspector)))

  (eval
   '(module orig racket/base
      (require (for-syntax racket/base
                           'disarm)
               'mapply)
      (define-syntax (m stx)
        (syntax-case stx ()
          [(_ e) (disarm #'e)]))
      (mapply m))))

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
;; Make sure somethign reasonable happens when a `for-syntax` `define`
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

(err/rt-test (begin
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

(err/rt-test (begin
               (eval '(require 'module-also-compiles-but-does-not-visit))
               ;; triggers visit:
               (eval #t))
             (lambda (exn)
               (and (exn:fail:syntax? exn) ; the error is from `#%plain-module-begin`
                    (regexp-match? #rx"not currently transforming a module" (exn-message exn)))))

;; ----------------------------------------

(report-errs)


(load-relative "loadtest.rktl")

(require racket/class)

(Section 'object)

;; ------------------------------------------------------------
;; Test syntax errors

(syntax-test #'class)
(syntax-test #'(class))
(syntax-test #'(class . object%))
(test #t class? (class object%))
(syntax-test #'(class object% . 10))

(error-test #'(send 7) (lambda (x) (and (exn:fail:syntax? x) (regexp-match "method" (exn-message x)))))
(error-test #'(send/apply 7) (lambda (x) (and (exn:fail:syntax? x) (regexp-match "method" (exn-message x)))))
(error-test #'(send/keyword-apply 7) (lambda (x) (and (exn:fail:syntax? x) (regexp-match "method" (exn-message x)))))
(error-test #'(send/keyword-apply 7 method) (lambda (x) (and (exn:fail:syntax? x) (regexp-match "list of keywords" (exn-message x)))))
(error-test #'(send/keyword-apply 7 method 8) (lambda (x) (and (exn:fail:syntax? x) (regexp-match "list of keyword arguments" (exn-message x)))))

(define (test-init/field init)
  (teval #`(test #t class? (class object% (#,init))))
  (syntax-test #`(class object% (#,init . x)))
  (syntax-test #`(class object% (#,init 10)))
  (syntax-test #`(class object% (#,init (x . 10))))
  (syntax-test #`(class object% (#,init (x 10 10))))
  (syntax-test #`(class object% (#,init (x 10 . 10))))
  (syntax-test #`(class object% (#,init (10 10))))
  (teval #`(test #t class? (class object% (#,init (x 10)))))
  (syntax-test #`(class object% (#,init ((x) 10))))
  (syntax-test #`(class object% (#,init ((x . y) 10))))
  (syntax-test #`(class object% (#,init ((x y . z) 10))))
  (syntax-test #`(class object% (#,init ((x y z) 10))))
  (syntax-test #`(class object% (#,init ((x 10) 10))))
  (syntax-test #`(class object% (#,init ((10 x) 10))))
  (teval #`(test #t class? (class object% (#,init ((x y) 10)))))
  (syntax-test #`(class object% (#,init ((x y) 10) . z)))
  (syntax-test #`(class object% (#,init ((x y) 10) z)))
  (syntax-test #`(class object% (#,init ((x y) 10) (z))))
  (teval #`(test #t class? (class object% (#,init ((x y) 10) (z 5)))))
  (syntax-test #`(class object% (#,init (x 10) y)))
  (syntax-test #`(class object% (#,init (x 10)) (#,init y)))

  (syntax-test #`(class object% (#,init x x)))
  (syntax-test #`(class object% (#,init x) (#,init x)))
  (syntax-test #`(class object% (#,init x) (#,init (x 10))))
  (syntax-test #`(class object% (#,init (x 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((y x) 10)) (#,init ((z x) 10))))
  (syntax-test #`(class object% (#,init x) (#,init x)))
  (syntax-test #`(class object% (#,init x) (#,init (x 10))))
  (syntax-test #`(class object% (#,init (x 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((y x) 10)) (#,init ((z x) 10))))

  (teval #`(test #t class? (class object% (#,init ((x x) 10)))))
  (teval #`(test #t class? (class object% (#,init ((x x) 10) ((y y) 10)))))
  (teval #`(test #t class? (class object% (#,init ((x x) 10)) (#,init ((y y) 10)))))
  (teval #`(test #t class? (class object% (#,init ((x y) 10)) (#,init ((y x) 10)))))

  'ok)

(define (test-init init)
  (teval #`(test #t class? (class object% (#,init x))))
  (teval #`(test #t class? (class object% (#,init ((x y))))))
  (test-init/field init)

  (syntax-test #`(class object% (init-rest) (#,init y)))
  (syntax-test #`(class object% (#,init x) (init-rest) (#,init y)))
  (syntax-test #`(class object% (#,init y) (init-rest y)))
  (teval #`(test #t class? (class object% (#,init [(x y)]) (init-rest y))))

  'ok)

(test-init #'init)
(test-init #'init-field)
(test-init/field #'field)

(syntax-test #'(class object% (init-rest 10)))
(syntax-test #'(class object% (init-rest . x)))
(syntax-test #'(class object% (init-rest x y)))
(syntax-test #'(class object% (init-rest) (init-rest x)))
(syntax-test #'(class object% (init-rest x) (init-rest)))

(syntax-test #'(class object% (init-field (x 10)) (init y)))
(syntax-test #'(class object% (init (x 10)) (init-field y)))
(syntax-test #'(class object% (init-rest x) (init y)))
(syntax-test #'(class object% (init-rest x) (init-field y)))

(define to-override-class%
  (class object%
    (public x y)
    (define (x) 1)
    (define (y) 2)))
(define to-augment-class%
  (class object%
    (pubment x y)
    (define (x) 1)
    (define (y) 2)))

(define to-override2-class%
  (class to-augment-class%
    (augride x y)
    (define (x) 1)
    (define (y) 1)))
(define to-augment2-class%
  (class to-override-class%
    (overment x y)
    (define (x) 1)
    (define (y) 1)))

(define to-override3-class%
  (class to-override2-class%
    (oevrride x y)
    (define (x) 1)
    (define (y) 1)))
(define to-augment3-class%
  (class to-augment2-class%
    (augment x y)
    (define (x) 1)
    (define (y) 1)))

(define (test-method basic? public object% over? aug? super-ok? inner-ok? over-ok? aug-ok?)
  (when basic?
    (teval #`(test #t class? (class #,object% (#,public))))
    
    (syntax-test #`(class #,object% (#,public . x)))
    (syntax-test #`(class #,object% (#,public 10)))
    (syntax-test #`(class #,object% (#,public (x))))
    (syntax-test #`(class #,object% (#,public (x . y))))
    (syntax-test #`(class #,object% (#,public (x 10))))
    (syntax-test #`(class #,object% (#,public (10 x))))
    (syntax-test #`(class #,object% (#,public x . y)))
    (syntax-test #`(class #,object% (#,public x 10)))
    
    (syntax-test #`(class #,object% (#,public x x)))
    (syntax-test #`(class #,object% (#,public x) (#,public x)))
    (syntax-test #`(class #,object% (#,public (x y) (x y))))
    (syntax-test #`(class #,object% (#,public (x y1) (x y))))
    (syntax-test #`(class #,object% (#,public (x y) (x2 y)))))

  (unless (free-identifier=? public #'private)
    (if (and (or (not over?) over-ok?)
	     (or (not aug?) aug-ok?))
	(begin
	  (teval #`(test #t class? (class #,object% (#,public (x x)) (define (x) 1))))
	  (teval #`(test #t class? (class #,object% (#,public (x y) (y x)) (define (x) 1) (define (y) 2)))))
	(begin
	  (teval #`(err/rt-test (class #,object% (#,public (x x)) (define (x) 1)) exn:fail:object?))
	  (teval #`(err/rt-test (class #,object% (#,public (x y) (y x)) (define (x) 1) (define (y) 2)) exn:fail:object?)))))

  ;; Use of external name for super/inner is always wrong (but
  ;; maybe because super/inner isn't allowed):
  (syntax-test #`(class #,object% (#,public [x ex]) (define (x y) (super ex 12))))
  (syntax-test #`(class #,object% (#,public [x ex]) (define (x y) (inner 5 ex 12))))

  (let ([expr #`(class #,object% 
		  (#,public x)
		  (define (x y) (super x 10)))])
    (if (and super-ok? over-ok?)
	(begin
	  (teval #`(test #t class? #,expr))
	  (teval #`(test #t class? (class #,object% 
				     (#,public [ex x])
				     (define (ex y) (super ex 10))))))
	(if super-ok?
	    (teval #`(err/rt-test #,expr exn:fail:object?))
	    (syntax-test expr))))
  (let ([expr #`(class #,object% 
		  (#,public x)
		  (define (x y) (inner 5 x 10)))])
    (if (and inner-ok? aug? aug-ok?)
	(begin
	  (teval #`(test #t class? #,expr))
	  (teval #`(test #t class? (class #,object% 
				     (#,public [ex x])
				     (define (ex y) (inner 5 ex 10))))))
	(if inner-ok?
	    (if (or (and aug? (not aug-ok?))
		    (and over? (not over-ok?)))
		(teval #`(err/rt-test #,expr exn:fail:object?))
		(teval #`(test #t class? #,expr)))
	    (syntax-test expr))))

  'ok)

(test-method #t #'public #'object% #f #f #f #f #f #f)
(test-method #t #'public-final #'object% #f #f #f #f #f #f)
(test-method #t #'pubment #'object% #f #f #f #t #f #f)
(test-method #t #'private #'object% #f #f #f #f #f #f)
(define (test-over/aug to-override-class% to-augment-class%)
  (test-method #t #'override to-override-class% #t #f #t #f #t #f)
  (test-method #f #'override to-augment-class% #t #f #t #f #f #t)
  (test-method #t #'override-final to-override-class% #t #f #t #f #t #f)
  (test-method #f #'override-final to-augment-class% #t #f #t #f #f #t)
  (test-method #t #'overment to-override-class% #t #f #t #t #t #f)
  (test-method #f #'overment to-augment-class% #t #f #t #t #f #t)
  (test-method #t #'augment to-override-class% #f #t #f #t #t #f)
  (test-method #f #'augment to-augment-class% #f #t #f #t #f #t)
  (test-method #t #'augment-final to-override-class% #f #t #f #f #t #f)
  (test-method #f #'augment-final to-augment-class% #f #t #f #f #f #t)
  (test-method #t #'augride to-override-class% #f #t #f #f #t #f)
  (test-method #f #'augride to-augment-class% #f #t #f #f #f #t))

(test-over/aug #'to-override-class% #'to-augment-class%)
(test-over/aug #'to-override2-class% #'to-augment2-class%)
(test-over/aug #'to-override3-class% #'to-augment3-class%)

(define (test-rename rename object%)
  (teval #`(test #t class? (class #,object% (#,rename))))
  (teval #`(err/rt-test (class #,object% (#,rename [x x])) exn:fail:object?))
  (teval #`(err/rt-test (class #,object% (#,rename [y x])) exn:fail:object?))
  (teval #`(err/rt-test (class #,object% (#,rename [y x][z x])) exn:fail:object?))
  (syntax-test #`(class #,object% (#,rename . x)))
  (syntax-test #`(class #,object% (#,rename x)))
  (syntax-test #`(class #,object% (#,rename [x 1])))
  (syntax-test #`(class #,object% (#,rename [1 x])))
  (syntax-test #`(class #,object% (#,rename [x 1 2])))
  (syntax-test #`(class #,object% (#,rename [x y][x y])))
  
  10)

(test-rename #'rename-super #'object%)
(test-rename #'rename-inner #'object%)

(define (test-abstract object%)
  (syntax-test #`(class #,object% (abstract . x)))
  (syntax-test #`(class #,object% (abstract 1)))
  (syntax-test #`(class #,object% (abstract [x 1])))
  (syntax-test #`(class #,object% (abstract [x y])))
  (syntax-test #`(class #,object% (abstract [x 1 2])))
  (syntax-test #`(class #,object% (abstract [x] [y]))))

(test-abstract #'object%)

(define (class-keyword-test kw)
  (syntax-test kw)
  (syntax-test #`(#,kw (x) 10)))
  
(class-keyword-test #'public)
(class-keyword-test #'public-final)
(class-keyword-test #'private)
(class-keyword-test #'pubment)
(class-keyword-test #'override)
(class-keyword-test #'override-final)
(class-keyword-test #'overment)
(class-keyword-test #'augment)
(class-keyword-test #'augment-final)
(class-keyword-test #'augride)
(class-keyword-test #'rename-super)
(class-keyword-test #'rename-inner)
(class-keyword-test #'inherit)
(class-keyword-test #'inherit-field)
(class-keyword-test #'public*)
(class-keyword-test #'private*)
(class-keyword-test #'pubment*)
(class-keyword-test #'override*)
(class-keyword-test #'overment*)
(class-keyword-test #'augment*)
(class-keyword-test #'augride*)
(class-keyword-test #'abstract)
(class-keyword-test #'define/public)
(class-keyword-test #'define/private)
(class-keyword-test #'define/pubment)
(class-keyword-test #'define/override)
(class-keyword-test #'define/overment)
(class-keyword-test #'define/augment)
(class-keyword-test #'define/augride)
(class-keyword-test #'super)
(class-keyword-test #'inner)
(class-keyword-test #'this)
(class-keyword-test #'this%)
(class-keyword-test #'super-new)
(class-keyword-test #'super-make-object)
(class-keyword-test #'super-instantiate)
(class-keyword-test #'inspect)

(test #t class? (class object% (define-values () (values))))

;; ------------------------------------------------------------
;; Test basic functionality

(define eater<%> (interface () eat))

(define-syntax mk-noop
  (syntax-rules 
   ()
   [(_ name)
    (begin
      (define (name) (blah))
      (define (blah)
	(printf "hi\n")))]))

(define fish%
  (class* object% (eater<%>)
    (public get-size grow eat)
    (public-final noop)

    (mk-noop noop)
    (private increase-size eat-sized-fish)

    (init-field [size 1])

    ;; Private methods
    (define (increase-size s)
      (set! size (+ s size)))
    (define (eat-sized-fish s)
      (grow s))

    ;; Public methods
    (define (get-size) size)
    (define (grow s)
      (noop)
      (set! size (+ s size))
      size)
    (define (eat f)
      (let ([this 5]) ; <- make sure methods still work...
	(grow (send f get-size))))

    (super-instantiate ())))

(define fish1 (make-object fish% 10))
(define fish2 (make-object fish% 100))

(test 10 'f1 (send fish1 get-size))
(test 100 'f2 (send fish2 get-size))

(test 12 'g1 (send fish1 grow 2))
(test 103 'g2 (send fish2 grow 3))

(test 115 'e (send fish2 eat fish1))

(define fish-size (class-field-accessor fish% size))
(test 12 fish-size fish1)
(test 115 fish-size fish2)

(define color-fish%
  (class fish%
    (pubment die)
    (public-final die2)
    (inherit get-size)
    (inherit-field size)

    (init-field [color 'red])

    (define (die)
      (unless (= size (get-size))
	(error 'bad))
      (set! color 'black))
    (define (die2) (die))

    (super-new)))

(define blue-fish (instantiate color-fish% () (color 'blue) (size 10)))
(define red-fish (instantiate color-fish% () (size 1)))
(define dynamic-blue-fish (dynamic-instantiate color-fish% '() '([color . blue] [size . 10])))
(define dynamic-red-fish (dynamic-instantiate color-fish% '() '([size . 1])))

(define color-fish-color (class-field-accessor color-fish% color))

(let ()
  (define (go red-fish blue-fish)
    (test 'red color-fish-color red-fish)
    (test 'blue color-fish-color blue-fish)
    (test 'red color-fish-color red-fish)
    (test 'blue color-fish-color blue-fish)

    (test 1 'fr (send red-fish get-size))
    (test 10 'fb (send blue-fish get-size))

    (send red-fish grow 30)

    (test 31 'fr (send red-fish get-size))

    (test (void) 'fv (send blue-fish die))
    (test 'black color-fish-color blue-fish))

  (go red-fish blue-fish)
  (go dynamic-red-fish dynamic-blue-fish))

(let ([exn (with-handlers ([exn:fail? (lambda (exn) exn)])
	     (send red-fish get-size 10))])
  (test #t exn:fail:contract:arity? exn)
  ;; (test 1 exn:application-value exn)
  ;; (test 0 exn:application:arity-expected exn)
  )

(define picky-fish%
  (class fish%
    (override grow)
    (public set-limit)
    (rename-super [super-grow grow])

    (define pickiness 1)
    (define upper-limit 50)

    (define grow
      ;; Test method-declaration shape with variable:
      (let ([grow (lambda (s)
		    (super-grow (min upper-limit (- s pickiness))))])
	grow))
    (define set-limit 
      ;; Test method-declaration shape with body method:
      (let* ([check-pickiness (lambda (p)
				(unless (= p pickiness)
				  (error 'ack)))]
	     [set-upper (lambda (v p)
			  (check-pickiness p)
			  (set! upper-limit v))])
	(lambda (v)
	  (set-upper v pickiness))))

    (super-instantiate () (size 12))))

(define picky (make-object picky-fish%))

(test 12 'pf (send picky get-size))
(test 42 'pfe (send picky eat red-fish))
(test 42 'pf (send picky get-size))

(test (void) 'pfp (send picky set-limit 20))
(test 62 'pfe (send picky eat red-fish))

(test #t is-a? picky object%)
(test #t is-a? picky fish%)
(test #t is-a? picky picky-fish%)
(test #f is-a? picky color-fish%)

(test #t is-a? red-fish object%)
(test #t is-a? red-fish fish%)
(test #f is-a? red-fish picky-fish%)
(test #t is-a? red-fish color-fish%)

(test #t is-a? fish1 eater<%>)
(test #t is-a? picky eater<%>)
(test #t is-a? red-fish eater<%>)

(test #f is-a? 5 picky-fish%)
(test #f is-a? 5 eater<%>)

(test #t is-a? picky (class->interface picky-fish%))
(test #f is-a? red-fish (class->interface picky-fish%))

(err/rt-test (is-a? 1 2) exn:fail?)
(err/rt-test (is-a? red-fish 2) exn:fail?)
(test #f is-a? 11 eater<%>)

(err/rt-test (instantiate fish% () (bad-size 10)) exn:fail:object?)
(err/rt-test (dynamic-instantiate fish% '() '([bad-size . 10])) exn:fail:object?)
(err/rt-test (instantiate fish% () (size 10) (size 12)) exn:fail:object?)
(err/rt-test (dynamic-instantiate fish% '() '([size . 10] [size . 12])) exn:fail:object?)
(err/rt-test (instantiate fish% (10) (size 12)) exn:fail:object?)
(err/rt-test (dynamic-instantiate fish% '(10) '([size . 12])) exn:fail:object?)
(err/rt-test (instantiate picky-fish% () (size 17)) exn:fail:object?)
(err/rt-test (dynamic-instantiate picky-fish% '() '([size . 17])) exn:fail:object?)

(err/rt-test (color-fish-color picky))
(err/rt-test (color-fish-color 6))

;; ----------------------------------------
;;  Final and inner

;; Can't actually override `final', but it might call `inner'...

;; color-fish%'s die doesn't call inner:
(test (void) 
      'no-overment
      (send (new (class color-fish% (augment die) (define die (lambda () 'x)) (super-new))) die))
;; Can't override (only augment):
(err/rt-test (class color-fish% (override die) (define die (lambda () 'x))) exn:fail:object?)
(err/rt-test (class color-fish% (overment die) (define die (lambda () 'x))) exn:fail:object?)

;; color-fish%'s die2 is final:
(err/rt-test (class color-fish% (override die2) (define die2 (lambda () 'x))) exn:fail:object?)
(err/rt-test (class color-fish% (augment die2) (define die2 (lambda () 'x))) exn:fail:object?)
(err/rt-test (class color-fish% (overment die2) (define die2 (lambda () 'x))) exn:fail:object?)
(err/rt-test (class color-fish% (augride die2) (define die2 (lambda () 'x))) exn:fail:object?)

;; Can't augment (only override):
(err/rt-test (class color-fish% (augment eat) (define eat (lambda (f) 'x))) exn:fail:object?)
(err/rt-test (class color-fish% (augride eat) (define eat (lambda (f) 'x))) exn:fail:object?)

;; Can't use inner without a `final' here or in superclass
(syntax-test #'(class object% (define/public (f x) x) (rename-inner [inner-f f])))
(syntax-test #'(class object% (define/public (f x) (inner (void) f x))))
(err/rt-test (class object% (rename-inner [inner-f f])) exn:fail:object?)
(err/rt-test (class (class object% (define/public (f x) x)) (rename-inner [inner-f f])) exn:fail:object?)

;; Can't use `rename-super' for a final method:
(err/rt-test (class (class object% (define/pubment (f x) x)) 
	       (rename-super [super-f f])) exn:fail:object?)

(define bfoo-jgoo%
  (class object%
    (define/pubment (foo x)
      (inner (list 1 x) foo (list 2 x)))
    (define/public (goo x)
      (list 3 x))
    (define/public (zoo x)
      (inner (list 10 x) foo (list 20 x)))
    (super-new)))

(define bjfoo-jbgoo% 
  (class bfoo-jgoo%
    (define/augride (foo x) (list 4 x))
    (define/overment (goo x) 
      (let ([y (super goo x)])
	(inner (list 5 y) goo (list 6 y))))
    (rename-super [super-zoo zoo])
    (define/public (hoo y) (super-zoo (list 7 y)))
    (super-new)))

(define bjjfoo-jbjgoo% 
  (class bjfoo-jbgoo% 
    (define/override (foo x)
      (list 8 x))
    (define/augride (goo x) 
      (list 9 x))
    (super-new)))

(define bjjbfoo-jbjjgoo% 
  (class bjjfoo-jbjgoo% 
    (define/overment (foo x) 
      (let ([z (super foo (list 10 x))])
	(inner (list 11 z) foo (list 12 z))))
    (define/override (goo x) 
      (super goo (list 13 x)))
    (super-new)))

(define bjjbjfoo-jbjjbgoo% 
  (class bjjbfoo-jbjjgoo% 
    (define/augride (foo x) 
      (list 14 x))
    (define/overment (goo x) 
      (super goo (list 15 x)))
    (super-new)))

(test '(1 12) 'bjt (send (new bfoo-jgoo%) foo 12))
(test '(3 13) 'bjt (send (new bfoo-jgoo%) goo 13))
(test '(10 13.5) 'bjt (send (new bfoo-jgoo%) zoo 13.5))

(test '(4 (2 14)) 'bjt (send (new bjfoo-jbgoo%) foo 14))
(test '(5 (3 15)) 'bjt (send (new bjfoo-jbgoo%) goo 15))
(test '(4 (20 (7 16))) 'bjt (send (new bjfoo-jbgoo%) hoo 16))

(test '(8 (2 17)) 'bjt (send (new bjjfoo-jbjgoo%) foo 17))
(test '(9 (6 (3 18))) 'bjt (send (new bjjfoo-jbjgoo%) goo 18))
(test '(8 (20 (7 19))) 'bjt (send (new bjjfoo-jbjgoo%) hoo 19))

(test '(11 (8 (10 (2 20)))) 'bjt (send (new bjjbfoo-jbjjgoo%) foo 20))
(test '(9 (13 (6 (3 21)))) 'bjt (send (new bjjbfoo-jbjjgoo%) goo 21))
(test '(11 (8 (10 (20 (7 22))))) 'bjt (send (new bjjbfoo-jbjjgoo%) hoo 22))

(test '(14 (12 (8 (10 (2 23))))) 'bjt (send (new bjjbjfoo-jbjjbgoo%) foo 23))
(test '(9 (13 (15 (6 (3 24))))) 'bjt (send (new bjjbjfoo-jbjjbgoo%) goo 24))
(test '(14 (12 (8 (10 (20 (7 25)))))) 'bjt (send (new bjjbjfoo-jbjjbgoo%) hoo 25))

;; ----------------------------------------
;; Make sure inner default isn't called when augment is available:

(let ([x 0])
  (define c% (class object%
	       (define/pubment (m v)
		 (inner (set! x (+ x v)) m v))
	       (super-new)))
  (define d% (class c%
	       (define/augment (m v)
		 (list v))
	       (super-new)))
  (test (void) 'no-inner (send (new c%) m 5))
  (test 5 values x)
  (test '(6) 'inner (send (new d%) m 6))
  (test 5 values x))

;; ----------------------------------------

(define rest-arg-fish%
  (class fish%
    (public greeting)

    (begin ; should get flattened
      (init -first-name)
      (init-field last-name)
      (init-rest -nicknames))

    (define first-name -first-name) 
    (define nicknames -nicknames) 

    (define greeting
      (letrec ([loop
		(case-lambda
		 [() (loop first-name last-name)]
		 [(last-name first-name) ;; intentionally backwards to test scope
		  (format "~a ~a, a.k.a.: ~a"
			  last-name first-name
			  nicknames)]
		 [(a b c d . rest) 'never-get-here])])
	loop))

    (define/public useless-method (case-lambda))

    (super-new (size 12))))

(define rest-fish (make-object rest-arg-fish% "Gil" "Finn" "Slick"))

(test "Gil Finn, a.k.a.: (Slick)" 'osf (send rest-fish greeting))

(let ([exn (with-handlers ([exn:fail? (lambda (exn) exn)])
	     (send rest-fish greeting 1 2 3))])
  (test #t exn:fail:contract:arity? exn)
  ;; (test 3 exn:application-value exn)
  ;; (test (list 0 2 (make-arity-at-least 4)) exn:application:arity-expected exn)
  )
(let ([exn (with-handlers ([exn:fail? (lambda (exn) exn)])
	     (send rest-fish useless-method 3))])
  (test #t exn:fail:contract:arity? exn)
  ;; (test 1 exn:application-value exn)
  ;; (test null exn:application:arity-expected exn)
  )

;; Missing last-name:
(err/rt-test (instantiate rest-arg-fish% () (-first-name "Gil") (-nicknames null)) 
	     exn:fail:object?)

(define rest-fish-0 (instantiate rest-arg-fish% () (-first-name "Gil") (last-name "Finn")))
(test "Gil Finn, a.k.a.: ()" 'osf (send rest-fish-0 greeting))
(define rest-fish-0 (dynamic-instantiate rest-arg-fish% '() '([-first-name . "Gil"] [last-name . "Finn"])))
(test "Gil Finn, a.k.a.: ()" 'osf (send rest-fish-0 greeting))

;; Keyword order doesn't matter:
(define rest-fish-0.5 (instantiate rest-arg-fish% () (last-name "Finn") (-first-name "Gil")))
(test "Gil Finn, a.k.a.: ()" 'osf (send rest-fish-0.5 greeting))
(define rest-fish-0.5 (dynamic-instantiate rest-arg-fish% '() '([last-name . "Finn"] [-first-name . "Gil"])))
(test "Gil Finn, a.k.a.: ()" 'osf (send rest-fish-0.5 greeting))

(err/rt-test (instantiate rest-arg-fish% () 
			  (-first-name "Gil") (last-name "Finn") 
			  (-nicknames "Slick"))
	     exn:fail:object?)
(err/rt-test (instantiate rest-arg-fish% () 
			  (-first-name "Gil") (last-name "Finn") 
			  (anything "Slick"))
	     exn:fail:object?)
(err/rt-test (dynamic-instantiate rest-arg-fish% '() 
                                  '([-first-name . "Gil"]
                                    [last-name . "Finn"] 
                                    [-nicknames . "Slick"]))
	     exn:fail:object?)
(err/rt-test (dynamic-instantiate rest-arg-fish% '()
                                  '([-first-name . "Gil"]
                                    [last-name . "Finn"] 
                                    [anything . "Slick"]))
	     exn:fail:object?)

;; Redundant by-pos:
(err/rt-test (instantiate rest-arg-fish% ("Gil") (-first-name "Gilly") (last-name "Finn"))
	     exn:fail:object?)

(define no-rest-fish%
  (class fish%
    (public greeting)

    (init-field first-name)
    (init-field last-name)
    (init-rest)

    (define (greeting)
      (format "~a ~a" last-name first-name))

    (super-instantiate (12))))

;; Too many by-pos:
(err/rt-test (instantiate no-rest-fish% ("Gil" "Finn" "hi" "there"))
	     exn:fail:object?)

(define no-rest-0 (instantiate no-rest-fish% ("Gil" "Finn")))
(test 12 'norest (send no-rest-0 get-size))
(define no-rest-0 (dynamic-instantiate no-rest-fish% '("Gil" "Finn") '()))
(test 12 'norest (send no-rest-0 get-size))

(define allow-rest-fish%
  (class fish%
    (public greeting)

    (init-field first-name)
    (init-field last-name)

    (define (greeting)
      (format "~a ~a" last-name first-name))

    (super-instantiate ())))

;; Too many by-pos:
(err/rt-test (instantiate no-rest-fish% ("Gil" "Finn" 18 20))
	     exn:fail:object?)

(define no-rest-0 (instantiate allow-rest-fish% ("Gil" "Finn" 18)))
(test 18 'allowrest (send no-rest-0 get-size))
(define no-rest-0 (dynamic-instantiate allow-rest-fish% '("Gil" "Finn" 18) '()))
(test 18 'allowrest (send no-rest-0 get-size))


(define allow-rest/size-already-fish%
  (class fish%
    (public greeting)

    (init-field first-name)
    (init-field last-name)

    (define (greeting)
      (format "~a ~a" last-name first-name))

    (super-instantiate (12))))

;; Unused by-pos:
(err/rt-test (instantiate allow-rest/size-already-fish% ("Gil" "Finn" 18))
	     exn:fail:object?)


;; Subclass where superclass has rest arg, check by-pos:

(define with-rest%
  (class object%
    (init-rest args)
    (field [a args])

    (public get-args)
    (define (get-args) a)
    (super-instantiate ())))

(define to-rest%
  (class with-rest%
    (super-instantiate ())))

(test '("hi" "there") 'to-rest (send (instantiate to-rest% ("hi" "there")) get-args))
(err/rt-test (instantiate to-rest% () (by-name "hi"))
	     exn:fail:object?)

;; Check by-pos with super-instantiate:

(define to-rest2%
  (class with-rest%
    (super-instantiate ("hey,"))))

(test '("hey," "hi" "there") 'to-rest (send (instantiate to-rest2% ("hi" "there")) get-args))
(err/rt-test (instantiate to-rest2% () (by-name "hi"))
	     exn:fail:object?)

;; Even more nested:

(define to-rest3%
  (class to-rest2%
    (super-instantiate ("um..."))))

(test '("hey," "um..." "hi" "there") 'to-rest (send (instantiate to-rest3% ("hi" "there")) get-args))

;; ------------------------------------------------------------
;; ...-final  clauses.
;; Internal calls to public-final and pubment are direct,
;;  but other calls must be indirect.

(let ()
  (define c% (class object%
	       (define/pubment (foo x)
		 1)
	       (define/public-final (fool x)
		 10)
	       (define/public (aoo x) (foo x))
	       (define/public (aool x) (fool x))
	       (define/public (foo2 x) 2)
	       (super-new)))
  (define d% (class c%
	       (define/augment (foo y)
		 2)
	       (define/public (goo z)
		 (foo z))
	       (define/override-final (foo2 x) 
		 20)
	       (define/public (goo2 z)
		 (foo2 z))
	       (super-new)))
  (define e% (class c%
	       (define/augment-final (foo y)
		 2)
	       (define/public (goo z)
		 (foo z))
	       (super-new)))
  
  (test 1 'foo (send (new c%) foo 0))
  (test 1 'foo (send (new d%) foo 0))
  (test 1 'foo (send (new d%) goo 0))
  (test 1 'foo (send (new e%) goo 0))

  (test 2 'foo (send (new c%) foo2 0))
  (test 20 'foo (send (new d%) foo2 0))
  (test 20 'foo (send (new d%) goo2 0))

  (test 1 'aoo (send (new c%) aoo 0))
  (test 10 'aool (send (new d%) aool 0))
  (test 10 'aool (send (new d%) aool 0))

  (err/rt-test (class c% (define/override (fool x) 12)) exn:fail:object?)
  (err/rt-test (class c% (define/augment (fool x) 12)) exn:fail:object?)
  (err/rt-test (class d% (define/override (foo2 x) 12)) exn:fail:object?)
  (err/rt-test (class d% (define/augment (foo2 x) 12)) exn:fail:object?)
  (err/rt-test (class e% (define/override (foo x) 12)) exn:fail:object?)
  (err/rt-test (class e% (define/augment (foo x) 12)) exn:fail:object?)
  )

;; ------------------------------------------------------------
;; Test abstract clauses

;; examples taken from the DPC book
(define bt%
  (class object%
    (super-new)
    (init-field number)
    (abstract count sum)
    (define/public (double n)
      (new node% [number n] [left this] [right this]))))

(define leaf%
  (class bt%
    (super-new)
    (inherit-field number)
    (define/override (sum) number)
    (define/override (count) 1)))

(define node%
  (class bt%
    (super-new)
    (init-field left right)
    (inherit-field number)
    (define/override (sum) (+ number
                              (send left sum)
                              (send right sum)))
    (define/override (count) (+ 1
                                (send left count)
                                (send right count)))))

(err/rt-test (new bt% [number 5]) exn:fail:object?)
(test 22 'bt (send (send (new leaf% [number 7]) double 8) sum))
(test 3 'bt (send (send (new leaf% [number 7]) double 8) count))

;; calling abstracts from concrete methods
(let* ([foo% (class (class object%
                      (super-new)
                      (abstract foo)
                      (define/public (bar)
                        (add1 (foo)))
                      (define/public (baz) 15))
               (super-new)
               (define/override (foo) 10))]
       [o (new foo%)])
      (test 10 'abstract (send o foo))
      (test 11 'abstract (send o bar))
      (test 15 'abstract (send o baz)))

;; super calls to an abstract should just be (void)
(let ([foo% (class (class object%
                     (super-new)
                     (abstract m))
              (super-new)
              (define/override (m) (super m)))])
  (test (void) 'super (send (new foo%) m)))

;; failing to implement abstract methods
(define bad-leaf% (class bt% (inherit-field number)))
(define bad-leaf2% (class bt% (inherit-field number)
                     (define/override (sum) number)))

(err/rt-test (new bad-leaf% [number 5]) exn:fail:object?)
(err/rt-test (new bad-leaf2% [number 10]) exn:fail:object?)

;; cannot define publics over abstracts
(err/rt-test (class bt% (inherit-field number)
               (define/public (sum) number))
             exn:fail:object?)
(err/rt-test (class bt% (inherit-field number)
               (define/pubment (sum) number))
             exn:fail:object?)

;; example from PR 13798
(let ()
  (define test%
    (class object% (super-new)
      (define/public (name) "test")))
  (define sub-abstract%
    (class test% (super-new) (inherit name)
      (abstract broken)
      (define/public (full-name) (string-append (name) "subabs"))))
  (define sub-concrete%
    (class sub-abstract% (super-new)
      (define/override (broken) "broken")))

  (test "testsubabs" 'pr13798 (send (new sub-concrete%) full-name)))

;; ------------------------------------------------------------
;; Test send/apply dotted send and method-call forms:

(define dotted% (class object%
		  (public f g h)
		  (define (f x y z)
		    (list z y x))
		  (define (g x)
		    (let ([w (list x (add1 x) (+ x 2))])
		      (f . w)))
		  (define (h x #:y [y 12])
		    (list x y))
		  (super-make-object)))
(define dotted (make-object dotted%))
(test '(3 2 1) 'dotted (send dotted f 1 2 3))
(test '(9 8 7) 'dotted (send dotted g 7))
(let ([l (list 3 5 6)])
  (test '(6 5 3) 'dotted (send dotted f . l))
  (test '(6 5 3) 'dotted (send/apply dotted f l))
  (test '(9 8 7) 'dotted (send/apply dotted f '(7 8 9))))
(let ([l (list 6 8)])
  (test '(8 6 2) 'dotted (send dotted f 2 . l))
  (test '(8 6 2) 'dotted (send/apply dotted f 2 l))
  (test '(9 7 3) 'dotted (send/apply dotted f 3 '(7 9))))
(test '(1 12) 'dotted (send/apply dotted h (list 1)))
(test '(2 12) 'dotted (send/keyword-apply dotted h null null (list 2)))
(test '(3 8) 'dotted (send/keyword-apply dotted h '(#:y) (list 8) (list 3)))

(test '(c b a) dynamic-send dotted 'f 'a 'b 'c)
(test '(c b a) apply dynamic-send dotted 'f 'a '(b c))
(test '(e 12) apply dynamic-send dotted 'h '(e))
(test '(f 13) 'dotted (apply dynamic-send dotted 'h '(f) #:y 13))
(test '(g 14) keyword-apply dynamic-send '(#:y) '(14) dotted 'h '(g))

(syntax-test #'(send/apply dotted f 2 . l))

;; ------------------------------------------------------------
;; Test init & feld external names

(define many-fields% (class object%
		       (init i1
			     [(i2* i2)])
		       (init-field i3
				   [(i4* i4)])
		       (init [i5 5]
			     [(i6* i6) 6])
		       (init-field (i7 7)
				   [(i8* i8) 8])
		       (field [a 10]
			      [(b* b) 12])
		       (define inits+fields (list i1 i2* i3 i4* i5 i6* i7 i8* a b*))
		       (define/public (get-fields)
			 (list i3 i4* i7 i8* a b*))
		       (define/public (get-inits+fields)
			 inits+fields)
		       (super-instantiate ())))

(let ([om1 (make-object many-fields% 10 20 30 40)]
      [oi1 (instantiate many-fields% () [i1 11] [i2 21] [i3 31] [i4 41])]
      [om2 (make-object many-fields% 12 22 32 42 52 62 72 82)]
      [oi2 (instantiate many-fields% () [i1 13] [i2 23] [i3 33] [i4 43] [i5 53] [i6 63] [i7 73] [i8 83])])
  (test '(10 20 30 40 5 6 7 8 10 12) 'om1-if (send om1 get-inits+fields))
  (test '(11 21 31 41 5 6 7 8 10 12) 'oi1-if (send oi1 get-inits+fields))
  (test '(30 40 7 8 10 12) 'om1-f (send om1 get-fields))
  (test '(31 41 7 8 10 12) 'oi1-f (send oi1 get-fields))
  (test '(12 22 32 42 52 62 72 82 10 12) 'om2-if (send om2 get-inits+fields))
  (test '(13 23 33 43 53 63 73 83 10 12) 'oi2-if (send oi2 get-inits+fields))
  (test '(32 42 72 82 10 12) 'om2-f (send om2 get-fields))
  (test '(33 43 73 83 10 12) 'oi2-f (send oi2 get-fields))
  (test 10 (class-field-accessor many-fields% a) om1)
  (test 12 (class-field-accessor many-fields% b) om1))

;; ------------------------------------------------------------
;; Test this%
(let ()
  (define base%
    (class object% 
      (super-new)
      (define/public (factory)
        (new this%))))
  (define derived%
    (class base%
      (super-new)
      (init-field [f 4])
      (define/public (double)
        (set! f (* 2 f)))))
  (let* ([factory-derived (send (new derived%) factory)])
    (test 4 'factory-derived-f (get-field f factory-derived))
    (test 4 dynamic-get-field 'f factory-derived)
    (send factory-derived double)
    (test 8 'factory-derived-f-doubled (get-field f factory-derived))))

(let ()
  (define account%
    (class object% 
      (super-new)
      (init-field balance)
      (define/public (add n)
        (new this% [balance (+ n balance)]))))
  (define savings%
    (class account%
      (super-new)
      (inherit-field balance)
      (define interest 0.04)
      (define/public (add-interest)
        (send this add (* interest balance)))))
  (let* ([acct (new savings% [balance 500])]
         [acct (send acct add 500)]
         [acct (send acct add-interest)])
    (test 1040.0 'acct-balance (get-field balance acct))))

;; ------------------------------------------------------------
;; Test public*, define-public, etc.

(syntax-test #'(class object% public*))
(syntax-test #'(class object% (public* . x)))
(syntax-test #'(class object% (public* x)))
(syntax-test #'(class object% (public* [x])))
(syntax-test #'(class object% (public* [x . y])))
(syntax-test #'(class object% (public* [x 7 8])))
(syntax-test #'(class object% (public* [7 8])))

(syntax-test #'(class object% override*))
(syntax-test #'(class object% (override* . x)))
(syntax-test #'(class object% (override* x)))
(syntax-test #'(class object% (override* [x])))
(syntax-test #'(class object% (override* [x . y])))
(syntax-test #'(class object% (override* [x 7 8])))
(syntax-test #'(class object% (override* [7 8])))

(syntax-test #'(class object% private*))
(syntax-test #'(class object% (private* . x)))
(syntax-test #'(class object% (private* x)))
(syntax-test #'(class object% (private* [x])))
(syntax-test #'(class object% (private* [x . y])))
(syntax-test #'(class object% (private* [x 7 8])))
(syntax-test #'(class object% (private* [7 8])))

(syntax-test #'(class object% define/public))
(syntax-test #'(class object% (define/public)))
(syntax-test #'(class object% (define/public x)))
(syntax-test #'(class object% (define/public x 1 2)))
(syntax-test #'(class object% (define/public 1 2)))
(syntax-test #'(class object% (define/public (x 1) 2)))
(syntax-test #'(class object% (define/public (1 x) 2)))
(syntax-test #'(class object% (define/public (x . 1) 2)))
(syntax-test #'(class object% (define/public ((x 1) . a) 2)))
(syntax-test #'(class object% (define/public ((x b b) a) 2)))

(syntax-test #'(class object% define/override))
(syntax-test #'(class object% (define/override)))
(syntax-test #'(class object% (define/override x)))
(syntax-test #'(class object% (define/override x 1 2)))
(syntax-test #'(class object% (define/override 1 2)))
(syntax-test #'(class object% (define/override (x 1) 2)))
(syntax-test #'(class object% (define/override (1 x) 2)))
(syntax-test #'(class object% (define/override (x . 1) 2)))

(syntax-test #'(class object% define/private))
(syntax-test #'(class object% (define/private)))
(syntax-test #'(class object% (define/private x)))
(syntax-test #'(class object% (define/private x 1 2)))
(syntax-test #'(class object% (define/private 1 2)))
(syntax-test #'(class object% (define/private (x 1) 2)))
(syntax-test #'(class object% (define/private (1 x) 2)))
(syntax-test #'(class object% (define/private (x . 1) 2)))

(define c*1% (class object%
	       (define/public (x) (f))
	       (define/public ((higher-order a) b) (+ a b))
	       (public*
		[y (lambda () 2)]
		[z (lambda () 3)])
	       (private*
		[f (lambda () 1)])
	       (super-make-object)))

(define c*2% (class c*1%
	       (override*
		[y (lambda () 20)])
	       (define/override z (lambda () (g)))
	       (define/private (g) 30)
	       (super-make-object)))

(define o*1 (make-object c*1%))
(define o*2 (make-object c*2%))

(test 1 'o1 (send o*1 x))
(test 2 'o1 (send o*1 y))
(test 3 'o1 (send o*1 z))
(test 1 'o2 (send o*2 x))
(test 20 'o2 (send o*2 y))
(test 30 'o2 (send o*2 z))
(test 7 'o2 ((send o*2 higher-order 1) 6))

;; ----------------------------------------
;; Macro definitions in classes

(define cm1%
  (class object%
    (public meth)
    
    (define-syntax (macro stx)
      (syntax 10))
    
    (field [x (macro)])
    (init-field [y (macro)])
    (init  [-z (macro)])
    (field [z -z])

    (define w (macro))

    (define meth (lambda () (macro)))
    (define meth2 (lambda () (macro)))

    (define/public get-z (lambda () z))
    (define/public get-w (lambda () w))
    
    (super-instantiate ())))

(test 10 'cm1-meth (send (make-object cm1%) meth))
(test 10 'cm1-x ((class-field-accessor cm1% x) (make-object cm1%)))
(test 10 'cm1-y ((class-field-accessor cm1% y) (make-object cm1%)))
(test 10 'cm1-z (send (make-object cm1%) get-z))
(test 10 'cm1-w (send (make-object cm1%) get-w))

;; Make sure that local and syntax names do not confuse enclosing syntax for
;; definition RHSs
(test #t class? (let-syntax ([see-outer (lambda (x) (syntax (lambda () 10)))])
		  (class object%
		    (define see-outer-x 10)
		    (public meth)
		    (define meth (see-outer)))))
(test #t class? (let-syntax ([see-outer (lambda (x) (syntax (lambda () 10)))])
		  (class object%
		    (define-syntax see-outer-x 10)
		    (public meth)
		    (define meth (see-outer)))))

;; Make sure that declared method names, field names, etc.
;; *do* shadow for definition RHSs
(let ([mk-syntax-test
       (lambda (mk)
	 (syntax-test (datum->syntax
		       (quote-syntax here)
		       `(let-syntax ([dont-see-outer (lambda (x) (syntax (lambda () 10)))])
			  (class object% 
			    ,@(mk 'dont-see-outer)
			    (public meth)
			    (define meth (dont-see-outer)))))))])
  (mk-syntax-test (lambda (id) `((init ,id))))
  (mk-syntax-test (lambda (id) `((init-rest ,id))))
  (mk-syntax-test (lambda (id) `((field [,id 10]))))
  (mk-syntax-test (lambda (id) `((inherit-field ,id))))
  (mk-syntax-test (lambda (id) `((inherit ,id))))
  (mk-syntax-test (lambda (id) `((rename-super [,id old-id]))))
  (mk-syntax-test (lambda (id) `((public ,id) (define (id) 10))))
  (mk-syntax-test (lambda (id) `((private ,id) (define (id) 10))))
  (mk-syntax-test (lambda (id) `((override ,id) (define (id) 10)))))


(syntax-test #'(class-field-accessor))
(syntax-test #'(class-field-accessor ok))
(syntax-test #'(class-field-accessor ok 7))
(syntax-test #'(class-field-accessor ok% ok ok))
(syntax-test #'(class-field-accessor ok% . ok))
(syntax-test #'(class-field-mutator))
(syntax-test #'(class-field-mutator ok))
(syntax-test #'(class-field-mutator ok 7))
(syntax-test #'(class-field-mutator ok% ok ok))
(syntax-test #'(class-field-mutator ok% . ok))

(syntax-test #'(define-local-member-name . a))
(syntax-test #'(define-local-member-name 7))
(syntax-test #'(define-local-member-name a 7))
(syntax-test #'(define-local-member-name a a))

;; ------------------------------------------------------
;; Private names

(let ([o (let ()
	   (define-local-member-name priv)
	   (let ([o (make-object
		     (class object%
		       (define/public (priv) (let ([priv 73]) priv))
		       (super-make-object)))])
	     (test 73 'priv (send o priv))
	     o))])
  (err/rt-test (send o priv) exn:fail:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init-field priv)
			(super-make-object))])
	      (test 100 'priv ((class-field-accessor c% priv) (make-object c% 100)))
	      (test 100 'priv ((class-field-accessor c% priv) (instantiate c% () [priv 100])))
	      c%))])
  (err/rt-test (class-field-accessor c% priv) exn:fail:object?)
  (test #t object? (make-object c% 10))
  (err/rt-test (instantiate c% () [priv 10]) exn:fail:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init priv)
			(define xpriv priv)
			(define/public (m) xpriv)
			(super-make-object))])
	      (test 100 'priv (send (make-object c% 100) m))
	      (test 100 'priv (send (instantiate c% () [priv 100]) m))
	      c%))])
  (test 101 'priv (send (make-object c% 101) m))
  (err/rt-test (instantiate c% () [priv 101]) exn:fail:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init xpriv)
			(field [priv xpriv])
			(define/public (m) priv)
			(super-make-object))])
	      (test 100 'priv ((class-field-accessor c% priv) (make-object c% 100)))
	      (test 101 'priv (send (make-object c% 101) m))
	      (test 100 'priv (send (instantiate c% () [xpriv 100]) m))
	      (test 100 'priv ((class-field-accessor c% priv) (instantiate c% () [xpriv 100])))
	      c%))])
  (err/rt-test (class-field-accessor c% priv) exn:fail:object?)
  (test 101 'priv (send (make-object c% 101) m))
  (test 101 'priv (send (instantiate c% () [xpriv 101]) m))
  (err/rt-test (instantiate c% () [priv 10]) exn:fail:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let* ([i<%> (interface () priv)]
                   [c% (class* object% (i<%>)
                         (init-field val)
                         (define/public (priv) val)
                         (super-make-object))])
	      (test 100 'priv (send (make-object c% 100) priv))
	      (test 100 'priv (send* (make-object c% 100) (priv)))
              (test 100 'priv (send+ (make-object c% 100) (priv)))
	      (test 100 'priv (with-method ([p ((make-object c% 100) priv)]) (p)))
	      (test 100 'gen-priv-cls (send-generic (make-object c% 100) (generic c% priv)))
              (test 100 'gen-priv-intf (send-generic (make-object c% 100) (generic i<%> priv)))
	      (err/rt-test (make-generic c% 'priv) exn:fail:object?)
	      c%))])
  (test #t object? (make-object c% 10))
  (err/rt-test (send (make-object c% 10) priv) exn:fail:object?)
  (err/rt-test (send* (make-object c% 10) (priv)) exn:fail:object?)
  (err/rt-test (send+ (make-object c% 10) (priv)) exn:fail:object?)
  (err/rt-test (with-method ([p ((make-object c% 100) priv)]) (p)) exn:fail:object?)
  (err/rt-test (generic c% priv) exn:fail:object?)
  (err/rt-test (make-generic c% 'priv) exn:fail:object?))

;; Make sure local name works with `send' in an area where the
;;  name is also directly bound:
(let ([c% (let ()
	    (define-local-member-name priv)
	    (class object%
	      (define/public (priv x) (+ x 10))
	      (define/public (pub y) (send this priv (* 2 y)))
	      (super-new)))])
  (test 16 'send-using-local (send (new c%) pub 3)))

;; Make sure local names are checked correctly for deciding
;; conflicting method names:
(let ()
  (define-syntax-rule (defclss name% x y)
    (begin
      (define-local-member-name f)
      (define name%
        (class object%
          (super-new)
          (define/public (f) 5)
          (define/public (x) 6)
          (define/public (y) 7)))
      (test 5 'local-f (send (new name%) f))))

  (defclss one% f y)
  (test 6 'normal-f (send (new one%) f))
  (test 7 'normal-y (send (new one%) y))
  
  (define-syntax-rule (defclss2 name% x)
    (begin
      (define-local-member-name f)
      (defclss name% f x)
      (test 6 'middle-f (send (new name%) f))))

  (defclss2 two% f)
  (test 7 'normal-f (send (new two%) f)))

(syntax-test #'(let ()
                 (define-local-member-name f)
                 (class object%
                   (super-new)
                   (define/public (f) 5)
                   (define/public (f) 6))))

;; ------------------------------------------------------------
;; `send+' tests

(let ([c% (class object%
            (define/public (m . args) this)
            (super-new))])
  (syntax-test #'(send+ (new c%) m 5))
  (syntax-test #'(send+ (new c%) . 5))

  (test #t object? (send+ (new c%) (m 5) (m 15)))
  (test #t object? (send+ (new c%) (m 5) (m . (1 2 3 4)))))

;; ------------------------------------------------------------
;; `new' tests

(syntax-test #'(new))
(syntax-test #'(new x x))
(syntax-test #'(new x ()))
(syntax-test #'(new x (x)))
(syntax-test #'(new x ("a" x)))

(test #t object? (new object%))
(test #t object? (new (class object% (init-field x) (super-instantiate ())) (x 1)))

;; ------------------------------------------------------------
;; `field' tests


(syntax-test #'(get-field))
(syntax-test #'(get-field a))
(syntax-test #'(get-field 1 b))
(syntax-test #'(get-field a b c))

(error-test #'(get-field x 1) exn:fail:contract?)
(error-test #'(get-field x (new object%)) exn:fail:object?)
(error-test #'(get-field x (new (class object% (define x 1) (super-new))))
            exn:fail:object?)
(error-test #'(let ([o (let ()
                         (define-local-member-name f)
                         (new (class object%
                                (field [f 0])
                                (super-new))))])
                (get-field f o))
            exn:fail:object?)
(test 0 'get-field1 (get-field x (new (class object% (field [x 0]) (super-new)))))
(test 0 'get-field2 (let ()
                      (define-local-member-name f)
                      (get-field f (new (class object% (field [f 0]) (super-new))))))
(let ([o (new (class (class object% (field [f 10]) (super-new))
		(field [g 11])
		(super-new)))])
  (test 10 'get-field3 (get-field f o))
  (test 11 'get-field3 (get-field g o)))

(syntax-test #'(set-field!))
(syntax-test #'(set-field! a))
(syntax-test #'(set-field! a b))
(syntax-test #'(set-field! 1 b c))
(syntax-test #'(set-field! a b c d))

(error-test #'(set-field! x 1 2))
(error-test #'(set-field! x (new object%) 2) exn:fail:object?)
(error-test #'(set-field! x (new object%) 2) 
            (λ (exn) (regexp-match? #rx"^set-field!" (exn-message exn))))
(error-test #'(dynamic-set-field! 'x (new object%) 2) 
            (λ (exn) (regexp-match? #rx"^dynamic-set-field!" (exn-message exn))))
(error-test #'(set-field! x (new (class object% (define x 1) (super-new))) 2)
            exn:fail:object?)
(error-test #'(let ([o (let ()
                         (define-local-member-name f)
                         (new (class object%
                                (field [f 0])
                                (super-new))))])
                (set-field! f o 2))
            exn:fail:object?)
(test 1 'set-field!1 (let ([o (new (class object% (field [x 0]) (super-new)))])
                       (set-field! x o 1)
                       (get-field x o)))
(test 1 'set-field!2 (let ()
                       (define-local-member-name f)
                       (define o (new (class object% (field [f 0]) (super-new))))
                       (set-field! f o 1)
                       (get-field f o)))
(let ([o (new (class (class object% (field [f 10]) (super-new))
                (field [g 11])
                (super-new)))])
  (test 12 'set-field!3 (begin (set-field! f o 12)
                               (get-field f o)))
  (test 14 'set-field!4 (begin (set-field! g o 14)
                               (get-field g o)))
  (test 18 'set-field!5 (begin (dynamic-set-field! 'g o 18)
                               (dynamic-get-field 'g o))))

(syntax-test #'(field-bound?))
(syntax-test #'(field-bound? a))
(syntax-test #'(field-bound? 1 b))
(syntax-test #'(field-bound? a b c))

(error-test #'(field-bound? x 1) exn:application:mismatch?)
(test #t 'field-bound?1 (field-bound? x (new (class object% (field [x 0]) (super-new)))))
(test #f 'field-bound?2 (field-bound? y (new (class object% (field [x 0]) (super-new)))))
(test #f 'field-bound?3 (field-bound? y (new object%)))

(test #f
      'field-bound?/local-name1
      (let ([o (let ()
                 (define-local-member-name f)
                 (new (class object% (field [f 10]) (super-new))))])
        (field-bound? f o)))

(test #t
      'field-bound?/local-name2
      (let ()
        (define-local-member-name f)
        (field-bound? f (new (class object% (field [f 10]) (super-new))))))

(test '(f) field-names (new (class object% (field [f 1]) (super-new))))
(test '(g)
      field-names 
      (let ()
        (define-local-member-name f)
        (new (class object% (field [f 1] [g 1]) (super-new)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a macro expansion to init, etc,
;; is taint-armed correctly:

(define (check-class-taint form rename?)
  (define class-taint-%%-init (gensym 'class-taint-%%-init))
  (define class-taint-%%-client (gensym 'class-taint-%%-client))
  (teval
   `(module ,class-taint-%%-init racket/base
      (require racket/class
               (for-syntax racket/base))
      (define-syntax (init-private stx)
	(syntax-case stx ()
	  [(_ name value)
	   (with-syntax ([(internal-name)
			  (generate-temporaries #'(internal-name))])
	     #'(begin
		 (,form (,(if rename? '(internal-name name) 'internal-name)
			 value))
		 (define name internal-name)))]))
      (provide (all-defined-out))))
  ;; Shouldn't fail with a taint erorr:
  (teval
   `(module ,class-taint-%%-client racket/base
      (require racket/class
	       ',class-taint-%%-init)
      (define taint-error%
	(class object%
	  (init-private thing "value")
	  (define/public (to-string)
	    thing)
	  (super-new))))))

(map (lambda (rename?)
       (check-class-taint 'init rename?)
       (check-class-taint 'field rename?) 
       (check-class-taint 'init-field rename?))
     '(#t #f))


;; ------------------------------------------------------------
;; Check define-member-name, etc.:

(let ([mk
       (lambda (% a-name aa-name b-name c-name d-name e-name)
	 (define-member-name a a-name) ; init
	 (define-member-name aa aa-name) ; super init
	 (define-member-name b b-name) ; public
	 (define-member-name c c-name) ; override
	 (define-member-name d d-name) ; augment
	 (define-member-name e e-name) ; inherit
	 (class %
	   (init a)
	   (define a-val a)
	   (inherit e)
	   (define/public (b x)
	     (list 'b a-val x (e x)))
	   (define/override (c y)
	     (list 'c a-val y (super c y)))
	   (define/augment (d y)
	     (list 'd a-val y (inner #f d y)))
	   (super-new [aa (list a)])))])
  (define x% (class object%
	       (init x-a)
	       (define/public (x-e y)
		 (list 'x-e y))
	       (define/public (x-c y)
		 (list 'x-c y))
	       (define/pubment (x-d y)
		 (list 'x-d y (inner #f x-d y)))
	       (super-new)))
  (let* ([x+% (mk x% 
		  (member-name-key a+)
		  (member-name-key x-a)
		  (member-name-key b+)
		  (member-name-key x-c)
		  (member-name-key x-d)
		  (member-name-key x-e))]
	 [o (new x+% [a+ 'a-val])])
    (test '(b a-val 1 (x-e 1)) 'send-b+ (send o b+ 1))
    (test '(c a-val 2 (x-c 2)) 'send-b+ (send o x-c 2))
    (test '(x-d 3 (d a-val 3 #f)) 'send-b+ (send o x-d 3))
    (void)))


;; ----------------------------------------
;;  Localizing init names:

(let ()
  (define-local-member-name the-local-name)
  (define counter%
    (class object%
      (super-new)
      (init [the-local-name 0])
      (define private-field the-local-name)
      (define/public (increment)
        (new counter% [the-local-name (+ private-field 1)]))
      (define/public (value)
        private-field)))
  (test 14 'send-increment (send (send (new counter% [the-local-name 13]) increment) value)))


(let ()
  (define-local-member-name the-local-name)
  (define counter%
    (class object%
      (super-new)
      (init [(my-local-name the-local-name) 0])
      (define private-field my-local-name)
      (define/public (increment)
        (new counter% [the-local-name (+ private-field 1)]))
      (define/public (value)
        private-field)))
  (test 9 'send-increment (send (send (new counter% [the-local-name 8]) increment) value)))

;; Make sure redirect works for assignment and application:
(let ()
  (define-local-member-name the-local-name)
  (define c%
    (class object%
      (super-new)
      (init the-local-name)
      (define fld (list the-local-name
                        (the-local-name 3)
                        (set! the-local-name 12)
                        the-local-name))
      (define/public (get-fld) fld)))
  (test (list add1 4 (void) 12) 'send-fld (send (new c% [the-local-name add1]) get-fld)))

;; ----------------------------------------
;; Tail position behavior:

(let* ([callee
        (lambda ()
          (with-continuation-mark
           'tail 'callee
           (continuation-mark-set->list
            (current-continuation-marks)
            'tail)))]
       [superclass%
        (class object%
          (super-new)
          (public-final simple-tail-method)
          (pubment unaugmented-tail-method augmented-tail-method)
          (public overridden-tail-method supercall-tail-method)

          (define (simple-tail-method)
            (callee))

          (define (unaugmented-tail-method)
            (inner (callee) unaugmented-tail-method))

          (define (augmented-tail-method)
            (inner (void) augmented-tail-method))

          (define (overridden-tail-method)
            (void))

          (define (supercall-tail-method)
            (callee)))]
       [subclass%
        (class superclass%
          (super-new)
          (augment augmented-tail-method)
          (override overridden-tail-method supercall-tail-method)

          (define (augmented-tail-method)
            (callee))

          (define (overridden-tail-method)
            (callee))

          (define (supercall-tail-method)
            (super supercall-tail-method)))])

  (define-syntax (test-call stx)
    (syntax-case stx ()
      [(form method)
       (syntax/loc stx
         (test '(callee)
               'method
               (with-continuation-mark
                'tail 'caller
                (send (new subclass%) method))))]))

  (test-call simple-tail-method)
  (test-call unaugmented-tail-method)
  (test-call augmented-tail-method)
  (test-call overridden-tail-method)
  (test-call supercall-tail-method))

;; ----------------------------------------
;; names

(let ([c% (class object% (define foo (lambda () 10)) (define/public (get) foo) (super-new))])
  (test 'foo object-name (send (new c%) get)))

(let ([w-s (λ (x) 
             (define sp (open-output-string))
             (write x sp)
             (get-output-string sp))])
  
  (test 'object% object-name object%)
  (test "#<class:object%>" w-s object%)
  
  (test 'c% object-name (let ([c% (class object% (super-new))]) c%))
  (test "#<class:c%>" w-s (let ([c% (class object% (super-new))]) c%))
  
  (test 'i<%> object-name (let ([i<%> (interface ())]) i<%>))
  (test "#<interface:i<%>>" w-s (let ([i<%> (interface ())]) i<%>))
  
  (test 'interface:object% object-name (class->interface object%))
  (test "#<interface:object%>" w-s (class->interface object%))
  
  (test 'interface:c% object-name (let ([c% (class object% (super-new))])
                                    (class->interface c%)))
  (test "#<interface:c%>" w-s (let ([c% (class object% (super-new))])
                                (class->interface c%)))

)

;; ----------------------------------------
;; object=?

(let ([c% (class object% (super-new))])
  (let ([o1 (new c%)]
        [o2 (new c%)])
    (test #f object=? o1 o2)
    (test #t object=? o1 o1)
    
    (err/rt-test (object=? o1 #f))
    (err/rt-test (object=? #f o1))
    (err/rt-test (object=? 'a o1))
    (err/rt-test (object=? o1 'a))
    (err/rt-test (object=? #f #f))
    
    (test #f object-or-false=? o1 o2)
    (test #t object-or-false=? o1 o1)
    (test #f object-or-false=? o1 #f)
    (test #f object-or-false=? #f o1)
    (test #t object-or-false=? #f #f)
    
    (err/rt-test (object-or-false=? #f 'a))
    (err/rt-test (object-or-false=? 'a o1))
    (err/rt-test (object-or-false=? o1 'a))
    (err/rt-test (object-or-false=? #f 'a))))

;; ----------------------------------------
;; Implementing printable<%>

(let ()
  (define (check w-cycle? d-cycle?)
    (define c% (class* object% (printable<%>) 
                 (define/public (custom-write p)
                   (if w-cycle?
                       (write this p)
                       (display "hi" p)))
                 (define/public (custom-display p) 
                   (if d-cycle?
                       (display this p)
                       (display "HI" p)))
                 (define/public (custom-print p) 
                   (if d-cycle?
                       (print this p)
                       (display "HI" p)))
                 (super-new)))

    (let ([p (open-output-bytes)])
      (write (new c%) p)
      (test (if w-cycle? #"#0=#0#" #"hi")
            get-output-bytes p))
    (let ([p (open-output-bytes)])
      (display (new c%) p)
      (test (if d-cycle? #"#0=#0#" #"HI")
            get-output-bytes p))

    (let ([p (open-output-bytes)])
      (write (new (class c%
                    (define/override (custom-write p)
                      (write 777 p))
                    (super-new)))
             p)
      (test #"777" get-output-bytes p)))

  (check #f #f)
  (check #t #f)
  (check #f #t)
  (check #t #t))

;; ----------------------------------------
;; equal? on classes

(let ()
  (define a%
    (class object%
      (init-field x)
      (super-new)
      (define/public (m) #f)))

  ; subclasses are never equal? to their superclasses
  (define b%
    (class a%
      (super-new)))
  (test #f equal? a% b%)
  (test #f equal? b% a%)

  ; class contracts do not affect equality
  (define/contract a%+c1
    (class/c (init-field [x integer?])
             [m (->m integer?)])
    a%)
  (test #t equal? a% a%+c1)
  (test #t equal? a%+c1 a%)
  (test #f equal? a%+c1 b%)
  (test #f equal? b% a%+c1)

  ; still equal? even with different contracts
  (define/contract a%+c2
    (class/c (init-field [x string?])
             [m (->m string?)])
    a%)
  (test #t equal? a%+c1 a%+c2)
  (test #t equal? a%+c2 a%+c1))

;; ----------------------------------------
;; equal? on objects with (inspect #f)

(let ()
  (define no-fields/opaque%
    (class object%
      (super-new)))
  (test #f equal? (new no-fields/opaque%) (new no-fields/opaque%))

  (define no-fields/transparent%
    (class object%
      (inspect #f)
      (super-new)))
  (test #t equal? (new no-fields/transparent%) (new no-fields/transparent%))

  (define two-fields/transparent%
    (class object%
      (inspect #f)
      (init-field a b)
      (super-new)))
  (test #t equal? (new two-fields/transparent% [a 1] [b 2]) (new two-fields/transparent% [a 1] [b 2]))
  (test #f equal? (new two-fields/transparent% [a 2] [b 1]) (new two-fields/transparent% [a 1] [b 2]))
  (test #f equal?
        (new two-fields/transparent% [a #t] [b #t])
        (new two-fields/transparent% [a #t] [b #f]))
  (test #t equal?
        (new two-fields/transparent% [a (list 1)] [b (list 2)])
        (new two-fields/transparent% [a (list 1)] [b (list 2)]))

  ; having a transparent superclass doesn’t matter if you are opaque
  (define no-new-fields/opaque%
    (class two-fields/transparent%
      (super-new)))
  (test #f equal? (new no-new-fields/opaque% [a 1] [b 2]) (new no-new-fields/opaque% [a 1] [b 2]))

  ; transparent subclasses are equal? to other objects of the same class, but not of superclasses
  (define no-new-fields/transparent%
    (class two-fields/transparent%
      (inspect #f)
      (super-new)))
  (test #t equal?
        (new no-new-fields/transparent% [a 1] [b 2])
        (new no-new-fields/transparent% [a 1] [b 2]))
  (test #f equal?
        (new no-new-fields/transparent% [a 2] [b 1])
        (new no-new-fields/transparent% [a 1] [b 2]))
  (test #f equal?
        (new two-fields/transparent% [a 1] [b 2])
        (new no-new-fields/transparent% [a 1] [b 2]))
  (test #f equal?
        (new no-new-fields/transparent% [a 1] [b 2])
        (new two-fields/transparent% [a 1] [b 2]))

  ; transparent subclasses are only equal if all their fields are the same, including in superclasses
  (define one-new-field%
    (class two-fields/transparent%
      (inspect #f)
      (init-field c)
      (super-new)))
  (test #t equal? (new one-new-field% [a 1] [b 2] [c 3]) (new one-new-field% [a 1] [b 2] [c 3]))
  (test #f equal?
        (new one-new-field% [a #t] [b #t] [c #t])
        (new one-new-field% [a #t] [b #t] [c #f]))
  (test #f equal?
        (new one-new-field% [a #t] [b #t] [c #t])
        (new one-new-field% [a #f] [b #t] [c #t]))

  ; classes with opaque superclasses are never equal?, even if they are transparent
  (define two-fields/opaque%
    (class object%
      (init-field a b)
      (super-new)))
  (define no-new-fields/parent-opaque%
    (class two-fields/opaque%
      (inspect #f)
      (super-new)))
  (define one-new-fields/parent-opaque%
    (class two-fields/opaque%
      (inspect #f)
      (init-field c)
      (super-new)))
  (test #f equal?
        (new no-new-fields/parent-opaque% [a 1] [b 2])
        (new no-new-fields/parent-opaque% [a 1] [b 2]))
  (test #f equal?
        (new one-new-fields/parent-opaque% [a 1] [b 2] [c 3])
        (new one-new-fields/parent-opaque% [a 1] [b 2] [c 3]))

  ; class contracts do not affect object equality
  (define/contract two-fields/transparent%+c
    (class/c (init-field [a integer?] [b (vectorof symbol?)]))
    two-fields/transparent%)
  (test #t equal?
        (new two-fields/transparent% [a 1] [b (vector 'x)])
        (new two-fields/transparent%+c [a 1] [b (vector 'x)]))
  (test #f equal?
        (new two-fields/transparent% [a 1] [b (vector 'x)])
        (new two-fields/transparent%+c [a 2] [b (vector 'x)]))
  (test #f equal?
        (new two-fields/transparent% [a 1] [b (vector 'x)])
        (new two-fields/transparent%+c [a 1] [b (vector 'y)]))

  ; object contracts do not affect object equality
  (define/contract (make-two-fields/transparent%+c a b)
    (-> integer? (vectorof symbol?) (object/c (field [a integer?] [b (vectorof symbol?)])))
    (new two-fields/transparent% [a a] [b b]))
  (test #t equal?
        (new two-fields/transparent% [a 1] [b (vector 'x)])
        (make-two-fields/transparent%+c 1 (vector 'x)))
  (test #f equal?
        (new two-fields/transparent% [a 1] [b (vector 'x)])
        (make-two-fields/transparent%+c 2 (vector 'x)))
  (test #f equal?
        (new two-fields/transparent% [a 1] [b (vector 'x)])
        (make-two-fields/transparent%+c 1 (vector 'y))))

;; ----------------------------------------
;; Implementing equal<%>

(let ()
  (define c%
    (class* object% (equal<%>)
      (init-field x)

      (define/public (get-x) x)
      (define/public (set-x v) (set! x v))

      (define/public (equal-to? other recur-equal?)
        (recur-equal? x (send other get-x)))
      (define/public (equal-hash-code-of recur-hash-code)
        (+ 1 (recur-hash-code x)))
      (define/public (equal-secondary-hash-code-of recur-hash-code)
        (+ 1 (recur-hash-code x)))
      (super-new)))
  (test #t equal? (new c% [x 10]) (new c% [x 10]))
  (test #f equal? (new c% [x 10]) (new c% [x 12]))

  (let ([o (new c% [x 10])]
        [o2 (new c% [x 10])])
    (send o set-x o)
    (send o2 set-x o2)
    (test #t equal? o o2)
    (test #t equal? o (new c% [x o]))
    (test #f equal? o (new c% [x 10]))
    (let ([ht (make-hash)])
      (hash-set! ht o o)
      (hash-set! ht (new c% [x "hello"]) 'hi)
      (test #t eq? o (hash-ref ht o2))
      (test #f hash-ref ht (new c% [x 10]) #f)
      (test 'hi hash-ref ht (new c% [x "hello"]))
      (let ([d% (class c%
                  (super-new [x "hello"]))])
        (test 'hi hash-ref ht (new d%) #f))
      (let ([d% (class* c% (equal<%>)
                  (super-new [x "hello"]))])
        (test 'nope hash-ref ht (new d%) 'nope)))))

;; ----------------------------------------
;; Implementing new properties

(let ()
  (define proc<%>
    (interface* ()
                ([prop:procedure (lambda (o . args)
                                   (send/apply o apply args))])
                apply))
  (define c%
    (class* object% (proc<%>)
      (define/public (apply . args)
        (cons 'applied-to args))
      (super-new)))
  (test '(applied-to 1 2 3) (new c%) 1 2 3))

;; ----------------------------------------
;; Method error reporting:

(let ()
  (define required% (class object%
                      (define/public (m x #:y y) 1)
                      (super-new)))

  (define optional% (class object%
                      (define/public (m x #:y [y 1]) 1)
                      (super-new)))
  (define (given-3? exn) 
    (regexp-match? "given: 3" (exn-message exn)))

  (err/rt-test (send (new required%) m 1 2 3) given-3?)
  (err/rt-test (send (new optional%) m 1 2 3) given-3?))

;; ----------------------------------------
;; Origin tracking

(let ()
  ;; tries to find each of 'searching-for' in the
  ;; origin property of the fully expanded version
  ;; of stx. Returns the ones it cannot find.
  (define (search-prop property stx . searching-for)
    (define (check-prop o)
      (let loop ([o o])
        (cond
          [(pair? o)
           (loop (car o))
           (loop (cdr o))]
          [(identifier? o)
           (set! searching-for 
                 (filter (λ (x) (not (free-identifier=? x o)))
                         searching-for))])))
    
    (let loop ([stx (expand stx)])
      (cond
        [(pair? stx)
         (loop (car stx))
         (loop (cdr stx))]
        [(syntax? stx)
         (check-prop (syntax-property stx property))
         (loop (syntax-e stx))]))
    
    searching-for)
  
  (test '() search-prop 'origin
        '(class object%)
        #'class)
  (test '() search-prop 'disappeared-use
        '(class object% (inherit m)) 
        #'inherit)
  (test '() search-prop 'origin
        '(class object% (super-new)) #'super-new)
  (test '() search-prop 'disappeared-use
        '(class* object% () (inherit m))
        #'inherit)
  (test '() search-prop 'origin
        '(mixin () () (define/private (m x) x))
        #'define/private)
  (test '() search-prop 'origin
        '(class object% (super-make-object))
        #'super-make-object)
  (test '() search-prop 'origin
        '(class object% (define/public (m x) x))
        #'define/public)
  (test '() search-prop 'origin
        '(class object% (define/public m (lambda (x) x)))
        #'define/public
        #'lambda)
  (test '() search-prop 'origin
        '(class object% (define/augment m (lambda (x) x)))
        #'define/augment
        #'lambda)
  (test '() search-prop 'origin
        '(class object% (define/override (m x) (super m x)))
        #'define/override
        #'super)
  (test '() search-prop 'origin 
        '(class object% (define/augment (m x) (inner 1 m x)))
        #'define/augment
        #'inner)
  (test '() search-prop 'origin
        '(class object% (define f 11))
        #'define)
  (test '() search-prop 'origin
        '(class object% (public f) (define f (λ (x) x)))
        #'define
        #'λ)
  (test '() search-prop 'disappeared-use
        '(class object% (public f) (define f (λ (x) x)))
        #'public)
  (test '() search-prop 'origin
        '(class object% (private f) (define f (λ (x) x)))
        #'define
        #'λ)
  (test '() search-prop 'disappeared-use
        '(class object% (private f) (define f (λ (x) x)))
        #'private)
  (test '() search-prop 'origin
        '(class object%
           (begin
             (define/public (m x) x)
             (define/private (n x) x)))
        #'begin
        #'define/public
        #'define/private)
  (test '() search-prop 'disappeared-use
        '(class object% (inspect #f))
        #'inspect)
  (test '() search-prop 'origin
        '(class object% (field [x #f]))
        #'field)
  (test '() search-prop 'origin
        '(class object% (init x))
        #'init)
  (test '() search-prop 'origin
        '(class object% (init-field x))
        #'init-field)
  (test '() search-prop 'origin
        '(class object% (init-rest args))
        #'init-rest)
  (test '() search-prop 'origin
        '(class object% (init-rest))
        #'init-rest))

;; ----------------------------------------
;; Mixins

(let ()
  (define-local-member-name m)
  (define i<%> (interface () m n))
  (define mix
    (mixin (i<%>) ()
      (inherit m n)
      (super-new)
      (define/public (x) (+ (m) (n)))))
  (define c%
    (class* object% (i<%>)
      (define/public (m) 1)
      (define/public (n) 2)
      (super-new)))
  (test 3 'mixin-with-local-member-names (send (new (mix c%)) x)))

(err/rt-test (mixin (object%) () (super-new))
             exn:fail:object?
             #rx"not an interface")

;; ----------------------------------------
;; Class contracts & generics

(module c%-class-contract-tests racket/base
  (require racket/class
           racket/contract)

  (define c% (class object%
               (super-new)
               (define/public (m x) x)))
  
  (define i<%> (interface () m))
  (define c/i% (class* object% (i<%>)
                 (super-new)
                 (define/public (m x) x)))
  
  (define c%/c
    (class/c
     (m (->m integer? integer?))))
  
  (provide
   (contract-out
    [c% c%/c]
    [c/i% (class/c [m (-> any/c integer? integer?)])])
   is-c%?
   c%-is?
   is-a-c%?
   i<%>)
  
  (define (is-c%? c)
    (c . subclass? . c%))
  
  (define (c%-is? c)
    (c% . subclass? . c))

  (define (is-a-c%? v)
    (v . is-a? . c%)))

(require 'c%-class-contract-tests)

(test #t is-c%? c%)
(test #t c%-is? c%)

(test #t is-a-c%? (new c%))

(test 5 'send-generic (send-generic (new c%) (generic c% m) 5))
(test 6 'send-generic-interface (send-generic (new c/i%) (generic i<%> m) 6))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check error reporting for variable use before assignment

(define (make-undefined-exn? name)
  (lambda (exn)
    (and (exn:fail:contract:variable? exn)
         (eq? name (exn:fail:contract:variable-id exn)))))

(let ()
  (define c%
    (class object%
      (define x 7)
      (field [h 10])
      (super-new)))
  
  (define d%
    (class c%
      (define z z)
      (super-new)))

  (define d!%
    (class c%
      (define z (set! z 0))
      (super-new)))

  (define e%
    (class d%
      (define q 1)
      (field [s 1])
      (super-new)))

  (define d2%
    (class c%
      (define z 1)
      (field [f f])
      (super-new)))

  (define e2%
    (class d2%
      (define q 1)
      (field [s 1])
      (super-new)))

  (err/rt-test (new d%) (make-undefined-exn? 'z))
  (err/rt-test (new d!%) (make-undefined-exn? 'z))
  (err/rt-test (new e%) (make-undefined-exn? 'z))
  (err/rt-test (new d2%) (make-undefined-exn? 'f))
  (err/rt-test (new e2%) (make-undefined-exn? 'f)))

(let ()
  (define c%
    (class object%
      (field [s #f])
      (define/public (set-s)
        (set! s #f))
      (super-new)))
  (define d%
    (class c%
      (inherit set-s)
      (set-s)
      (super-new)))
  (err/rt-test (new d%) (make-undefined-exn? 's)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check optimization to omit use-before-definition chaperone:

;; Relies on expansion where the next-to-last argument of `compose-class`
;; is #t when the chaperone is needed.

(let ()
  (define (check-opt e opt?)
    (test #t
          `(,opt? ,e)
          (let ([e (expand e)])
            (let loop ([e e])
              (cond
               [(and (pair? e)
                     (pair? (cdr e))
                     (eq? (syntax-e (car e)) 'compose-class))
                (eq? opt? (syntax-case (list-ref e (- (length e) 2)) (quote)
                            [(quote #f) #t]
                            [(quote #t) #f]
                            [_ 'unknown]))]
               [(syntax? e)
                (loop (syntax-e e))]
               [(pair? e)
                (and (loop (car e)) (loop (cdr e)))]
               [else #t])))))

  (check-opt '(class object%) #t)
  (check-opt '(class object% (super-new)) #t)

  (check-opt '(class object% (define x 1)) #t)
  (check-opt '(class object% (define x 1) (super-new)) #t)
  (check-opt '(class object% (field [x 1]) (super-new)) #t)
  (check-opt '(class object% (init-field [x 1]) (super-new)) #t)
  (check-opt '(class object% (define x (+ 1 2))) #t)
  (check-opt '(class object% (define x (free-var 1))) #t)
  (check-opt '(class object% (set! y 7) (define x 2) (define y 1) (super-new)) #f)

  (check-opt '(class object% (define x x)) #f)
  (check-opt '(class object% (field [x x])) #f)
  (check-opt '(class object% (init-field [x x])) #f)
  (check-opt '(class object% (super-new) (define x 1)) #f)
  (check-opt '(class object% (super-make-object) (define x 1)) #f)
  (check-opt '(class object% (super-instantiate ()) (define x 1)) #f)
  (check-opt '(class object% (displayln this) (define x 1)) #f)
  (check-opt '(class object% (inherit m) (m) (define x 1)) #f)
  (check-opt '(class object% (define/override (m) x) (super m) (define x 1)) #f)

  (check-opt '(class object% (define x 1) (define y 1) (super-new)) #t)
  (check-opt '(class object% (define x 1) (define y x) (super-new)) #t)
  (check-opt '(class object% (field [x 1] [y x]) (super-new)) #t)
  (check-opt '(class object% (field [x 1] [y x]) (super-make-object)) #t)
  (check-opt '(class object% (init-field [x 1] [y x]) (super-new)) #t)
  (check-opt '(class object% (init-field [x 1]) (define y x) (super-new)) #t)
  (check-opt '(class object% (define x 1) (define y (list x)) (super-new)) #t)
  (check-opt '(class object% (define/public (m) x) (define x 1) (super-new)) #t)
  (check-opt '(class object% (define/override (m) x) (define x 1) (super-new)) #t)

  (check-opt '(class object% (define x y) (define y 1) (super-new)) #f)
  (check-opt '(class object% (init-field [x y] [y 1])) #f)

  (check-opt '(class object% (inherit-field f) (super-new) (displayln f)) #t)
  (check-opt '(class object% (inherit-field f) (displayln f) (super-new)) #f)
  (check-opt '(class object% (inherit-field f) (set! f 10) (super-new)) #t)
  (check-opt '(class object% (inherit-field f) (when ? (super-new)) (displayln f)) #f)
  
  (void))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test chaperone-procedure on method definitions

(test #t class? (class object% (public m) (define m (chaperone-procedure (λ (x) x) values))))
(test #t class? (class object%
                  (public m)
                  (define m
                    (chaperone-procedure (case-lambda [(x) x]) values))))
(test #t class? (class object%
                  (public m)
                  (define m
                    (let-values ([(m) (chaperone-procedure (λ (x) x) values)])
                      m))))
(test #t zero? (send (new (class object%
                            (super-new)
                            (public m)
                            (define m
                              (let-values ([(m) (chaperone-procedure (λ () 0) values)])
                                m))))
                     m))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that `set!` works right in forced subform expansion:

(let ()
  (define (time-print-mixin t%)
    (define start-time #f)
    (class t%
      (super-new)
      (define/override (m)
        (set! start-time 1)
        (super m))))
  (void time-print-mixin))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the class-seal and class-unseal operations

(define (seal cls key init field method)
  (class-seal cls key init field method
              (λ (cls) (error "cannot instantiate sealed class"))
              (λ (cls names) (error "cannot subclass with sealed member"))))

(define (unseal cls key)
  (class-unseal cls key (λ (cls) (error "no matching seal found"))))

(err/rt-test (new (seal object% 'key null null null)) exn:fail?)
(err/rt-test (new (seal (seal object% 'key null null null)
                        'key2 null null null))
             exn:fail?)
(err/rt-test (unseal object% 'g123) exn:fail?)
(err/rt-test (unseal (seal object% (gensym) null null null) 'g123)
             exn:fail?)
(err/rt-test (new (unseal (seal (seal object% 'key null null null)
                                'key2 null null null)
                          'key))
             exn:fail?)
(err/rt-test (let ([c% (seal object% (gensym) null null null)])
               (new (class c% (super-new))))
             exn:fail?)
(test #t class? (let ([c% (seal object% (gensym) null '(x) null)])
                  (class c% (super-new) (field [x 0]))))
(test #t class? (let ([c% (seal (seal object% (gensym) null '(x) null)
                                (gensym) null '(x y) null)])
                  (class c% (super-new) (field [x 0]))))
(err/rt-test (let ([c% (seal object% (gensym) null '(x) null)])
               (new (class c% (super-new) (field [x 0]))))
             exn:fail?)
(err/rt-test (let ([c% (seal (seal object% (gensym) null '(x) null)
                             (gensym) null '(y) null)])
               (class c% (super-new) (field [z 0])))
             exn:fail?)
(err/rt-test (let ([c% (seal object% (gensym) null null null)])
               (class c% (super-new) (field [x 0])))
             exn:fail?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for Check-Syntax support for a `struct`
;; form in `class`, which depends on proper handling
;; of bindings from an internal-definition context

(let ()
  (define binds null)
  (define refs null)
  
  (define (inspect stx)
    (when (and (identifier? stx)
               (eq? 's (syntax-e stx)))
      (set! refs (cons stx refs)))
    (cond
     [(syntax? stx)
      (check stx 'disappeared-binding)
      (inspect (syntax-e stx))
      (inspect (syntax-property stx 'origin))]
     [(pair? stx)
      (inspect (car stx))
      (inspect (cdr stx))]))
  
  (define (check stx prop)
    (define v (syntax-property stx prop))
    (when (and v (get-s v))
      (set! binds (cons (get-s v) binds))))

  (define (get-s e)
    (or (and (identifier? e)
             (eq? 's (syntax-e e))
             e)
        (and (pair? e)
             (or (get-s (car e))
                 (get-s (cdr e))))
        (and (syntax? e)
             (get-s (syntax-e e)))))

  (inspect (expand #'(module m racket/base
                       (require racket/class)
                       (class object%
                         (define-struct s ())
                         s))))
  
  (for ([r (in-list refs)])
    (test #t 'has-binding-match? (for/or ([b (in-list binds)])
                                   (free-identifier=? r b)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `define/public` doesn't add a dye pack unnecessarily

(define-syntax (change-five-to-six stx)
  (syntax-case stx ()
    [(_ e)
     (let loop ([e (local-expand #'e (syntax-local-context) (list #'begin #'define))])
       (syntax-case e (begin)
         [(begin . e)
          #`(begin #,@(map loop (syntax->list #'e)))]
         [(define id rhs)
          #`(define id #,(loop #'rhs))]
         [5 #'6]
         [_ e]))]))

(let ()
  (define-local-member-name x y)
  (test 6 values (send (new (class object%
                              (change-five-to-six
                               (define/public (x) 5))
                              (super-new)))
                       x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `class` applies an outside-edge scope

(let ()
  (define x 'good)
  (define-syntax-rule (m a) (define/public (a) x))
  (define c% (class object%
               (super-new)
               (define x 'bad)
               (m f)))
  (test 'good values (send (new c%) f)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `class` only removes use-site scopes for
;; its definition context from internal definitions, and
;; not those of the surrounding definition context.

(let ()
  (define x 'outer)
  
  (define c1%
    (class object%
      (super-new)
      (define-syntax-rule (m1 a)
        (define a 'inner-good))
      (m1 x)
      (define/public (f) x)))
  (test 'inner-good values (send (new c1%) f))
  
  (define-syntax-rule (m2 a)
    (class object%
      (super-new)
      (define a 'inner-bad)
      (define/public (f) x)))
  (define c2% (m2 x))
  (test 'outer values (send (new c2%) f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that interface contracts on a subclass work correctly.

(let ()
  (define sup%
    (class* object% ()
      (super-new)
      (define/public (f x) x)))

  (define f-returns-int<%>
    (interface () [f (->m any/c integer?)]))

  (define sub%
    (class* sup% (f-returns-int<%>)
      (super-new)))

  (define sup-obj (new sup%))
  (test #t equal? (send sup-obj f 10) 10)
  (test #t equal? (send sup-obj f "hi") "hi")

  (define sub-obj (new sub%))
  (test #t equal? (send sub-obj f 10) 10)
  (err/rt-test (send sub-obj f "hi") exn:fail:contract?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface with default methods

(let ()
  (define i<%>
    (interface ()
      m1
      [m2 #:public (lambda (arg)
                     (define v (m1 arg))
                     (test v (send this m1 arg))
                     (list v arg))]))
  (define cn%
    (class* object% (i<%>)
      (super-new)
      (define/public (m1 arg) (+ arg 1))))
  (define cy%
    (class* object% (i<%>)
      (super-new)
      (define/public (m1 arg) (+ arg 1))
      (define/override (m2 arg) (+ arg 2))))  
  (define pre-z%
    (class* object% (i<%>)
      (super-new)
      (define/public (m1 arg) "not here")
      (define/override (m2 arg) (+ arg 2))))
  (define cz%
    (class* pre-z% (i<%>)
      (super-new)
      (define/override (m1 arg) (+ arg 1))))

  (test 11 (send (new cn%) m1 10))
  (test (list 11 10) (send (new cn%) m2 10))
  (test 11 (send (new cy%) m1 10))
  (test 12 (send (new cy%) m2 10))
  (test 12 (send (new cz%) m2 10))

  (err/rt-test (class* object% (i<%>)
                 (super-new))
               exn:fail?
               #rx"missing interface-required method")
  (err/rt-test (class* object% (i<%>)
                 (super-new)
                 (define/override (m2 arg) (+ arg 2)))
               exn:fail?
               #rx"missing interface-required method")

  (let ()
    (define pre-z0%
      (class object%
        (super-new)
        (define/public (m2 arg) (+ arg 2))))
    (err/rt-test (class* pre-z0% (i<%>)
                   (super-new)
                   (define/override (m1 arg) (+ arg 1)))
                 exn:fail?
                 #rx"method implmentation in both superclass and superinterface"))

  (define i2<%>
    (interface (i<%>)
      m3
      [m4  (->m any/c list?) #:public (lambda (arg)
                                        (and arg
                                             (let ([v (m3 arg)])
                                               (test v (send this m3 arg))
                                               (list v arg))))]
      [m5 #:public (lambda (arg) (m4 arg))]))
  (define c2n%
    (class* object% (i2<%>)
      (super-new)
      (define/public (m1 arg) (+ arg 1))
      (define/public (m3 arg) (+ arg 3))))
  (define c2y%
    (class* object% (i2<%>)
      (super-new)
      (define/public (m1 arg) (+ arg 1))
      (define/override (m2 arg) (+ arg 2))
      (define/public (m3 arg) (+ arg 3))
      (define/override (m4 arg) (and arg (list (+ arg 4))))))

  (test 11 (send (new c2n%) m1 10))
  (test (list 11 10) (send (new c2n%) m2 10))
  (test 13 (send (new c2n%) m3 10))
  (test (list 13 10) (send (new c2n%) m4 10))
  (test 11 (send (new c2y%) m1 10))
  (test 12 (send (new c2y%) m2 10))
  (test 13 (send (new c2y%) m3 10))
  (test (list 14) (send (new c2y%) m4 10))

  (err/rt-test (send (new c2n%) m4 #f)
               exn:fail:contract?
               #rx"broke its own contract")
  (err/rt-test (send (new c2y%) m4 #f)
               exn:fail:contract?
               #rx"broke its own contract")
  (err/rt-test (send (new c2y%) m5 #f)
               exn:fail:contract?
               #rx"broke its own contract")

  ;; ok to have default implementations from same original interface
  (test #t interface? (interface (i<%> i2<%>)))
  (test #t interface? (interface (i2<%> (interface (i<%>)))))

  (err/rt-test (interface (i<%>
                           (interface ()
                             [m2 #:public (lambda (arg) "oops")])))
               exn:fail:object?
               #rx"conflicting default implementations")

  (err/rt-test (interface ()
                 [m2 #:override (lambda (arg) "oops")])
               exn:fail:object?
               #rx"method is not implemented in any superinterface")

  (test #t interface? (interface (i<%>
                                  (interface ()
                                    [m2 #:public (lambda (arg) "oops")]))
                        [m2 #:override (lambda (arg) "ok")]))

  (err/rt-test (interface (i<%>)
                 [m2 #:public (lambda (arg) "ok")])
               exn:fail:object?
               #rx"superinterface has default implementation of method")

  (void))

(let ()
  (define i<%>
    (interface ()
      m1
      [m2 #:public (lambda (arg)
                     (list arg))]))  
  (define cy%
    (class* object% (i<%>)
      (super-new)
      (define/public (m1 arg) (+ arg 1))
      (define/override (m2 arg) (super m2 (+ arg 2)))))
  (test (list 12) (send (new cy%) m2 10)))

(let ()
  (define i1<%>
    (interface ()
      [m2 #:public (lambda (arg)
                     (list arg))]))  
  (define i2<%>
    (interface ()
      [m2 #:public (lambda (arg)
                     (list arg))]))
  (define cy%
    (class* object% (i1<%> i2<%>)
      (super-new)
      (define/override (m2 arg) (super m2 (+ arg 2)))))
  (err/rt-test (send (new cy%) m2 10)
               exn:fail:object?
               #rx"method is implemented by multiple superinterfaces"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

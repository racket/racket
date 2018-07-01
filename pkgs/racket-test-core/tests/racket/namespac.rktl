
(load-relative "loadtest.rktl")

(Section 'namespaces)

(arity-test eval 1 2)
(arity-test compile 1 1)
(arity-test compiled-expression? 1 1)

(test #f compiled-expression? 1)
(test #t values (compiled-expression? (compile 1)))
(test #t values (compiled-expression? (let ([c (compile 1)]
					    [p (open-output-bytes)])
					(display c p)
					(parameterize ([read-accept-compiled #t])
					  (read (open-input-bytes (get-output-bytes p)))))))

(test `,void eval `',void)

#|
(test car eval 'car (scheme-report-environment 5))
(err/rt-test (eval 'car (null-environment 5)) exn:fail:contract:variable?)
(err/rt-test (eval 'lambda (null-environment 5)) exn:fail:syntax?)
(err/rt-test (eval 'lambda (scheme-report-environment 5)) exn:fail:syntax?)
(err/rt-test (eval 'lambda (null-environment 5)) exn:fail:syntax?)
|#

; Test primitive-name
(let ([gvl (parameterize ([current-namespace (make-base-namespace)]) 
             (namespace-require/copy 'scheme/base)
	     (map (lambda (s)
		    (cons s (with-handlers ([exn:fail? (lambda (x) #f)])
			      (namespace-variable-value s))))
		  (namespace-mapped-symbols)))]
      [aliases (let ([mkp (lambda (s)
                            (cons (string-append "make-" s) s))])
                 (list (cons "call/cc" "call-with-current-continuation")
                       (cons "call/ec" "call-with-escape-continuation")
                       (cons "interaction-environment" "current-namespace")
                       (mkp "arity-at-least")
                       (mkp "srcloc")
                       (mkp "date")
                       (mkp "date*")
                       (mkp "exn")
                       (mkp "exn:fail")
                       (mkp "exn:fail:contract")
                       (mkp "exn:fail:contract:arity")
                       (mkp "exn:fail:contract:divide-by-zero")
                       (mkp "exn:fail:contract:non-fixnum-result")
                       (mkp "exn:fail:contract:continuation")
                       (mkp "exn:fail:contract:variable")
                       (mkp "exn:fail:syntax")
                       (mkp "exn:fail:syntax:unbound")
                       (mkp "exn:fail:syntax:missing-module")
                       (mkp "exn:fail:read")
                       (mkp "exn:fail:read:eof")
                       (mkp "exn:fail:read:non-char")
                       (mkp "exn:fail:filesystem")
                       (mkp "exn:fail:filesystem:exists")
                       (mkp "exn:fail:filesystem:version")
                       (mkp "exn:fail:filesystem:errno")
                       (mkp "exn:fail:filesystem:missing-module")
                       (mkp "exn:fail:network")
                       (mkp "exn:fail:network:errno")
                       (mkp "exn:fail:out-of-memory")
                       (mkp "exn:fail:unsupported")
                       (mkp "exn:fail:user")
                       (mkp "exn:break")
                       (mkp "exn:break:hang-up")
                       (mkp "exn:break:terminate")))])
  (test #t 'names
	(andmap
	 (lambda (nv-pair)
	   (let ([name (car nv-pair)]
		 [value (cdr nv-pair)])
	     (or (not (primitive? value))
		 (let* ([s (symbol->string name)]
			[sr (if (char=? #\# (string-ref s 0))
				(substring s 2 (string-length s))
				s)]
			[st (let ([m (assoc sr aliases)])
			      (if m
				  (cdr m)
				  sr))])
		   (or (equal? st (symbol->string (object-name value)))
		       (and (fprintf (current-error-port)
				     "different: ~s ~s\n" st (object-name value))
			    #f))))))
	 gvl)))

;; Test empty namespace:
(let ([e (make-empty-namespace)]
      [orig (current-namespace)])
  (parameterize ([current-namespace e])
    (test null namespace-mapped-symbols)
    (test 'unbound 'empty-namespace
          (with-handlers ([void (lambda (exn) 'unbound)])
            (eval 'car)))
    (test 'unbound 'empty-namespace
          (with-handlers ([void (lambda (exn) 'unbound)])
            (eval '#%car)))
    (namespace-set-variable-value! 'hello 5)
    (namespace-attach-module orig 'racket/base)
    (namespace-require '(rename racket/base #%top #%top))
    (test 5 'empty-namespace (eval 'hello))
    (test #t 'top+hello (let ([s (namespace-mapped-symbols)])
                          (or (equal? s '(#%top hello))
                              (equal? s '(hello #%top)))))))

(arity-test namespace-mapped-symbols 0 1)
(arity-test namespace-variable-value 1 4)
(arity-test namespace-set-variable-value! 2 5)
(arity-test namespace-undefine-variable! 1 2)

(define n (make-base-namespace))
(test #t 'same-with-arg-and-param
      ((lambda (a b)
	 (and (andmap (lambda (ai) (memq ai b)) a)
	      (andmap (lambda (bi) (memq bi a)) b)
	      #t))
       (parameterize ([current-namespace n])
	 (namespace-mapped-symbols))
       (namespace-mapped-symbols n)))

(test (void) namespace-set-variable-value! 'foo 17 #t n)
(test 17 namespace-variable-value 'foo #t #f n)
(test (void) namespace-set-variable-value! 'lambda 18 #t n)
(test 18 namespace-variable-value 'lambda #t #f n)
(test (void) namespace-set-variable-value! 'define 19 #f n)
(test 19 namespace-variable-value 'define #f #f n)
(test 20 namespace-variable-value 'define #t (lambda () 20) n)
(test 21 'stx-err (with-handlers ([exn:fail:syntax? (lambda (x) 21)])
		    (namespace-variable-value 'define #t #f n)))
(test 22 'stx-err (with-handlers ([exn:fail:contract:variable? (lambda (x) 22)])
		    (namespace-variable-value 'not-ever-defined #t #f n)))
(test 23 'stx-err (with-handlers ([exn:fail:contract:variable? (lambda (x) 23)])
		    (namespace-variable-value 'define-values #f #f n)))
(test (void) namespace-undefine-variable! 'foo n)
(test 25 namespace-variable-value 'foo #t (lambda () 25) n)
(parameterize ([current-namespace n])
  (test (void) namespace-set-variable-value! 'bar 27)
  (test 27 namespace-variable-value 'bar)
  (test (void) namespace-undefine-variable! 'bar)
  (test 28 namespace-variable-value 'bar #t (lambda () 28)))

;; ----------------------------------------

(test #f
      variable-reference->module-path-index (#%variable-reference test))
(test (module-path-index-join ''#%kernel #f)
      variable-reference->module-path-index (#%variable-reference +))
(require (only-in racket/unsafe/ops
                  [unsafe-fx+ $$unsafe-fx+]))
(test (module-path-index-join ''#%unsafe #f)
      variable-reference->module-path-index (#%variable-reference $$unsafe-fx+))

(test #t variable-reference-constant? (#%variable-reference cons))
(require (only-in ffi/unsafe _bool))
(test #t variable-reference-constant? (#%variable-reference _bool))

;; ----------------------------------------

(module phaser scheme/base 
  (define x (variable-reference->phase
             (#%variable-reference x)))
  (define y (variable-reference->module-base-phase
             (#%variable-reference y)))
  (provide x y))

(test 0 dynamic-require ''phaser 'x)
(test 0 dynamic-require ''phaser 'y)

(let ([s (open-output-string)])
  (parameterize ([current-output-port s])
    (eval '(begin-for-syntax (display (dynamic-require ''phaser 'x)))))
  (test "1" get-output-string s))

(test 0 dynamic-require ''phaser 'x)

(let ([s (open-output-string)])
  (parameterize ([current-output-port s])
    (eval '(begin-for-syntax
            (let ([ns (make-base-namespace)])
              (namespace-attach-module (current-namespace) ''phaser ns)
              (eval '(require 'phaser) ns)
              (display (eval 'x ns))
              (display (eval 'y ns))))))
  (test "11" get-output-string s))

(let ([s (open-output-string)])
  (parameterize ([current-output-port s])
    (let ([ns (make-base-namespace)])
      (eval '(module m racket/base
               (require (for-syntax racket/base))
               (begin-for-syntax
                (define x 10)
                (displayln (variable-reference->phase
                            (#%variable-reference x)))
                (displayln (variable-reference->module-base-phase
                            (#%variable-reference x))))))
      (eval '(require (for-syntax 'm)))
      (eval '(begin-for-syntax 10))))
  (test "1\n0\n2\n1\n" get-output-string s))

(err/rt-test (variable-reference->module-declaration-inspector (#%variable-reference)))
(err/rt-test (variable-reference->module-declaration-inspector (#%variable-reference car)))
(test (void) eval `(module m racket/base (variable-reference->module-declaration-inspector (#%variable-reference))))

;; ----------------------------------------

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(define-namespace-anchor anchor))
  (test 1 eval '(eval 1 (namespace-anchor->namespace anchor))))

;; ----------------------------------------

(module va->ms racket/base
  (provide modsrc)
  (define modsrc (variable-reference->module-source (#%variable-reference))))

(test 'va->ms dynamic-require ''va->ms 'modsrc)

;; ----------------------------------------
;; Check that `namespace-attach-module-declaration' doesn't
;; trigger the module-name resolver in the wrong namespace:

(let ()
  (define ns0 (make-base-namespace))
  (eval '(module sample racket (#%module-begin)) ns0)
  (define ns1 (make-empty-namespace))
  (parameterize ([current-namespace ns1])
    (namespace-attach-module-declaration ns0 ''sample ns1)))

;; ----------------------------------------
;; Check that `namespace-attach-module' works with with for-template
;; requires.

(module nma-for-template-m racket/base
  (define x 1))

(module nma-for-template-n racket/base
  (require (for-syntax 'nma-for-template-m)))

(module nma-for-template-p racket/base
  (require (for-template 'nma-for-template-n)))

(require 'nma-for-template-p)

(let ([ns (make-base-namespace)])
  (namespace-attach-module (current-namespace) ''nma-for-template-p ns)
  (parameterize ([current-namespace ns])
    (namespace-require ''nma-for-template-p))
  (namespace-attach-module (current-namespace) ''nma-for-template-m ns))

;; ----------------------------------------
;; Check that `make-base-empty-namespace' is kill-safe,
;; which amounts to a test that the module-name resolver
;; is kill-safe. When the test fails, it probably gets
;; stuck.
(let ()
  (for ([i 100])
    (let ([th (thread (lambda () 
                        (let loop ()
                          (make-base-empty-namespace)
                          ;;(printf "made\n")
                          (loop))))])
      (sleep)
      ;;(printf "~s\n" i)
      (kill-thread th)))
  (test #t namespace? (make-base-empty-namespace)))

;; ----------------------------------------
;; Check module caching by monitoring `current-eval'.
;; If the module cache works, then it turns out that
;; the `module-compiled-imports' of modules to evaluate
;; will be `eq?' the second time around to the first time
;; around. This is a fragile and imprecise check, but it's
;; the best idea we have to checking that the module cache
;; works.

(let ([codes (make-hash)])
  (define (go-once)
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-eval
                    (let ([orig (current-eval)])
                      (lambda (x)
                        (when (syntax? x)
                          (when (compiled-module-expression? (syntax-e x))
                            (hash-set! codes (module-compiled-imports (syntax-e x)) #t)))
                        (orig x)))])
      (dynamic-require 'racket/string #f)))
  (go-once)
  (let ([pre (hash-count codes)])
    (go-once)
    (test pre hash-count codes)))

;; ----------------------------------------
;; Check `namespace-require' on marked syntax:

(parameterize ([current-namespace (make-base-namespace)])
  (let ([i (make-syntax-introducer)])
    (namespace-require (i (datum->syntax #f 'racket/list)))
    (let ([e (namespace-syntax-introduce (datum->syntax #f '(cons? #t)))])
      (err/rt-test (eval e))
      (test #f eval (i e)))))

;; ----------------------------------------
;; Check cannot-redefine error

(parameterize ([current-namespace (make-base-empty-namespace)])
  (namespace-require/constant 'racket/base)
  (err/rt-test (eval '(define + -)) #rx"cannot change constant"))

;; ----------------------------------------
;; Check that `namespace-require/copy` does define variables
;; but doesn't bind as required

(parameterize ([current-namespace (make-base-empty-namespace)])
  (namespace-require/copy 'racket/base)
  (test (void) eval '(void))
  (test #f identifier-binding (namespace-syntax-introduce (datum->syntax #f 'void)))
  (test #t list? (identifier-binding (namespace-syntax-introduce (datum->syntax #f 'lambda)))))

;; ----------------------------------------
;; Check that bulk `require` replaces individual bindings

(let ([ns (make-base-empty-namespace)])
  (parameterize ([current-namespace ns])
    (namespace-require '(only racket/base)))
  (eval #`(define #,(datum->syntax #f 'cons) 1) ns)
  (eval #`(define #,(datum->syntax #f 'extra) 2) ns)
  (test 1 eval 'cons ns)
  (eval #`(require #,(datum->syntax #f 'racket/base)) ns)
  (test cons eval 'cons ns)
  (test 2 eval 'extra ns))

(let ([ns (make-base-empty-namespace)])
  (parameterize ([current-namespace ns])
    ;; To ensure that the namespace ends up with more than
    ;; `racket/base` individual bindings:
    (namespace-require/copy 'racket/base))
  (eval #`(define #,(datum->syntax #f 'cons) 1) ns)
  (eval #`(define #,(datum->syntax #f 'extra) 2) ns)
  (test 1 eval 'cons ns)
  (eval #`(require #,(datum->syntax #f 'racket/base)) ns)
  (test cons eval 'cons ns)
  (test 2 eval 'extra ns))

;; ----------------------------------------
;; Check that compilation in one namespace can
;; be transferred to another namespace

(let ()
  (define (check-namespace-transfer compile-wrap)
    (let ()
      ;; transfer a `require`
      (define c
        (parameterize ([current-namespace (make-base-namespace)])
          (compile-wrap (compile '(require racket/base)))))
      (parameterize ([current-namespace (make-base-empty-namespace)])
        (test (void) 'eval (eval c))
        (test add1 eval 'add1)))

    (let ()
      ;; transfer a definition, reference is visible, original
      ;; namespace is unchanged
      (define-values (c get)
        (parameterize ([current-namespace (make-base-namespace)])
          (define c (compile-wrap (compile '(define one 1))))
          (values
           c
           (eval '(lambda () one)))))
      (parameterize ([current-namespace (make-base-empty-namespace)])
        (test (void) 'eval (eval c))
        (test 1 eval 'one)
        (err/rt-test (get) exn:fail:contract:variable?)))

    (let ()
      ;; transfer a definition of a macro-introduced variable, and
      ;; check access via a syntax object that is compiled at the same time:
      (define-values (c get)
        (parameterize ([current-namespace (make-base-namespace)])
          (eval '(define-syntax-rule (m id)
                  (begin
                    (define one 1)
                    (define id (quote-syntax one))
                    one)))
          (define c (compile-wrap (compile '(m id))))
          (values
           c
           (eval '(lambda () one)))))
      (parameterize ([current-namespace (make-base-empty-namespace)])
        (test 1 'eval (eval c))
        (err/rt-test (eval 'one) exn:fail:syntax?)
        (test #t identifier? (eval 'id))
        (test 1 eval (eval 'id))
        (err/rt-test (get) exn:fail:contract:variable?))))
  (check-namespace-transfer values)
  (check-namespace-transfer (lambda (c)
                              (define o (open-output-bytes))
                              (write c o)
                              (parameterize ([read-accept-compiled #t])
                                (read (open-input-bytes (get-output-bytes o)))))))

;; ----------------------------------------
;; Make sure compilation doesn't bind in the current namespace

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(define-syntax-rule (m2 c-id r-id)
          (begin
            (require (rename-in racket/base [+ plus]))
            (define (c-id) (compile #'(define plus 1)))
            (define (r-id) (eval #'plus)))))
  (eval '(m2 def ref))
  (test + eval '(ref))
  (eval '(def))
  (test + eval '(ref))
  (eval (eval '(def)))
  (test 1 eval '(ref)))

;; When a binding is present, it takes precedence over
;; a "temporary" binding:
(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax-rule (m3 c-id)
          (begin
            (define-syntax plus #f)
            (define (c-id) (compile #'(define plus plus))))))
  (eval '(m3 cdef))
  (err/rt-test (eval '(cdef)) exn:fail:syntax?))

;; A "temporary" binding should work on an identifier with
;; no `#%top` in its context:
(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax (m3 stx)
          (with-syntax ([(gen) (generate-temporaries '(gen))])
            (syntax-case stx ()
              [(_ id)
               #'(begin
                   (define (gen) gen)
                   (define id gen))]))))
  (eval '(m3 self))
  (test #t eval '(eq? self (self))))

;; ----------------------------------------

(module check-module-path-index-inside-and-outside racket/base
  (provide get)
  (define me 5)
  (define (get)
    (define-values (path1 base1) (module-path-index-split (car (identifier-binding #'me))))
    (define-values (path2 base2)
      (eval '(module-path-index-split (car (identifier-binding #'me)))
            (variable-reference->namespace (#%variable-reference))))
    (list (list path1 base1) (list path2 base2))))

(test '(('check-module-path-index-inside-and-outside #f) (#f #f))
      (dynamic-require ''check-module-path-index-inside-and-outside 'get))

;; ----------------------------------------

(report-errs)

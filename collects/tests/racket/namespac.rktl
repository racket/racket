
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
                       (mkp "exn")
                       (mkp "exn:fail")
                       (mkp "exn:fail:contract")
                       (mkp "exn:fail:contract:arity")
                       (mkp "exn:fail:contract:divide-by-zero")
                       (mkp "exn:fail:contract:non-fixnum-result")
                       (mkp "exn:fail:contract:continuation")
                       (mkp "exn:fail:contract:variable")
                       (mkp "exn:fail:syntax")
                       (mkp "exn:fail:read")
                       (mkp "exn:fail:read:eof")
                       (mkp "exn:fail:read:non-char")
                       (mkp "exn:fail:filesystem")
                       (mkp "exn:fail:filesystem:exists")
                       (mkp "exn:fail:filesystem:version")
                       (mkp "exn:fail:network")
                       (mkp "exn:fail:out-of-memory")
                       (mkp "exn:fail:unsupported")
                       (mkp "exn:fail:user")
                       (mkp "exn:break")))])
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
				     "different: ~s ~s~n" st (object-name value))
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
    (namespace-attach-module orig 'mzscheme)
    (namespace-require '(rename mzscheme #%top #%top))
    (test 5 'empty-namespace (eval 'hello))
    (test #t 'top+hello (let ([s (namespace-mapped-symbols)])
                          (or (equal? s '(#%top hello))
                              (equal? s '(hello #%top)))))))

(arity-test namespace-mapped-symbols 0 1)
(arity-test namespace-variable-value 1 4)
(arity-test namespace-set-variable-value! 2 4)
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

(module phaser scheme/base 
  (define x (variable-reference->phase
             (#%variable-reference x)))
  (provide x))

(test 0 dynamic-require ''phaser 'x)

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
              (display (eval 'x ns))))))
  (test "1" get-output-string s))

;; ----------------------------------------

(report-errs)

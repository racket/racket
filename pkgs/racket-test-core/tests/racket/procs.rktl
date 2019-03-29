
(load-relative "loadtest.rktl")

(Section 'procs)

;; ----------------------------------------

(define (f0) null)
(define (f0+ . x) x)
(define (f0+/drop1 . x) (cdr x))
(define (f1 x) (list x))
(define f1-m
  (let-syntax ([m (lambda (stx)
                    (syntax-property #'(lambda (x) (list x))
                                     'method-arity-error
                                     #t))])
    m))
(define (f1+ x . rest) (cons x rest))
(define (f1+/drop1 x . rest) rest)
(define (f0:a #:a a) (list a))
(define (f0:a? #:a [a 0]) (list a))
(define (f1:a x #:a a) (list x a))
(define (f1:a? x #:a [a 0]) (list x a))
(define (f1+:a x #:a a . args) (list* x a args))
(define (f1+:a? x #:a [a 0] . args) (list* x a args))
(define (f1+:a/drop x #:a a . args) (if (null? args)
                                        (list a)
                                        (list* (car args) a (cdr args))))
(define (f1+:a?/drop x #:a [a 0] . args) (if (null? args)
                                             (list a)
                                             (list* (car args) a (cdr args))))
(define (f2+:a?/drop x y #:a [a 0] . args) (list* y a args))
(define (f0:a:b #:a a #:b b) (list a b))
(define (f0:a?:b #:a [a 0] #:b b) (list a b))
(define (f1:a:b x #:a a #:b b) (list x a b))
(define (f1:a?:b x #:a [a 0] #:b b) (list x a b))
(define (f1+:a:b x #:a a #:b b . args) (list* x a b args))
(define (f1+:a?:b x #:a [a 0] #:b b . args) (list* x a b args))
(define (f0:a:b? #:a a #:b [b 1]) (list a b))
(define (f0:a?:b? #:a [a 0] #:b [b 1]) (list a b))
(define (f1:a:b? x #:a a #:b [b 1]) (list x a b))
(define (f1:a?:b? x #:a [a 0] #:b [b 1]) (list x a b))
(define (f1+:a:b? x #:a a #:b [b 1] . args) (list* x a b args))
(define (f1+:a?:b? x #:a [a 0] #:b [b 1] . args) (list* x a b args))
(define (f1+2:a:b x [y #f] #:a a #:b b) (if y
                                            (if (number? x)
                                                (list x y a b)
                                                (list y a b))
                                            (list x a b)))
(define f_ (case-lambda))
(define f_1_2 (case-lambda
               [(x) (list x)]
               [(x y) (list x y)]))
(define f_0_2+ (case-lambda
               [() null]
               [(x y . args) (list* x y args)]))
(define f1:+ (make-keyword-procedure
              (lambda (kws kw-args x)
                (cons x kw-args))
              (let ([f1:+ (lambda (x) (list x))])
                f1:+)))
(define f1:+/drop (make-keyword-procedure
                   (lambda (kws kw-args x)
                     kw-args)
                   (lambda (x) null)))

(struct wrap (v)
  #:property prop:procedure 0)
(define (wrap-m f)
  (struct wrap-m ()
    #:property prop:procedure f)
  (wrap-m))

(define procs
  `((,f0 0 () ())
    (,(wrap f0) 0 () ())
    (,f0+ ,(make-arity-at-least 0) () ())
    (,(wrap f0+) ,(make-arity-at-least 0) () ())
    (,(wrap-m f0+/drop1) ,(make-arity-at-least 0) () ())
    (,(wrap-m f1+/drop1) ,(make-arity-at-least 0) () ())
    (,f1 1 () ())
    (,f1-m 1 () () #t)
    (,(procedure->method f1) 1 () () #t)
    (,(procedure->method (wrap f1)) 1 () () #t)
    (,(procedure->method (wrap f0+)) ,(make-arity-at-least 0) () () #t)
    (,f1+ ,(make-arity-at-least 1) () ())
    (,f0:a 0 (#:a) (#:a))
    (,f0:a? 0 () (#:a))
    (,f1:a 1 (#:a) (#:a))
    (,f1:a? 1 () (#:a))
    (,f1+:a ,(make-arity-at-least 1) (#:a) (#:a))
    (,f1+:a? ,(make-arity-at-least 1) () (#:a))
    (,(wrap f1+:a) ,(make-arity-at-least 1) (#:a) (#:a))
    (,(wrap f1+:a?) ,(make-arity-at-least 1) () (#:a))
    (,(wrap-m f1+:a/drop) ,(make-arity-at-least 0) (#:a) (#:a))
    (,(wrap-m f1+:a?/drop) ,(make-arity-at-least 0) () (#:a))
    (,(procedure->method (wrap f1+:a?)) ,(make-arity-at-least 1) () (#:a) #t)
    (,f1+2:a:b (1 2) (#:a #:b) (#:a #:b))
    (,(wrap-m f1+2:a:b) (0 1) (#:a #:b) (#:a #:b))
    (,f0:a:b 0 (#:a #:b) (#:a #:b))
    (,f0:a?:b 0 (#:b) (#:a #:b))
    (,f1:a:b 1 (#:a #:b) (#:a #:b))
    (,f1:a?:b 1 (#:b) (#:a #:b))
    (,f1+:a:b ,(make-arity-at-least 1) (#:a #:b) (#:a #:b))
    (,f1+:a?:b ,(make-arity-at-least 1) (#:b) (#:a #:b))
    (,f0:a:b? 0 (#:a) (#:a #:b))
    (,f0:a?:b? 0 () (#:a #:b))
    (,f1:a:b? 1 (#:a) (#:a #:b))
    (,f1:a?:b? 1 () (#:a #:b))
    (,f1+:a:b? ,(make-arity-at-least 1) (#:a) (#:a #:b))
    (,f1+:a?:b? ,(make-arity-at-least 1) () (#:a #:b))
    (,f_ () () ())
    (,f_1_2 (1 2) () ())
    (,f_0_2+ ,(list 0 (make-arity-at-least 2)) () ())
    (,f1:+ 1 () #f)
    (,(wrap f1:+) 1 () #f)
    (,(wrap-m f1:+/drop) 0 () #f)))

((chaperone-procedure
  (wrap f1+:a)
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (if (null? kws)
         (apply values rest)
         (apply values kw-args rest)))))
 1
 #:a 2)
 
(define (check-arity-error p n err-n)
  (cond
   [(procedure-arity-includes? p n #t)
    (unless (n . <= . 1)
      (check-arity-error p 1 (+ 1 (- err-n n))))]
   [else
    (define-values (reqs allows) (procedure-keywords p))
    (err/rt-test (keyword-apply p reqs reqs (make-list n #f))
                 (lambda (exn)
                   (regexp-match? (format "given: ~a|no case matching ~a" 
                                          err-n 
                                          err-n)
                                  (exn-message exn))))]))

(define (run-procedure-tests procedure-arity procedure-reduce-arity)
  (define (get-maybe p n)
    (and ((length p) . > . n) (list-ref p n)))
  (define (try-combos procs add-chaperone) 
    (for-each (lambda (p)
                (let ([a (cadr p)]
                      [method? (get-maybe p 4)]
                      [p (list* (car p) (cadr p) (caddr p) (cadddr p)
                                (if ((length p) . >= . 5)
                                    (list-tail p 5)
                                    null))])
                  (test a procedure-arity (car p))
                  (when (number? a)
                    (let ([rx (regexp (format " mismatch;.*(expected number(?!.*expected:)|expected: ~a|required keywords:)"
                                              (if (zero? a) "(0|no)" (if method? (sub1 a) a))))]
                          [bad-args (cons 'extra (for/list ([i (in-range a)]) 'a))])
                      (test #t regexp-match? rx
                            (with-handlers ([exn:fail? (lambda (exn)
                                                         (exn-message exn))])
                              (apply (car p) bad-args)))
                      (unless (= a 1)
                        (test #t regexp-match? rx
                              (with-handlers ([exn:fail? (lambda (exn)
                                                           (exn-message exn))])
                                (for-each (car p) (list bad-args))
                                "done!")))))
                  (when (and (arity-at-least? a)
                             (positive? (arity-at-least-value a)))
                    (let ([a (arity-at-least-value a)])
                      (let ([rx (regexp (format " mismatch;.*(expected number(?!.*expected:)|expected: at least ~a)"
                                                (if (zero? a) "(0|no)" (if method? (sub1 a) a))))]
                            [bad-args (for/list ([i (in-range (sub1 a))]) 'a)])
                        (test #t regexp-match? rx
                              (with-handlers ([exn:fail? (lambda (exn)
                                                           (exn-message exn))])
                                (apply (car p) bad-args))))))
                  (test-values (list (caddr p) (cadddr p))
                               (lambda ()
                                 (procedure-keywords (car p))))
                  (define (check-ok n a)
                    (let loop ([a a])
                      (or (equal? a n)
                          (and (arity-at-least? a)
                               ((arity-at-least-value a) . <= . n))
                          (and (list? a)
                               (ormap loop a)))))
                  (let ([1-ok? (check-ok 1 a)]
                        [0-ok? (check-ok 0 a)])
                    (test 1-ok? procedure-arity-includes? (car p) 1 #t)
                    (test 0-ok? procedure-arity-includes? (car p) 0 #t)
                    ;; While we're here test renaming, etc.:
                    (test 'other object-name (procedure-rename (car p) 'other))
                    (test (procedure-arity (car p)) procedure-arity (procedure-rename (car p) 'other))
                    (test (procedure-arity (car p)) procedure-arity (procedure->method (car p)))
                    (check-arity-error (car p) 10 (if method? 9 10))
                    (check-arity-error (procedure->method (car p)) 10 9)
                    (unless (null? (list-tail p 4))
                      (test (object-name (list-ref p 4)) object-name (car p)))
                    (let ([allowed (cadddr p)]
                          [required (caddr p)])
                      ;; If some keyword is required, make sure that a plain
                      ;;  application fails:
                      (unless (null? required)
                        (err/rt-test
                         (apply (car p) (make-list (procedure-arity (car p)) #\0))))
                      ;; Other tests:
                      (if 1-ok?
                          (cond
                           [(equal? allowed '())
                            (test (let ([auto (let ([q (cddddr p)])
                                                (if (null? q)
                                                    q
                                                    (cdr q)))])
                                    (cond
                                     [(equal? auto '((#:a #:b))) '(1 0 1)]
                                     [(equal? auto '((#:a))) '(1 0)]
                                     [(equal? auto '((#:a))) '(1 0)]
                                     [else '(1)]))
                                  (car p) 1)
                            (err/rt-test ((car p) 1 #:a 0))
                            (err/rt-test ((car p) 1 #:b 0))
                            (err/rt-test ((car p) 1 #:a 0 #:b 0))]
                           [(equal? allowed '(#:a))
                            (test (if (and (pair? (cddddr p))
                                           (pair? (cddddr (cdr p))))
                                      '(10 20 1) ; dropped #:b
                                      '(10 20))
                                  (car p) 10 #:a 20)
                            (err/rt-test ((car p) 1 #:b 0))
                            (err/rt-test ((car p) 1 #:a 0 #:b 0))]
                           [(equal? allowed '(#:b))
                            (test '(10.0 20.0) (car p) 10.0 #:b 20.0)
                            (err/rt-test ((car p) 1 #:a 0))
                            (err/rt-test ((car p) 1 #:a 0 #:b 0))]
                           [(equal? allowed '(#:a #:b))
                            (test '(100 200 300) (car p) 100 #:b 300 #:a 200)
                            (err/rt-test ((car p) 1 #:a 0 #:b 0 #:c 3))]
                           [(equal? allowed #f)
                            (test '(1 2 3) (car p) 1 #:b 3 #:a 2)])
                          (begin
                            ;; Try just 1:
                            (err/rt-test ((car p) 1))
                            ;; Try with right keyword args, to make sure the by-position
                            ;; arity is checked:
                            (cond
                             [(equal? allowed '())
                              (void)]
                             [(equal? allowed '(#:a))
                              (err/rt-test ((car p) 1 #:a 1))]
                             [(equal? allowed '(#:b))
                              (err/rt-test ((car p) 1 #:b 1))]
                             [(equal? allowed '(#:a #:b))
                              (err/rt-test ((car p) 1 #:a 1 #:b 1))]
                             [(equal? allowed #f)
                              (err/rt-test ((car p) 1 #:a 1 #:b 1))])))))))
              (map
               add-chaperone
               (append procs
                       ;; reduce to arity 1 or nothing:
                       (map (lambda (p)
                              (let ([p (car p)]
                                    [method? (get-maybe p 4)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (null? allowed)
                                      (if (procedure-arity-includes? p 1 #t)
                                          (list (procedure-reduce-arity p 1) 1 req allowed method? p)
                                          (list (procedure-reduce-arity p '()) '() req allowed method? p))
                                      (if (procedure-arity-includes? p 1 #t)
                                          (list (procedure-reduce-keyword-arity p 1 req allowed) 1 req allowed method? p)
                                          (list (procedure-reduce-keyword-arity p '() req allowed) '() req allowed method? p))))))
                            procs)
                       ;; reduce to arity 0 or nothing:
                       (map (lambda (p)
                              (let ([p (car p)]
                                    [method? (get-maybe p 4)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (null? allowed)
                                      (if (procedure-arity-includes? p 0 #t)
                                          (list (procedure-reduce-arity p 0) 0 req allowed method? p)
                                          (list (procedure-reduce-arity p '()) '() req allowed method? p))
                                      (if (procedure-arity-includes? p 0 #t)
                                          (list (procedure-reduce-keyword-arity p 0 req allowed) 0 req allowed method? p)
                                          (list (procedure-reduce-keyword-arity p '() req allowed) '() req allowed method? p))))))
                            procs)
                       ;; reduce to arity 1 or nothing --- no keywords:
                       (map (lambda (p)
                              (let ([p (car p)]
                                    [method? (get-maybe p 4)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (procedure-arity-includes? p 1)
                                      (list* (procedure-reduce-arity p 1) 1 '() '() method? p
                                             (if (null? allowed)
                                                 null
                                                 (list allowed)))
                                      (begin
                                        (when (procedure-arity-includes? p 1 #t)
                                          (err/rt-test (procedure-reduce-arity p 1) exn:fail? #rx"has required keyword arguments"))
                                        (list (procedure-reduce-arity p '()) '() '() '() method? p))))))
                            procs)
                       ;; reduce to arity 0 or nothing --- no keywords:
                       (map (lambda (p)
                              (let ([p (car p)]
                                    [method? (get-maybe p 4)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (procedure-arity-includes? p 0)
                                      (list (procedure-reduce-arity p 0) 0 '() '() method? p)
                                      (list (procedure-reduce-arity p '()) '() '() '() method? p)))))
                            procs)
                       ;; make #:a required, if possible:
                       (map (lambda (p)
                              (let-values ([(req allowed) (procedure-keywords (car p))])
                                (let ([new-req (if (member '#:a req)
                                                   req
                                                   (cons '#:a req))])
                                  (list (procedure-reduce-keyword-arity 
                                         (car p)
                                         (cadr p)
                                         new-req
                                         allowed)
                                        (cadr p)
                                        new-req
                                        allowed
                                        (get-maybe p 4)
                                        (car p)))))
                            (filter (lambda (p)
                                      (let-values ([(req allowed) (procedure-keywords (car p))])
                                        (or (not allowed)
                                            (memq '#:a allowed))))
                                    procs))
                       ;; remove #:b, if allowed and not required:
                       (map (lambda (p)
                              (let-values ([(req allowed) (procedure-keywords (car p))])
                                (let ([new-allowed (if allowed
                                                       (remove '#:b allowed)
                                                       '(#:a))])
                                  (list* (procedure-reduce-keyword-arity 
                                          (car p)
                                          (cadr p)
                                          req
                                          new-allowed)
                                         (cadr p)
                                         req
                                         new-allowed
                                         (get-maybe p 4)
                                         (car p)
                                         (if allowed
                                             (list allowed)
                                             '())))))
                            (filter (lambda (p)
                                      (let-values ([(req allowed) (procedure-keywords (car p))])
                                        (and (or (not allowed)
                                                 (memq '#:b allowed))
                                             (not (memq '#:b req)))))
                                    procs))))))
  (try-combos procs values)
  (let ([add-chaperone (lambda (p)
                         (cons
                          (chaperone-procedure
                           (car p)
                           (make-keyword-procedure
                            (lambda (kws kw-args . rest)
                              (if (null? kws)
                                  (apply values rest)
                                  (apply values kw-args rest)))))
                          (cdr p)))])
    (try-combos procs add-chaperone)
    (try-combos (map add-chaperone procs) values)
    (try-combos (map add-chaperone procs) add-chaperone)))

(define (mask->arity mask)
  (let loop ([mask mask] [pos 0])
    (cond
     [(= mask 0) null]
     [(= mask -1) (arity-at-least pos)]
     [(bitwise-bit-set? mask 0)
      (let ([rest (loop (arithmetic-shift mask -1) (add1 pos))])
        (cond
         [(null? rest) pos]
         [(pair? rest) (cons pos rest)]
         [else (list pos rest)]))]
     [else
      (loop (arithmetic-shift mask -1) (add1 pos))])))

(define (arity->mask a)
  (cond
   [(exact-nonnegative-integer? a)
    (arithmetic-shift 1 a)]
   [(arity-at-least? a)
    (bitwise-xor -1 (sub1 (arithmetic-shift 1 (arity-at-least-value a))))]
   [(list? a)
    (let loop ([mask 0] [l a])
      (cond
       [(null? l) mask]
       [else
        (let ([a (car l)])
          (cond
           [(or (exact-nonnegative-integer? a)
                (arity-at-least? a))
            (loop (bitwise-ior mask (arity->mask a)) (cdr l))]
           [else #f]))]))]
   [else #f]))

(run-procedure-tests procedure-arity procedure-reduce-arity)
(run-procedure-tests (lambda (p) (mask->arity (procedure-arity-mask p)))
                     (lambda (p a [name #f]) (procedure-reduce-arity-mask p (arity->mask a) name)))

;; ------------------------------------------------------------
;; Check arity reporting for methods.

(map
 (lambda (jit?)
   (parameterize ([eval-jit-enabled jit?])
     (let ([mk-f (lambda ()
		   (eval (syntax-property #'(lambda (a b) a) 'method-arity-error #t)))]
	   [check-arity-error
	    (lambda (f cl?)
	      (test (if cl? '("given: 0")  '("expected: 1\n"))
                    regexp-match #rx"expected: 1\n|given: 0$"
		    (exn-message (with-handlers ([values values])
				   ;; Use `apply' to avoid triggering
				   ;; compilation of f:
				   (apply f '(1))))))])
       (test 2 procedure-arity (mk-f))
       (check-arity-error (mk-f) #f)
       (test 1 (mk-f) 1 2)
       (let ([f (mk-f)])
	 (test 1 (mk-f) 1 2)
	 (check-arity-error (mk-f) #f))
       (let ([mk-f (lambda ()
		     (eval (syntax-property #'(case-lambda [(a b) a] [(c d e) c]) 'method-arity-error #t)))])
	 (test '(2 3) procedure-arity (mk-f))
	 (check-arity-error (mk-f) #t)
	 (test 1 (mk-f) 1 2)
	 (let ([f (mk-f)])
	   (test 1 (mk-f) 1 2)
	   (check-arity-error (mk-f) #t))))
     (let* ([f (lambda (a b) a)]
            [meth (procedure->method f)]
            [check-arity-error
             (lambda (f cl?)
               (test (if cl? '("given: 0")  '("expected: 1\n"))
                    regexp-match #rx"expected: 1\n|given: 0$"
                     (exn-message (with-handlers ([values values])
                                    ;; Use `apply' to avoid triggering
                                    ;; compilation of f:
                                    (apply f '(1))))))])
       (test 2 procedure-arity meth)
       (check-arity-error meth #f)
       (test 1 meth 1 2)
       (let* ([f (case-lambda [(a b) a] [(c d e) c])]
              [meth (procedure->method f)])
	 (test '(2 3) procedure-arity meth)
	 (check-arity-error meth #t)
	 (test 1 meth 1 2)))))
 '(#t #f))

;; ----------------------------------------
;; Check error for non-procedures
(err/rt-test (1 2 3) (lambda (x) (regexp-match? "not a procedure" (exn-message x))))
(err/rt-test (1 #:x 2 #:y 3) (lambda (x) (regexp-match? "not a procedure" (exn-message x))))

;; ----------------------------------------
;; Check error reporting of `procedure-reduce-keyword-arity'

(err/rt-test (procedure-reduce-keyword-arity void 1 '(#:b #:a) null)
             (lambda (exn) (regexp-match #rx"position: 3rd" (exn-message exn))))
(err/rt-test (procedure-reduce-keyword-arity void 1 null '(#:b #:a))
             (lambda (exn) (regexp-match #rx"position: 4th" (exn-message exn))))

;; ----------------------------------------
;; Check mutation of direct-called keyword procedure

(let ()
  (define (f #:x x) (list x))
  (set! f (lambda (#:y y) (box y)))
  (test (box 8) (lambda () (f #:y 8))))

(let ()
  (define (f #:x [x 1]) x)
  (test 7 (lambda () (f #:x 7)))
  (set! f #f))

;; ----------------------------------------
;; Check mutation of direct-called keyword procedure

(let ()
  (define (f1 #:x x) (list x))
  (test 'f1 object-name f1))
(let ()
  (define (f2 #:x [x 8]) (list x))
  (test 'f2 object-name f2))

;; ----------------------------------------
;; Check `procedure-arity-includes?' with method-style `prop:procedure' value
;; and `procedure-arity-reduce':

(let ()
  (struct a ()
    #:property prop:procedure (procedure-reduce-arity 
                               (lambda (x y [z 5]) (+ y z))
                               3))
  (test #t procedure-arity-includes? (a) 2))

;; ----------------------------------------
;; Make sure that keyword function application propagates syntax properties
;; on the argument expressions
;;
;; This feature is needed for Typed Racket to cooperate with the contract
;; system's addition of extra arguments for modular boundary contracts
;;
;; See typed-racket/typecheck/tc-app-keywords.rkt for its use.

(let ()
  (define (f x #:foo foo) x)
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ f a . es)
       #`(f #,(syntax-property #'a 'test #t) . es)]))

  (define-syntax (expander stx)
    (syntax-case stx ()
      [(_ e)
       (let ([expansion (local-expand #'e 'expression null)])
         (displayln (syntax->datum expansion))
         (syntax-case expansion (let-values #%plain-app if)
           [(let-values _
              (if _
                  _
                  (#%plain-app (#%plain-app . _) _ _ a)))
            (if (syntax-property #'a 'test)
                #'#t
                #'#f)]))]))

  ;; the syntax for `1` should have the syntax property
  ;; in which case this expands to (lambda () #t)
  (test #t (lambda () (expander (m f 1 #:foo 3)))))

;; ----------------------------------------
;; Test that syntax property propagation in kw applications isn't
;; buggy for symbolic inferred names.

(let ()
  (define (f #:x [x 10]) x)

  (define-syntax (ba stx)
    (syntax-property #'(#%app f #:x 8)
                     'inferred-name
                     'bind-accum))

  (test 8 (lambda () (ba))))

;; ----------------------------------------
;; Test that procedure-rename doesn't accidentally convert procedures
;; into methods or methods into procedures.

(let ()
  (define (f a b c d e #:x [x 5]) a)
  (err/rt-test (f)
               (lambda (exn)
                 (regexp-match? #rx"expected: 5 plus an optional argument with keyword #:x"
                                (exn-message exn))))

  ;; procedure-rename shouldn't change this arity string
  (define f* (procedure-rename f 'f))
  (err/rt-test (f*)
               (lambda (exn)
                 (regexp-match? #rx"expected: 5 plus an optional argument with keyword #:x"
                                (exn-message exn))))

  ;; but procedure->method should
  (define fm (procedure->method f))
  (define fm* (procedure-rename fm 'fm))
  (err/rt-test (fm)
               (lambda (exn)
                 (regexp-match? #rx"expected: 4 plus an optional argument with keyword #:x"
                                (exn-message exn))))
  (err/rt-test (fm*)
               (lambda (exn)
                 (regexp-match? #rx"expected: 4 plus an optional argument with keyword #:x"
                                (exn-message exn)))))

;; ----------------------------------------
;; Make sure that optional-argument handling doesn't go wrong with literal gensyms

(let ()
  (eval (let ([s (gensym)])
          `(module optional-argument-with-gensym-default racket/base
             (define (f #:x [x ',s])
               (eq? x ',s))
             (provide f))))
  (namespace-require ''optional-argument-with-gensym-default)
  (let ([o (open-output-bytes)])
    (write (compile '(f)) o)
    (test #t 'same? (eval (parameterize ([read-accept-compiled #t])
                            (read (open-input-bytes (get-output-bytes o))))))))

;; ----------------------------------------
;; Check prop:arity-string

(err/rt-test (let ()
               (struct a (x)
                 #:property prop:arity-string 'bad)
               (a 0)))

(err/rt-test (let ()
               (struct evens (proc)
                 #:property prop:procedure (struct-field-index proc)
                 #:property prop:arity-string
                 (lambda (p)
                   "an even number of arguments"))
               ((evens (lambda (x y) x)) 100))
             exn:fail:contract?
             #rx"an even number of arguments")

;; ----------------------------------------

(report-errs)

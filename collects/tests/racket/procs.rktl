
(load-relative "loadtest.rktl")

(Section 'procs)

;; ----------------------------------------

(define (f0) null)
(define (f1 x) (list x))
(define (f1+ x . rest) (cons x rest))
(define (f0:a #:a a) (list a))
(define (f0:a? #:a [a 0]) (list a))
(define (f1:a x #:a a) (list x a))
(define (f1:a? x #:a [a 0]) (list x a))
(define (f1+:a x #:a a . args) (list* x a args))
(define (f1+:a? x #:a [a 0] . args) (list* x a args))
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
              (lambda (x) (list x))))

(define procs
  `((,f0 0 () ())
    (,f1 1 () ())
    (,f1+ ,(make-arity-at-least 1) () ())
    (,f0:a 0 (#:a) (#:a))
    (,f0:a? 0 () (#:a))
    (,f1:a 1 (#:a) (#:a))
    (,f1:a? 1 () (#:a))
    (,f1+:a ,(make-arity-at-least 1) (#:a) (#:a))
    (,f1+:a? ,(make-arity-at-least 1) () (#:a))
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
    (,f1:+ 1 () #f)))

(let ()
  (define (try-combos procs add-chaperone) 
    (for-each (lambda (p)
                (let ([a (cadr p)])
                  (test a procedure-arity (car p))
                  (when (number? a)
                    (let ([rx (regexp (format "arity mismatch;.*expected: (|at least )~a" 
                                              (if (zero? a) "(0|no)" a)))]
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
                  (test-values (list (caddr p) (cadddr p))
                               (lambda ()
                                 (procedure-keywords (car p))))
                  (let ([1-ok? (let loop ([a a])
                                 (or (equal? a 1)
                                     (and (arity-at-least? a)
                                          ((arity-at-least-value a) . <= . 1))
                                     (and (list? a)
                                          (ormap loop a))))])
                    (test 1-ok? procedure-arity-includes? (car p) 1 #t)
                    ;; While we're here test renaming, etc.:
                    (test 'other object-name (procedure-rename (car p) 'other))
                    (test (procedure-arity (car p)) procedure-arity (procedure-rename (car p) 'other))
                    (test (procedure-arity (car p)) procedure-arity (procedure->method (car p)))
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
                              (let ([p (car p)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (null? allowed)
                                      (if (procedure-arity-includes? p 1 #t)
                                          (list (procedure-reduce-arity p 1) 1 req allowed p)
                                          (list (procedure-reduce-arity p '()) '() req allowed p))
                                      (if (procedure-arity-includes? p 1 #t)
                                          (list (procedure-reduce-keyword-arity p 1 req allowed) 1 req allowed p)
                                          (list (procedure-reduce-keyword-arity p '() req allowed) '() req allowed p))))))
                            procs)
                       ;; reduce to arity 0 or nothing:
                       (map (lambda (p)
                              (let ([p (car p)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (null? allowed)
                                      (if (procedure-arity-includes? p 0 #t)
                                          (list (procedure-reduce-arity p 0) 0 req allowed p)
                                          (list (procedure-reduce-arity p '()) '() req allowed p))
                                      (if (procedure-arity-includes? p 0 #t)
                                          (list (procedure-reduce-keyword-arity p 0 req allowed) 0 req allowed p)
                                          (list (procedure-reduce-keyword-arity p '() req allowed) '() req allowed p))))))
                            procs)
                       ;; reduce to arity 1 or nothing --- no keywords:
                       (map (lambda (p)
                              (let ([p (car p)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (procedure-arity-includes? p 1)
                                      (list* (procedure-reduce-arity p 1) 1 '() '() p
                                             (if (null? allowed)
                                                 null
                                                 (list allowed)))
                                      (list (procedure-reduce-arity p '()) '() '() '() p)))))
                            procs)
                       ;; reduce to arity 0 or nothing --- no keywords:
                       (map (lambda (p)
                              (let ([p (car p)])
                                (let-values ([(req allowed) (procedure-keywords p)])
                                  (if (procedure-arity-includes? p 0)
                                      (list (procedure-reduce-arity p 0) 0 '() '() p)
                                      (list (procedure-reduce-arity p '()) '() '() '() p)))))
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

(report-errs)

#lang racket
#|

This is the semantics of Beginner Scheme, one of the
languages in DrRacket.

The first test case fails because the beginner spec
is broken for that program (ie, the model faithfully
reflects the (broken) spec).

|#

(require redex/reduction-semantics)

(provide run-tests
         run-big-test
         reductions)

#|
  
  `lang' below is actually more generous than beginner, but the
  reductions assume that the programs are all syntactically
  well-formed programs in beginner scheme (with the additional
  constraints that all makers are properly applied and function-
  defined variables are only applied and non-function-defined
  variables are never applied -- except for the maker check,
  these will be in a future version of beginner)
  
  still missing: many primops and characters
  
  are there any primops that take zero arguments?
  (should that be syntacically disallowed?)

  |#

(define-language lang
  (p (d/e ...))
  (d/e (define (x x x ...) e)
       (define x (lambda (x x ...) e))
       (define x e)
       (define-struct x (x ...))
       e)
  (e (x e e ...)
     (prim-op e ...) 
     (cond (e e) (e e) ...)
     (cond (e e) ... (else e))
     (if e e e)
     (and e e e ...)
     (or e e e ...)
     x
     v)
  
  (prim-op + * / cons first rest empty? struct? symbol=?)
  
  (p-ctxt (d/e-v ... d/e-ctxt d/e ...))
  (d/e-ctxt (define x e-ctxt)
            e-ctxt)
  (e-ctxt hole
          (x v ... e-ctxt e ...)
          (prim-op v ... e-ctxt e ...)
          (cond [false e] ... [e-ctxt e] [e e] ...)
          (cond [false e] ... [e-ctxt e] [e e] ... [else e])
          (and true ... e-ctxt e ...)
          (or false ... e-ctxt e ...))
  
  (d/e-v (define x (lambda (x x ...) e))
         (define x v)
         (define (x x x ...) e)
         (define-struct x (x ...))
         v)
  
  (v (maker v ...)
     non-struct-value)
  (non-struct-value number
                    list-value
                    boolean
                    string
                    'x)
  (list-value empty
              (cons v list-value))
  (boolean true
           false)
  
  (maker (side-condition variable_1 (maker? (term variable_1))))
  
  (x (side-condition
      (name 
       x
       (variable-except define
                        define-struct
                        lambda
                        cond
                        else
                        if
                        and
                        or
                        empty
                        true
                        false
                        quote))
      (not (prim-op? (term x))))))

(define-metafunction lang
  [(beg-e-subst (x v x)) v]
  [(beg-e-subst (x v (any_1 ...))) ((beg-e-subst (x v any_1)) ...)]
  [(beg-e-subst (x v any)) any])

(define (maker? v)
  (and (symbol? v)
       (regexp-match #rx"^make-" (symbol->string v))))

(define p? (redex-match lang p))
(define prim-op? (redex-match lang prim-op))

(define reductions
  (reduction-relation
   lang
   ((and true ... false e ...) . ==> . false)
   ((and true ...) . ==> . true)
   ((side-condition (and true ... v_1 e ...)
                    (and (not (eq? (term v_1) 'true))
                         (not (eq? (term v_1) 'false))))
    . e==> .
    "and: question result is not true or false")
   ((or false ... true e ...) . ==> . true)
   ((or false ...) . ==> . false)
   ((side-condition (or false ... v_1 e ...)
                    (and (not (eq? (term v_1) 'true))
                         (not (eq? (term v_1) 'false))))
    . e==> .
    "or: question result is not true or false")
   
   ((if true e_1 e_2) . ==> . e_1)
   ((if false e_1 e_2) . ==> . e_2)
   (e==> (if v_1 e_1 e_2)
         "if: question result is not true or false"
         (side-condition (and (not (eq? (term v_1) 'false))
                              (not (eq? (term v_1) 'true)))))
   
   
   ((cond (false e) ... (true e_1) (e_2 e_3) ...) . ==> . e_1)
   ((cond (false e) ... (true e_1) (e_2 e_3) ... (else e_4)) . ==> . e_1)
   ((cond (false e) ... (else e_1)) . ==> . e_1)
   ((cond (false e) ...) . e==> . "cond: all question results were false")
   
   ((side-condition
     (cond (false e_1) ... (v_1 e_2) (e_3 e_4) ...)
     (and (not (eq? (term v_1) 'false))
          (not (eq? (term v_1) 'true))
          (not (eq? (term v_1) 'else))))
    . e==> .
    "cond: question result is not true or false")
   
   ((side-condition
     (cond (false e_1) ... (v_1 e_2) (e_3 e_4) ... (else e_5))
     (and (not (eq? (term v_1) 'false))
          (not (eq? (term v_1) 'true))
          (not (eq? (term v_1) 'else))))
    . e==> .
    "cond: question result is not true or false")
   
   
   ((empty? empty) . ==> . true)
   ((side-condition (empty? v_1)
                    (not (eq? (term v_1) 'empty)))
    . ==> . 
    false)
   ((empty?) . e==> . "empty?: expects one argument")
   ((empty? v_1 v_2 v_3 ...) . e==> . "empty?: expects one argument")
   
   ((side-condition (cons v v_1)
                    (and (not (eq? (term v_1) 'empty))
                         (not (and (pair? (term v_1))
                                   (eq? (car (term v_1)) 'cons)))))
    . e==> .
    "cons: second argument must be of type <list>")
   
   ((first (cons v_1 list-value)) . ==> . v_1)
   ((first) . e==> . "first: expects one argument")
   ((first v_1 v_2 v_3 ...) . e==> . "first: expects one argument")
   ((side-condition (first v_1)
                    (not (and (pair? (term v_1)) 
                              (eq? (car (term v_1)) 'cons))))
    . e==> .
    "first: expects argument of type <pair>")
   
   ((rest (cons v list-value_1)) . ==> . list-value_1)
   ((rest v_1 v_2 v_3 ...) . e==> . "rest: expects one argument")
   ((rest) . e==> . "rest: expects one argument")
   
   ((side-condition (rest v_1)
                    (not (and (pair? (term v_1)) 
                              (eq? (car (term v_1)) 'cons))))
    . e==> .
    "rest: expects argument of type <pair>")
   
   ((symbol=? 'x_1 'x_2) . ==> . ,(if (eq? (term x_1) (term x_2)) (term true) (term false)))
   ((side-condition (symbol=? v_1 v_2)
                    (or (not (and (pair? (term v_1))
                                  (eq? (car (term v_1)) 'quote)))
                        (not (and (pair? (term v_2))
                                  (eq? (car (term v_2)) 'quote)))))
    . e==> .
    "symbol=?: expects argument of type <symbol>")
   ((symbol=?)
    . e==> .
    "procedure symbol=?: expects 2 arguments")
   ((symbol=? v_1 v_2 v_3 v_4 ...)
    . e==> .
    "procedure symbol=?: expects 2 arguments")
   
   ((+ number_1 ...) . ==> . ,(apply + (term (number_1 ...))))
   ((* number_1 ...) . ==> . ,(apply * (term (number_1 ...))))
   ((side-condition (+ v_arg ...)
                    (ormap (lambda (v_arg) (not (number? v_arg))) (term (v_arg ...))))
    . e==> .
    "+: expects type <number>")
   
   ((side-condition (/ number_1 number_2 ...)
                    (andmap (lambda (number_2) (not (zero? number_2))) (term (number_2 ...))))
    . ==> .
    ,(apply / (term (number_1 number_2 ...))))
   ((side-condition (/ number_1 number_2 ...)
                    (ormap (lambda (number_2) (zero? number_2)) (term (number_2 ...))))
    . e==> . 
    "/: division by zero")
   ((side-condition (/ v_arg ...)
                    (ormap (lambda (v_arg) (not (number? v_arg))) (term (v_arg ...))))
    . e==> .
    "/: expects type <number>")
   
   ;; unbound id application
   (--> (side-condition
         (d/e-v_before ...
          (in-hole d/e-ctxt (x_f v ...))
          d/e ...)
         (and (not (prim-op? (term x_f)))
              (not (defined? (term x_f) (term (d/e-v_before ...))))))
        ,(format "reference to undefined identifier: ~a" (term x_f)))
   
   ;; procedure application as lambda
   (--> (d/e-v_before ...
         (define x_f (lambda (x_var ...) e_body))
         d/e-v_middle ...
         (in-hole d/e-ctxt_1 (x_f v_arg ...))
         d/e_after ...)
        (d/e-v_before ...
         (define x_f (lambda (x_var ...) e_body))
         d/e-v_middle ...
         (in-hole d/e-ctxt_1
                  ,(multi-subst (term (x_var ...))
                                (term (v_arg ...))
                                (term e_body)))
         d/e_after ...))
   
   ;; define-style procedure application
   (--> (d/e-v_before ...
         (define (x_f x_var ...) e_body)
         d/e-v_middle ...
         (in-hole d/e-ctxt (x_f v_arg ...))
         (name after d/e) ...)
        (d/e-v_before ...
         (define (x_f x_var ...) e_body)
         d/e-v_middle ...
         (in-hole d/e-ctxt
                  ,(multi-subst (term (x_var ...))
                                (term (v_arg ...))
                                (term e_body)))
         after ...))
   
   ;; reference to non-procedure define:
   (--> (d/e-v_before ...
         (name defn (define (name a x) (name val v)))
         d/e-v_middle ...
         (in-hole (name ctxt d/e-ctxt) (name a x))
         d/e_after ...)
        (d/e-v_before ...
         defn
         d/e-v_middle ...
         (in-hole ctxt val)
         d/e_after ...))
   
   ;; unbound reference to top-level id in hole:
   (-->
    (side-condition
     (d/e-v_before ...
      (in-hole d/e-ctxt (name a x))
      d/e ...)
     (and (not (prim-op? (term a)))
          (not (defined? (term a) (term (d/e-v_before ...))))))
    ,(format "reference to undefined identifier: ~a" (term a)))
   
   ;; reference to procedure-bound var in hole:
   (--> (d/e-v_1 ...
         (define (x_f x_var ...) (name body e))
         d/e-v_2 ...
         (in-hole d/e-ctxt x_f)
         d/e ...)
        ,(format "~a is a procedure, so it must be applied to arguments" (term x_f)))
   
   ;; reference to non-procedure-bound-var in application
   (--> (d/e-v_1 ...
         (define x_a v_val)
         d/e-v_2 ...
         (in-hole d/e-ctxt (x_a v ...))
         d/e ...)
        ,(format "procedure application: expected procedure, given: ~a" (term v_val)))
   
   ((struct? ((name maker maker) v ...)) . ==> . true)
   ((struct? non-struct-value) . ==> . false)
   
   ;; struct predicate passes
   (--> (side-condition
         (d/e-v_before ...
          (define-struct x_struct (x_field ...))
          d/e-v_middle ...
          (in-hole d/e-ctxt (x_predicate (x_maker v_arg ...)))
          d/e_after ...)
         (and (maker-name-match? (term x_struct) (term x_maker))
              (predicate-name-match? (term x_struct) (term x_predicate))))
        (d/e-v_before ...
         (define-struct x_struct (x_field ...))
         d/e-v_middle ...
         (in-hole d/e-ctxt true)
         d/e_after ...))
   
   ;; struct predicate fail to another struct
   (--> (side-condition
         (d/e-v_before ...
          (define-struct x_struct (x_field ...))
          d/e-v_middle ...
          (in-hole d/e-ctxt (x_predicate (x_maker v ...)))
          (name after d/e) ...)
         (and (not (maker-name-match? (term x_struct) (term x_maker)))
              (predicate-name-match? (term x_struct) (term x_predicate))))
        (d/e-v_before ...
         (define-struct x_struct (x_field ...))
         d/e-v_middle ...
         (in-hole d/e-ctxt false)
         after ...))
   
   ;; struct predicate fail to another value
   (--> (side-condition
         (d/e-v_before ...
          (define-struct x_struct (x_field ...))
          d/e-v_middle ...
          (in-hole (name ctxt d/e-ctxt) (x_predicate non-struct-value))
          d/e_after ...)
         (predicate-name-match? (term x_struct) (term x_predicate))) 
        (d/e-v_before ...
         (define-struct x_struct (x_field ...))
         d/e-v_middle ...
         (in-hole ctxt false)
         d/e_after ...))
   
   ;; misapplied selector 1
   (--> (side-condition
         (d/e-v_before ...
          (define-struct x_struct (x_field ...))
          d/e-v_middle ...
          (in-hole d/e-ctxt (x_selector (x_maker v_arg ...)))
          d/e_after ...)
         (and (not (maker-name-match? (term x_struct) (term x_maker)))
              (selector-name-match? (term x_struct) (term (x_field ...)) (term x_selector))))
        ,(format "~a: expects argument of matching struct" (term x_selector)))
   
   ;; misapplied selector 2
   (--> (side-condition
         (d/e-v_before ...
          (define-struct x_struct (x_field ...))
          d/e-v_middle ...
          (in-hole d/e-ctxt (x_selector non-struct-value))
          d/e_after ...)
         (selector-name-match? (term x_struct) (term (x_field ...)) (term x_selector)))
        ,(format "~a: expects argument of matching struct" (term x_selector)))
   
   ;; well-applied selector
   (--> (side-condition
         (d/e-v_before ...
          (define-struct x_struct (x_field ...))
          d/e-v_middle ...
          (in-hole (name ctxt d/e-ctxt) (x_selector (x_maker v_arg ...)))
          d/e_after ...)
         (and (maker-name-match? (term x_struct) (term x_maker))
              (selector-name-match? (term x_struct) (term (x_field ...)) (term x_selector))))
        (d/e-v_before ...
         (define-struct x_struct (x_field ...))
         d/e-v_middle ...
         (in-hole ctxt
                  ,(list-ref (term (v_arg ...))
                             (struct-index (term x_struct) 
                                           (term (x_field ...))
                                           (term x_selector))))
         d/e_after ...))
   
   with
   [(--> (in-hole p-ctxt_1 a) (in-hole p-ctxt_1 b)) (==> a b)]
   [(--> (in-hole p-ctxt a) b) (e==> a b)]))

(define (defined? f befores)
  (ormap
   (lambda (before)
     (match before
       [`(define (,a-name ,x ...) ,b)
        (eq? f a-name)]
       [`(define ,a-name (lambda ,x ...))
        (eq? f a-name)]
       [`(define-struct ,struct-name (,fields ...))
        (or (ormap (lambda (field)
                     (eq? f (string->symbol (format "~a-~a" struct-name field))))
                   fields)
            (eq? f (string->symbol (format "make-~a" struct-name)))
            (eq? f (string->symbol (format "~a?" struct-name))))]
       [else #t]))
   befores))

(define (multi-subst orig-vars orig-args body)
  (let loop ([args orig-args]
             [vars orig-vars]
             [body body])
    (cond
      [(and (null? args) (null? vars))
       body]
      [(or (null? args) (null? vars))
       (error 'multi-subst 
              "malformed program, formals ~s and actuals ~s do not have the same size"
              orig-vars
              orig-args)]
      [else (loop (cdr args) 
                  (cdr vars)
                  (term-let ((x (car vars))
                             (v (car args))
                             (body body))
                    (term (beg-e-subst (x v body)))))])))

(define (selector-name-match? struct fields selector)
  (ormap (lambda (field) (string=? (format "~a-~a" struct field) 
                                   (symbol->string selector)))
         fields))

(define (struct-index struct init-fields selector)
  (let loop ([i 0]
             [fields init-fields])
    (cond
      [(null? fields) (error 'struct-index "~s ~s ~s" struct init-fields selector)]
      [else (let ([field (car fields)])
              (if (string=? (format "~a-~a" struct field) 
                            (symbol->string selector))
                  i
                  (loop (+ i 1)
                        (cdr fields))))])))

(define (maker-name-match? name maker)
  (let* ([names (symbol->string name)]
         [makers (symbol->string maker)]
         [namel (string-length names)]
         [makerl (string-length makers)])
    (and (makerl . > . namel)
         (string=? (substring makers (- makerl namel) makerl)
                   names))))

(define (predicate-name-match? name predicate)
  (eq? (string->symbol (format "~a?" name)) predicate))

(define failed-tests 0)
(define total-tests 0)

(define (test in out)
  (set! total-tests (+ total-tests 1))
  (let/ec k
    (let* ([failed
            (lambda (msg)
              (set! failed-tests (+ failed-tests 1))
              (eprintf "FAILED: ~a\n" msg)
              (k (void)))]
           [got (normalize in failed)])
      (unless (equal? got out)
        (eprintf "FAILED:   ~s\ngot:      ~s\nexpected: ~s\n" in got out)
        (set! failed-tests (+ failed-tests 1))))))

(define (test-all step . steps)
  (set! total-tests (+ total-tests 1))
  (let loop ([this step]
             [rest steps])
    (let ([nexts (apply-reduction-relation reductions this)])
      (cond
        [(null? rest)
         (unless (null? nexts)
           (set! failed-tests (+ failed-tests 1))
           (eprintf "FAILED: ~s\n  last step: ~s\n  reduced to: ~s\n"
                    step
                    this
                    nexts))]
        [else
         (cond
           [(and (pair? nexts)
                 (null? (cdr nexts)))
            (let ([next (car nexts)])
              (if (equal? next (car rest))
                  (loop (car rest)
                        (cdr rest))
                  (begin
                    (set! failed-tests (+ failed-tests 1))
                    (eprintf "FAILED: ~s\n     step: ~s\n expected: ~s\n      got: ~s\n"
                             step
                             this
                             (car rest)
                             next))))]
           [else
            (set! failed-tests (+ failed-tests 1))
            (eprintf "FAILED: ~s\n  step: ~s\n  not single step: ~s\n"
                     step
                     this
                     nexts)])]))))

(define show-dots (make-parameter #f))
(define (normalize orig-term failed)
  (let loop ([term orig-term]
             [n 1000])
    (unless (p? term)
      (failed (format "not a p: ~s orig: ~s" term orig-term)))
    (let ([nexts (apply-reduction-relation reductions term)])
      (when (show-dots)
        (display #\.)
        (flush-output))
      (cond
        [(= n 0)
         (when (show-dots)
           (newline))
         (error 'normalize "found too many reductions")]
        [(null? nexts) 
         (when (show-dots)
           (newline))
         term]
        [(string? (car nexts))
         (when (show-dots)
           (newline))
         (car nexts)]
        [(null? (cdr nexts)) (loop (car nexts) (- n 1))]
        [else 
         (when (show-dots)
           (newline))
         (failed (format "found more than one reduction\n ~s\n ->\n~s" term nexts))]))))

(define (show-test-results)
  (cond
    [(= failed-tests 0) 
     (fprintf (current-output-port) "passed all ~a tests\n" total-tests)]
    [else
     (eprintf "failed ~a out of ~a tests\n" failed-tests total-tests)]))

(define-syntax (tests stx)
  (syntax-case stx ()
    [(_ args ...)
     (syntax
      (begin
        (set! failed-tests 0)
        (set! total-tests 0)
        args ...
        (show-test-results)))]))

(define (run-tests [run-struct-test? #t])
  (tests
   (when run-struct-test?
     (test
      '((define-struct s ())
	(s? (make-s)))
      '((define-struct s ())
	true)))
   
   (test
    '((define-struct s (a b))
      (s-a (make-s 1 3)))
    '((define-struct s (a b))
      1))
   
   (test
    '((define-struct s (a b))
      (s-b (make-s 1 3)))
    '((define-struct s (a b))
      3))
   
   (test
    '((define-struct s (a b))
      (define-struct t (x y))
      (t-x (make-s 1 2)))
    "t-x: expects argument of matching struct")
   
   (test
    '((define-struct t (x y))
      (t-x 12))
    "t-x: expects argument of matching struct")
   
   (test
    '((define-struct s (a b))
      (define-struct t (x y))
      (s? (make-s 1 2)))
    '((define-struct s (a b))
      (define-struct t (x y))
      true))
   
   (test
    '((define-struct s (a b))
      (define-struct t (x y))
      (t? (make-s 1 2)))
    '((define-struct s (a b))
      (define-struct t (x y))
      false))
   
   (test
    '((define-struct s (a b))
      (struct? (make-s 1 2))
      (struct? 1))
    '((define-struct s (a b))
      true
      false))
   
   (test
    '((define (f x) x)
      (f 1))
    '((define (f x) x)
      1))
   
   (test
    '((define (f x) x)
      (define (g x) x)
      (define (h x) x)
      (g g))
    "g is a procedure, so it must be applied to arguments")
   
   (test
    '((define (f x) x)
      (define x 1)
      (define (h x) x)
      (x 2))
    "procedure application: expected procedure, given: 1")
   
   (test
    '((define (double l) (+ l l))
      (double 2))
    '((define (double l) (+ l l))
      4))
   
   (test
    '((define f (lambda (x) x))
      (f 1))
    '((define f (lambda (x) x))
      1))
   
   (test
    '((define double (lambda (l) (+ l l)))
      (double 2))
    '((define double (lambda (l) (+ l l)))
      4))
   
   (test
    '((f 1))
    "reference to undefined identifier: f")
   
   (test
    '((f 1)
      (define (f x) x))
    "reference to undefined identifier: f")
   
   (test
    '((make-s 1)
      (define-struct s (a b)))
    "reference to undefined identifier: make-s")
   
   (test
    '((+ 1 2 3))
    '(6))
   
   (test
    '((+ 1 "2" 3))
    "+: expects type <number>")
   
   (test
    '((/ 1 2 3))
    '(1/6))
   
   (test
    '((/ 1 2 0 3))
    "/: division by zero")
   
   (test
    '((/ 1 "2" 3))
    "/: expects type <number>")
   
   (test '((+ 1 (/ (+ 3 5) (+ 2 2)))) '(3))
   
   (test '((symbol=? 'x 'x)) '(true))
   (test '((symbol=? 'x 'y)) '(false))
   (test '((symbol=? 1 'x)) 
         "symbol=?: expects argument of type <symbol>")
   (test '((symbol=? 'x 1)) 
         "symbol=?: expects argument of type <symbol>")
   
   (test '((cons 1 empty)) '((cons 1 empty)))
   (test '((cons 1 2))
         "cons: second argument must be of type <list>")
   (test '((+ (first (cons 1 2)) 2))
         "cons: second argument must be of type <list>")
   (test '((+ (first (cons 1 empty)) 2))
         '(3))
   
   (test
    '((first (cons 1 empty)))
    '(1))
   
   (test
    '((first 1))
    "first: expects argument of type <pair>")
   
   (test
    '((first 1 2))
    "first: expects one argument")
   
   (test
    '((first))
    "first: expects one argument")
   
   (test
    '((rest (cons 1 empty)))
    '(empty))
   
   (test
    '((rest 1))
    "rest: expects argument of type <pair>")
   
   (test
    '((rest 1 2))
    "rest: expects one argument")
   
   (test
    '((rest))
    "rest: expects one argument")
   
   (test
    '((empty? empty))
    '(true))
   
   (test
    '((empty? 1))
    '(false))
   
   (test
    '((empty?))
    "empty?: expects one argument")
   
   (test
    '((empty? 1 2))
    "empty?: expects one argument")
   
   (test
    '((cond [true 1]))
    '(1))
   
   (test
    '((cond [else 1]))
    '(1))
   
   (test-all
    '((cond [false 1] [else 2]))
    '(2))
   
   (test-all
    '((cond [false 1] [false 2]))
    "cond: all question results were false")
   
   (test
    '((cond [1 1]))
    "cond: question result is not true or false")
   
   (test
    '((cond [(empty? empty) 'infinite] [else 3]))
    '('infinite))
   
   (test-all
    '((cond [(if false false false) 'x] [(if true true true) 'y] [(if false false false) 'z]))
    '((cond [false 'x] [(if true true true) 'y] [(if false false false) 'z]))
    '((cond [false 'x] [true 'y] [(if false false false) 'z]))
    '('y))
   
   (test-all
    '((cond [(if false false false) 'x] [(if true true true) 'y] [else 'z]))
    '((cond [false 'x] [(if true true true) 'y] [else 'z]))
    '((cond [false 'x] [true 'y] [else 'z]))
    '('y))
   
   (test-all
    '((cond [(if false false false) 'x] [(if false false false) 'y] [else 'z]))
    '((cond [false 'x] [(if false false false) 'y] [else 'z]))
    '((cond [false 'x] [false 'y] [else 'z]))
    '('z))
   
   (test-all
    '((and true true 3))
    "and: question result is not true or false")
   
   (test-all
    '((and 1 true true))
    "and: question result is not true or false")
   
   (test-all
    '((and true true true false))
    '(false))
   
   (test-all
    '((and false true))
    '(false))
   
   (test-all
    '((or false false 3))
    "or: question result is not true or false")
   
   (test-all
    '((or 1 false false))
    "or: question result is not true or false")
   
   (test-all
    '((or false false false true))
    '(true))
   
   (test-all
    '((or true false))
    '(true))
   
   (test-all
    '((or (if false false false) (if false false false) (if true true true) (if false false false)))
    '((or false (if false false false) (if true true true) (if false false false)))
    '((or false false (if true true true) (if false false false)))
    '((or false false true (if false false false)))
    '(true))
   
   (test-all
    '((and (if true true true) (if true true true) (if false false false) (if true true true)))
    '((and true (if true true true) (if false false false) (if true true true)))
    '((and true true (if false false false) (if true true true)))
    '((and true true false (if true true true)))
    '(false))
   
   (test
    '((if 1 2 3))
    "if: question result is not true or false")
   
   (test
    '((if true 'x 'y))
    '('x))
   
   (test
    '((if false 'x 'y))
    '('y))
   
   ; test non-procedure-defs in context:
   (test
    `((+ 3 4) (define a 3) (+ 5 6))
    `(7 (define a 3) 11))
   
   ; test reduction of non-procedure-defs:
   (test 
    `((define a 13) (define b (+ a 9)) (+ 3 4))
    `((define a 13) (define b 22) 7))
   
   ; test reduction of unbound ids in hole:
   (test 
    `(x)
    "reference to undefined identifier: x")
   
   ; test reduction of function-bound id in hole:
   (test
    `((define (a x) (+ x 1)) a)
    "a is a procedure, so it must be applied to arguments")
   
   ; test reduction of non-procedure-def in application:
   (test
    `((define a 3) (a 9))
    "procedure application: expected procedure, given: 3")))

(define (run-big-test)
  (parameterize ([show-dots #t])
    (tests
     (test
      '((define-struct pr (hd tl))
        (define (avg l)
          (cond
            [(empty? l) 'infinite]
            [else (/ (sum l) (howmany/acc l 0))]))
        (define (sum l)
          (cond
            [(empty? (pr-tl l)) (pr-hd l)]
            [else (+ (pr-hd l) (sum (pr-tl l)))]))
        (define (howmany/acc l acc)
          (cond
            [(empty? l) acc]
            [else (howmany/acc (pr-tl l) (+ acc 1))]))
        (avg empty)
        (avg (make-pr 3 (make-pr 4 (make-pr 5 (make-pr 6 (make-pr 7 (make-pr 8 (make-pr 9 empty)))))))))
      
      '((define-struct pr (hd tl))
        (define (avg l)
          (cond
            [(empty? l) 'infinite]
            [else (/ (sum l) (howmany/acc l 0))]))
        (define (sum l)
          (cond
            [(empty? (pr-tl l)) (pr-hd l)]
            [else (+ (pr-hd l) (sum (pr-tl l)))]))
        (define (howmany/acc l acc)
          (cond
            [(empty? l) acc]
            [else (howmany/acc (pr-tl l) (+ acc 1))]))
        'infinite
        6))
     (test
      '((define (contains-sym? s l)
          (cond
            [(empty? l) false]
            [true (or (symbol=? s (first l))
                      (contains-sym? s (rest l)))]))
        (contains-sym? 'x (cons 'z (cons 'y (cons 'x empty))))
        (contains-sym? 'a (cons 'p (cons 'q (cons 'p (cons 'q (cons 'p (cons 'q empty))))))))
      '((define (contains-sym? s l)
          (cond
            [(empty? l) false]
            [true (or (symbol=? s (first l))
                      (contains-sym? s (rest l)))]))
        true
        false)))))

(define (main)
  (run-tests)
  (run-big-test))

(module+ main (main))

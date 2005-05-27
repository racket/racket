(module subst-test mzscheme
  (require "../subst.ss"
           (lib "match.ss"))
  
  (define (lc-subst1 var val exp) (subst/proc var val exp lc-separate))
  (define (lc-free-vars exp) (free-vars/memoize (make-hash-table) exp lc-separate))
  (define (lc-rename old-name new-name exp) (alpha-rename old-name new-name exp lc-separate))
  
  (define lc-subst2
    (subst
     [`(lambda ,vars ,body)
      (all-vars vars)
      (build (lambda (vars body) `(lambda ,vars ,body)))
      (subterm vars body)]
     [`(let (,l-var ,exp) ,body)
      (all-vars (list l-var))
      (build (lambda (l-vars exp body) `(let (,@l-vars ,exp) ,body)))
      (subterm '() exp)
      (subterm (list l-var) body)]
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(,fun ,@(args ...))
      (all-vars '())
      (build (lambda (vars fun . args) `(,fun ,@args)))
      (subterm '() fun)
      (subterms '() args)]))
  
  (define (lc-separate exp constant variable combine sub-piece)
    (match exp
      [`(lambda ,vars ,body)
       (combine (lambda (vars body) `(lambda ,vars ,body))
                vars
                (sub-piece vars body))]
      [`(let (,l-var ,exp) ,body)
       (combine (lambda (l-vars exp body) `(let (,(car l-vars) ,exp) ,body))
                (list l-var)
                (sub-piece '() exp)
                (sub-piece (list l-var) body))]
      [(? symbol?) (variable (lambda (x) x) exp)]
      [(? number?) (constant exp)]
      [`(,fun ,@(args ...))
       (apply 
        combine
        (lambda (variables fun . args) `(,fun ,@args))
        '()
        (append
         (list (sub-piece '() fun))
         (map (lambda (x) (sub-piece '() x)) args)))]))
  
  (define-syntax (test stx)
    (syntax-case stx  ()
      [(_ test-exp expected)
       (syntax (test test-exp expected equal?))]
      [(_ test-exp expected same?)
       (syntax
        (let ([actual test-exp]
              [expected-v expected])
          ;(printf "testing: ~s\n" (syntax-object->datum #'test-exp))
          (unless (same? actual expected-v)
            (printf "     test: ~s\n expected: ~s\n      got: ~s\n" 
                    (syntax-object->datum #'test-exp)
                    expected-v
                    actual))))]))
  
  (define (set-equal? xs ys)
    (and (andmap (lambda (x) (memq x ys)) xs)
         (andmap (lambda (y) (memq y xs)) ys)))
  
  (define (lc-tests) 
    (tests lc-free-vars lc-subst1 lc-rename)
    (tests #f lc-subst2 #f))
  
  (define (tests free-vars subst rename)
    (when free-vars
      (test (free-vars 'x) '(x) set-equal?)
      (test (free-vars '(lambda (x) x)) '() set-equal?)
      (test (free-vars '(lambda (x) y)) '(y) set-equal?)
      (test (free-vars '(let (x 1) x)) '() set-equal?)
      (test (free-vars '(let (x 1) y)) '(y) set-equal?)
      (test (free-vars '(let (x x) y)) '(y x) set-equal?)
      (test (free-vars '(let (x 1) (y y))) '(y) set-equal?)
      (test (free-vars '(lambda (y) (y y))) '() set-equal?))
    
    (when rename
      (test (rename 'x 'y 'x) 'x)
      (test (rename 'x 'y '(lambda (x) x)) '(lambda (y) y)))
    
    (test (subst 'x 1 'x) 1)
    (test (subst 'x 1 'y) 'y)
    (test (subst 'x 1 '(lambda (x) x)) '(lambda (x) x))
    (test (subst 'x 1 '(lambda (y) x)) '(lambda (y) 1))
    (test (subst 'x 'y '(lambda (y) x)) '(lambda (y@) y))
    (test (subst 'x 'y '(lambda (y) (x y))) '(lambda (y@) (y y@)))
    (test (subst 'x 'y '(let (x 1) 1)) '(let (x 1) 1))
    (test (subst 'x 'y '(let (x 1) x)) '(let (x 1) x))
    (test (subst 'x 'y '(let (x 1) y)) '(let (x 1) y))
    (test (subst 'x 'y '(let (y 1) (x y))) '(let (y@ 1) (y y@)))
    (test (subst 'q '(lambda (x) y) '(lambda (y) y)) '(lambda (y) y))
    (test (subst 'q '(lambda (x) y) '(let ([y q]) y)) '(let ([y (lambda (x) y)]) y))
    (test (subst 'p '1 '(let (t 2) ((p t) t)))
          '(let (t 2) ((1 t) t)))
    (test (subst 'p '(lambda (s) s) 
                 '(let (t (lambda (s) s)) ((p t) t)))
          '(let (t (lambda (s) s)) (((lambda (s) s) t) t)))
    (test (subst 'p
                 '(lambda (s) (s s))
                 '(let (t (lambda (s) s))
                    p))
          '(let (t (lambda (s) s))
             (lambda (s) (s s))))
    
    (test (subst 's 
                 '(lambda (z) (s z))
                 '(lambda (s) (lambda (z) (s z))))
          '(lambda (s) (lambda (z) (s z))))
    
    (test (subst 's
                 '(lambda (s) (lambda (z) (s z)))
                 '(lambda (z) (s z)))
          '(lambda (z) ((lambda (s) (lambda (z) (s z))) z)))))


(require syntax/toplevel)

(load-relative "loadtest.rktl")

;; test that expansion preserves source location information
;; for fully expanded terms

(Section 'expand)

(let () 
  (define (compare-expansion stx) 
    (let ([expanded (expand stx)])
      (ensure-good-test-case stx expanded)
      (test #t compare-objs stx expanded)))
  
  ;; o1 is the original, and o2 is the expansion result
  (define (compare-objs o1 o2)
    (let loop ([o1 o1]
               [o2 o2]
	       [extra-ok? #f])
      (cond
        [(both? syntax? o1 o2)
         (and (sel-same syntax-position o1 o2)
              (sel-same syntax-source o1 o2)
              (sel-same syntax-column o1 o2)
              (sel-same syntax-line o1 o2)
              (loop (syntax-e o1) (syntax-e o2) #f))]
        [(both? pair? o1 o2)
         (and (loop (car o1) (car o2) #f)
              (loop (cdr o1) (cdr o2) #t))]
	[(both? vector? o1 o2)
         (and (sel-same vector-length o1 o2)
              (andmap (lambda (a b) (loop a b #f))
                      (vector->list o1)
                      (vector->list o2)))]
        [(both? null? o1 o2) #t]
        [(both? symbol? o1 o2)
         (eq? o1 o2)]
        [(both? boolean? o1 o2)
         (equal? o1 o2)]
	[(both? number? o1 o2)
         (equal? o1 o2)]
	[(both? string? o1 o2)
         (equal? o1 o2)]
	;; It's ok for `expand' to make more things syntax objects
        [(and extra-ok? (pair? o1) (syntax? o2) (pair? (syntax-e o2)))
	 (and (loop (car o1) (car (syntax-e o2)) #f)
	      (loop (cdr o1) (cdr (syntax-e o2)) #t))]
	[(and extra-ok? (null? o1) (syntax? o2) (null? (syntax-e o2)))
	 #t]
        [else #f])))
  
  ;; this error indicates that the expansion wasn't
  ;; really idempotent, on the structure. Assume that
  ;; the test case is broken, not expand.
  (define (ensure-good-test-case o1 o2)
    (let ([d1 (syntax->datum o1)]
          [d2 (syntax->datum o2)])
      (unless (equal? d1 d2)
        (error 'compare-objs "bad test case: ~e ~e" d1 d2))))
  
  (define (sel-same sel o1 o2) (equal? (sel o1) (sel o2)))
  (define (both? p? o1 o2) (and (p? o1) (p? o2)))
  
  (compare-expansion #''())
  (compare-expansion #'(quote 1))
  (compare-expansion #'(#%top . x))
  (compare-expansion #'(if (#%top . a) (#%top . b) (#%top . c)))
  (compare-expansion #'(#%plain-lambda () (#%top . x)))
  (compare-expansion #'(#%plain-lambda (x) x))
  (compare-expansion #'(#%plain-lambda (x y z) x))
  (compare-expansion #'(#%plain-lambda (x) x x x))
  (compare-expansion #'(case-lambda))
  (compare-expansion #'(case-lambda [() (quote 1)]))
  (compare-expansion #'(case-lambda [() (quote 1)] [(x) x]))
  (compare-expansion #'(case-lambda [(x y) x]))
  (compare-expansion #'(define-values () (#%top . x)))
  (compare-expansion #'(define-values (x) (#%top . x)))
  (compare-expansion #'(define-values (x y z) (#%top . x)))
  (compare-expansion #'(define-syntaxes () (#%top . x)))
  (compare-expansion #'(define-syntaxes (s) (#%top . x)))
  (compare-expansion #'(define-syntaxes (s x y) (#%top . x)))
  (compare-expansion #'(#%require mzscheme))
  (compare-expansion #'(#%require (lib "list.ss")))
  (compare-expansion #'(#%require (lib "list.ss") mzscheme))
  (compare-expansion #'(#%require (for-syntax mzscheme)))
  (compare-expansion #'(#%require (for-syntax (lib "list.ss"))))
  (compare-expansion #'(#%require (for-syntax (lib "list.ss") mzscheme)))
  (compare-expansion #'(begin))
  (compare-expansion #'(begin (#%top . x)))
  (compare-expansion #'(begin (#%top . x) (quote 2)))
  (compare-expansion #'(begin0 (#%top . x)))
  (compare-expansion #'(begin0 (#%top . x) (quote 2)))
  (compare-expansion #'(begin0 (#%top . x) (quote 2) (quote 2)))
  (compare-expansion #'(let-values () (#%top . q)))
  (compare-expansion #'(let-values (((x y) (#%top . p))) (#%top . q)))
  (compare-expansion #'(let-values (((x y) (#%top . p)) ((z) (quote 12))) (#%top . q)))
  (compare-expansion #'(let-values (((x y) (#%top . p))) (#%top . q) (#%top . p)))
  (compare-expansion #'(letrec-values () (#%top . q)))
  (compare-expansion #'(letrec-values (((x y) (#%top . p))) (#%top . q)))
  (compare-expansion #'(letrec-values (((x y) (#%top . p)) ((z) (quote 12))) (#%top . q)))
  (compare-expansion #'(letrec-values (((x y) (#%top . p))) (#%top . q) (#%top . p)))
  (compare-expansion #'(set! x (#%top . y)))
  (compare-expansion #'(quote-syntax x))
  (compare-expansion #'(with-continuation-mark (#%top . x) (#%top . x) (#%top . x)))
  (compare-expansion #'(#%plain-app (#%top . f)))
  (compare-expansion #'(#%plain-app (#%top . f) (quote 1))))

(define expand-test-use-toplevel? #f)

(define datum->top-level-syntax
  (lambda (v)
    (namespace-syntax-introduce (datum->syntax #f v))))

(define now-expanding (make-parameter #f))

; Tests macro expansion by setting the eval handler and
;  running all tests

(namespace-variable-value
 'expand-load
 #f
 (lambda ()
   (namespace-set-variable-value! 'expand-load "mz-tests.rktl")))

(let ([orig (current-eval)])
  (dynamic-wind
   (lambda ()
     (current-eval
      (lambda (x)
	(if (now-expanding)
	    (orig x)
	    (begin
	      (set! mz-test-syntax-errors-allowed? #t)
	      (let ([x (if (or (compiled-expression? x)
			       (and (syntax? x) (compiled-expression? (syntax-e x))))
			   x
			   (parameterize ([current-module-declare-name #f]
					  [now-expanding expand-test-use-toplevel?])
			     (expand-syntax
			      ((if expand-test-use-toplevel?
				   expand-top-level-with-compile-time-evals
				   expand-syntax)
			       ((if (syntax? x) values datum->top-level-syntax) x)))))])
		(set! mz-test-syntax-errors-allowed? #f)
		(orig x)))))))
   (lambda ()
     (load-relative expand-load))
   (lambda ()
     (current-eval orig))))

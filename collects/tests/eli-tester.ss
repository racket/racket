#lang scheme/base

(require scheme/match scheme/list scheme/string
         (for-syntax scheme/base scheme/match))

(define-syntax (safe stx)
  (syntax-case stx ()
    [(_ expr)
     ;; catch syntax errors while expanding, turn them into runtime errors
     (with-handlers ([exn? (lambda (e) #`(list 'error #,(exn-message e)))])
       (define-values (_ opaque)
         (syntax-local-expand-expression
          #'(with-handlers ([exn? (lambda (e) (list 'error (exn-message e)))])
              (cons 'values (call-with-values (lambda () expr) list)))))
       opaque)]))

(define show
  (match-lambda [(list 'error msg) (format "error: ~a" msg)]
                [(list 'values x)  (format "~e" x)]
                [(list 'values xs ...) (format "~e" (cons 'values xs))]))

(define test-context (make-parameter #f))

(define-syntax (test-thunk stx)
  (define (blame e fmt . args)
    (define loc
      (string->symbol
       (format "~a:~a" (or (syntax-source e) "(unknown)")
               (let ([l (syntax-line e)] [c (syntax-column e)])
                 (cond [(and l c) (format "~a:~a" l c)]
                       [l l]
                       [(syntax-position e) => (lambda (p) (format "#~a" p))]
                       [else "?"])))))
    (with-syntax ([e e] [fmt fmt] [(arg ...) args] [loc loc])
      #'(error 'loc "test failure in ~e\n  ~a" 'e (format fmt arg ...))))
  (define (t1 x)
    #`(let ([x (safe #,x)])
        (unless (and (eq? 'values (car x)) (= 2 (length x)) (cadr x))
          #,(blame x "expected non-#f single value; got: ~a" #'(show x)))))
  (define (t2 x y)
    #`(let ([x (safe #,x)] [y (safe #,y)])
        (cond [(and (eq? 'error (car y)) (eq? 'values (car x)))
               #,(blame x "expected an error; got ~a" #'(show x))]
              [(and (eq? 'error (car x)) (eq? 'error (car y)))
               (unless (regexp-match (regexp-quote (cadr y)) (cadr x))
                 #,(blame x "bad error message, expected ~s; got ~s"
                          #'(cadr y) #'(cadr x)))]
              [(not (equal? x y))
               #,(blame x "expected ~a; got: ~a" #'(show y) #'(show x))])))
  (define (te x y) (t2 x #`(error #,y)))
  (define (try t . args)
    #`(let ([c (test-context)])
        (with-handlers ([exn? (lambda (e) (set-mcdr! c (cons e (mcdr c))))])
          (set-mcar! c (add1 (mcar c)))
          #,(apply t args))))
  (define (tb x) x)
  (let loop ([xs (map (lambda (x)
                        (if (memq (syntax-e x) '(do => <= =error> <error=))
                          (syntax-e x) x))
                      (cdr (syntax->list stx)))]
             [r '()])
    (let ([t (match xs
               [(list* 'do x r)        (cons (tb x) r)]
               [(list* x '=> y r)      (cons (try t2 x y) r)]
               [(list* y '<= x r)      (cons (try t2 x y) r)]
               [(list* x '=error> y r) (cons (try te x y) r)]
               [(list* y '<error= x r) (cons (try te x y) r)]
               [(list* x r)
                ;; if x = (test ...), then it's implicitly in a `do'
                ;; (not really needed, but avoids an extra count of tests)
                (syntax-case x (test)
                  [(test x0 x1 ...) (cons (tb x) r)]
                  [_ (cons (try t1 x) r)])]
               [(list) '()])])
      (if (pair? t)
        (loop (cdr t) (cons (car t) r))
        #`(lambda () #,@(reverse r))))))

(define (run-tests thunk force-new-context?)
  (if (and (test-context) (not force-new-context?))
    (thunk)
    (let ([c (mcons 0 '())])
      (parameterize ([test-context c])
        (dynamic-wind
          void
          thunk
          (lambda ()
            (test-context #f)
            (let ([num (mcar c)] [exns (mcdr c)])
              (if (null? exns)
                (printf "~a tests passed\n" num)
                (error 'test "~a/~a test failures:~a" (length exns) num
                       (string-append*
                        (append-map (lambda (e) (list "\n" (exn-message e)))
                                    (reverse exns))))))))))))

(provide test test*)
(define-syntax-rule (test  x0 x ...) (run-tests (test-thunk x0 x ...) #f))
(define-syntax-rule (test* x0 x ...) (run-tests (test-thunk x0 x ...) #t))

#; ;; test the `test' macro

(test
 ;; test usage
 1 => 1
 #t
 (< 1 2)
 (+ 1 2) => 3
 (+ 1 2) <= 3
 ;; multiple values
 (values 1) => 1
 (values 1) <= 1
 (quotient/remainder 10 3) => (values 3 1)
 ;; runtime errors
 (car '()) =error> "expects argument of type"
 (car '()) => (error "expects argument of type")
 ;; syntax errors
 (if 1) =error> "if: bad syntax"

 ;; test `test' errors
 (test* (/ 0)) =error> "expected non-#f single value"
 (test* 1 => 2) =error> "expected 2"
 (test* 1 =error> "") =error> "expected an error"
 (test* (/ 0) =error> "zzz") =error> "bad error message"
 )

;; SchemeUnit stuff
;; (examples that should fail modified to ones that shouldn't)
#|

;; Quick Start example
(define (file-tests)
  ;; Tests for file.scm
  ;; (=> source location is sufficient, no need for test names in the code)
  (test
   (+ 1 1) => 2
   (* 1 2) => 2
   ;; List has length 4 and all elements even
   do (let ([lst (list 2 4 6 8)])
        (test (length lst) => 4
              do (for ([x lst]) (test (even? x)))))))
(file-tests)

;; API listing
(test
 ;; (check < 2 3)
 (< 2 3)
 ;; (check-eq? 1 1 "allocated data not eq?")
 (eq? 1 1)
 ;; (check-not-eq? (list 1) (list 1) "integers are eq?")
 (not (eq? (list 1) (list 1)))
 ;; (check-eqv? 1.0 1.0 "not eqv?")
 (eqv? 1.0 1.0)
 ;; (check-equal? 1.0 1.0 "not equal?")
 (equal? 1.0 1.0)
 1.0 => 1.0 ; alternative
 ;; (check-not-equal? 1 1.0 "equal?")
 (not (equal? 1 1.0))
 ;; (check-pred string? "I work")
 (string? "I work")
 ;; (check-= 1.0 1.001 0.01 "I work")
 (< (abs (- 1.0 1.001)) 0.01)
 ;; (check-true (< 1 2))
 (eq? #t (< 1 2))
 ;; (check-false (< 2 1))
 (not (< 2 1))
 ;; (check-not-false (< 1 2))
 (< 1 2)
 ;; (check-exn exn?
 ;;            (lambda ()
 ;;              (raise (make-exn "Hi there"
 ;;                               (current-continuation-marks)))))
 (raise (make-exn "Hi there" (current-continuation-marks)))
 =error> ""
 ;; (check-not-exn (lambda () 1))
 (void 1)
 ;; (fail)
 ;; (error "foo") -> no real equivalent, since `fail' doesn't throw an error
 ;; (check-regexp-match "a+bba" "aaaaaabba")
 (regexp-match "a+bba" "aaaaaabba")
 )
|#

#lang racket/base

(require scheme/match scheme/list scheme/string rackunit/log
         (for-syntax scheme/base scheme/match))

(define-syntax (safe stx)
  (syntax-case stx ()
    [(_ expr)
     ;; catch syntax errors while expanding, turn them into runtime errors
     (with-handlers ([exn:fail:syntax? (lambda (e) #`(list 'error #,(exn-message e) #f))])
       (define-values (_ opaque)
         (syntax-local-expand-expression
          #'(with-handlers
                ([(lambda (_) #t)
                  (lambda (e) (list 'error (and (exn? e) (exn-message e)) e))])
              (cons 'values (call-with-values (lambda () expr) list)))))
       opaque)]))

(define show
  (match-lambda
    [(list 'values x)
     (format "~e" x)]
    [(list 'values xs ...)
     (string-append "(values "
                    (string-join (map (lambda (x) (format "~e" x)) xs))
                    ")")]
    [(list 'error err val)
     (cond [(procedure? err) (format "error satisfying ~.s" err)]
           [(regexp? err)    (format "error matching ~.s" err)]
           [err              (format "error: ~.a" err)]
           [else             (format "a raised non-exception ~.s" val)])]
    [x (format "INTERNAL ERROR, unexpected value: ~.s" x)]))

(define test-context (make-parameter #f))
(define failure-format
  (make-parameter
   (lambda (prefix qe fmt . args)
     (define str
       (regexp-replace #rx"\n" (apply format fmt args) "\n  "))
     (string-join (reverse (cons (format "test failure in ~.s\n  ~a" qe str)
                                 prefix))
                  " > "))))

(define (make-failure-message msg)
  (define str (regexp-replace #rx"\n" msg "\n  "))
  (define real-msg (format "test failure\n  ~a" str))
  (lambda (prefix qe fmt . args) real-msg))
(define failure-prefix-mark (gensym 'failure-prefix))

(define (make-location src line col pos)
  (string->symbol
   (format "~a:~a" (or src "(unknown)")
           (let ([l line] [c col])
             (cond [(and l c) (format "~a:~a" l c)]
                   [l l]
                   [pos => (lambda (p) (format "#~a" p))]
                   [else "?"])))))
    
(define-syntax (test-thunk stx)
  (define (blame e fmt . args)
    (with-syntax ([e e] [fmt fmt] [(arg ...) args] 
                  [src (syntax-source e)]
                  [line (syntax-line e)]
                  [col (syntax-column e)]
                  [pos (syntax-position e)])
      #'(let* ([form (failure-format)]
               [prefix (continuation-mark-set->list (current-continuation-marks)
                                                    failure-prefix-mark)])
          (error (make-location 'src 'line 'col 'pos) "~a" (form prefix 'e fmt arg ...)))))
  (define (test-1 x)
    #`(let ([x (safe #,x)])
        (unless (and (eq? 'values (car x)) (= 2 (length x)) (cadr x))
          #,(blame x "expected: non-#f single value\n     got: ~a"
                   #'(show x)))))
  (define (test-2 x y [eval2? #t])
    #`(let* ([x (safe #,x)]                   [xtag (car x)]
             [y #,(if eval2? #`(safe #,y) y)] [ytag (car y)])
        (cond
          [(eq? ytag 'values)
           (unless (equal? x y)
             #,(blame x "expected: ~a\n     got: ~a" #'(show y) #'(show x)))]
          [(eq? xtag 'values)
           #,(blame x "expected: an error\n     got: ~a" #'(show x))]
          ;; both are errors (or other raised values)
          [(not (cadr x)) ; expecting a non-exception raise
           (unless (or (equal? x y)
                       (and (procedure? (cadr y)) ((cadr y) (caddr x))))
             #,(blame x "expected ~a\n     got: ~a" #'(show y) #'(show x)))]
          [else
           (let ([xerr (cadr x)] [xval (caddr x)] [yerr (cadr y)])
             (cond [(string? yerr)
                    (unless (regexp-match? (regexp-quote yerr) xerr)
                      #,(blame x "bad error message, expected: ~s\ngot: ~s"
                               #'yerr #'xerr))]
                   [(regexp? yerr)
                    (unless (regexp-match? yerr xerr)
                      #,(blame x "bad error message, expected ~a: ~s\ngot: ~s"
                               "a match for" #'yerr #'xerr))]
                   [(and (procedure? yerr) (procedure-arity-includes? yerr 1))
                    (unless (yerr xval)
                      #,(blame x "bad error message, expected ~a: ~s\ngot: ~s"
                               "an exception satisfying" #'yerr #'xerr))]
                   [else (error 'test "bad error specification: ~e" yerr)]))])))
  (define (test-error x y) (test-2 x #`(list 'error #,y #f) #f))
  (define (try t . args)
    #`(let ([c (test-context)])
        (with-handlers ([exn? (lambda (e) (set-mcdr! c (cons e (mcdr c))))])
          (set-mcar! c (add1 (mcar c)))
          #,(apply t args))))
  (define (test-0 x) x)
  (let loop ([xs (map (lambda (x)
                        (let ([e (syntax-e x)])
                          (if (or (memq e '(do => <= =error> <error=))
                                  (keyword? e))
                              e x)))
                      (cdr (syntax->list stx)))]
             [r '()])
    (let ([t (let tloop ([xs xs])
               (match xs
                 [(list* #:failure-prefix msg r)
                  (let ([r (tloop r)])
                    (if (pair? r)
                      (cons #`(with-continuation-mark failure-prefix-mark
                                                      #,msg #,(car r))
                            (cdr r))
                      r))]
                 [(list* #:failure-message msg r)
                  (let ([r (tloop r)])
                    (if (pair? r)
                        (cons
                         #`(parameterize ([failure-format
                                           (make-failure-message #,msg)])
                             #,(car r))
                         (cdr r))
                        r))]
                 [(list* 'do x r) ; to avoid counting non-test exprs as tests
                  (cons (test-0 x) r)]
                 [(list* x '=> y r)      (cons (try test-2 x y) r)]
                 [(list* y '<= x r)      (cons (try test-2 x y) r)]
                 [(list* x '=error> y r) (cons (try test-error x y) r)]
                 [(list* y '<error= x r) (cons (try test-error x y) r)]
                 [(list* x r)
                  ;; if x = (test ...), then it's implicitly in a `do'
                  ;; (not really needed, but avoids an extra count of tests)
                  (syntax-case x (test)
                    [(test x0 x1 ...) (cons (test-0 x) r)]
                    [_ (cons (try test-1 x) r)])]
                 [(list) '()]))])
      (if (pair? t)
          (loop (cdr t) (cons (car t) r))
          #`(lambda () #,@(reverse r))))))

;; pass 'quiet to have nothing printed on success
(define (run-tests thunk force-new-context? #:on-pass [pass 'loud])
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
               (for ([i (in-range num)]) (test-log! #t))
               (for ([i (in-list exns)]) (test-log! #f))
               (if (null? exns)
                   (case pass
                     [(loud) (printf "~a test~a passed\n" num (if (= num 1) "" "s"))]
                     [(quiet) (void)])
                   (error 'test "~a/~a test failures:~a" (length exns) num
                          (string-append*
                           (append-map (lambda (e) (list "\n" (exn-message e)))
                                       (reverse exns))))))))))))

(define-syntax-rule (define-test name force-context)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(_ #:on-pass pass x0 x (... ...))
       #'(run-tests #:on-pass pass (test-thunk x0 x (... ...)) force-context)]
      [(_ x0 x (... ...))
       #'(run-tests (test-thunk x0 x (... ...)) force-context)])))

(provide test test*)
(define-test test  #f)
(define-test test* #t)

#; ; test the `test' macro

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

 ;; error (and non-exception raises) predicates
 (+ 1 "2")   =error> exn:fail:contract?
 (+ 1 "2")   =error> (lambda (x) (not (exn:fail:filesystem? x)))
 (+ 1 "2")   =error> #rx"expects.*<number>"
 (error "1") =error> exn?
 (raise 1)   =error> number?
 (raise "1") =error> string?

 ;; test `test' errors
 (test* (/ 0)) =error> "expected: non-#f single value"
 (test* 1 => 2) =error> "expected: 2"
 (test* 1 =error> "") =error> "expected: an error"
 (test* (/ 0) =error> "zzz") =error> "bad error message"
 (test* (raise 1) =error> "foo") =error> "raised non-exception"
 (test* #:failure-message "FOO" (/ 0) => 1) =error> "FOO"
 (test* #:failure-message "FOO" (/ 0)) =error> "FOO"
 (test* #:failure-prefix "FOO" (/ 0)) =error> "FOO"

 ;; test possitive message
 (let ([o (open-output-bytes)])
   (list (begin (parameterize ([current-output-port o]) (test* 1 => 1))
                (get-output-bytes o #t))
         (begin (parameterize ([current-output-port o]) (test* 1 => 1 (odd? 1)))
                (get-output-bytes o #t))))
 => '(#"1 test passed\n" #"2 tests passed\n")
 )

;; RackUnit stuff
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

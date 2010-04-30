#lang scheme/base

(provide go go/text)

(require rktunit rktunit/text-ui
         mzlib/etc scheme/port
         compiler/compiler
         scheme/match
         "unit-tests/all-tests.ss"
         "unit-tests/test-utils.ss")

(define (scheme-file? s)
  (regexp-match ".*[.](ss|scm)" (path->string s)))

(define-namespace-anchor a)

(define (exn-matches . args)
  (values
   (lambda (val)
     (and (exn? val)
          (for/and ([e args])
                   (cond [(procedure? e) (e val)]
                         [(number? e)
                          (and (exn:fail:syntax? val)
                               (= e (length (exn:fail:syntax-exprs val))))]
                         [else                          
                          (regexp-match e (exn-message val))]))))
   args))
  
(define (exn-pred p)
  (let ([sexp (with-handlers
                  ([exn:fail? (lambda _ #f)])
                (call-with-input-file*
                 p
                 (lambda (prt) 
                   (read-line prt 'any) (read prt))))])
    (match sexp
      [(list-rest 'exn-pred e)
       (eval `(exn-matches . ,e) (namespace-anchor->namespace a))]
      [_ 
       (exn-matches ".*Type Checker.*" exn:fail:syntax?)])))

(define (mk-tests dir loader test)
  (lambda ()
    (define path (build-path (this-expression-source-directory) dir))  
    (define tests
      (for/list ([p (directory-list path)]
                 #:when (scheme-file? p)
		 ;; skip backup files
		 #:when (not (regexp-match #rx".*~" (path->string p))))
        (test-suite
         (path->string p)
         (test
          (build-path path p)
          (lambda ()
            (parameterize ([read-accept-reader #t]
                           [current-load-relative-directory path]
                           [current-directory path]
                           [current-output-port (open-output-nowhere)])
              (loader p)))))))
    (make-test-suite dir tests)))

(define (dr p)
  #;((compile-zos #f) (list p) 'auto)
  (parameterize ([current-namespace (make-base-empty-namespace)])    
    (dynamic-require `(file ,(path->string p)) #f)))

(define succ-tests (mk-tests "succeed" 
                             dr
                             (lambda (p thnk) (check-not-exn thnk))))
(define fail-tests (mk-tests "fail"
                             dr
                             (lambda (p thnk)
                               (define-values (pred info) (exn-pred p))
                               (parameterize ([error-display-handler void])
                                 (with-check-info
                                  (['predicates info])
                                  (check-exn pred thnk))))))

(define int-tests
  (test-suite "Integration tests"
              (succ-tests)
              (fail-tests)))

(define tests
  (test-suite "Typed Scheme Tests"
              unit-tests int-tests))

(define (go) (test/gui tests))
(define (go/text) (run-tests tests 'verbose))

(provide go go/text)




#lang scheme/base

(provide go go/text)

(require rackunit rackunit/text-ui racket/file
         mzlib/etc scheme/port
         compiler/compiler
         scheme/match mzlib/compile
         "unit-tests/all-tests.ss"
         "unit-tests/test-utils.ss")

(define (scheme-file? s)
  (regexp-match ".*[.](rkt|ss|scm)$" (path->string s)))

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
                         [(or (string? e) (regexp? e))
                          (regexp-match e (exn-message val))]
                         [else (error 'exn-pred "bad argument" e)]))))
   args))

(define (cfile file)
  ((compile-zos #f) (list file) 'auto))
  
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
  (parameterize ([current-namespace (make-base-empty-namespace)])    
    (dynamic-require `(file ,(if (string? p) p (path->string p))) #f)))

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

(provide tests int-tests unit-tests)

(define (go tests) (test/gui tests))
(define (go/text tests) (run-tests tests 'verbose))

(define (just-one p*)
  (define-values (path p b) (split-path p*))
  (define f
    (if (equal? "fail/" (path->string path))
        (lambda (p thnk)
          (define-values (pred info) (exn-pred p))
          (parameterize ([error-display-handler void])
            (with-check-info
             (['predicates info])
             (check-exn pred thnk))))
        (lambda (p thnk) (check-not-exn thnk))))
  (test-suite 
   (path->string p)
   (f
    (build-path path p)
    (lambda ()
      (parameterize ([read-accept-reader #t]
                     [current-load-relative-directory
                      (path->complete-path path)]
                     [current-directory path]
                     [current-output-port (open-output-nowhere)])
        (dr p))))))

(define (compile-benchmarks)
  (define (find dir)
    (for/list ([d (directory-list dir)]
               #:when (scheme-file? d))
      d))
  (define shootout (collection-path "tests" "racket" "benchmarks" "shootout" "typed"))
  (define common (collection-path "tests" "racket" "benchmarks" "common" "typed"))
  (define (mk path)
    (make-test-suite (path->string path)
                     (for/list ([p (find path)])                       
                       (parameterize ([current-load-relative-directory
                                       (path->complete-path path)]
                                      [current-directory path])
                         (test-suite (path->string p)
                                     (check-not-exn (λ () (cfile (build-path path p)))))))))
  (test-suite "compiling"
              (mk shootout)
              (delete-directory/files (build-path shootout "compiled"))
              (mk common)
              (delete-directory/files (build-path common "compiled"))))

(provide go go/text just-one compile-benchmarks)




#lang racket/base

(require rackunit rackunit/text-ui racket/file
         mzlib/etc racket/port
         compiler/compiler racket/promise
         racket/match syntax/modcode
         racket/promise
         "unit-tests/all-tests.rkt"
         "unit-tests/test-utils.rkt"
         "optimizer/run.rkt"
         "places.rkt" "send-places.rkt")

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

(define (mk-tests dir test #:error [error? #f])
  (lambda ()
    (define path (build-path (this-expression-source-directory) dir))
    (define prms
      (for/list ([p (directory-list path)]
                 #:when (scheme-file? p)
		 ;; skip backup files
		 #:when (not (regexp-match #rx".*~" (path->string p))))
        (define p* (build-path path p))
        (define prm (list path p 
                          (if (places)
                              (delay/thread
                                (run-in-other-place p* error?))
                              (delay 
                                (parameterize ([read-accept-reader #t]
                                               [current-load-relative-directory path]
                                               [current-directory path]
                                               [current-output-port (open-output-nowhere)])
                                  (dr p))))))
        prm))
    (define tests
      (for/list ([e prms])
        (match-define (list path p prm) e)
        (test-suite
         (path->string p)
         (test
          (build-path path p)
          (λ ()
            (when (verbose?)
              (log-warning (format "TR tests: waiting for ~a ~a" dir p)))
            (force prm))))))
    (make-test-suite dir tests)))

(define succ-tests (mk-tests "succeed"
                             (lambda (p thnk) 
                               (check-not-exn thnk))))
(define fail-tests (mk-tests "fail"
                             (lambda (p thnk)
                               (define-values (pred info) (exn-pred p))
                               (parameterize ([error-display-handler void])
                                 (with-check-info
                                  (['predicates info])
                                  (check-exn pred thnk))))
                             #:error #t))

(define (int-tests)
  (test-suite "Integration tests"
              (succ-tests)
              (fail-tests)))

(define (compile-benchmarks)
  (define shootout (collection-path "tests" "racket" "benchmarks" "shootout" "typed"))
  (define common (collection-path "tests" "racket" "benchmarks" "common" "typed"))
  (define (mk dir)
    (let ((promised-results
            (for/hash ([file (in-list (directory-list dir))]
                        #:when (scheme-file? file))
              (values (path->string file)
                      (delay/thread (compile-path (build-path dir file)))))))
      (make-test-suite (path->string dir)
        (for/list ([(name results) promised-results])
           (test-suite name
              (check-not-exn (λ () (force results))))))))


  (test-suite "Compiling Benchmark tests"
              (mk shootout)
              (mk common)))


(define (just-one p*)
  (define-values (path p b) (split-path p*))
  (define f
    (let ([dir (path->string path)])
      (cond [(regexp-match? #rx"fail/$" dir )
             (lambda (p thnk)
               (define-values (pred info) (exn-pred p))
               (parameterize ([error-display-handler void])
                 (with-check-info
                  (['predicates info])
                  (check-exn pred thnk))))]
            [(regexp-match? #rx"succeed/$" dir)
             (lambda (p thnk) (check-not-exn thnk))]
            [(regexp-match? #rx"optimizer/tests/$" dir)
             (lambda (p* thnk) (test-opt p))]
            [(regexp-match? #rx"optimizer/missed-optimizations/$" dir)
             (lambda (p* thnk) (test-missed-optimization p))]
            [else
              (error 'just-one "Unknown test kind for test: ~a" p*)])))
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


(define (test/gui suite)
  (((dynamic-require 'rackunit/private/gui/gui 'make-gui-runner))
   suite))


(define (go tests) (test/gui tests))
(define (go/text tests)
  (force (delay/thread (run-tests tests 'verbose))))

(provide go go/text just-one places start-workers
         verbose?
         int-tests unit-tests compile-benchmarks
         optimization-tests missed-optimization-tests)

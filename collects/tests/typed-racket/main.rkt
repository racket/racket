#lang racket/base

(require rackunit rackunit/text-ui racket/file
         mzlib/etc racket/port
         compiler/compiler racket/promise
         racket/match mzlib/compile
         "unit-tests/all-tests.rkt"
         "unit-tests/test-utils.rkt"
         "optimizer/run.rkt"
         "places.rkt")

(define places (make-parameter (and (place-enabled?) (min 8 (processor-count)))))

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
                              (run-in-other-place p* error?)
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

(define int-tests
  (test-suite "Integration tests"
              (succ-tests)
              (fail-tests)))

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

(require racket/place data/queue racket/async-channel)


(define-values (enq-ch deq-ch) (place-channel))
(define (start-workers)
  (when (places)
    (for ([i (places)])
      (start-worker deq-ch i))))

(define (run-in-other-place p* [error? #f])
  (define-values (res-ch res-ch*) (place-channel))
  (place-channel-put enq-ch (vector p* res-ch* error?))
  (delay/thread
   (define res (place-channel-get res-ch))
   (when (s-exn? res)
     (raise (deserialize-exn res)))))


(define (just-one p*)
  (define-values (path p b) (split-path p*))
  (define f
    (let ([dir (path->string path)])
      (cond [(equal? dir "fail/")
             (lambda (p thnk)
               (define-values (pred info) (exn-pred p))
               (parameterize ([error-display-handler void])
                 (with-check-info
                  (['predicates info])
                  (check-exn pred thnk))))]
            [(equal? dir "succeed/")
             (lambda (p thnk) (check-not-exn thnk))]
            [(equal? dir "optimizer/tests/")
             (lambda (p* thnk) (test-opt p))]
            [(equal? dir "optimizer/missed-optimizations/")
             (lambda (p* thnk) (test-missed-optimization p))])))
  (test-suite
   (path->string p)
   (f
    (build-path path p)
    (lambda ()
      (force (run-in-other-place p*))
      #;
      (parameterize ([read-accept-reader #t]
                     [current-load-relative-directory
                      (path->complete-path path)]
                     [current-directory path]
                     [current-output-port (open-output-nowhere)])
        (dr p))))))


(define (go tests) (test/gui tests))
(define (go/text tests) (run-tests tests 'verbose))

(provide go go/text just-one places start-workers
         int-tests unit-tests compile-benchmarks
         optimization-tests missed-optimization-tests)

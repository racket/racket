#lang racket/base

(require redex/reduction-semantics
         racket/contract
         (for-syntax racket/base
                     setup/path-to-relative))

(define-struct test-suite (name reductions to-mz equal? tests))
(define-struct test (name input expecteds run-mz? around file line))

(define (show-dup-error from dup)
  (string->immutable-string
   (format "FOUND DUPLICATE!\n----\n~s\nwent to this twice:\n~s\n----\n"
           from
           dup)))

(define (uniq from lot)
  (let loop ((thelist lot))
    (unless (null? thelist)
      (when (member (car thelist) (cdr thelist))
        (raise (make-exn:fail:duplicate 
                (show-dup-error from (car thelist))
                (current-continuation-marks))))
      (loop (cdr thelist)))))
(define-struct (exn:fail:duplicate exn:fail) ())

(define (evaluate reductions t progress? [intermediate-state-test void] #:only-first-answer? [only-first-answer? #f])
  (let ([cache (make-hash)]
        [count 0]
        [results (make-hash)])
    
    (let loop ([t t]
               [depth 0])
      (unless (hash-ref cache t (λ () #f))
        (hash-set! cache t #t)
        (set! count (+ count 1))
        (intermediate-state-test t)
        (when progress?
          (cond
            [(eq? progress? 'dots)
             (when (= 0 (modulo count 100))
               (printf ":")
               (flush-output))]
            [else
             (when (= 0 (modulo count 5000))
               (printf "~s states ... " count)
               (flush-output))]))
        (let ([nexts (apply-reduction-relation reductions t)])
          (cond
            [(null? nexts)
             (hash-set! results t #t)]
            [only-first-answer?
             (loop (car nexts) (+ depth 1))]
            [else
             (uniq t nexts)
             (for-each (λ (t) (loop t (+ depth 1)))
                       nexts)]))))
    
    (when progress?
      (unless (eq? progress? 'dots)
        (printf "~s state~a total\n" count (if (= 1 count) "" "s"))))
    (hash-map results (λ (x y) x))))

(define (set-same? s1 s2 same?)
  (define (in-s1? s2-ele) (ormap (lambda (s1-ele) (same? s1-ele s2-ele)) s1))
  (define (in-s2? s1-ele) (ormap (lambda (s2-ele) (same? s1-ele s2-ele)) s2))
  (and (andmap in-s1? s2)
       (andmap in-s2? s1)
       #t))

(define-syntax (-test stx)
  (syntax-case stx ()
    [(_ name term expected) 
     (with-syntax ([line (syntax-line stx)]
                   [source (and (path? (syntax-source stx))
                                (path->relative-string/library (syntax-source stx)))])
       (syntax (build-test name term (list expected) #t #f line source)))]
    [(_ name term expected mz?) 
     (with-syntax ([line (syntax-line stx)]
                   [source (and (path? (syntax-source stx))
                                (path->relative-string/library (syntax-source stx)))])
       (syntax (build-test name term (list expected) mz? #f line source)))]
    [(_ name term expected mz? around)
     (with-syntax ([line (syntax-line stx)]
                   [source (and (path? (syntax-source stx))
                                (path->relative-string/library (syntax-source stx)))])
       (syntax (build-test name term (list expected) mz? around line source)))]))

(define-syntax (test/anss stx)
  (syntax-case stx ()
    [(_ name term expecteds) 
     (with-syntax ([line (syntax-line stx)]
                   [source (and (path? (syntax-source stx))
                                (path->relative-string/library (syntax-source stx)))])
       (syntax (build-test name term expecteds #t #f line source)))]))

(define (build-test name term expecteds mz? around line source)
  (make-test name term expecteds mz? (or around (λ (t) (t)))
             (cond
               [(path? source)
                (let-values ([(base name dir?) (split-path source)])
                  (path->string name))]
               [else "<unknown file>"]) 
             line))

(define (run-test-suite test-suite)
  (printf "running test suite: ~a\n" (test-suite-name test-suite))
  (let ([count 0])
    (for-each (λ (test) 
                (set! count (+ count 1))
                (run-test test-suite test))
              (test-suite-tests test-suite))
    (printf "ran ~a tests\n" count)))

(define-struct multiple-values (lst) #:transparent)

(define (run-test test-suite test)
  (let* ([name (test-name test)]
         [input (test-input test)]
         [expecteds (test-expecteds test)]
         [file (test-file test)]
         [line (test-line test)]
         [got 
          ((test-around test)
           (λ ()
             (evaluate (test-suite-reductions test-suite)
                       input
                       #f)))])
    (unless (set-same? got expecteds (test-suite-equal? test-suite))
      (eprintf "line ~a of ~a ~a\n    test: ~s\n     got: ~s\nexpected: ~s\n\n"
               line
               file
               name
               input
               (separate-lines got)
               (separate-lines expecteds)))
    (when (test-run-mz? test)
      (let* ([mv-wrap
              (λ vals
                (if (= 1 (length vals))
                    (car vals)
                    (make-multiple-values vals)))]
             [mz-got 
              (with-handlers ([exn? values]) 
                (call-with-values
                 (λ () (eval ((test-suite-to-mz test-suite) input)))
                 mv-wrap))]
             [expected (car expecteds)]
             [mz-expected (with-handlers ([exn? values])
                            (call-with-values
                             (λ () (eval ((test-suite-to-mz test-suite) expected)))
                             mv-wrap))])
        (unless (same-mz? mz-got mz-expected)
          (parameterize ([print-struct #t])
            (eprintf "line ~s of ~a ~a\nMZ  test: ~s\n     got: ~s\nexpected: ~s\n\n" 
                     line
                     file
                     name
                     input 
                     (if (exn? mz-got) (exn-message mz-got) mz-got)
                     (if (exn? mz-expected) (exn-message mz-expected) mz-expected))))))))

(define (separate-lines sexps)
  (cond
    [(null? sexps) ""]
    [(null? (cdr sexps)) (car sexps)]
    [else (apply string-append (map (λ (x) (format "\n~s" x)) sexps))]))

(define (same-mz? mz-got mz-expected)
  (or (same-mz-single-value? mz-got mz-expected)
      
      (and (multiple-values? mz-got)
           (multiple-values? mz-expected)
           (andmap same-mz-single-value?
                   (multiple-values-lst mz-got)
                   (multiple-values-lst mz-expected)))
      
      (and (exn? mz-got) 
           (exn? mz-expected)
           (equal? (exn-message mz-got)
                   (exn-message mz-expected)))
      
      (and (exn? mz-got)
           (regexp? mz-expected)
           (regexp-match mz-expected (exn-message mz-got)))))

(define (same-mz-single-value? mz-got mz-expected)
  (or (equal? mz-got mz-expected)
      (and (procedure? mz-got) 
           (procedure? mz-expected)
           (equal? (procedure-arity mz-got)
                   (procedure-arity  mz-expected)))))


(define (-test-suite n a b e? . c) (make-test-suite n a b e? c))

(provide (rename-out [-test test]))
(provide/contract [rename -test-suite
                          test-suite
                          (->* (string?
                                reduction-relation?
                                (-> any/c any)
                                (-> any/c any/c boolean?))
                               (listof test?)
                               test-suite?)]
                  [run-test-suite (-> test-suite? any)])

(provide test-suite-tests
         test?
         test-name
         test-input
         test-expecteds
         test-file
         test-line
         test/anss
         
         evaluate
         exn:fail:duplicate?
         set-same?)

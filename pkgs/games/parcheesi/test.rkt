(module test racket
  (require racket/pretty)
  (provide test test-list test-err test-results)
  
  (define show-tests? #t)
  
  (define test-count (box 0))
  (define failure-count  (box 0))
  (define (test-results)
    (cond
      [(= 0 (unbox failure-count))
       (eprintf "All ~a tests passed." (unbox test-count))]
      [else
       (eprintf "~a tests failed, ~a tests total"
                (unbox failure-count)
                (unbox test-count))]))
  
  (define-syntax (test-err stx)
    (syntax-case stx ()
      [(_ actual pred)
       (with-syntax ([line (syntax-line (syntax actual))])
         (syntax
          (with-handlers ([pred (lambda (x) (void))])
            (when show-tests? (printf "> running test ~s\n" line))
            (set-box! test-count (+ (unbox test-count) 1))
            (eprintf "test ~a ~s:\n expected error, got ~a\n\n"
                     line
                     'actual
                     (flatten-list (call-with-values (lambda () actual) list)))
            (set-box! failure-count (+ (unbox failure-count) 1)))))]))
  
  (define-syntax (test stx) 
    (syntax-case stx ()
      [(_ actual expecteds ...) (syntax (test/pred equal? actual expecteds ...))]))
    
  (define-syntax (test/pred stx)
    (syntax-case stx ()
      [(_ equal? actual expecteds ...)
       (with-syntax ([line (syntax-line (syntax actual))])
         (syntax
          (begin
            (when show-tests? (printf "> running test ~s\n" line))
            (let ([actual-xs (call-with-values (lambda () actual) list)]
                  [expect-xs (list expecteds ...)])
              (set-box! test-count (+ (unbox test-count) 1))
              (unless (equal? actual-xs expect-xs)
                (set-box! failure-count (+ (unbox failure-count) 1))
                (eprintf "test ~a ~s:\ngot:\n" line 'actual)
                (for-each (lambda (x) (pretty-print x (current-error-port))) actual-xs)
                (eprintf "expected:\n")
                (for-each (lambda (x) (pretty-print x (current-error-port))) expect-xs))))))]))
  
  (define-syntax (test-list stx)
    (syntax-case stx ()
      [(_ actual expected)
       (with-syntax ([line (syntax-line (syntax actual))])
         (syntax
          (begin
            (when show-tests? (printf "> running test ~s\n" line))
            (let ([actual-x actual]
                  [expect-x expected]
                  [show-err
                   (lambda (in not-in val)
                     (set-box! failure-count (+ (unbox failure-count) 1))
                     (eprintf "test ~a ~s found in ~a but not in ~a:\n"
                              line 'actual in not-in)
                     (pretty-print val (current-error-port)))])
              (set-box! test-count (+ (unbox test-count) 1))
              (for-each (lambda (one-actual)
                          (unless (member one-actual expect-x)
                            (show-err "actual" "expected" one-actual)))
                        actual-x)
              (for-each (lambda (one-expected)
                          (unless (member one-expected actual-x)
                            (show-err "expected" "actual" one-expected)))
                        expect-x)))))]))
  
  (define (flatten-list lst)
    (cond
      [(null? lst) ""]
      [else
       (let loop ([lst (cdr lst)]
                  [ss (list (format "~e" (car lst)))])
         (cond
           [(null? lst) (apply string-append (reverse ss))]
           [else (loop (cdr lst)
                       (list* (format "~e" (car lst))
                              " "
                              ss))]))])))

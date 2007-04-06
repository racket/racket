;; This file is is used in the context of sandboxed code, it uses the
;; stacktrace interface from errortrace to find uncovered expressions.
(module sandbox-coverage mzscheme
  (require (lib "stacktrace.ss" "errortrace") (lib "unit.ss") (lib "list.ss"))

  ;; Test coverage run-time support
  (define test-coverage-enabled (make-parameter #t))
  (define test-coverage-info (make-hash-table))
  (define (initialize-test-coverage-point key expr)
    (hash-table-put! test-coverage-info key (cons expr #f)))
  (define (test-covered key)
    (set-cdr! (hash-table-get test-coverage-info key) #t))

  (define (get-uncovered-expressions)
    (let* ([xs (hash-table-map test-coverage-info (lambda (k v) v))]
           [xs (filter (lambda (x) (syntax-position (car x))) xs)]
           [xs (sort xs (lambda (x1 x2)
                          (let ([p1 (syntax-position (car x1))]
                                [p2 (syntax-position (car x2))])
                            (or (< p1 p2) ; earlier first
                                (and (= p1 p2)
                                     (> (syntax-span (car x1)) ; wider first
                                        (syntax-span (car x2))))))))]
           [xs (reverse! xs)])
      (if (null? xs)
        xs
        (let loop ([xs (cdr xs)] [r (list (car xs))])
          (if (null? xs)
            (map car (filter (lambda (x) (not (cdr x))) r))
            (loop (cdr xs)
                  (cond [(not (and (= (syntax-position (caar xs))
                                      (syntax-position (caar r)))
                                   (= (syntax-span (caar xs))
                                      (syntax-span (caar r)))))
                         (cons (car xs) r)]
                        [(cdar r) r]
                        [else (cons (car xs) (cdr r))])))))))

  (provide get-uncovered-expressions)

  ;; no profiling
  (define profile-key #f)
  (define profiling-enabled (lambda () #f))
  (define initialize-profile-point void)
  (define register-profile-start void)
  (define register-profile-done void)
  ;; no marks
  (define (with-mark mark expr) expr)

  (define-values/invoke-unit/infer stacktrace@)

  (define errortrace-compile-handler
    (let ([orig (current-compile)]
          [ns (current-namespace)])
      (lambda (e immediate-eval?)
        (orig (if (and (eq? ns (current-namespace))
                       (not (compiled-expression?
                             (if (syntax? e) (syntax-e e) e))))
                (annotate-top
                 (expand-syntax (if (syntax? e)
                                  e
                                  (namespace-syntax-introduce
                                   (datum->syntax-object #f e))))
                 #f)
                e)
              immediate-eval?))))

  (current-compile errortrace-compile-handler))

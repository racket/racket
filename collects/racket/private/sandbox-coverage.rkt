;; This file is is used in the context of sandboxed code, it uses the
;; stacktrace interface from errortrace to find uncovered expressions.
#lang racket/base
(require errortrace/stacktrace racket/unit (for-template racket/base))

;; Test coverage run-time support
(define test-coverage-enabled (make-parameter #t))
(define test-coverage-info (make-hasheq))
(define (initialize-test-coverage-point expr)
  (hash-set! test-coverage-info expr (mcons #f #f)))
(define (test-covered expr)
  (let ([v (hash-ref test-coverage-info expr
                     (lambda ()
                       (error 'sandbox-coverage
                              "internal error: no info for ~.s" expr)))])
    (and v (with-syntax ([v v]) #'(#%plain-app set-mcar! v #t)))))

(define (get-uncovered-expressions)
  (let* ([xs (hash-map test-coverage-info
                       (lambda (k v) (cons k (mcar v))))]
         [xs (filter (lambda (x) (syntax-position (car x))) xs)]
         [xs (sort xs (lambda (x1 x2)
                        (let ([p1 (syntax-position (car x1))]
                              [p2 (syntax-position (car x2))])
                          (or (< p1 p2) ; earlier first
                              (and (= p1 p2)
                                   (> (syntax-span (car x1)) ; wider first
                                      (syntax-span (car x2))))))))]
         [xs (reverse xs)])
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
(define (with-mark mark expr phase) expr)

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
                                 (datum->syntax #f e))))
               (namespace-base-phase))
              e)
            immediate-eval?))))

(current-compile errortrace-compile-handler)

#lang scheme
(require (only-in srfi/13 string-contains)
         plai/private/command-line)

(provide plai-error test test/pred test/regexp test/exn)
(provide/contract
 [exn:plai? (any/c . -> . boolean?)]
 [abridged-test-output (parameter/c boolean?)]
 [plai-ignore-exn-strings (parameter/c boolean?)]
 [plai-catch-test-exn (parameter/c boolean?)]
 [test-inspector (parameter/c inspector?)]
 [test-inexact-epsilon (parameter/c number?)])

(define thunk (-> any))

(define test-inspector (make-parameter (current-inspector)))
(define test-inexact-epsilon (make-parameter 0.01))

; We only catch exceptions of this type.  plai-error throws such exceptions.
(define-struct (exn:plai exn:fail) () #:transparent)

(define (plai-error . args)
  (with-handlers
      [(exn:fail? (λ (exn) 
                    (raise
                     (make-exn:plai (exn-message exn)
                                    (exn-continuation-marks exn)))))]
    (apply error args)))

(define-struct (exn:test exn:fail) ())

(define (install-test-inspector)
  (test-inspector (current-inspector))
  (current-inspector (make-inspector))
  (print-struct #t))

(define (maybe-command-line arg)
  (and (member arg (vector->list (current-command-line-arguments))) true))

(define halt-on-errors? (maybe-command-line "--plai-halt-on-errors"))
(define print-only-errors? (maybe-command-line "--plai-print-only-errors"))

(provide/contract (halt-on-errors (() (boolean?) . ->* . void?)))
(define (halt-on-errors [halt? true])
  (set! halt-on-errors? halt?))

(provide/contract (print-only-errors (() (boolean?) . ->* . void?)))
(define (print-only-errors [print? true])
  (set! print-only-errors? print?))

; list of all test results
(provide plai-all-test-results)
(define plai-all-test-results empty)

; set to true if 
(define plai-ignore-exn-strings (make-parameter false))

(define (may-print-result result)
  (parameterize ([current-inspector (test-inspector)]
                 [print-struct #t])
    (define error? 
      (not (eq? (first result) 'good)))
    (define print?
      (if print-only-errors?
          (if error?
              #t
              #f)
          #t))
    (set! plai-all-test-results (cons result plai-all-test-results))
    (when print?
      (if (abridged-test-output)
          (apply printf "(~s ~v ~v)" result)
          (apply printf "(~s ~s ~v ~v ~s)" result))
      (newline))
    (when (and halt-on-errors? error?)
      (raise (make-exn:test (string->immutable-string (format "test failed: ~s" result))
                            (current-continuation-marks))))))

(define plai-catch-test-exn (make-parameter true))
(define (exn+catching? x)
  (and (exn? x) (plai-catch-test-exn)))

;;; If the expression raises an exception, it is returned as a value, only if
;;; the exception subclasses struct:exn.
(define-syntax (return-exception stx)
  (syntax-case stx ()
    [(_ expr)
     #'(with-handlers
           ([exn+catching? (λ (exn) exn)])
         expr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax forms for the test procedures make it unnecessary to enclose the
;;; expression in a thunk.  More importantly, they automatically specify the
;;; line number of the test as the comment.

(provide generic-test equal~?)
(define (abridged v)
  (if (abridged-test-output)
      empty
      (list v)))
(define (print-error case test-sexp test-result expected-val loc)
  `(,case
    ,@(abridged test-sexp)
    ,test-result
    ,expected-val
    ,@(abridged loc)))

(define (generic-test test-thunk pred test-sexp loc)
  (unless (disable-tests)
    (may-print-result
     (with-handlers
         ; Applying the predicate shouldn't raise an exception.
         ([exn+catching? (λ (exn) 
                  (print-error
                   'pred-exception
                   test-sexp
                   (exn-message exn)
                   '<no-expected-value>
                   loc))])
       (let ([test-result (return-exception (test-thunk))])
         (if (or (exn:plai? test-result)
                 (not (exn? test-result)))
             (local [(define-values (test-value expected-val)
                       (pred test-result))]
               (print-error 
                (cond
                  [(exn:plai? test-value) 'exception]
                  [test-value 'good]
                  [else 'bad]) 
                test-sexp
                (if (exn:plai? test-result)
                    (exn-message test-result)
                    test-result)
                expected-val loc))
             (print-error
              'exception
              test-sexp
              (exn-message test-result)
              '<no-expected-value>
              loc)))))))

(define (equal~? x y)
  (or (parameterize ([current-inspector (test-inspector)])
        (equal? x y))
      (and (number? x) (number? y)
           (or (inexact? x) (inexact? y))
           ; If one of them is inexact, we do the math.
           (< (abs (- x y)) (test-inexact-epsilon)))))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ result-expr expected-expr)
     #`(generic-test 
        (λ () result-expr) 
        (λ (result-value)
          (define expected-val expected-expr)
          (values
           (cond
             [(exn:plai? result-value) result-value]
             [(equal~? result-value expected-val) true]
             [else false])
           expected-val))
        (quote #,(syntax->datum #'result-expr))
        (format "at line ~a" #,(syntax-line stx)))]))

(define-syntax (test/pred stx)
  (syntax-case stx ()
    [(_ test-expr pred)
     #`(generic-test 
        (λ () test-expr)
        (λ (val)
          (values
           (cond
             [(exn:plai? val) val]
             [else (pred val)])
           (quote #,(syntax->datum #'pred))))
        (quote #,(syntax->datum #'test-expr))
        (format "at line ~a" #,(syntax-line stx)))]))

(define-syntax (test/exn stx)
  (syntax-case stx ()
    [(_ test-expr exception-substring)
     #`(generic-test
        (λ () test-expr)
        (λ (val)
          (define exception-substring-val exception-substring)
          (values
           (and (exn:plai? val)
                (or (plai-ignore-exn-strings)
                    (string-contains (exn-message val) exception-substring-val)))
           exception-substring-val))
        (quote #,(syntax->datum #'test-expr))
        (format "at line ~a" #,(syntax-line stx)))]))

(define-syntax (test/regexp stx)
  (syntax-case stx ()
    [(_ test-expr regexp)
     #`(generic-test
        (λ () test-expr)
        (λ (val)
          (define regexp-val regexp)
          (values 
           (and (exn:plai? val)
                (or (plai-ignore-exn-strings)
                    (regexp-match regexp (exn-message val))))
           regexp-val))
        (quote #,(syntax->datum #'test-expr))
        (format "at line ~a" #,(syntax-line stx)))]))

(install-test-inspector)

#lang racket/base
(require (for-syntax racket/base))

(provide test/no-error
         test/spec-passed
         test/spec-passed/result
         test/spec-failed
         test/pos-blame
         test/neg-blame
         test/well-formed
         test ctest ctest/rewrite
         
         current-contract-namespace
         make-basic-contract-namespace
         make-full-contract-namespace
         
         contract-syntax-error-test
         contract-error-test
         
         contract-eval
         contract-compile
         contract-expand-once
         
         rewrite-to-add-opt/c
         
         test-cases failures)

(define test-cases 0)
(define failures 0)

(define (test expected fun arg1 . args)
  (set! test-cases (+ test-cases 1))
  ;(printf "\n\nexpected ~s\n" arg1)
  (define result
    (cond
      [(procedure? fun) (apply fun arg1 args)]
      [else arg1]))
  (unless (equal? result expected)
    (set! failures (+ failures 1))
    (eprintf "FAILED ~s\n       got ~s\n  expected ~s\n"
            fun
            result
            expected)))

(define (test-an-error name thunk sexp predicate?)
  (set! test-cases (+ test-cases 1))
  (define exn
    (with-handlers ((exn:fail? values))
      (thunk)
      'no-exn-raised))
  (unless (predicate? exn)
    (set! failures (+ failures 1))
    (eprintf "FAILED ~s\n" name)))

(define current-contract-namespace (make-parameter 'current-contract-namespace-not-initialized))
(define (make-basic-contract-namespace . addons)
  (define n (make-base-namespace))
  (parameterize ([current-namespace n])
    (namespace-require 'racket/contract/base)
    (namespace-require '(only racket/contract/private/blame exn:fail:contract:blame?))
    (for ([addon (in-list addons)])
      (namespace-require addon)))
  n)
(define (make-full-contract-namespace . addons)
  (apply make-basic-contract-namespace 
         'racket/contract
         'racket/class
         'racket/set
         addons))

(define (contract-eval x #:test-case-name [test-case #f])
  (with-handlers ((exn:fail? (λ (x)
                               (when test-case
                                 (eprintf "exception raised while running test case ~a\n"
                                          test-case))
                               (raise x))))
  (parameterize ([current-namespace (current-contract-namespace)])
    (eval x))))

(define (contract-compile x)
  (parameterize ([current-namespace (current-contract-namespace)])
    (compile x)))

(define (contract-expand-once x)
  (parameterize ([current-namespace (current-contract-namespace)])
    (expand-once x)))

(define-syntax (ctest stx)
  (syntax-case stx ()
    [(_ a ...)
     (syntax (contract-eval `(,test a ...)))]))

(define-syntax (ctest-no-error stx)
  (syntax-case stx ()
    [(_ name e)
     (syntax
      (ctest
       #t
       name
       (with-handlers ([exn:fail? (lambda (x) `(exn ,(exn-message x)))])
         e
         #t)))]))

(define (contract-error-test name exp exn-ok?)
  (test #t
        name
        (contract-eval #:test-case-name name
                       `(with-handlers ((exn:fail? (λ (x) (and (,exn-ok? x) #t)))) ,exp))))

(define (contract-syntax-error-test name exp [reg #rx""])
  (test #t
        name
        (contract-eval #:test-case-name name
                       `(with-handlers ((exn:fail:syntax?
                                         (lambda (x) (and (regexp-match ,reg (exn-message x)) #t))))
                          (eval ',exp)))))

;; test/spec-passed : symbol sexp -> void
;; tests a passing specification
(define (test/spec-passed name expression)
  (parameterize ([compile-enforce-module-constants #f])
    (contract-eval
     #:test-case-name name
     `(,test
       (void)
       (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
       (list ',expression '(void))))
    (let ([new-expression (rewrite-out expression)])
      (when new-expression
        (contract-eval
         #:test-case-name (format "~a rewrite-out" name)
         `(,test
           (void)
           (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
           (list ',new-expression '(void)))))))
  
  (let/ec k
    (contract-eval
     #:test-case-name (format "~a rewrite-to-add-opt/c" name)
     `(,test (void)
             (let ([for-each-eval (lambda (l) (for-each (λ (x) (eval x)) l))])
               for-each-eval)
             (list ',(rewrite-to-add-opt/c expression k) '(void))))))

(define (test/spec-passed/result name expression result)
  (parameterize ([compile-enforce-module-constants #f])
    (contract-eval #:test-case-name name `(,test ',result eval ',expression))
    (let/ec k
      (contract-eval
       #:test-case-name (format "~a rewrite-to-add-opt/c" name)
       `(,test
         ',result
         eval
         ',(rewrite-to-add-opt/c expression k))))
    (let ([new-expression (rewrite-out expression)])
      (when new-expression
        (contract-eval
         #:test-case-name (format "~a rewrite-out" name)
         `(,test
           ',result
           eval
           ',new-expression))))))

;; rewrites `provide/contract' to use `contract-out'
(define (rewrite-out orig-exp)
  (define rewrote? #f)
  (define maybe-rewritten?
    (let loop ([exp orig-exp])
      (cond
        [(and (list? exp)
              (>= (length exp) 3)
              (eq? (car exp) 'module))
         (define modname (list-ref exp 1))
         (define lang (list-ref exp 2))
         (define bodies (list-tail exp 3))
         (define at-beginning '())
         (define at-end '())
         
         ;; remove (and save) the provide/contract & contract-out
         ;; declarations, switching their senses
         (define removed-bodies
           (apply
            append
            (for/list ([body (in-list bodies)])
              (cond
                [(and (pair? body)
                      (eq? (car body) 'provide/contract))
                 (define args (cdr body))
                 (set! rewrote? #t)
                 (set! at-beginning (cons `(provide (contract-out . ,args))
                                          at-beginning))
                 (list)]
                [(and (list? body)
                      (= 2 (length body))
                      (eq? (car body) 'provide)
                      (let ([sub-part (list-ref body 1)])
                        (and (pair? sub-part)
                             (eq? (car sub-part) 'contract-out))))
                 (define args (cdr (list-ref body 1)))
                 (set! rewrote? #t)
                 (set! at-end (cons `(provide/contract . ,args)
                                    at-end))
                 (list)]
                [else
                 (list body)]))))
         
         ;; insert the provide/contract (rewrite to contract-out) after the
         ;; first require that has 'contract' in it
         (define inserted-bodies
           (if (equal? lang 'racket)
               (append (reverse at-beginning)
                       removed-bodies)
               (apply
                append
                (for/list ([body (in-list removed-bodies)])
                  (define (good-thing? l)
                    (for/or ([x (in-list l)])
                      (and (symbol? x)
                           (regexp-match #rx"contract" (symbol->string x)))))
                  (cond
                    [(and (pair? body)
                          (eq? (car body) 'require)
                          (good-thing? (cdr body)))
                     (cons body (reverse at-beginning))]
                    [else
                     (list body)])))))
         `(module ,modname ,lang 
            (void) ;; always insert this to work around bug in 'provide'
            ,@inserted-bodies ,@(reverse at-end))]
        [(list? exp)
         (map loop exp)]
        [else exp])))
  
  (and rewrote? maybe-rewritten?))

;; rewrites `contract' to use opt/c. If there is a module definition in there, we skip that test.
(define (rewrite-to-add-opt/c exp k)
  (let loop ([exp exp])
    (cond
      [(null? exp) null]
      [(list? exp)
       (case (car exp)
         [(contract) `(contract (opt/c ,(loop (cadr exp))) ,@(map loop (cddr exp)))]
         [(module) (k #f)]
         [else (map loop exp)])]
      [(pair? exp) (cons (loop (car exp))
                         (loop (cdr exp)))]
      [else exp])))

;; blame : (or/c 'pos 'neg string?)
;;   if blame is a string, expect to find the string (format "blaming: ~a" blame) in the exn message
(define (test/spec-failed name expression blame)
  (define (has-proper-blame? msg)
    (define reg
      (cond
        [(eq? blame 'pos) #rx"blaming: pos"]
        [(eq? blame 'neg) #rx"blaming: neg"]
        [(string? blame) (string-append "blaming: " (regexp-quote blame))]
        [else #f]))
    (and reg (regexp-match? reg msg)))
  (contract-eval
   #:test-case-name name
   `(,test-an-error
     ',name
     (lambda () ,expression)
     ',expression
     (lambda (exn)
       (and (exn:fail:contract:blame? exn)
            (,has-proper-blame? (exn-message exn))))))
  (let/ec k
    (let ([rewritten (rewrite-to-add-opt/c expression k)])
      (contract-eval
       #:test-case-name (format "~a rewrite-to-add-opt/c" name)
       `(,test-an-error
         ',(string->symbol (format "~a+opt/c" name))
         (lambda () ,rewritten)
         ',rewritten
         (lambda (exn)
           (and (exn:fail:contract:blame? exn)
                (,has-proper-blame? (exn-message exn)))))))))

(define (test/pos-blame name expression) (test/spec-failed name expression 'pos))
(define (test/neg-blame name expression) (test/spec-failed name expression 'neg))

(define-syntax (ctest/rewrite stx)
  (syntax-case stx ()
    [(_ expected name expression)
     #'(begin
         (contract-eval #:test-case-name 'name `(,test expected 'name expression))
         (let/ec k
           (let ([new-name '#,(string->symbol (format "~a+opt/c" 'name))])
             (contract-eval
              #:test-case-name new-name
              `(,test expected 
                      ',new-name
                      ,(rewrite-to-add-opt/c 'expression k))))))]))

(define (test/well-formed stx)
  (contract-eval
   `(,test (void)
           (let ([expand/ret-void (lambda (x) (expand x) (void))]) expand/ret-void)
           ,stx)))

(define (test/no-error sexp)
  (contract-eval
   `(,test (void)
           eval
           '(begin ,sexp (void))))
  (let/ec k
    (contract-eval
     `(,test (void)
             eval
             '(begin ,(rewrite-to-add-opt/c sexp k) (void))))))

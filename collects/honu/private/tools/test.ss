(module test mzscheme

  (require (lib "list.ss"))
  
  #|
  Test case and test suite macros:

  Expressions:
  (test-case NAME EXPR PREDICATE)
  (test-suite NAME CASE ...)

  Definitions:
  (define-test-case NAME EXPR PREDICATE)
  (define-test-suite NAME CASES ...)
  |#

  (define (report? obj) (or (void? obj) (list? obj)))

  (define (exn-sexp exn)
    `(error ,(exn-message exn)))
  
  (define-for-syntax (syntax-rest stx)
    (syntax-case stx () [(_ . REST) #'REST]))

  (define-for-syntax (translate-predicate stx)
    (syntax-case stx (equal: error: pred:)
      [(equal: VALUE)
       #`(lambda (name expr thunk)
           (lambda ()
             (with-handlers ([(lambda (exn) #t)
                              (lambda (exn) `(,name : ,expr raised ,(exn-sexp exn)))])
               (let* ([result (thunk)])
                 (with-handlers ([(lambda (exn) #t)
                                  (lambda (exn)
                                    `(,name : expected value VALUE raised ,(exn-sexp exn)))])
                   (let* ([expected VALUE])
                     (if (equal? result expected)
                         (void)
                         `(,name : ,expr = ,result != ,expected))))))))]
      [error:
       #`(lambda (name expr thunk)
           (lambda ()
             (with-handlers ([(lambda (exn) #t) (lambda (exn) (void))])
               (let* ([result (thunk)])
                 `(,name : ,expr = ,result but expected error)))))]
      [(error: PRED)
       #`(lambda (name expr thunk)
           (lambda ()
             (with-handlers
                 ([(lambda (exn) #t)
                   (lambda (exn)
                     (with-handlers
                         ([(lambda (exn) #t)
                           (lambda (exn)
                             `(,name : predicate PRED raised ,(exn-sexp exn)))])
                       (if (PRED exn)
                           (void)
                           `(,name : ,expr raised ,(exn-sexp exn) which failed PRED))))])
               (let* ([result (thunk)])
                 `(,name : ,expr = ,result but expected error)))))]
      [(pred: PRED)
       #`(lambda (name expr thunk)
           (lambda ()
             (with-handlers ([(lambda (exn) #t)
                              (lambda (exn) `(,name : ,expr raised ,(exn-sexp exn)))])
               (let* ([result (thunk)])
                 (with-handlers ([(lambda (exn) #t)
                                  (lambda (exn)
                                    `(,name : predicate PRED raised ,(exn-sexp exn)))])
                   (if (PRED result)
                       (void)
                       `(,name : ,expr = ,result failed PRED)))))))]
      ))
  
  (define-for-syntax (translate-test-case stx)
    (syntax-case stx ()
      [(NAME EXPR PREDICATE)
       #`(#,(translate-predicate #'PREDICATE) 'NAME 'EXPR (lambda () EXPR))]))

  (define-for-syntax (translate-test-suite stx)
    (syntax-case stx ()
      [(NAME CASE ...)
       #`(let* ([cases (list CASE ...)])
           (lambda ()
             (let* ([reports (map (lambda (case) (case)) cases)]
                    [errors (filter (lambda (report) (not (void? report))) reports)])
               (cond [(null? errors) (void)]
                     [(= (length errors) 1) (cons 'NAME (first errors))]
                     [else (cons 'NAME (cons ': errors))]))))]))

  (define-for-syntax (translate-define-test-case stx)
    (syntax-case stx ()
      [(NAME . _) #`(define NAME #,(translate-test-case stx))]))
  
  (define-for-syntax (translate-define-test-suite stx)
    (syntax-case stx ()
      [(NAME . _) #`(define NAME #,(translate-test-suite stx))]))
  
  (define-syntax (test-case stx)
    (translate-test-case (syntax-rest stx)))
  
  (define-syntax (test-suite stx)
    (translate-test-suite (syntax-rest stx)))

  (define-syntax (define-test-case stx)
    (translate-define-test-case (syntax-rest stx)))

  (define-syntax (define-test-suite stx)
    (translate-define-test-suite (syntax-rest stx)))
  
  (provide report? test-case test-suite define-test-case define-test-suite)
  )

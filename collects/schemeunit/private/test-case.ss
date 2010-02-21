#lang scheme/base

(require (for-syntax scheme/base)
         "base.ss"
         "format.ss"
         "check-info.ss"
         "check.ss")

(provide current-test-name
         current-test-case-around

         test-begin
         test-case

         before
         after
         around)

(define current-test-name
  (make-parameter
   #f
   (lambda (v)
     (if (string? v)
         v
         (raise-mismatch-error
          'current-test-name
          "string?"
          v)))))

;; test-case-around : ( -> a) -> a
;;
;; Run a test-case immediately, printing information on failure
(define test-case-around
  (lambda (thunk)
    (with-handlers
        ([exn:test:check?
          (lambda (e)
            (display-delimiter)
            (display-test-name (current-test-name))
            (display-failure)(newline)
            (display-check-info-stack (exn:test:check-stack e))
            (display-delimiter))]
         [exn?
          (lambda (e)
            (display-delimiter)
            (display-test-name (current-test-name))
            (display-error)(newline)
            (display-exn e)
            (display-delimiter))])
      (thunk))))
    
(define current-test-case-around
  (make-parameter
   test-case-around
   (lambda (v)
     (if (procedure? v)
         v
         (raise-type-error 'current-test-case-around "procedure" v)))))      

;; test-case-check-handler : (-> exn void)
;;
;; Raise any exceptions that occur in checks, halting
;; evaluation of following expression within the scope of
;; the test case
(define test-case-check-handler raise)
  
(define-syntax (test-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     (syntax/loc stx
       ((current-test-case-around)
        (lambda ()
          (parameterize
              ([current-check-handler test-case-check-handler]
               [current-check-around  check-around])
            expr ...))))]
    [_
     (raise-syntax-error
      #f
      "Correct form is (test-begin expr ...)"
      stx)]))

  
(define-syntax test-case
  (syntax-rules ()
    [(test-case name expr ...)
     (parameterize
         ([current-test-name name])
       (test-begin expr ...))]))

  
(define-syntax before
  (syntax-rules ()
    ((_ before-e expr1 expr2 ...)
     (dynamic-wind
       (lambda ()
         before-e)
       (lambda ()
         expr1 expr2 ...)
       (lambda ()
         (void))))
    ((before error ...)
     (raise-syntax-error
      'before
      "Incorrect use of before macro.  Correct format is (before before-expr expr1 expr2 ...)"
      'before
      '(error ...)))))

(define-syntax after
  (syntax-rules ()
    ((_ expr1 expr2 ... after-e)
     (dynamic-wind
       (lambda ()
         (void))
       (lambda ()
         expr1 expr2 ...)
       (lambda ()
         after-e)))
    ((after error ...)
     (raise-syntax-error
      'before
      "Incorrect use of after macro.  Correct format is (after expr1 expr2 ... after-expr)"
      'after
      '(error ...)))))

(define-syntax around
  (syntax-rules ()
    ((_ before-e expr1 expr2 ... after-e)
     (dynamic-wind
       (lambda ()
         before-e)
       (lambda ()
         expr1 expr2 ...)
       (lambda ()
         after-e)))
    ((around error ...)
     (raise-syntax-error
      'around
      "Incorrect use of around macro.  Correct format is (around before-expr expr1 expr2 ... after-expr)"
      'around
      '(error ...)))))


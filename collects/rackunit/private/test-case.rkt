#lang racket/base

(require (for-syntax racket/base)
         "base.rkt"
         "format.rkt"
         "check-info.rkt"
         "check.rkt")

(provide current-test-name
         current-test-case-around

         test-begin
         test-case

         before
         after
         around)

(define USE-ERROR-HANDLER? #f)

(define current-test-name
  (make-parameter
   #f
   (lambda (v)
     (if (string? v)
         v
         (raise-type-error 'current-test-name "string" v)))))

;; test-case-around : ( -> a) -> a
;;
;; Run a test-case immediately, printing information on failure
(define (default-test-case-around thunk)
  (with-handlers ([exn? default-test-case-handler])
    (thunk)))

;; default-test-case-handler : exn -> any
(define (default-test-case-handler e)
  (let ([out (open-output-string)])
    ;;(display "test case failed\n" out)
    (parameterize ((current-output-port out))
      (display-delimiter)
      (display-test-name (current-test-name))
      (cond [(exn:test:check? e)
             (display-failure)(newline)
             (display-check-info-stack (exn:test:check-stack e))]
            [(exn? e)
             (display-error)(newline)
             (display-exn e)])
      (display-delimiter))
    (cond [USE-ERROR-HANDLER?
           ((error-display-handler) (get-output-string out)
            ;; So that DrRacket won't recognize exn:fail:syntax, etc
            (make-exn (exn-message e) (exn-continuation-marks e)))]
          [else
           (display (get-output-string out) (current-error-port))])))

(define current-test-case-around
  (make-parameter
   default-test-case-around
   (lambda (v)
     (if (procedure? v)
         v
         (raise-type-error 'current-test-case-around "procedure" v)))))

(define-syntax (test-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     (syntax/loc stx
       ((current-test-case-around)
        (lambda ()
          (parameterize
              ([current-check-handler raise]
               [current-check-around  check-around])
            (void)
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

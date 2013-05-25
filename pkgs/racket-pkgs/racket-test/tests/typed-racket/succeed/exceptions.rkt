#lang typed/racket

(: abort (Parameter (Any -> Nothing)))

(define abort (make-parameter (lambda (x) (error 'abort))))

(define-syntax (with-abort stx)
 (syntax-case stx ()
  ((_ body ...)
   #'(call/cc (lambda: ((k : (Any -> Nothing)))
                (parameterize ((abort k))
                  body ...))))))

(call-with-exception-handler
 (lambda (v) (displayln v) ((abort) v))
 (lambda ()
  (with-abort 2)
  (with-abort (raise 3))
  (with-abort (error 'foo))
  (with-abort (error 'foo "Seven"))
  (with-abort (error 'foo "Seven ~a" 5))
  (with-abort (raise-user-error 'foo))
  (with-abort (raise-user-error 'foo "Seven"))
  (with-abort (raise-user-error 'foo "Seven ~a" 5))
  (with-abort (raise-type-error 'slash "foo" 1))
  (with-abort (raise-type-error 'slash "foo" 1 #\a #\c))

  (with-abort (raise-mismatch-error 'er "foo" 2))

  (with-abort (raise-syntax-error #f "stx-err"))
  (with-abort (raise-syntax-error #f "stx-err" 45))
  (with-abort (raise-syntax-error #f "stx-err" 4 5))
  (with-abort (raise-syntax-error #f "stx-err" 4 5 (list #'stx)))

  (void)
  ))

(parameterize ((uncaught-exception-handler (lambda (x) ((abort) x)))
               (error-escape-handler (lambda () (void)))
               (error-display-handler (lambda: ((s : String) (e : Any)) (void)))
               (error-print-width 4)
               (error-print-context-length 10)
               (error-value->string-handler (lambda: ((v : Any) (n : Natural)) "str"))
               (error-print-source-location 'yes))
 (void))

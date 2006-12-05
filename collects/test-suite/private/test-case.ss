#|
This module provides a test-case macro for the test-case-box to expand into.
The test-case box does not immediatly expand into the body of the macro itself
because the macro is able to check the (syntax-local-context) of the invocation
to give better error messages when the test-case is not at the top level.
|#

(module test-case mzscheme
  
  (require-for-syntax (lib "shared.ss" "stepper" "private"))
  (provide test-case test-error-case)
  
  ;; STATUS : Abstract these two syntaxes and use string constant for the error  
  (define-syntax (test-case stx)
    (syntax-case stx ()
      [(_ test to-test-stx exp-stx record set-actuals)
       (case (syntax-local-context)
         [(module top-level)
          (stepper-syntax-property
           #`(define-values ()
               (let ([to-test-values (call-with-values
                                      (lambda () #,(stepper-syntax-property #`to-test-stx
                                                                            'stepper-test-suite-hint
                                                                            #t))
                                      list)]
                     [exp-values (call-with-values (lambda () exp-stx) list)])
                 (record (and (= (length to-test-values) (length exp-values))
                              (andmap test to-test-values exp-values)))
                 (set-actuals to-test-values)
                 (values)))
           'stepper-skipto
           (append 
            ;; define-values->body
            skipto/third
            ;; rhs of first binding of let-values: 
            skipto/second
            skipto/first
            skipto/second
            ;; 2nd arg of call-with-values application:
            skipto/cdr
            skipto/second
            ;; first (only) body of lambda:
            skipto/cddr
            skipto/first))]
         [else (raise-syntax-error #f
                                   "test case not at toplevel"
                                   (syntax/loc stx (test-case to-test-stx exp-stx)))])]))
  
  (define-syntax (test-error-case stx)
    (syntax-case stx ()
      [(_ to-test-stx exn-pred exn-handler record set-actuals)
       (case (syntax-local-context)
         [(module top-level)
          (stepper-syntax-property
           #'(define-values ()
               (with-handlers ([exn-pred
                                (lambda (v)
                                  (set-actuals (list v))
                                  (record (exn-handler v))
                                  (values))]
                               [void
                                (lambda (v)
                                  (set-actuals v)
                                  (record #f)
                                  (values))])
                 to-test-stx
                 (record #f)
                 (values)))
           'stepper-skipto
           `(,@skipto/third
             ;; with-handlers:
             ,@skipto/fourth
             ))]
         [else (raise-syntax-error #f
                                   "test case not at toplevel"
                                   (syntax/loc stx (test-case to-test-stx exp-stx)))])]))
  )

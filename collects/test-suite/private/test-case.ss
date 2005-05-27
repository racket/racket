#|
This module provides a test-case macro for the test-case-box to expand into.
The test-case box does not immediatly expand into the body of the macro itself
because the macro is able to check the (syntax-local-context) of the invocation
to give better error messages when the test-case is not at the top level.
|#

(module test-case mzscheme
  
  (provide test-case test-error-case)
  
  ;; STATUS : Abstract these two syntaxes and use string constant for the error  
  (define-syntax (test-case stx)
    (syntax-case stx ()
      [(_ test to-test-stx exp-stx record set-actuals)
       (case (syntax-local-context)
         [(module top-level)
          (syntax-property
           #`(define-values ()
               (let ([to-test-values (call-with-values
                                      (lambda () #,(syntax-property #`to-test-stx
                                                                    'stepper-test-suite-hint
                                                                    #t))
                                                       list)]
                     [exp-values (call-with-values (lambda () exp-stx) list)])
                 (record (and (= (length to-test-values) (length exp-values))
                              (andmap test to-test-values exp-values)))
                 (set-actuals to-test-values)
                 (values)))
           'stepper-skipto
           (list ;define-values
            syntax-e cdr cdr car 
            ; let-values
            syntax-e cdr car 
            ; clauses
            syntax-e car syntax-e cdr car
            ; call-with-values
            syntax-e cdr syntax-e cdr car 
            ; lambda
            syntax-e cdr cdr car
            ))]
         [else (raise-syntax-error #f
                                   "test case not at toplevel"
                                   (syntax/loc stx (test-case to-test-stx exp-stx)))])]))
  
  (define-syntax (test-error-case stx)
    (syntax-case stx ()
      [(_ to-test-stx exn-pred exn-handler record set-actuals)
       (case (syntax-local-context)
         [(module top-level)
          (syntax-property
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
           (list ;; define-values
            syntax-e cdr cdr car
            ;; with-handlers
            syntax-e cdr cdr cdr car
            ))]
         [else (raise-syntax-error #f
                                   "test case not at toplevel"
                                   (syntax/loc stx (test-case to-test-stx exp-stx)))])]))
  )

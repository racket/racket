(module java-tests scheme/base
    
  (require scheme/class
           (lib "etc.ss")
           (lib "display-java.ss" "profj"))
  (require "test-engine.scm"
           "test-display.scm"
           "test-info.scm"
           "test-coverage.scm")
    
  (define (java-test-maker test-info-class style) 
    (class* test-engine% ()
      
      (inherit initialize-test run-test)
      (inherit-field test-info test-display)
      
      (super-instantiate ())
      
      (field [tests null] 
             [test-objs null])
      
      (define/override (info-class) test-info-class)
      
      (define/public (install-tests tsts) (set! tests tsts))
      (define/public (get-info) 
        (unless test-info (send this setup-info style))
        test-info)
           
      (define/public (test-objects) test-objs)
      
      (define/augment (run)
        (for-each (lambda (t) (initialize-test t)) tests)
        (inner (void) run)
        (for-each (lambda (t) (run-test t)) tests))
                  
      ))
  
  (define (java-test test-info-class)
    (class* (java-test-maker test-info-class 'test-basic) ()
      
      (super-instantiate ())
      (inherit-field test-info test-objs)
      
      (define/augride (run-test test)
        (let ([test-name (car test)]
              [test-class (cadr test)]
              [test-src (caddr test)])
          (send test-info add-test-class test-name test-src) ;need to run constructor
          (let ([test-obj (make-object test-class)])
            (set! test-objs (cons test-obj test-objs))
            (for-each (lambda (tc) (run-testcase tc))
                      (send test-obj testMethods))
            (let ([tested-classes (send test-obj testedClasses)])
              (send test-info add-tests-info tested-classes 
                    (map (lambda (c) (send test-obj testedMethods c)) tested-classes)
                    (map (lambda (c) (send test-obj testedMethodsSrcs c)) tested-classes))))
          (send test-info complete-test)))
      
      (define/augride (run-testcase tc)
        (send test-info add-testcase (car tc) (car tc))
        ;put this in a with-handlers
        (let ([res ((cadr tc))])
          (send test-info complete-testcase res)))
      
      ))

  (define (java-examples test-info-class)
    (class* (java-test-maker test-info-class 'test-basic) ()
      (super-instantiate ())
      
      (inherit-field test-info test-objs)
      
      (define/augride (run-test test)
        (let ([test-name (car test)]
              [test-class (cadr test)]
              [test-src (caddr test)])
          (send test-info add-test-class test-name test-src)
          (let ([test-obj (make-object test-class)])
            (set! test-objs (cons test-obj test-objs))
            (with-handlers ((exn? (lambda (e) (raise e))))
              ((current-eval)
               #`(send #,test-obj #,(string->symbol (string-append test-name "-constructor")))))
            (for-each (lambda (tc) (run-testcase tc))
                      (build-testcases test-obj))
            (send test-info complete-test))))
                        
      (define/private (build-testcases object)
        (let ([methods (reverse (interface->method-names (object-interface object)))])
          (map (lambda (m) (list m
                                 (lambda () ((current-eval) #`(send #,object #,m)))
                                 #f))
               methods)))
      
      (define/augride (run-testcase tc)
        (cond 
          [(test-method? (car tc))
           (send test-info add-testcase (car tc) (car tc))
           (let ([res ((cadr tc))])
             (send test-info complete-testcase res))] ;insert with-handlers
          [(test-method-name? (car tc))
           (send test-info add-malformed-test (car tc))]
          [(close-to-test-name? (car tc))
           (send test-info add-nearly-testcase (car tc))]
          [else (void)]))
            
      (define (test-method? name)
        (and (test-method-name? name) (no-args? name)))
      
      (define (test-method-name? name)
        (regexp-match "^test" (symbol->string name)))
      
      (define (no-args? name)
        (not (regexp-match "-" (symbol->string name))))
      
      (define (close-to-test-name? name)
        (let ((n (symbol->string name)))
          (or (regexp-match "^tst" n)
              (regexp-match "^tet" n)
              (regexp-match "^Test" n)
              (regexp-match "^tes" n))))
     
      
      ))
  
  (define-struct test-stat (name src tests cases) #:mutable)
  (define-struct tests-data (c-name methods method-srcs))
  (define-struct testcase-stat (name src pass? checks) #:mutable)
  
  (define java-test-info%
    (class* test-info-base% ()
      (inherit add-test test-failed)
      
      (define test-class-stats null)
      
      (define current-testcase #f)
      (define current-test #f)
      
      (define/pubment (add-test-class name src)
        (set! current-test (make-test-stat name src null null))
        (inner (void) add-test-class name src))
      
      (define/public (add-tests-info tests test-methods test-method-srcs)
        (set-test-stat-tests! current-test 
                              (map make-tests-data tests test-methods test-method-srcs)))
      
      (define/pubment (complete-test)
        (set! test-class-stats (cons current-test test-class-stats))
        (inner (void) complete-test))
      (define/public (get-current-test) current-test)
      (define/public (get-test-results) test-class-stats)
      
      (define/pubment (add-testcase name src)
        (set! current-testcase (make-testcase-stat name src #t null))
        (add-test)
        (inner (void) add-testcase name src))
      
      (define/pubment (complete-testcase pass?)
        (set-testcase-stat-pass?! current-testcase pass?)
        (unless pass? (test-failed (get-current-testcase)))
        (set-test-stat-cases! current-test (cons current-testcase 
                                                (test-stat-cases current-test)))
        (inner (void) complete-testcase pass?))
      (define/public (get-current-testcase) current-testcase)
      
      (define/augment (check-failed msg src)
        (when current-testcase
          (set-testcase-stat-checks! 
           current-testcase
           (cons (make-failed-check src msg) (testcase-stat-checks current-testcase))))
        (inner (void) check-failed msg src))
      
      (define/public (format-value value)
        (make-java-snip value (make-format-style #t 'field #f)))
      
      (super-instantiate ())
      
      ))
  
  (define java-examples-info%
    (class* java-test-info% ()
      (define nearly-tests null)
      (define nearly-testcases null)
    
      (define/public (add-nearly-test name) (set! nearly-tests (cons name nearly-tests)))
      (define/public (add-nearly-testcase name) (set! nearly-testcases (cons name nearly-testcases)))
      (define/public (close-tests) nearly-tests)
      (define/public (close-testcases) nearly-testcases)
      
      (super-instantiate ())))    
  
  (define (analyzed-test-mixin% test-info-parent)
    (class* test-info-parent ()
      (inherit get-current-test get-current-testcase)
      (inherit-field analyses)
      
      (define/augment (add-test-class name src)
        (for-each (lambda (a) (send a register-test name src)) analyses)
        (inner (void) add-test-class name src))
      (define/augment (complete-test)
        (for-each (lambda (a) (send a de-register-test (test-stat-src (get-current-test)))) analyses)
        (inner (void) complete-test))
      (define/augment (add-testcase name src)
        (for-each (lambda (a) (send a register-testcase name src)) analyses)
        (inner (void) add-testcase name src))
      (define/augment (complete-testcase pass?)
        (for-each (lambda (a) (send a de-register-testcase (testcase-stat-src (get-current-testcase)))) analyses)
        (inner (void) complete-testcase pass?))
      
      (super-instantiate ())))
  
  (define java-test-display%
    (class* test-display% ()
      
      (super-instantiate ())
      (inherit next-line)
      
      (define/public (test-name) "tests")
      (define/public (testcase-name) "testcases")
      
      (define/pubment (insert-test-name editor test-stat src-editor)
        (send editor insert (test-stat-name test-stat))
        (inner (void) insert-test-name editor test-stat src-editor)
        (send editor insert "\n"))
      
      (define/pubment (insert-testcase-name editor testcase-stat src-editor)
        (send editor insert (format "~a ~a" 
                                    (testcase-stat-name testcase-stat)
                                    (if (testcase-stat-pass? testcase-stat) "succeeded!" "failed")))
        (inner (void) insert-testcase-name editor testcase-stat src-editor)
        (next-line editor))
      
      (define/augment (insert-test-results editor test-info src-editor)
        (inner (void) insert-test-results editor test-info src-editor)
        (insert-tests editor test-info src-editor)
        )
      
      (define/pubment (insert-tests editor test-info src-editor)
        (send editor insert (format "Ran the following ~a:\n" (send this test-name)))
        (for-each
         (lambda (test)
           (send editor insert "\n")
           (send this insert-test-name editor test src-editor)
           (unless (null? (test-stat-cases test))
             (let* ([run-tests (reverse (test-stat-cases test))]
                    [num-tests (length run-tests)]
                    [failed-tests (filter (compose not testcase-stat-pass?) run-tests)])
               (next-line editor)
               (send editor insert (format "Ran ~a ~a." num-tests (send this testcase-name)))
               (next-line editor)
               (if (null? failed-tests)
                   (send editor insert (format "All ~a passed!" (send this testcase-name)))
                   (send editor insert (format "~a of ~a ~a failed:"
                                               (length failed-tests) num-tests
                                               (send this testcase-name))))
               (next-line editor)
               (for-each 
                (lambda (testcase)
                  (send this insert-testcase-name editor testcase src-editor)
                  (cond
                    [(null? (testcase-stat-checks testcase))
                     (send editor insert "All checks succeeded!\n")]
                    [else 
                     (send this display-check-failures (testcase-stat-checks testcase)
                           editor test-info src-editor)])
                  (next-line editor))
                run-tests)
               (inner (void) insert-tests editor test-info src-editor))))
         (send test-info get-test-results)
         ))
      ))
  
  (define java-examples-display%
    (class* java-test-display% ()
      (super-instantiate ())
      
      (define/override (test-name) "Example classes")
      (define/override (testcase-name) "test methods")
      
      (define/augment (insert-tests editor test-info src-editor)
        (unless (null? (send test-info close-tests))
          (send editor insert "\n")
          (send editor insert "The following classes were not run, but are similar to example classes:\n")
          (for-each (lambda (name) (send editor insert (format "\t~a\n" name)))
                    (send test-info close-tests)))
        (inner (void) insert-tests editor test-info src-editor))
      ))
  
  (define (java-coverage-display-mixin parent)
    (class* parent ()
      
      (field (coverage-info #f))
      (inherit insert-covered-button)
      
      (define/augment (install-info t)
        (let ([info (send t extract-info (lambda (a) (is-a? a coverage-track%)))])
          (unless (null? info) (set! coverage-info (car info))))
        (inner (void) install-info t))
      
      (define/augment (insert-test-results editor test-info src-editor)
        (insert-covered-button editor coverage-info #f src-editor #f)
        (send editor insert "\n")
        (inner (void) insert-test-results editor test-info src-editor))
      
      (define/augment (insert-test-name editor test-stat src-editor)
        (insert-covered-button editor coverage-info (test-stat-src test-stat) src-editor #t)
        (send editor insert "\n")
        (for-each 
         (lambda (tested)
           (unless (send coverage-info covers-spans (tests-data-method-srcs tested))
             (send editor insert (format-uncovered-message (test-stat-name test-stat)
                                                           (tests-data-c-name tested)))
             (for-each (lambda (sub sub-span)
                         (if (send coverage-info covers-span sub-span)
                             (send editor insert (format-covered-sub sub))
                             (send editor insert (format-uncovered-sub sub))))
                       (tests-data-methods tested)
                       (tests-data-method-srcs tested))))
         (test-stat-tests test-stat))
        (inner (void) insert-test-name editor test-stat src-editor))
      
      (define (format-uncovered-message test tests)
        (format "test ~a failed to fully cover tested class ~a" test tests))
      (define (format-covered-sub method)
        (format "method ~a is fully covered" method))
      (define (format-uncovered-sub method)
        (format "method ~a is not fully covered" method))
        
      
      (define/augride (insert-testcase-name editor testcase-stat src-editor)
        (insert-covered-button editor coverage-info (testcase-stat-src testcase-stat) src-editor #t))
      
      (super-instantiate ())))
  
  (define java-test-base% (java-test (analyzed-test-mixin% java-test-info%)))
  (define java-test-graphics% java-test-display%)
  (define java-test-coverage-graphics% (java-coverage-display-mixin
                                        (test-coverage-button-mixin
                                         java-test-display%)))                                   
  
  (define java-examples-engine% (java-examples (analyzed-test-mixin% java-examples-info%)))
  (define java-examples-graphics% java-examples-display%)
  (define java-examples-coverage-graphics% (java-coverage-display-mixin
                                           (test-coverage-button-mixin
                                            java-examples-display%)))

  (provide java-test-base% java-test-graphics%  java-test-coverage-graphics%
           java-examples-engine% java-examples-graphics% java-examples-coverage-graphics%)
  
  )
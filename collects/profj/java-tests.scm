#lang scheme/base

(require scheme/class
         mzlib/etc
         profj/display-java
         (lib "test-engine.scm" "test-engine")
         (lib "test-display.scm" "test-engine")
         (lib "test-info.scm" "test-engine")
         (lib "test-coverage.scm" "test-engine"))

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
      (for ([t tests]) (initialize-test t))
      (inner (void) run)
      (for ([t tests]) (run-test t)))))

(define (java-test test-info-class)
  (class* (java-test-maker test-info-class 'test-basic) ()

    (super-instantiate ())
    (inherit-field test-info test-objs)

    (define/augride (run-test test)
      (let ([test-name (car test)]
            [test-class (cadr test)]
            [test-src (caddr test)])
        ;; need to run constructor
        (send test-info add-test-class test-name test-src)
        (let ([test-obj (make-object test-class)])
          (set! test-objs (cons test-obj test-objs))
          (for ([tc (send test-obj testMethods)]) (run-testcase tc))
          (let ([tested-classes (send test-obj testedClasses)])
            (send test-info add-tests-info tested-classes
                  (map (lambda (c) (send test-obj testedMethods c))
                       tested-classes)
                  (map (lambda (c) (send test-obj testedMethodsSrcs c))
                       tested-classes))))
          (send test-info complete-test)))

    (define/augride (run-testcase tc)
      (send test-info add-testcase (car tc) (car tc))
      ;; put this in a with-handlers
      (let ([res ((cadr tc))]) (send test-info complete-testcase res)))))

(define (java-examples test-info-class)
  (class* (java-test-maker test-info-class 'test-basic) ()
    (super-instantiate ())

    (inherit-field test-info test-objs)

    (define/augride (run-test test)
      (let ([test-name (car test)]
            [test-class (cadr test)]
            [test-src (caddr test)])
        (send test-info add-test-class test-name test-src)
        (send test-info add-inits test-name test-src)
        (let ([test-obj (make-object test-class)])
          (send test-info complete-testcase #t)
          (set! test-objs (cons test-obj test-objs))
          (with-handlers ((exn? (lambda (e) (raise e))))
            ((current-eval)
             #`(send #,test-obj
                     #,(string->symbol (string-append test-name
                                                      "-constructor")))))
          (for ([tc (build-testcases test-obj)]) (run-testcase tc))
          (send test-info complete-test))))

    (define/private (build-testcases object)
      (let ([methods (reverse (interface->method-names
                               (object-interface object)))])
        (map (lambda (m)
               (list m (lambda () ((current-eval) #`(send #,object #,m))) #f))
             methods)))

    (define/augride (run-testcase tc)
      (cond [(test-method? (car tc))
             (send test-info add-testcase (car tc) (car tc))
             (let ([res ((cadr tc))])
               (send test-info complete-testcase res))] ; insert with-handlers
            [(test-method-name? (car tc))
             (send test-info add-malformed-testcase (car tc))]
            [(close-to-test-name? (car tc))
             (send test-info add-nearly-testcase (car tc))]
            [else (void)]))

    (define (test-method? name)
      (and (test-method-name? name) (no-args? name)))

    (define (test-method-name? name)
      (regexp-match? #rx"^test" (symbol->string name)))

    (define (no-args? name)
      (not (regexp-match? #rx"-" (symbol->string name))))

    (define (close-to-test-name? name)
      (let ([n (symbol->string name)])
        (regexp-match? "^(?:tst|tet|Test|tes)" n)))))

;(make-test-stat String [U String Src] [listof tests-data] init-testcase-stat [listof tc-stat])
(define-struct test-stat (name src tests init cases) #:mutable)
(define-struct tests-data (c-name methods method-srcs))
;(make-tc-stat String [U String Src] [listof check-info])
(define-struct tc-stat (name src checks) #:mutable)
(define-struct (testcase-stat tc-stat) (pass?) #:mutable)
(define-struct (init-testcase-stat tc-stat) () #:mutable)

(define java-test-info%
  (class* test-info-base% ()
    (inherit add-test test-failed)

    (define test-class-stats null)

    (define current-testcase #f)
    (define current-test #f)

    (define/pubment (add-test-class name src)
      (set! current-test (make-test-stat name src null #f null))
      (inner (void) add-test-class name src))

    (define/public (add-tests-info tests test-methods test-method-srcs)
      (set-test-stat-tests! current-test
                            (map make-tests-data
                                 tests test-methods test-method-srcs)))

    (define/pubment (complete-test)
      (set! test-class-stats (cons current-test test-class-stats))
      (inner (void) complete-test))
    (define/public (get-current-test) current-test)
    (define/public (get-test-results) test-class-stats)

    ;add-testcase: (U string 'fields) (U string src) -> void
    ;adds testcase specific information to the info storage
    (define/pubment (add-testcase name src)
      (set! current-testcase (make-testcase-stat name src null #t))
      (add-test)
      (inner (void) add-testcase name src))
    
    (define/pubment (add-inits name src)
      (set! current-testcase (make-init-testcase-stat name src null))
      (inner (void) add-inits name src))

    (define/pubment (complete-testcase pass?)
      (cond
        [(testcase-stat? current-testcase)
         (set-testcase-stat-pass?! current-testcase pass?)
         (unless pass? (test-failed (get-current-testcase)))
         (set-test-stat-cases! current-test (cons current-testcase
                                                  (test-stat-cases current-test)))]
        [(init-testcase-stat? current-testcase)
         (set-test-stat-init! current-test current-testcase)])
      (inner (void) complete-testcase pass?))
    (define/public (get-current-testcase) current-testcase)

    (define/augment (check-failed msg src exn)
      (when current-testcase
        (set-tc-stat-checks!
         current-testcase
         (cons (make-failed-check msg exn)
               (tc-stat-checks current-testcase))))
      (inner (void) check-failed msg src exn))

    (define/public (format-value value)
      (make-java-snip value (make-format-style #t 'field #f)))

    (super-instantiate ())))

(define java-examples-info%
  (class* java-test-info% ()
    (define nearly-tests null)
    (define nearly-testcases null)
    (define malformed-testcases null)

    (define/public (add-nearly-test name)
      (set! nearly-tests (cons name nearly-tests)))
    (define/public (add-nearly-testcase name)
      (set! nearly-testcases (cons name nearly-testcases)))
    (define/public (add-malformed-testcase name)
      (set! malformed-testcases (cons name malformed-testcases)))
    (define/public (close-tests) nearly-tests)
    (define/public (close-testcases) nearly-testcases)
    (define/public (bad-testcases) malformed-testcases)

    (super-instantiate ())))

(define (analyzed-test-mixin% test-info-parent)
  (class* test-info-parent ()
    (inherit get-current-test get-current-testcase)
    (inherit-field analyses)

    (define/augment (add-test-class name src)
      (for ([a analyses]) (send a register-test name src))
      (inner (void) add-test-class name src))
    (define/augment (complete-test)
      (for ([a analyses])
        (send a de-register-test (test-stat-src (get-current-test))))
      (inner (void) complete-test))
    (define/augment (add-testcase name src)
      (for ([a analyses]) (send a register-testcase name src))
      (inner (void) add-testcase name src))
    (define/augment (complete-testcase pass?)
      (for ([a analyses])
        (send a de-register-testcase (tc-stat-src (get-current-testcase))))
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
      (cond
        [(testcase-stat? testcase-stat)
         (send editor insert (format "~a ~a"
                                     (tc-stat-name testcase-stat)
                                     (if (testcase-stat-pass? testcase-stat)
                                         "succeeded!" "failed.")))]
        [(init-testcase-stat? testcase-stat)
         (send editor insert (format "~a ~a" 
                                     (tc-stat-name testcase-stat)
                                     " contained failed checks."))]
        [else (void)])
      (inner (void) insert-testcase-name editor testcase-stat src-editor)
      (next-line editor))

    (define/augment (insert-test-results editor test-info src-editor)
      (inner (void) insert-test-results editor test-info src-editor)
      (insert-tests editor test-info src-editor))

    (define/pubment (insert-tests editor test-info src-editor)
      (send editor insert
            (format "Ran the following ~a:\n" (send this test-name)))
      (for ([test (send test-info get-test-results)])
        (send editor insert "\n")
        (send this insert-test-name editor test src-editor)
        (when (and (test-stat-init test)
                   (not (null? (tc-stat-checks (test-stat-init test)))))
          (send this insert-testcase-name editor (test-stat-init test) src-editor)
          (send this display-check-failures (tc-stat-checks (test-stat-init test))
                editor test-info src-editor)
          (next-line editor))
        (unless (null? (test-stat-cases test))
          (let* ([run-tests (reverse (test-stat-cases test))]
                 [num-tests (length run-tests)]
                 [failed-tests (filter (compose not testcase-stat-pass?)
                                       run-tests)])
            (next-line editor)
            (send editor insert
                  (format "Ran ~a ~a." num-tests (send this testcase-name)))
            (next-line editor)
            (send editor insert
                  (if (null? failed-tests)
                    (format "All ~a passed!" (send this testcase-name))
                    (format "~a of ~a ~a failed:"
                            (length failed-tests) num-tests
                            (send this testcase-name))))
            (next-line editor)
            (for ([testcase run-tests])
              (send this insert-testcase-name editor testcase src-editor)
              (if (null? (tc-stat-checks testcase))
                (send editor insert "All checks succeeded!\n")
                (send this display-check-failures
                      (tc-stat-checks testcase)
                      editor test-info src-editor))
              (next-line editor))
            (inner (void) insert-tests editor test-info src-editor)))))))

(define java-examples-display%
  (class* java-test-display% ()
    (super-instantiate ())

    (define/override (test-name) "Example classes")
    (define/override (testcase-name) "test methods")

    (define/augment (insert-tests editor test-info src-editor)
      (unless (null? (send test-info close-tests))
        (send editor insert "\n")
        (send editor insert "The following classes were not run, but are similar to example classes:\n")
        (for ([name (send test-info close-tests)])
          (send editor insert (format "\t~a\n" name))))
      (inner (void) insert-tests editor test-info src-editor))))

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
      (insert-covered-button editor coverage-info (test-stat-src test-stat)
                             src-editor #t)
      (send editor insert "\n")
      (for ([tested (test-stat-tests test-stat)])
        (unless (send coverage-info covers-spans
                      (tests-data-method-srcs tested))
          (send editor insert
                (format-uncovered-message (test-stat-name test-stat)
                                          (tests-data-c-name tested)))
          (for ([sub (tests-data-methods tested)]
                [sub-span (tests-data-method-srcs tested)])
            (send editor insert
                  (if (send coverage-info covers-span sub-span)
                    (format-covered-sub sub)
                    (format-uncovered-sub sub))))))
      (inner (void) insert-test-name editor test-stat src-editor))

    (define (format-uncovered-message test tests)
      (format "test ~a failed to fully cover tested class ~a" test tests))
    (define (format-covered-sub method)
      (format "method ~a is fully covered" method))
    (define (format-uncovered-sub method)
      (format "method ~a is not fully covered" method))


    (define/augride (insert-testcase-name editor testcase-stat src-editor)
      (insert-covered-button editor coverage-info
                             (tc-stat-src testcase-stat)
                             src-editor #t))

    (super-instantiate ())))

(define java-test-base% (java-test (analyzed-test-mixin% java-test-info%)))
(define java-test-graphics% java-test-display%)
(define java-test-coverage-graphics%
  (java-coverage-display-mixin
   (test-coverage-button-mixin java-test-display%)))

(define java-examples-engine%
  (java-examples (analyzed-test-mixin% java-examples-info%)))
(define java-examples-graphics% java-examples-display%)
(define java-examples-coverage-graphics%
  (java-coverage-display-mixin
   (test-coverage-button-mixin java-examples-display%)))

(provide java-test-base%
         java-test-graphics%
         java-test-coverage-graphics%
         java-examples-engine%
         java-examples-graphics%
         java-examples-coverage-graphics%)

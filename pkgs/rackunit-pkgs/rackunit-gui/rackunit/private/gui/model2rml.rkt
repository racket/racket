#lang racket/base
(require racket/class
         unstable/class-iop
         racket/gui/base
         racket/match
         racket/path
         mrlib/include-bitmap
         (prefix-in drlink: "drracket-ui.rkt")
         "interfaces.rkt"
         "config.rkt")
(provide model-renderer%
         output-icon)

(define (output-icon)
  (make-object image-snip% (include-bitmap "output-icon.png")))

(define top-align (make-object style-delta% 'change-alignment 'top))

(define model-renderer%
  (class object%
    (init-field controller)
    (init-field editor)
    (super-new)

    (define/public (render-model/short model)
      (cond [(is-a? model suite<%>) ;; (test-suite? test)
             (render-suite-short-form model)]
            [(is-a? model case<%>) ;; (test-case? test)
             (render-case-short-form model)]))

    (define/public (render-model/long model)
      (cond [(is-a? model suite<%>) ;; (test-suite? test)
             (render-suite-long-form model)]
            [(is-a? model case<%>) ;; (test-case? test)
             (render-case-long-form model)]))

    (define/private (put styles . texts)
      (send/apply editor insert/styles styles texts))

    (define/private (put+click styles clickback . texts)
      (send/apply editor insert/styles+click styles clickback texts))

    (define/private (blank)
      (send editor newline))

    (define/private (n-things number singular plural)
      (if (= 1 number)
          (format "~a ~a" number singular)
          (format "~a ~a" number plural)))
    
    (define/private (render-suite-long-form model)
      (let [(suite (send/i model suite<%> get-test))
            (children (send/i model suite<%> get-children))
            (parent (send/i model suite<%> get-parent))]
        (let* [(successes
                (filter (lambda (c) (send/i c result<%> success?)) children))
               (failures
                (filter (lambda (c) (send/i c result<%> failure?)) children))
               (errors
                (filter (lambda (c) (send/i c result<%> error?)) children))
               (unexecuted
                (filter (lambda (c) (not (send/i c result<%> finished?))) children))
               (finished? (send/i model suite<%> finished?))
               (num-tests (length children))
               (count-successes (length successes))
               (count-failures (length failures))
               (count-errors (length errors))
               (count-unexecuted (length unexecuted))
               (total-successes (send/i model suite<%> get-total-successes))
               (total-failures (send/i model suite<%> get-total-failures))
               (total-cases (send/i model suite<%> get-total-cases))
               (output? (send/i model suite<%> has-output?))
               (trash? (send/i model suite<%> has-trash?))]
          (put '(large italic bold) (send/i model suite<%> get-name) "\n")
          (when parent
            (put '() " (in ")
            (put+click '(clickback bold)
                       (lambda _ (send/i controller controller<%> set-selected-model parent))
                       (send/i parent result<%> get-name))
            (put '() ")\n"))
          (blank)
          (if (positive? total-cases)
              (begin (put '() (format "Total: ~a"
                                      (n-things total-successes "success" "successes")))
                     (when (positive? total-failures)
                       (put '() (format ", ~a"
                                        (n-things total-failures "failure" "failures"))))
                     (blank)
                     (blank)
                     (when trash?
                       (put (list top-align) (output-icon))
                       (put '() "Tests did not clean up resources.\n"))
                     (when output?
                       (put (list top-align) (output-icon))
                       (put '() "Tests produced output.\n"))
                     (when (or trash? output?)
                       (blank))
                     (when (positive? count-failures)
                       (put '(large)
                            (format "Failures (~a/~a)\n"
                                    count-failures
                                    num-tests))
                       (for-each (lambda (m) (render-model/short m)) failures)
                       (blank))
                     (when (positive? count-errors)
                       (put '(large)
                            (format "Errors (~a/~a)\n"
                                    count-errors
                                    num-tests))
                       (for-each (lambda (m) (render-model/short m)) errors)
                       (blank))
                     (when (positive? count-unexecuted)
                       (put '(large)
                            (format "Unexecuted (~a/~a)\n"
                                    (length unexecuted)
                                    num-tests))
                       (for-each (lambda (m) (render-model/short m)) unexecuted)
                       (blank))
                     (when (positive? count-successes)
                       (put '(large)
                            (format "Successes (~a/~a)\n"
                                    count-successes
                                    num-tests))
                       (for-each (lambda (m) (render-model/short m)) successes)
                       (blank))
                     (render-timing model))
              (if finished?
                  (put '()
                       "This test suite is empty.")
                  (put '(test-unexecuted)
                       "This test suite has not been executed."))))))

    (define/private (render-model-link model suite?)
      (let ([styles (if suite? '(clickback bold) '(clickback))])
        (put+click styles
                   (lambda _ (send/i controller controller<%> set-selected-model model))
                   (send/i model result<%> get-name))
        (when (or (send/i model result<%> has-output?)
                  (send/i model result<%> has-trash?))
          (put styles (output-icon)))))

    (define/private (render-suite-short-form model)
      (let [(suite (send/i model suite<%> get-test))]
        (let* [(children (send/i model suite<%> get-children))
               (successes
                (filter (lambda (c) (send/i c result<%> success?)) children))
               (failures
                (filter (lambda (c) (send/i c result<%> failure?)) children))
               (errors
                (filter (lambda (c) (send/i c result<%> error?)) children))
               (unexecuted
                (filter (lambda (c) (not (send/i c result<%> finished?))) children))
               (num-tests (length children))
               (total-failures (send/i model suite<%> get-total-failures))]
          (let [(style (if (and (null? failures) (null? errors)) 'normal 'red))]
            (render-model-link model #t)
            (if (send/i model suite<%> finished?)
                (when (positive? total-failures)
                  (put `(,style)
                       (format " (~a)"
                               (n-things total-failures "failure" "failures"))))
                (put '(test-unexecuted) " not yet finished"))
            (blank)))))

    (define/private (render-case-short-form model)
      (cond [(send/i model case<%> success?)
             (render-success-short-form model)]
            [(send/i model case<%> failure?)
             (render-failure-short-form model)]
            [(send/i model case<%> error?)
             (render-error-short-form model)]
            [(not (send/i model case<%> finished?))
             (render-not-executed-short-form model)]))

    (define/private (render-success-short-form model)
      (render-model-link model #f)
      (put '() " succeeded\n"))

    (define/private (render-failure-short-form model)
      (let* [(exn (send/i model case<%> get-result))
             (exnmsg (send/i model case<%> get-property prop:failure-message))
             (exnname (send/i model case<%> get-property prop:failure-assertion))]
        (render-model-link model #f)
        (put '() " failed")
        (when exnname
          (put '() " on ")
          (put '(fail-type) (format "~a" exnname)))
        (when exnmsg
          (put '() " with message: ")
          (put '(exn-message) exnmsg))
        (blank)))

    (define/private (render-error-short-form model)
      (let [(exn (send/i model case<%> get-result))]
        (render-model-link model #f)
        (cond [(exn? exn)
               (put '() " threw an exception of type ")
               (put '(exn-type) (format "~a" (object-name exn)))
               (put '() " with message: ")
               (put '(exn-message) (exn-message exn))]
              [else
               (put '() (format " raised the value ~e" exn))])
        (blank)))

    (define/private (render-not-executed-short-form model)
      (render-model-link model #f)
      (put '(test-unexecuted) " has not been executed\n"))

    (define/private (render-case-long-form model)
      (cond [(send/i model case<%> success?)
             (render-success-long-form model)]
            [(send/i model case<%> failure?)
             (render-failure-long-form model)]
            [(send/i model case<%> error?)
             (render-error-long-form model)]
            [(not (send/i model case<%> finished?))
             (render-not-executed-long-form model)])
      (when (send/i model case<%> finished?)
        (render-timing model)
        (render-trash model)
        (render-output model)))

    (define/private (render-model-link* model suite?)
      (let [(parent (send/i model result<%> get-parent))]
        (let ([styles (if suite? '(bold) '())])
          (put `(large italic ,@styles)
               (send/i model result<%> get-name))
          (blank)
          (when parent
            (put '() "  in ")
            (put+click `(clickback bold)
                       (lambda _ (send/i controller controller<%> set-selected-model parent))
                       (send/i parent result<%> get-name))
            (blank))
          (blank))))

    (define/private (render-success-long-form model)
      (render-model-link* model #f)
      (put '() "The test case succeeded.\n\n"))

    (define/private (render-failure-long-form model)
      (render-model-link* model #f)
      (let* [(exn (send/i model case<%> get-result))
             (failure-msgs (send/i model case<%> get-property-set prop:failure-message))
             (messages
              (if (string? (exn-message exn))
                  (cons (exn-message exn) failure-msgs)
                  failure-msgs))
             (exnname (send/i model case<%> get-property prop:failure-assertion))
             (exnlocs
              (send/i model case<%> get-property-set prop:failure-location))
             (expecteds (send/i model case<%> get-property-set 'expected))
             (actuals (send/i model case<%> get-property-set 'actual))
             (params (send/i model case<%> get-property prop:failure-parameters))
             (other-properties
              (filter (lambda (p) (not (known-property? (car p))))
                      (send/i model case<%> get-all-properties)))
             (exn2 (send/i model case<%> get-property 'exception))]
        (put '() "The test case failed on ")
        (put '(fail-type) (format "~a" exnname))
        (put '() ".\n\n")
        (render-source-location "Check location" exnlocs)
        (render-backtrace-link "Backtrace of check failure:" exn)
        (render-messages messages)
        (if (and (pair? expecteds) (pair? actuals))
            (render-expected+actual (car expecteds) (car actuals))
            (render-parameters params))
        (render-embedded-exception exn2)
        (render-other-properties other-properties)
        ))
    
    (define/private (render-error-long-form model)
      (render-model-link* model #f)
      (let [(exn (send/i model case<%> get-result))]
        (cond [(exn? exn)
               (put '() "The test case threw an exception of type ")
               (put '(exn-type) (format "~a" (object-name exn)))
               (put '() ".\n\n")
               (render-backtrace-link "Exception backtrace:" exn)
               (render-messages (list (exn-message exn)))
               (render-value "Exception value:\n" exn)]
              [else
               (put '() "The test raised a value that was not "
                    "an instance of an exception struct.\n\n")
               (render-value "Raised value:\n" exn)])))

    (define/private (render-not-executed-long-form model)
      (render-model-link* model #f)
      (put '(test-unexecuted)
           "The test case has not been executed."))

    (define/private (render-value text value)
      (put '() text)
      (render-value-box value)
      (blank))

    (define/private (render-value-box value)
      (send editor insert-wide-box
            (lambda (editor)
              (send editor insert/styles '(value) (format "~v" value)))))

    (define/private (render-value-box/display value)
      (send editor insert-wide-box
            (lambda (editor)
              (send editor insert/styles '(value) (format "~a" value)))))

    (define/private (render-messages messages)
      (when (pair? messages)
        (put '() "Message:\n")
        (for-each
         (lambda (message)
           (send editor insert-wide-box
                 (lambda (editor)
                   (send editor insert/styles '(exn-message) message))))
         messages)
        (blank)))

    (define/private (render-expected+actual expected actual)
      (put '() "Actual:\n")
      (render-value-box actual)
      (put '() "Expected:\n")
      (render-value-box expected)
      (blank))
    
    (define/private (render-parameters parameters)
      (when (and parameters (pair? parameters))
        (put '() "Parameters:\n")
        (for-each (lambda (parameter) (render-value-box parameter))
                  parameters)
        (blank)))

    (define/private (render-other-properties properties)
      (when (pair? properties)
        (put '() "Additional information:\n")
        (for-each (lambda (p) (render-other-property (car p) (cdr p)))
                  properties)
        (blank)))

    (define/private (render-other-property key value)
      (put '() (format "key ~s:" key))
      (when (exn? value)
        (inline-backtrace-link "" value))
      (put '() "\n")
      (render-value-box value))

    (define/private (render-embedded-exception exn)
      (when (exn? exn)
        (put '() "Received exception")
        (inline-backtrace-link "" exn)
        (put '() ":\n")
        (render-value-box exn)
        (blank)))

    (define/private (render-source-location label location-reps)
      (when (pair? location-reps)
        (let* ([location-reps (reverse location-reps)]
               [rep0 (car location-reps)]
               [reps (cdr location-reps)])
          (put '() label ": ")
          (inline-source-location/1 rep0)
          (blank)
          (when (pair? reps)
            (put '() "Inner locations: ")
            (for-each (lambda (r) (inline-source-location/1 r)) reps)
            (blank))
          (blank)))
      (unless (pair? location-reps)
        (put '() label " is not available.")
        (blank)
        (blank)))

    (define/private (inline-source-location/1 location)
      (match location
        [(list src line col pos span)
         (let* ([file-name (resolve-source-file src)]
                [short-file-name
                 (if (or (path? file-name) (string? file-name))
                     (path->string (file-name-from-path file-name))
                     file-name)]
                [source-location (format "~a:~a:~a" short-file-name line col)])
           (inline-source-location/h source-location src pos span))]
        [_ (put '() "not usable")]))

    (define/private (inline-source-location/h source-location src pos span)
      (if (and src (drlink:can-show-source?))
          (put+click '(clickback)
                     (lambda _ (drlink:show-source src pos span))
                     source-location)
          (put '() source-location)))

    (define/private (render-backtrace-link text exn)
      (when (exn? exn)
        (if (drlink:has-backtrace? exn)
            (inline-backtrace-link text exn)
            (begin (put '() text)
                   (put '() " not available")))
        (blank)
        (blank)))

    (define/private (inline-backtrace-link text exn)
      (when (drlink:has-backtrace? exn)
        (put '() text))
      (when (drlink:has-errortrace-backtrace? exn)
        (put '() " ")
        (put+click '(clickback)
                   (lambda _ (drlink:show-errortrace-backtrace exn))
                   "[from DrRacket]"))
      (when (drlink:has-primitive-backtrace? exn)
        (put '() " ")
        (put+click '(clickback)
                   (lambda _ (drlink:show-primitive-backtrace exn))
                   "[from racket]")))

    (define/private (render-output model)
      (let [(output (send/i model case<%> get-output))]
        (when (pair? output)
          (put '() "Output:\n")
          (send editor insert-wide-box
                (lambda (editor)
                  (for ([mode+text output])
                    (let ([styles
                           (if (eq? (car mode+text) 'error)
                               '(red italic)
                               '(darkblue))]
                          [buf (apply bytes-append
                                      (reverse (cdr mode+text)))])
                      (send editor insert/styles styles
                            (bytes->string/utf-8 buf #\?))))))
          (blank))))

    (define/private (render-timing model)
      (let [(timing (send/i model result<%> get-timing))]
        (when timing
          (let ([cpu (car timing)]
                [real (cadr timing)]
                [gc (caddr timing)])
            (put '() "Timing:\n")
            (put '() (format "cpu: ~a; real: ~a; gc: ~a\n\n"
                             cpu real gc))))))

    (define/private (render-trash model)
      (let ([trash (send/i model case<%> get-trash)])
        (when (pair? trash)
          (put '() "Test did not clean up resources:\n")
          (for-each (lambda (t) (render-value-box/display t)) trash)
          (blank))))

    (define/private (resolve-source-file src)
      (or (and (is-a? src editor<%>)
               (let* [(tmp?-box (box #t))
                      (filename (send src get-filename tmp?-box))]
                 (if (and filename (not (unbox tmp?-box)))
                     filename
                     #f)))
          (cond [(path? src) (path->string src)]
                [(string? src) src]
                [else 'unknown])))
    ))

(module testing mzscheme

  (require (lib "teachprims.ss" "lang" "private")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "pretty.ss")
           (lib "pconvert.ss")
           (lib "class.ss"))
  
  (require-for-syntax (only (lib "list.ss") foldr)
                      (lib "shared.ss" "stepper" "private"))
  
  (provide 
   check-expect ;; syntax : (check-expect <expression> <expression>)
   check-within ;; syntax : (check-within <expression> <expression> <expression>)
   check-error  ;; syntax : (check-error <expression> <expression>)
   generate-report ;; -> true
   )
  
  ;; slap a bunch of syntax-properties related to the stepper on the body 
  ;; expressions of check-expect forms.
  (define-for-syntax (add-stepper-properties tag exp)
    (foldr (lambda (pr exp)
             (stepper-syntax-property exp (car pr) (cadr pr)))
           exp
           `((stepper-hint ,tag)
             (stepper-no-retval-wrap #t)
             (stepper-dont-show-reduction #t)
             (stepper-render-completed-as ,#`(finished-test-case)))))
  
  (define INEXACT-NUMBERS-FMT
    "check-expect cannot compare inexact numbers. Try (check-within test ~a range).")
  (define CHECK-ERROR-STR-FMT
    "check-error requires a string for the second argument, representing the expected error message. Given ~s")
  (define CHECK-WITHIN-INEXACT-FMT
    "check-within requires an inexact number for the range. ~a is not inexact.")

  (define-for-syntax CHECK-EXPECT-STR
    "check-expect requires two expressions. Try (check-expect test expected).")
  (define-for-syntax CHECK-ERROR-STR
    "check-error requires two expressions. Try (check-error test message).")
  (define-for-syntax CHECK-WITHIN-STR
    "check-within requires three expressions. Try (check-within test expected range).")
  
  (define-for-syntax CHECK-EXPECT-DEFN-STR
    "check-expect cannot be used as an expression")
  (define-for-syntax CHECK-WITHIN-DEFN-STR
    "check-within cannot be used as an expression")
  (define-for-syntax CHECK-ERROR-DEFN-STR
    "check-error cannot be used as an expression")

  ;(make-src (U editor file-name) int int int)
  (define-struct src (file line col pos span))
  
  (define-struct check-fail (src))

  ;(make-unexpected-error src string)
  (define-struct (unexpected-error check-fail) (expected message))
  ;(make-unequal src scheme-val scheme-val)
  (define-struct (unequal check-fail) (test actual))
  ;(make-outofrange src scheme-val scheme-val inexact)
  (define-struct (outofrange check-fail) (test actual range))
  ;(make-incorrect-error src string)
  (define-struct (incorrect-error check-fail) (expected message))
  ;(make-expected-error src string scheme-val)
  (define-struct (expected-error check-fail) (message value))
  
  (define-syntax (check-expect stx)
    (syntax-case stx ()
      ((_ test actual)
       (not (eq? (syntax-local-context) 'expression))
       (quasisyntax/loc stx
         (define #,(gensym 'test)
           #,(add-stepper-properties
              'comes-from-check-expect
              #`(check-values-expected
                 (lambda () test) actual (make-src #,@(list (syntax-source stx)
                                                            (syntax-line stx)
                                                            (syntax-column stx)
                                                            (syntax-position stx)
                                                            (syntax-span stx))))))))
      ((_ test)
       (not (eq? (syntax-local-context) 'expression))
       (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx))
      ((_ test actual extra ...)
       (not (eq? (syntax-local-context) 'expression))
       (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx))
      ((_ test ...)
       (eq? (syntax-local-context) 'expression)
       (raise-syntax-error 'check-expect CHECK-EXPECT-DEFN-STR stx))))

  ;check-values-expected: (-> scheme-val) scheme-val src -> void
  (define (check-values-expected test actual src)
    (error-check (lambda (v) (if (number? v) (exact? v) #t))
                 actual INEXACT-NUMBERS-FMT)
    (update-num-checks)
    (run-and-check (lambda (v1 v2 _) (beginner-equal? v1 v2))
                   (lambda (src v1 v2 _) (make-unequal src v1 v2))
                   test actual #f src))
  
  (define-syntax (check-within stx)
    (syntax-case stx ()
      ((_ test actual within)
       (not (eq? (syntax-local-context) 'expression))
       (quasisyntax/loc stx
         (define #,(gensym 'test-within)
           #,(add-stepper-properties
              `comes-from-check-within
              #`(check-values-within (lambda () test) actual within
                                      (make-src #,@(list (syntax-source stx)
                                                         (syntax-line stx)
                                                         (syntax-column stx)
                                                         (syntax-position stx)
                                                         (syntax-span stx))))))))
      ((_ test actual)
       (not (eq? (syntax-local-context) 'expression))
       (raise-syntax-error 'check-within CHECK-WITHIN-STR stx))
      ((_ test)
       (not (eq? (syntax-local-context) 'expression))
       (raise-syntax-error 'check-within CHECK-WITHIN-STR stx))
      ((_ test actual within extra ...)
       (not (eq? (syntax-local-context) 'expression))
       (raise-syntax-error 'check-within CHECK-WITHIN-STR stx))
      ((_ test ...)
       (eq? (syntax-local-context) 'expression)
       (raise-syntax-error 'check-within CHECK-WITHIN-DEFN-STR stx))))

  (define (check-values-within test actual within src)
    (error-check number? within CHECK-WITHIN-INEXACT-FMT)
    (update-num-checks)
    (run-and-check beginner-equal~? make-outofrange test actual within src))

  (define-syntax (check-error stx)
    (syntax-case stx ()
      ((_ test error)
       (not (eq? (syntax-local-context) 'expression))
       (quasisyntax/loc stx
         (define #,(gensym 'test-error)
           #,(add-stepper-properties
              `comes-from-check-error
              #`(check-values-error (lambda () test) error (make-src #,@(list (syntax-source stx)
                                                                              (syntax-line stx)
                                                                              (syntax-column stx)
                                                                              (syntax-position stx)
                                                                              (syntax-span stx))))))))
      ((_ test)
       (not (eq? (syntax-local-context) 'expression))
       (raise-syntax-error 'check-error CHECK-ERROR-STR stx))
      ((_ test ...)
       (eq? (syntax-local-context) 'expression)
       (raise-syntax-error 'check-error CHECK-ERROR-DEFN-STR stx))))

  (define (check-values-error test error src)
    (error-check string? error CHECK-ERROR-STR-FMT)
    (update-num-checks)
    (let ([result (with-handlers ((exn? 
                                   (lambda (e) 
                                     (or (equal? (exn-message e) error)
                                         (make-incorrect-error src error (exn-message e))))))
                    (let ([test-val (test)])
                      (make-expected-error src error test-val)))])
      (when (check-fail? result) (update-failed-checks result))))

  (define (error-check pred? actual fmt)
    (unless (pred? actual)
      (raise (make-exn:fail:contract (format fmt actual)
                                     (current-continuation-marks)))))
  
  

  
  
  ;run-and-check: (scheme-val scheme-val scheme-val -> boolean) 
  ;               (scheme-val scheme-val scheme-val -> check-fail) 
  ;               ( -> scheme-val) scheme-val scheme-val -> void
  (define (run-and-check check maker test expect range src)
    (let ([result 
           (with-handlers ((exn? (lambda (e) (make-unexpected-error src expect (exn-message e)))))
             (let ([test-val (test)])
               (or (check test-val expect range)
                   (maker src test-val expect range))))])
      (when (check-fail? result) (update-failed-checks result))))
  
  (define (update-num-checks) (set! num-checks (add1 num-checks)))  
  (define num-checks 0)
  
  (define failed-check null)
  (define (update-failed-checks failure) (set! failed-check (cons failure failed-check)))

  (define (generate-report)
    (let* ([num-failed-tests (length failed-check)]
           [my-text (new (editor:standard-style-list-mixin text%))]
           [my-frame (new frame% [label "Test Results"][width 300] [height 200])]
           [my-editor (new editor-canvas% [editor my-text] [parent my-frame]
                           [style '(auto-hscroll auto-vscroll)])])
      (send my-text insert 
            (format "Recorded ~a check~a. ~a"
                    num-checks
                    (if (= 1 num-checks) "" "s")
                    (if (= num-failed-tests 0)
                        "All checks succeeded!"
                        (format "~a check~a failed."
                                num-failed-tests (if (= 1 num-failed-tests) "" "s")))))
      (unless (null? failed-check)
        (send my-text insert "\n")
        (for-each (lambda (f) (report-check-failure f my-text)) 
                  (reverse failed-check))
        (send my-frame resize 
              (min (+ 300 (* 5 (send my-text line-end-position num-failed-tests #f))) 1000)
              (min (+ 200 (* 5 num-failed-tests)) 1000)))
      (send my-text move-position 'home)
      (send my-text lock #t)
      (send my-frame show #t)
      #t))
    
  
  (define (report-check-failure fail text)
    (make-link text (check-fail-src fail))
    (send text insert "\n   ")
    (cond
      [(unexpected-error? fail)
       (send text insert "check encountered the following error instead of the expected value, ")
       (insert-value text (unexpected-error-expected fail))
       (send text insert (format ". ~n   :: ~a~n" (unexpected-error-message fail)))]
      [(unequal? fail)
       (send text insert "Actual value ")
       (insert-value text (unequal-test fail))
       (send text insert " differs from ")
       (insert-value text (unequal-actual fail))
       (send text insert ", the expected value.\n")]
      [(outofrange? fail)
       (send text insert "Actual value ")
       (insert-value text (outofrange-test fail))
       (send text insert (format " is not within ~v of expected value " (outofrange-range fail)))
       (insert-value text (outofrange-actual fail))
       (send text insert ".\n")]
      [(incorrect-error? fail) 
       (send text insert
             (format "check-error encountered the following error instead of the expected ~a~n   :: ~a ~n"
                     (incorrect-error-expected fail) (incorrect-error-message fail)))]
      [(expected-error? fail) 
       (send text insert "check-error expected the following error, but instead received the value ")
       (insert-value text (expected-error-value fail))
       (send text insert (format ".~n ~a~n" (expected-error-message fail)))]))
  
  (define (insert-value text value)
    (send text insert
          (cond
            [(is-a? value snip%)
             (send value set-style (send (send text get-style-list)
                                   find-named-style "Standard"))
             value]
            [(or (pair? value) (struct? value))
             (parameterize ([constructor-style-printing #t]
                            [pretty-print-columns 40])
               (let* ([text* (new (editor:standard-style-list-mixin text%))]
                      [text-snip (new editor-snip% [editor text*])])
                 (pretty-print (print-convert value) (open-output-text-editor text*))
                 (send text* lock #t)
                 (send text-snip set-style (send (send text get-style-list)
                                                 find-named-style "Standard"))
                 text-snip))]
            [else (format "~v" value)])))
  
  ;make-link: text% (listof (U string snip%)) src -> void
  (define (make-link text dest)
    (let ((start (send text get-end-position)))
      (send text insert "check failed ")
      (send text insert (format-src dest))
      (send text set-clickback 
            start (send text get-end-position)
            (lambda (t s e)
              (open-and-highlight-in-file dest))
            #f #f)
      (let ((end (send text get-end-position))
            (c (new style-delta%)))
        (send text insert " ")
        (send text change-style (make-object style-delta% 'change-underline #t)
              start end #f)
        (send c set-delta-foreground "royalblue")
        (send text change-style c start end #f))))
    
  (define (open-and-highlight-in-file srcloc)
    (let* ([position (src-pos srcloc)]
           [span (src-span srcloc)]
           [rep/ed (get-editor srcloc #t)])
      (when rep/ed
        (let ((highlight
               (lambda ()
                 (send (car rep/ed) highlight-error (cadr rep/ed) position (+ position span)))))
          (queue-callback highlight)))))
  
  (define (get-editor src rep?)
    (let* ([source (src-file src)]
           [frame (cond
                    [(path? source) (handler:edit-file source)]
                    [(is-a? source editor<%>)
                     (let ([canvas (send source get-canvas)])
                       (and canvas
                            (send canvas get-top-level-window)))])]
           [editor (cond
                     [(path? source)
                      (cond
                        [(and frame (is-a? frame #;drscheme:unit:frame<%>))
                         (send frame get-definitions-text)]
                        [(and frame (is-a? frame frame:editor<%>))
                         (send frame get-editor)]
                        [else #f])]
                     [(is-a? source editor<%>) source])]
           [rep (and frame 
                     #;(is-a? frame drscheme:unit:frame%)
                     (send frame get-interactions-text))])
      (when frame 
        (unless (send frame is-shown?) (send frame show #t)))
      (if (and rep? rep editor) 
          (list rep editor)
          (and rep editor))))
  
  (define (format-src src)
    (string-append (cond
                     ((path? (src-file src)) (string-append "in " (path->string (src-file src)) " at "))
                     ((is-a? (src-file src) editor<%>) "at "))
                   "line " (number->string (src-line src))
                   " column " (number->string (src-col src))))
  
  
  )
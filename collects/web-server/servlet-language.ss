(module servlet-language mzscheme
  (require (lib "class.ss")
           (lib "tool.ss" "drscheme")
           ;(lib "mred.ss" "mred")
           (lib "unitsig.ss"))

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      (define (phase1) (void))
      (define (phase2)
        (add-servlet-language))

      ;; add-servlet-language : -> void
      ;; adds the servlet language to drscheme
      (define (add-servlet-language)
        (define servlet-language%
          (srvlt-lang-mixin
           ((drscheme:language:get-default-mixin)
            (drscheme:language:module-based-language->language-mixin
             (drscheme:language:simple-module-based-language->module-based-language-mixin
              drscheme:language:simple-module-based-language%)))))
        (drscheme:language-configuration:add-language
         (instantiate servlet-language% ())))

      (define servlet-language<%>
        (interface ()
          ))

      ;; srvlt-lang-mixin : (implements drscheme:language:language<%>)
      ;;                  -> (implements drscheme:language:language<%>)
      (define (srvlt-lang-mixin %)
        (class* % (servlet-language<%>)

          (field [program-results #f])

          (define/override (on-execute settings run-in-user-thread)
            (set! program-results #f)
            (super on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (error-display-handler
                (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
               (current-eval
                (drscheme:debug:make-debug-eval-handler (current-eval)))
               (with-handlers ([void (lambda (x)  (printf "~a~n" (exn-message x)))])
                 (eval '(define start-servlet (dynamic-require '(lib "servlet-startup.ss" "web-server") 'start-servlet)))
                 (eval '(define raw-servlet->unit/sig (dynamic-require '(lib "servlet-startup.ss" "web-server") 'raw-servlet->unit/sig)))
                 (eval '(define create-module-servlet (dynamic-require '(lib "servlet-startup.ss" "web-server") 'create-module-servlet)))))))

          (define/override (front-end/complete-program input settings teachpack-cache)
            (let ([super-thunk (super front-end/complete-program input settings teachpack-cache)])
              (unless program-results
                (let loop ([continue-with-results
                            (lambda (rslts)
                              (set! program-results rslts))]
                           [res-cadr #f])
                  (let ([res-car (super-thunk)])
                    (cond
                      ;; end of empty definitions window
                      [(and (eof-object? res-car)
                            (eof-object? res-cadr))
                       (continue-with-results '())]

                      ;; end of non-empty definitions window
                      [(and (eof-object? res-car) res-cadr)
                       ;(continue-with-results (list #`(ignore-it #,res-cadr)))
                       (continue-with-results (list #`(start-servlet
                                                       (raw-servlet->unit/sig #,res-cadr))))
                       ]

                      ;; middle
                      [res-cadr
                       (loop
                        (lambda (rslts)
                          (continue-with-results (cons res-cadr rslts)))
                        res-car)]

                      ;; start of definitions window
                      [else
                       (loop continue-with-results res-car)])))))
            (lambda ()
              (if (null? program-results)
                  eof
                  (begin0
                    (car program-results)
                    (set! program-results (cdr program-results)))))
              )

          (super-instantiate ()
            (module '(lib "plt-mred.ss" "lang"))

            ;; GregP: when you settle on a name, use string-constant here
            (language-position (list "Web Server" "Advanced Servlet"))

            ;; GregP: figure out what these language-numbers are
            ;(language-numbers (list -1000 1000))
            ))

        )))
  )

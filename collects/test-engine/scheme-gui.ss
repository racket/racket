(module scheme-gui scheme/base
  
  (require mred framework scheme/class 
           mzlib/pconvert mzlib/pretty)
  
  (require (except-in "scheme-tests.ss" test) "test-display.scm")
  
  (define (format-value value)
    (cond
      [(is-a? value snip%) value]
      [(or (pair? value) (struct? value))
       (parameterize ([constructor-style-printing #t]
                      [pretty-print-columns 40])
         (let* ([text* (new (editor:standard-style-list-mixin text%))]
                [text-snip (new editor-snip% [editor text*])])
           (pretty-print (print-convert value) (open-output-text-editor text*))
           (send text* lock #t)
           text-snip))]
      [else (format "~v" value)]))
  
  (define (test) (run-tests) (pop-up))
  
  (define (pop-up)
    (let ([test-info (namespace-variable-value 'test~object #f builder (current-namespace))])
      (parameterize ([scheme-test-format format-value])
        (and test-info
             (send test-info refine-display-class test-display%)
             (send test-info setup-display #f #f)
             (send test-info summarize-results (current-output-port))))))
  
  (provide test format-value (all-from-out "scheme-tests.ss"))
  
  )

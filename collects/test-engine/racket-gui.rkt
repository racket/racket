(module scheme-gui scheme/base
  
  (require mred framework scheme/class 
           mzlib/pconvert mzlib/pretty
	   (for-syntax scheme/base))
  
  (require (except-in "scheme-tests.rkt" test) "test-display.scm")
  
  (define (make-formatter printer)
    (lambda (value)
      (let* ([text* (new (editor:standard-style-list-mixin text%))]
             [text-snip (new editor-snip% [editor text*])])
        (printer value (open-output-text-editor text* 0))
        (send text* delete (send text* get-end-position) 'back)
        (send text* lock #t) 
        text-snip)))
  
  (define (format-value value)
    (parameterize ([constructor-style-printing #t]
                   [pretty-print-columns 40])
      (make-formatter (lambda (v o) (pretty-print (print-convert v) o)))))
  
  #;(define (format-value value)
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
  
  (define (test*)
    (printf "test*\n")
    (run-tests)
    (pop-up))
  
  (define-syntax (test stx) 
    (syntax-case stx ()
      [(_)
       (syntax-property
	#'(test*)
	'test-call #t)]))

  (define (pop-up)
    (let ([test-info (namespace-variable-value 'test~object #f builder (current-namespace))])
      (parameterize ([test-format format-value])
        (and test-info
             (send test-info refine-display-class test-display%)
             (send test-info setup-display #f #f)
             (printf "calling summarize-results method\n")
             (send test-info summarize-results (current-output-port))))))
  
  (provide test format-value make-formatter (all-from-out "scheme-tests.rkt"))
  
  )

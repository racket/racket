
(load-relative "loadtest.rktl")

(Section 'syntax/....)

(require (for-syntax syntax/template))

(let ()
  (define-syntax (a-template-test stx)
    (syntax-case stx ()
      [(_ tmpl)
       (let ([v (transform-template #'tmpl
                           #:save (lambda (stx) stx)
                           #:restore-stx (lambda (v stx datum)
                                           (datum->syntax stx datum stx stx stx)))])
         v)]))

  (test '(1 #s(x "a" 1/2 8 9)
            (2 3) 
            (#s(y 8) #s(y 9))
            #(3 4 8 9)
            . 6)
        syntax->datum
        (with-syntax ([(w ...) #'(8 9)])
          (a-template-test (1 #s(x "a" 1/2 w ...) 
                              (2 3) 
                              (#s(y w) ...)
                              #(3 4 w ...)
                              . 6)))))

(report-errs)

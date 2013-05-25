(module test-macro mzscheme
  
  (require mzlib/etc)
  
  (provide test)
  
  ;; test: (lambda (a?) ((a? a? . -> . boolean?) a? a? . -> . (void))
  ;; tests to see if the expression is true and prints and error if it's not
  (define-syntax test
    (syntax-rules (identity)
      ((_ test actual expected)
       (let ([result
              (with-handlers
                  ([exn? identity])
                actual)])
         (and (not (exn? result))
              (test result expected))))))
  )

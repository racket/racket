(define-syntax test-error 
  (lambda (stx)
    (syntax-case stx ()
      [(_ form ...)
       (syntax
        (with-handlers ([exn? (lambda (e) 
                                (printf "~a~n" (exn-message e))
                                #t)])
          form ...
          #f))])))


#| Tests: 
(not (test-error 1 2 3))
(test-error (/ 1 0) 2 3)
|#
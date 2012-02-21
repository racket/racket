(module utils (lib "eopl.ss" "eopl")

  ;; a very simple macro for inline testing

  (provide equal?? report-unit-tests-completed)

  ;; simple-minded magic for tests
  (define-syntax equal??
    (syntax-rules ()
      ((_ x y)
       (let ((x^ x) (y^ y))
         (if (not (equal? x y))
           (eopl:error 'equal??
             "~s is not equal to ~s" 'x 'y))))))

  (define report-unit-tests-completed
    (lambda (fn-name)
      (eopl:printf "unit tests completed: ~s~%" fn-name)))

)
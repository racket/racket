(module defun-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "defun.ss" "web-server" "lang")
           (lib "util.ss" "web-server" "lang"))
  (provide defun-tests)
  
  (define-syntax vwrap
    (syntax-rules ()
      [(_ e)
       (call-with-values
        (lambda () e)
        (lambda x x))]))
  
  (define defun-tests
    (test-suite
     "Defunctionalization"
     
     (test-not-exn "define-struct" (lambda () (vwrap (defun (expand (syntax (define-struct posn (x y))))))))
     (test-not-exn "quote-syntax" (lambda () (vwrap (defun (expand (syntax #'provide/contract-id-set-a-date-day!))))))
     #;(test-not-exn "provide/contract" (lambda () (vwrap (defun (expand (syntax (module t mzscheme
                                                                                 (require (lib "contract.ss"))
                                                                                 (define x 1)
                                                                                 (provide/contract
                                                                                  [x integer?]))))))))
     )))
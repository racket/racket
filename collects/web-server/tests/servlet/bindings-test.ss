(module bindings-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "bindings.ss" "web-server" "servlet"))
  (provide bindings-tests)
  
  (define bs `([foo . 3] [foos . 1] [foos . 2]))
  
  (define bindings-tests
    (test-suite
     "Bindings"
     
     (test-case
      "exists-binding? - true"
      (check-true (exists-binding? 'foo bs)))
     (test-case
      "exists-binding? - false"
      (check-false (exists-binding? 'bar bs)))
     
     (test-case
      "extract-bindings"
      (check-equal? (extract-bindings 'foos bs) (list 1 2)))
     
     (test-case
      "extract-binding/single - success"
      (check-equal? (extract-binding/single 'foo bs) 3))
     (test-case
      "extract-binding/single - failure"
      (check-exn (lambda (exn) (regexp-match "not found" (exn-message exn)))
                 (lambda () (extract-binding/single 'bar bs) 3)))
     (test-case
      "extract-binding/single - multiple"
      (check-exn (lambda (exn) (regexp-match "multiple times" (exn-message exn)))
                 (lambda () (extract-binding/single 'foos bs) 3))))))
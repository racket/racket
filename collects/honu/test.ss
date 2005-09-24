(module test mzscheme

  (require (lib "contract.ss")
           "top.ss"
           "utils.ss")

  (define/p examples
    (list "examples/BoundedStack.honu"
          "examples/EvenOddClass.honu"
          "examples/List.honu"
          "examples/Y.honu"
          "examples/bind-tup-top.honu"
          "examples/cond-test.honu"
          "examples/even-odd.honu"
          "examples/exprs.honu"
          "examples/point.honu"
          "examples/struct.honu"
          "examples/tup-bind.honu"
          "examples/types-error.honu"
          "examples/types.honu"
          "examples/nonexistent.honu"))

  (define/c (test-file file) (path-string? . -> . any)
    (with-handlers
        ([exn:fail? (lambda (exn) `(error ,(exn-message exn)))])
      (let* ([honu-path (if (path? file) file (string->path file))]
             [test-path (path-replace-suffix honu-path "-test.ss")])
        (top:run-program honu-path)
        (load test-path))))

  (define/c (run-tests) (-> (listof any/c))
    (map test-file examples))
  
  )

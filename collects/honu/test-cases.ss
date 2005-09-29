(module test-cases mzscheme

  (require (lib "contract.ss")
           "utils.ss"
           "top.ss")
  
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
;          "examples/types-error.honu"
          "examples/types.honu"
;          "examples/nonexistent.honu"
          ))

  (define/c (run-tests) (-> (listof any/c))
    (map test-file examples))
  
  )
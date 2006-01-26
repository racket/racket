(module program-tests mzscheme

  (require (lib "contract.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (prefix srfi1: (lib "1.ss" "srfi"))
           "../../top.ss"
           )

  (provide/contract [program-tests test-suite?])

  (define program-files
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

  (define-assertion (assert-test-file program-file)
    (let* ([results (test-file program-file)]
           [indices (srfi1:iota (length results))]
           [errors
            (srfi1:filter-map (lambda (result index) (if result #f index))
                              results indices)])
      (if (null? errors)
          #t
          (with-assertion-info
           (['error-indices errors])
           (fail-assertion)))))
  
  (define (make-program-test program-file)
    (make-test-case program-file (assert-test-file program-file)))
  
  (define program-tests
    (apply make-test-suite
           "Honu"
           (map make-program-test program-files)))

  )

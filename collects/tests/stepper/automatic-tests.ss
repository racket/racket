(module automatic-tests mzscheme
  (require "through-tests.ss")
  
  (parameterize ([display-only-errors #t])
    (if (run-all-tests-except '(check-expect begin-let-bug prims qq-splice time set! local-set! lazy1 lazy2 lazy3))
        (exit 1)
        (exit 0))))

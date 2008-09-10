(module run-nightly-tests mzscheme
  (require "through-tests.ss")
  
  (parameterize ([display-only-errors #t])
    (run-all-tests-except '(prims qq-splice time set! local-set! lazy1 lazy2 lazy3))))
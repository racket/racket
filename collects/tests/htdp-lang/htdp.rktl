(let ([ol (current-load)]) 
  (current-load
   (λ x
     (printf "cl ~s\n" x)
     (apply ol x))))
   
(load-relative "../racket/loadtest.rktl")

(load-in-sandbox "beginner.rktl" #:testing "../racket/testing.rktl") 
(load-in-sandbox "beginner-abbr.rktl" #:testing "../racket/testing.rktl")
(load-in-sandbox "intermediate.rktl" #:testing "../racket/testing.rktl")
(load-in-sandbox "intermediate-lambda.rktl" #:testing "../racket/testing.rktl")
(load-in-sandbox "advanced.rktl" #:testing "../racket/testing.rktl")

(report-errs)

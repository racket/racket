(require (planet "beginner.ss" ("robby" "redex.plt") "examples"))
(collect-garbage)
(printf "Now\n")
(time (begin 
	(run-tests) (run-tests) (run-tests) (run-tests) (run-tests)
	))

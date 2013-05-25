#lang racket

(require racklog)

(provide (all-defined-out))

;This is the puzzle solver described in Sterling & Shapiro, p. 214

;As S & S say, it is a "trivial" piece of code
;that successively solves each clue and query, which are expressed
;as Prolog goals and are executed with the meta-variable facility.

;The code in "real" Prolog, for comparison, is:
;
;  solve_puzzle(Clues, Queries, Solution) 
;                 :- solve(Clues), solve(Queries).
;
;  solve([Clue|Clues]) :- Clue, solve(Clues).
;  solve([]).

(define %solve-puzzle
  (%rel (clues queries solution)
    ((clues queries solution)
      (%solve clues)
      (%solve queries))))

(define %solve
  (%rel (clue clues)
    (((cons clue clues))
      clue 
      (%solve clues))
    (('()))))

;evaluate (solve-puzzle %puzzle) to get the solution to
;%puzzle.  Here %puzzle is a relation that is defined to
;hold for the three arguments clues, queries and solution=,
;iff they satisfy the constraints imposed by the puzzle.
;solve-puzzle finds an (the?) instantiation for the solution=
;variable.

(define solve-puzzle
  (lambda (%puzzle)
    (%let (clues queries)
      (%which (solution=)
	(%and
	  (%puzzle clues queries solution=)
	  (%solve-puzzle clues queries solution=))))))

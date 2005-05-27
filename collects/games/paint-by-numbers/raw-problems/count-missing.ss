#!/bin/sh

string=? ; exec mzscheme -qr $0

(printf "checking problems in ~s~n" (collection-path "games" "paint-by-numbers"))

(require-library "sig.ss" "games" "paint-by-numbers")

(require-library "errortrace.ss" "errortrace")

(define counter
  (unit/sig ()
    (import paint-by-numbers:problem^
	    paint-by-numbers:all-problems^)

    (define total 0)
    (define total-missing 0)

    (define (filter p l)
      (cond
       [(null? l) null]
       [(p (car l)) (cons (car l) (filter p (cdr l)))]
       [else (filter p (cdr l))]))

    (define (check-set problems set-name)
      (let ([missing (filter (lambda (x) x)
			     (map (lambda (problem) (if (problem-solution problem) #f (problem-name problem)))
				  problems))])
	(set! total (+ (length problems) total))
	(set! total-missing (+ (length missing) total-missing))
	(unless (null? missing)
	  (printf "~a missing ~a~n" set-name (length missing))
	  (for-each (lambda (x) (printf "  ~a~n" x)) missing))))


    (for-each check-set problemss set-names)
    (printf "missing ~a of ~a~n" total-missing total)))

(invoke-unit/sig
 (compound-unit/sig (import)
   (link
    [problem : paint-by-numbers:problem^ ((require-library "problem.ss" "games" "paint-by-numbers"))]
    [all : paint-by-numbers:all-problems^ ((require-library "all-problems.ss" "games" "paint-by-numbers") problem)]
    [counter : () (counter problem all)])
   (export)))

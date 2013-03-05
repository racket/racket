#lang racket

#| 

This script constructs the contents of the problems directory
from the solutions directory. This process merely consists of
reading in each file in the solutions directory (based on the 
directory file) and rewriting it into the format described
in ...

|#

;; shrink-file : string -> string
(define (shrink-file filename)
  (printf "shrinking ~a..." filename)
  (flush-output)
  (let ([shrunk (shrink-set (call-with-input-file (build-path 'up "solution-sets" filename) read))])
    (call-with-output-file (build-path 'up "problems" filename)
      (lambda (port)
        (write shrunk port)
        (newline port))))
  (printf "done\n"))

;; shrink-set sexp[set] -> sexp[set]
(define (shrink-set set)
  (match set
    [`(unit 
        (import paint-by-numbers:problem^)
        (export paint-by-numbers:problem-set^)
        (define set-name ,set-name)
        (define problems (list ,problems ...)))
     `(unit 
        (import paint-by-numbers:problem^)
        (export paint-by-numbers:problem-set^)
        (define set-name ,set-name)
        (define problems (list ,@(map shrink-problem problems))))]))
     
;; shrink-problem : sexp[problem] -> sexp[problem]
(define (shrink-problem problem)
  (match problem
    [`(make-problem ,name ,rows ,cols ',solution)
     `(make-problem ,name ,rows ,cols ',(shrink-solution solution))]))

;; shrink-soution :    (union #f (vectorof (vectorof (union 'on 'off 'unknown))))
;;                  -> (union #f (listof string))
;; produces the data in a representation that is much smaller when written
(define (shrink-solution soln)
  (and soln
       (map (lambda (line)
              (apply string (map (lambda (x)
                                   (case x
                                     [(on) #\x]
                                     [(off) #\space]
                                     [(unknown) #\U]))
                                 (vector->list line))))
            (vector->list soln))))


;; erase old contents of the solutions directory
(for-each
 (lambda (file) (when (file-exists? (build-path 'up "problems" file))
                  (delete-file (build-path 'up "problems" file))))
 (directory-list (build-path 'up "problems")))

(copy-file (build-path 'up "solution-sets" "directory")
           (build-path 'up "problems" "directory"))

(provide main)
(define (main)
  (for-each shrink-file (call-with-input-file (build-path 'up "problems" "directory") read)))

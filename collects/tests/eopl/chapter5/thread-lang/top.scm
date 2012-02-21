(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all N), where N is the size of the
  ;; time slice.


  (require "drscheme-init.scm")
  (require "data-structures.scm")   
  (require "lang.scm")                  ; for scan&parse
  (require "interp.scm")                ; for value-of-program
  (require "tests.scm")                 ; for test-list
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  (define run
    (lambda (timeslice string)
      (value-of-program timeslice (scan&parse string))))
  
  (define run-all
    (lambda (timeslice)
      (run-tests! 
        (lambda (string) (run timeslice string))
        equal-answer? test-list)))
  
  (define run-one
    (lambda (timeslice test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run timeslice (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        ((list? sloppy-val) (list-val (map sloppy->expval sloppy-val)))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))


  ;; (stop-after-first-error #t)
  ;; (run-all 5)
  ;; (run-one 1000 'producer-consumer)

  )





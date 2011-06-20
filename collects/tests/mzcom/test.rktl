; test.rkt for MzCOM
; requires MysterX to be installed

(require mysterx)
(define mzcom (cci/progid "MzCOM.MzObj"))

(define num-errors 0)
(define (add-error!)
  (set! num-errors (add1 num-errors)))

(define (run . s)
  (apply com-invoke mzcom s))

(define (mzeval s)
  (run "Eval" s))

(print-struct #t)

; should show an About box
(printf "You should see the About box\n")
(run "About")

; tests whether Eval returns sensible result
(if (string=? (mzeval "(+ 20 22)")
		  "42")
    (printf "1st Eval test ok\n")
    (begin
      (add-error!)
      (fprintf (current-error-port) "1st Eval test failed\n")))

(mzeval "(define x 42)")
; tests whether preceding definition really holds
(if (string=? "42" (mzeval "x"))
    (printf "define test ok\n")
    (begin
      (add-error!)
      (fprintf (current-error-port) "define test failed\n")))

(printf "Resetting environment\n")
(run "Reset") ; removes binding for x

; tests for removal of binding
(with-handlers
 ([void (lambda (exn) (printf "2nd Eval test looks ok\nexn was: ~a\n" exn))])
 (mzeval "x") ; binding for x missing
 (add-error!)
 (fprintf (current-error-port) "2nd Eval test failed\n"))

; tests if a Scheme error results in a COM error
(with-handlers
 ([void (lambda (exn) (printf "3rd Eval test looks ok\nexn was: ~a\n" exn))])
 (mzeval "(+ 'foo 42)") ; should raise Scheme error
 (add-error!)
 (fprintf (current-error-port) "3rd Eval test failed\n"))

(when (> num-errors 0)
      (fprintf (current-error-port) "There were ~a errors.\n" num-errors))

(printf "End of MzCOM tests.\n")

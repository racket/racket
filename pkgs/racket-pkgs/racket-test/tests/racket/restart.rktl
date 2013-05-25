
(load-relative "loadtest.rktl")

(require mzlib/restart)

(Section 'restart)

(test #t restart-mzscheme #("ignore-me") values #("") void)
(let ([test-in-out 
       (lambda (pre post in out err?)
         (let ([err-out (open-output-string)])
           (test (if out
                     (string-append "> " out "\n> ")
                     "")
                 'result
                 (let ([s (open-output-string)])
                   (parameterize ([current-input-port (open-input-string in)]
                                  [current-output-port s]
                                  [current-error-port err-out])
                     (restart-mzscheme pre values post void)
                     (get-output-string s))))
           (test err? not (string=? "" (get-output-string err-out)))))])
  (test-in-out #("-q" "ignore-me") #("-i") "(current-command-line-arguments)" "#()" #f)
  (test-in-out #("-q") #("-i") "'Hello" "Hello" #f)
  (test-in-out #("-q") #() "'Hello" "Hello" #f)
  (test-in-out #("-q") #("-e" "1") "" #f #f)
  (test-in-out #("-q") #("-i" "-e" "1") "'Hello" "Hello" #f)
  (test-in-out #("-q") #("-e" "z") "" #f #t)
  (test-in-out #("-q") #("-e" "car" "-i") "'Hello" "Hello" #f)
  (test-in-out #("-q") #("-e" "car") "" #f #f)
  (test-in-out #("-q") #("-l" "scheme/list" "-e" "car") "" #f #t)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

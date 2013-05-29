#lang racket

;; this file shows how you might build a command-line debugger. It doesn't provide much in
;; the way of convenience functions....

(provide (contract-out [run-program (-> path-string? void?)]
                       [continue (-> void?)]
                       [this-step (or/c false? step?)]
                       [srcloc (-> any)])
         (struct-out step)
         )

(require "external-interface.rkt"
         "private/marks.rkt")

(struct step (context-thunks break-kind values-returned) #:prefab)

;; for convenience, we just provide mutable top-level bindings for some functions. This
;; will cause terrible problems if you try to debug two programs at once...
(define this-step #f)
(define debugged-program-semaphore (make-semaphore))
;; in case you want to kill it...:
(define debugged-program-thread #f)

;; this function gets invoked on the debugged program's thread each time a step
;; arrives.
(define (step-receiver a b c)
  ;; if you wanted breakpoints, you could check if this is the
  ;; desired step before halting...
  (set! this-step (step a b c))
  (printf "*ding* a new step has arrived:\n~e\n" this-step)
  (semaphore-wait debugged-program-semaphore))

;; given a program path, run the debugger
(define (run-program program-path)
  (set!
   debugged-program-thread
   (thread
    (lambda ()
      (step-program-file program-path step-receiver)))))

;; continue running the debugger:
(define (continue)
  (semaphore-post debugged-program-semaphore))

;; show the innermost source location associated with the current-step
(define (srcloc)
  (match this-step
    [#f (error 'step-srcloc "no steps yet")]
    [(struct step ((cons first-mark _) _ _))
     (mark-source first-mark)]
    [other (error 'step-srcloc "no source contexts in this step")]))

;; EXAMPLE:

;; Put the following code in /tmp/foo.rkt:

#|
#lang racket

(define (f x) (+ x 14))

(f 9)

Then, you might run it from the command line like this:

jclements-09740:~/plt/collects/stepper clements> racket
Welcome to Racket v5.3.4.7.
> (require "command-line-debugger-example.rkt")
> (run-program "/tmp/foo.rkt")
> *ding* a new step has arrived:
'#s(step #f expr-finished-break ((#<procedure:...ate/annotate.rkt:1061:62> #f #<procedure>)))
(srcloc)
step-srcloc: no source contexts in this step
  context...:
   /Users/clements/plt/collects/racket/private/misc.rkt:87:7
> (continue)
> *ding* a new step has arrived:
'#s(step (#<procedure> #<procedure> #<procedure>) normal-break #f)
(srcloc)
#<syntax:5:1 f>
> (continue)
> *ding* a new step has arrived:
'#s(step (#<procedure> #<procedure> #<procedure>) result-value-break (#<procedure>))
(srcloc)
#<syntax:5:1 f>
> (continue)
> *ding* a new step has arrived:
'#s(step (#<procedure> #<procedure>) normal-break #f)
(srcloc)
#<syntax:5:0 (#%app f (quote 9))>
> (exit)

|#
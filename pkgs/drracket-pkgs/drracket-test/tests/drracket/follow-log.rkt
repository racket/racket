#lang racket/base

#|

This file sets up a log receiver and then
starts up DrRacket. It catches log messages and 
organizes them on event boundaries, printing
out the ones that take the longest
(possibly dropping those where a gc occurs)

The result shows, for each gui event, the
log messages that occured during its dynamic
extent as well as the number of milliseconds
from the start of the gui event before the
log message was reported.

(This is not really a test suite, but instead
 a tool to help understand DrRacket's performance)

|#

(define start-right-away? #t) ;; only applies if the 'main' module is loaded
(define script-drr? #t)

(require racket/list
         racket/class
         racket/match
         racket/pretty
         racket/gui/base
         framework/private/logging-timer
         framework/private/follow-log)

(define drr-eventspace (current-eventspace))
(require tests/drracket/private/drracket-test-util
         framework/test)

(test:use-focus-table #t)

;; running on controller-frame-eventspace handler thread
(define (run-drracket-script)
  (test:use-focus-table #t)
  (test:current-get-eventspaces (λ () (list drr-eventspace)))
  (define drr (wait-for-drracket-frame))
  
  (define (wait-until something)
    (define chan (make-channel))
    (let loop ()
      (sleep 1)
      (parameterize ([current-eventspace drr-eventspace])
        (queue-callback
         (λ () 
           (channel-put chan (something)))))
      (unless (channel-get chan)
        (loop))))
  
  (define (online-syncheck-done)
    (define-values (colors labels) (send (send drr get-current-tab) get-bkg-running))
    (equal? colors '("forestgreen")))
  
  (define (syntax-coloring-done)
    (send (send drr get-definitions-text) is-lexer-valid?))
  
  (sync
   (thread
    (λ ()
      (current-eventspace drr-eventspace)
      (test:current-get-eventspaces (λ () (list drr-eventspace)))
      (test:use-focus-table #t)
      (test:menu-select "View" "Hide Interactions")
      (test:menu-select "Edit" "Find")
      
      (define s (make-semaphore))
      (parameterize ([current-eventspace drr-eventspace])
        (queue-callback
         (λ () 
           (define defs (send drr get-definitions-text))
           (send defs load-file (collection-file-path "class-internal.rkt" "racket" "private"))
           (define open-quote-pos (send defs find-string "\""))
           (when open-quote-pos (send defs set-position open-quote-pos))
           (send (send defs get-canvas) focus)
           (semaphore-post s)))
        #f)
      (semaphore-wait s)

      ;(wait-until online-syncheck-done)
      
      (for ([x (in-range 1)])
        
        
        (let ([s "fdjafjdklafjkdalsfjdaklfjdkaslfdjafjdklafjkdalsfjdaklfjdkasl"])
          (for ([c (in-string s)])
            (test:keystroke c)
            ;(test:keystroke #\return)
            (sleep .3))
          #;
          (for ([c (in-string s)])
            (test:keystroke #\backspace)
            (test:keystroke #\backspace)))
        #;
        (begin
          (test:keystroke #\")
          (test:keystroke #\a)
          (wait-until syntax-coloring-done)
          (test:keystroke #\backspace)
          (test:keystroke #\backspace)
          (wait-until syntax-coloring-done))
        ) 
      '(sleep 10)))) ;; let everything finish
  
  (stop-and-dump)
  (exit))
    

(module+ main
  (when start-right-away?
    (parameterize ([current-eventspace controller-frame-eventspace])
      (queue-callback sb-callback)))
  (dynamic-require 'drracket #f)
  (when script-drr?
    (parameterize ([current-eventspace controller-frame-eventspace])
      (queue-callback
       (λ () 
         (run-drracket-script))))))


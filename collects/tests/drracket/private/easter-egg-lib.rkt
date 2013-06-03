#lang racket/base

(require (only-in "drracket-test-util.rkt"
                  fire-up-separate-drracket-and-run-tests
                  queue-callback/res)
         racket/date
         racket/class
         racket/contract)

(provide 
 (contract-out
  [start-up-on-day
   (-> (and/c integer? (between/c 0 12))
       (and/c integer? (between/c 0 31))
       string?
       void?)]
  [start-up-and-check-car (-> void?)]))

(define (start-up-on-day month day what)
  (define the-seconds (find-seconds 1 0 0
                                    day month 
                                    (date-year (seconds->date (current-seconds)))))
  (printf "trying ~a, ~a/~a PLTDREASTERSECONDS=~a\n" what month day the-seconds)
  (unless (putenv "PLTDREASTERSECONDS" (number->string the-seconds))
    (error 'easter-egg-lib.rkt "putenv failed"))
  (start-up-and-check-car)
  
  ;; start up with (an approximation to) broken image files
  (unless (putenv "PLTDRBREAKIMAGES" "yes")
    (error 'easter-egg-lib.rkt "putenv.2 failed"))
  (printf "trying ~a, ~a/~a PLTDREASTERSECONDS=~a PLTDRBREAKIMAGES=yes\n" what month day the-seconds)
  (start-up-and-check-car)
  (environment-variables-set! (current-environment-variables)
                              #"PLTDRBREAKIMAGES"	 
                              #f))

(define (start-up-and-check-car)
  (fire-up-separate-drracket-and-run-tests
   (λ ()
     
     (define-syntax-rule
       (define/fw x)
       (define x (dynamic-require 'framework 'x)))
     
     (define/fw test:keystroke)
     (define/fw test:run-one)
     (define/fw test:use-focus-table)
     (define/fw test:get-active-top-level-window)
     (define/fw test:menu-select)
     (define/fw test:set-radio-box-item!)
     (define/fw test:button-push)
     (define current-eventspace (dynamic-require 'racket/gui/base 'current-eventspace))
     
     (define (main)
       (queue-callback/res (λ () (test:use-focus-table #t)))
       (test:use-focus-table #t)
       (define drr-frame (wait-for-drracket-frame))
       (set-module-language! drr-frame)
       (queue-callback/res (λ () (send (send (send drr-frame get-definitions-text) get-canvas) focus)))
       (for ([x (in-string "(car 'x)")])
         (test:keystroke x))
       (let ([button (queue-callback/res (λ () (send drr-frame get-execute-button)))])
         (test:run-one (lambda () (send button command))))
       (wait-for-run-to-finish drr-frame)
       (define res 
         (queue-callback/res (λ () (send (send drr-frame get-interactions-text) get-text))))
       (unless (regexp-match #rx"contract violation.*expected: pair[?]" res)
         (eprintf "easter-egg-lib.rkt: interactions looks wrong; got: ~s\n" res)))
     
     (define (set-module-language! drr-frame)
       (test:menu-select "Language" "Choose Language...")
       (define language-dialog (wait-for-new-frame drr-frame))
       (test:set-radio-box-item! #rx"The Racket Language")
       
       (with-handlers ([exn:fail? (lambda (x) (void))])
         (test:button-push "Show Details"))
       
       (test:button-push "Revert to Language Defaults")
       
       (test:button-push "OK")
       (define new-frame (wait-for-new-frame language-dialog))
       (unless (eq? new-frame drr-frame)
         (error 'set-module-level! 
                "didn't get drracket frame back, got: ~s (drr-frame ~s)\n"
                new-frame
                drr-frame)))
     
     (define (wait-for-run-to-finish drr-frame)
       (define (run-finished)
         (send (send drr-frame get-execute-button) is-enabled?))
       (wait-for-something run-finished))
     
     (define (wait-for-drracket-frame)
       (define (drracket-frame-frontmost)
         (define active (test:get-active-top-level-window))
         (and active 
              (method-in-interface? 'get-execute-button (object-interface active))
              active))
       (wait-for-something drracket-frame-frontmost))
     
     (define (wait-for-new-frame old-frame)
       (wait-for-something
        (λ ()
          (define active (test:get-active-top-level-window))
          (and active 
               (not (eq? active old-frame))
               active))))
     
     (define (wait-for-something thing?)
       (define total-time-to-wait 20) ;; in seconds
       (define time-to-wait-in-one-iteration 1/10) ;; also in seconds
       (let loop ([n (/ total-time-to-wait time-to-wait-in-one-iteration)])
         (cond
           [(thing?) => values]
           [(zero? n)
            (error 'wait-for-something "~s didn't happen" thing?)]
           [else
            (sleep time-to-wait-in-one-iteration)
            (loop (- n 1))])))
     
     (main))))

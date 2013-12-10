#lang at-exp racket/base
(require "private/drracket-test-util.rkt"
         framework/test
         racket/class
         racket/gui/base)

(define (setup-racket/base-raw) (setup/rb "No debugging or profiling"))
(define (setup-racket/base-debug) (setup/rb "Debugging"))
(define (setup-racket/base-profile) (setup/rb "Debugging and profiling"))
(define (setup-racket/base-coverage) (setup/rb "Syntactic test suite coverage"))

(define (setup/rb which-rb)
  (set-module-language! #f)
  (test:set-radio-box-item! which-rb)
  (let ([f (test:get-active-top-level-window)])
    (test:button-push "OK")
    (wait-for-new-frame f)))

(define (run errortrace-stack? setup-language)
  (define drracket-frame (wait-for-drracket-frame))
  
  (define ints-text (queue-callback/res (λ () (send drracket-frame get-interactions-text))))
  
  (setup-language)
  (clear-definitions drracket-frame)
  (insert-in-definitions
   drracket-frame
   @string-append{
    #lang racket/base
    (define (f x)
      (+ 1 (/ x)))

    (define (g x)
      (+ 1 (+ 1 (+ 1 (f x)))))

    (g 0)})
                  
  (do-execute drracket-frame)
  
  (define ints-content (queue-callback/res (λ () (send ints-text get-text))))
  (unless (regexp-match? #rx"division by zero" ints-content)
    (error 'errortrace-stacks.rkt
           "expected a division by zero error in the interactions window, got:\n~a"
           ints-content))
  
  ;; try to find the stacktrace button in the interactions window
  (define cb
    (queue-callback/res
     (λ ()
       (let loop ([snip (send ints-text find-first-snip)])
         (cond
           [snip 
            (define cb 
              (with-handlers ([exn:fail? (λ (x) #f)])
                ;; string snips will fail this
                (send snip get-callback)))
            (cond
              [cb (λ () (cb snip))]
              [else (loop (send snip next))])]
           [else #f])))))

  (unless cb
    (error 'errortrace-stacks.rkt 
           (string-append
            "could not find the second clickable snip"
            "in the interactions text, got: ~a")
           ints-content))
  
  (queue-callback (λ () (cb)))
  
  (define stacks (wait-for-new-frame drracket-frame))

  ;; #f => no tab panel in the frame
  (define tab-panel-labels
    (queue-callback/res
     (λ ()
       (let loop ([window stacks])
         (cond
           [(is-a? window tab-panel%)
            (for/list ([i (in-range (send window get-number))])
              (send window get-item-label i))]
           [(is-a? window area-container<%>)
            (for/or ([child (in-list (send window get-children))])
              (loop child))]
           [else #f])))))
  
  (define test-passed?
    (cond
      [errortrace-stack?
       (equal? tab-panel-labels '("Errortrace" "Builtin"))]
      [else
       (equal? tab-panel-labels #f)]))
  
  (unless test-passed?
    (error 'errortrace-stacks.rkt 
           "errortrace-stack? ~s and tab-panel-labels ~s don't match up for ~s"
           errortrace-stack? tab-panel-labels setup-language))
  
  ;; close the stacks window
  (queue-callback/res (λ () (send stacks close)))
  
  ;; wait for it to close
  (wait-for-new-frame stacks))

(fire-up-drracket-and-run-tests
 (λ ()
   (run #f setup-racket/base-raw)
   (run #t setup-racket/base-debug)
   (run #t setup-racket/base-profile)
   (run #t setup-racket/base-coverage)))

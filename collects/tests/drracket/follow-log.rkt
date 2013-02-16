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

(require racket/list
         racket/class
         racket/match
         racket/pretty
         racket/gui/base
         framework/private/logging-timer)


(define lr (make-log-receiver (current-logger)
                              'debug 'racket/engine
                              'debug 'GC
                              'debug 'gui-event
                              'debug 'framework/colorer
                              'debug 'timeline))
(define gc-only-lr
  (make-log-receiver (current-logger)
                     'debug 'GC))

(define top-n-events 30)
(define drop-gc? #f)
(define start-right-away? #t) ;; only applies if the 'main' module is loaded
(define script-drr? #t)
(define interesting-range-start 26)
(define interesting-range-end +inf.0)

(define log-done-chan (make-channel))
(define bt-done-chan (make-channel))

(define start-log-chan (make-channel))
(void 
 (thread
  (λ ()
    (let loop () 
      (define lr (sync start-log-chan))
      (let loop ([events '()])
        (sync
         (handle-evt
          lr
          (λ (info)
            (loop (cons info events))))
         (handle-evt
          log-done-chan
          (λ (resp-chan)
            (channel-put resp-chan events)))))
      (loop)))))

(define thread-to-watch (current-thread))
(let ([win (get-top-level-windows)])
  (unless (null? win)
    (define fr-thd (eventspace-handler-thread (send (car win) get-eventspace)))
    (unless (eq? thread-to-watch fr-thd)
      (eprintf "WARNING: current-thread and eventspace thread aren't the same thread\n"))))
(define start-bt-chan (make-channel))
(void 
 (thread
  (λ ()
    (let loop () 
      (sync start-bt-chan)
      (let loop ([marks '()])
        (sync
         (handle-evt
          (alarm-evt (+ (current-inexact-milliseconds) 10))
          (λ (_)
            (loop (cons (continuation-marks thread-to-watch)
                        marks))))
         (handle-evt
          bt-done-chan
          (λ (resp-chan)
            (define stacks (map continuation-mark-set->context marks))
            (channel-put resp-chan stacks)))))
      (loop)))))

(define controller-frame-eventspace (make-eventspace))
(define f (parameterize ([current-eventspace controller-frame-eventspace])
            (new frame% [label "Log Follower"])))
(define sb (new button% [label "Start Following Log"] [parent f]
               [callback
                (λ (_1 _2)
                  (sb-callback))]))
(define sb2 (new button% [label "Start Collecting Backtraces"] [parent f]
                 [callback
                  (λ (_1 _2)
                    (start-bt-callback))]))
(define sb3 (new button% [label "Start Following GC Log"] [parent f]
                 [callback
                  (λ (_1 _2)
                    (start-gc-callback))]))
(define db (new button% [label "Stop && Dump"] [parent f] [enabled #f]
                [callback
                 (λ (_1 _2)
                   (stop-and-dump))]))

(let ([m 0])
  (send f reflow-container)
  (for ([x (in-list (send f get-children))])
    (when (is-a? x button%)
      (set! m (max m (send x get-width)))))
  (for ([x (in-list (send f get-children))])
    (when (is-a? x button%)
      (send x min-width m))))

(define (stop-and-dump)
  (define sp (open-output-string))
  (parameterize ([current-output-port sp])
    (cond
      [following-log?
       (define resp (make-channel))
       (channel-put log-done-chan resp)
       (show-results (channel-get resp))
       (set! following-log? #f)]
      [following-bt?
       (define resp (make-channel))
       (channel-put bt-done-chan resp)
       (define stacks (channel-get resp))
       (show-bt-results stacks)
       (set! following-bt? #f)]
      [following-gc-log?
       (define resp (make-channel))
       (channel-put log-done-chan resp)
       (show-gc (channel-get resp))
       (set! following-gc-log? #f)])
    (send db enable #f)
    (send sb enable #t)
    (send sb2 enable #t)
    (send sb3 enable #t))
  (show-string (get-output-string sp)))

(define (show-string str)
  (define t (new text%))
  (send t insert str)
  (send t change-style (make-object style-delta% 'change-family 'modern)
        0
        (send t last-position))
  (define f (new frame% [width 600] [height 800] [label "Log Follower Results"]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (define mb (new menu-bar% [parent f]))
  (define edit-menu (new menu% [label "Edit"] [parent mb]))
  (append-editor-operation-menu-items edit-menu)
  (send f show #t))

(define following-log? #f)
(define following-bt? #f)
(define following-gc-log? #f)

(define (sb-callback)
  (set! following-log? #t)
  (send sb enable #f)
  (send sb2 enable #f)
  (send sb3 enable #f)
  (send db enable #t)
  (channel-put start-log-chan lr))

(define (start-bt-callback)
  (set! following-bt? #t)
  (send sb enable #f)
  (send sb2 enable #f)
  (send sb3 enable #f)
  (send db enable #t)
  (channel-put start-bt-chan #t))

(define (start-gc-callback)
  (set! following-gc-log? #t)
  (send sb enable #f)
  (send sb2 enable #f)
  (send sb3 enable #f)
  (send db enable #t)
  (channel-put start-log-chan gc-only-lr))
  

(send f show #t)

(define (show-gc lst)
  (for ([x (in-list lst)])
    (printf "~a\n" (vector-ref x 1))))

(define (show-bt-results stacks)
  (define top-frame (make-hash))
  (for ([stack (in-list stacks)])
    (unless (null? stack)
      (define k (car stack))
      (hash-set! top-frame k (cons stack (hash-ref top-frame k '())))))
  (define sorted (sort (hash-map top-frame (λ (x y) y)) > #:key length))
  (printf "top 10: ~s\n" (map length (take sorted (min (length sorted) 10))))
  (define most-popular (cadr sorted))
  (for ([x (in-range 10)])
    (printf "---- next stack\n")
    (pretty-print (list-ref most-popular (random (length most-popular))))
    (printf "\n"))
  (void))

(struct gui-event (start end name) #:prefab)

(define (print-gui-event-hist gui-events)
  (print-hist 
   (for/list ([gui-event (in-list gui-events)])
     (gui-event->delta gui-event))))

(define (print-hist nums)
  (define bucket-size 2) ;; in milliseconds
  (define (δ->bucket δ)
    (* bucket-size
       (inexact->exact (round (* δ (/ 1.0 bucket-size))))))
  
  (define buckets (make-hash))
  (for ([num (in-list nums)])
    (define bucket (δ->bucket num))
    (hash-set! buckets bucket (+ (hash-ref buckets bucket 0) 1)))
  (pretty-print
   (sort (hash-map buckets vector)
         <
         #:key (λ (x) (vector-ref x 0)))))

(define (gui-event->delta x)
    (define i (vector-ref x 2))
    (- (gui-event-end i)
       (gui-event-start i)))

(define (show-results evts)
  (define gui-events (filter (λ (x) 
                               (define i (vector-ref x 2))
                               (and (gui-event? i)
                                    (number? (gui-event-end i))))
                             evts))
  
  (define (show-hist)
    (define gc-starts+ends
      (filter 
       values
       (for/list ([evt (in-list evts)])
         (cond
           [(gc-info? (vector-ref evt 2))
            (cons (gc-info-start-time (vector-ref evt 2))
                  (gc-info-end-time (vector-ref evt 2)))]
           [else #f]))))
    
    (printf "gc deltas\n")
    (print-hist (map (λ (x) (- (cdr x) (car x))) gc-starts+ends))
    
    (define (has-a-gc? evt-vec)
      (define evt (vector-ref evt-vec 2))
      (for/or ([gc-start+end (in-list gc-starts+ends)])
        (<= (gui-event-start evt) 
            (car gc-start+end)
            (gui-event-end evt))))
    
    (define-values (has-gc-events no-gc-events)
      (partition has-a-gc? gui-events))
    (printf "\nwith gc\n")
    (print-gui-event-hist has-gc-events)
    (printf "\nwithout gc\n")
    (print-gui-event-hist no-gc-events)
    (printf "\nboth with and without gc\n")
    (print-gui-event-hist gui-events))

  (define (show-top-events)
     (define interesting-gui-events
       (let ([candidate-events
              (sort (filter (λ (x)
                              (<= interesting-range-start 
                                  (gui-event->delta x)
                                  interesting-range-end))
                            gui-events)
                    >
                    #:key gui-event->delta)])
         (take candidate-events (min (length candidate-events) top-n-events))))
     
     (define with-other-events
       (for/list ([gui-evt (in-list interesting-gui-events)])
         (match (vector-ref gui-evt 2)
           [(gui-event start end name)
            (define in-the-middle
              (append (map (λ (x) (list (list 'δ (- (get-start-time x) start)) x))
                           (sort
                            (filter (λ (x) (and (not (gui-event? (vector-ref x 2)))
                                                (<= start (get-start-time x) end)))
                                    evts)
                            <
                            #:key get-start-time))
                      (list (list (list 'δ (- end start)) 'end-of-gui-event))))
            (list* (- end start)
                   gui-evt
                   in-the-middle)])))
     
     (define (has-a-gc-event? x)
       (define in-the-middle (cddr x))
       (ormap (λ (x) 
                (and (vector? (list-ref x 1))
                     (gc-info? (vector-ref (list-ref x 1) 2))))
              in-the-middle))
     
     (pretty-print
      (if drop-gc?
          (filter (λ (x) (not (has-a-gc-event? x)))
                  with-other-events)
          with-other-events)))
  
  (show-top-events)
  (printf "\n\n============================================================\n\n\n")
  (show-hist))

(struct gc-info (major? pre-amount pre-admin-amount code-amount
                        post-amount post-admin-amount
                        start-process-time end-process-time
                        start-time end-time)
  #:prefab)
(struct engine-info (msec name) #:prefab)

(define (get-start-time x)
  (cond
    [(gc-info? (vector-ref x 2))
     (gc-info-start-time (vector-ref x 2))]
    [(engine-info? (vector-ref x 2))
     (engine-info-msec (vector-ref x 2))]
    [(regexp-match #rx"framework" (vector-ref x 1))
     (vector-ref x 2)]
    [(timeline-info? (vector-ref x 2))
     (timeline-info-milliseconds (vector-ref x 2))]         
    [else
     (unless (regexp-match #rx"^GC: 0:MST @" (vector-ref x 1))
       (eprintf "unk: ~s\n" x))
     0]))

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


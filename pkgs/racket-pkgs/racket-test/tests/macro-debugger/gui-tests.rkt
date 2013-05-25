#lang racket/base
(require racket/class
         racket/list
         racket/gui/base
         framework
         mzlib/etc)

(require macro-debugger/model/trace
         macro-debugger/view/view
         macro-debugger/view/prefs)

(provide test-stepper
         test-stepper*)

(define (wait) (sleep .1))
(define (waitb) (sleep .025))

(define (get-active-frame)
  (let ([frame (get-top-level-focus-window)])
    (unless frame
      (error 'get-active-frame "no active frame"))
    frame))

(define (find-object base class pred)
  (define (find-loop obj)
    (cond [(and (is-a? obj class) (pred obj)) obj]
          [(is-a? obj area-container<%>)
           (ormap find-loop (send obj get-children))]
          [else #f]))
  (let ([obj (find-loop base)])
    (unless obj
      (error 'find-object "no such ~s object satisfying ~s" class pred))
    obj))

(define (find:next frame)
  (find-object frame button% (has-label "Step ->")))
(define (find:prev frame)
  (find-object frame button% (has-label "<- Step")))
(define (find:start frame)
  (find-object frame button% (has-label "<-- Start")))
(define (find:end frame)
  (find-object frame button% (has-label "End -->")))
(define (find:up frame)
  (find-object frame button% (has-label "Previous term")))
(define (find:down frame)
  (find-object frame button% (has-label "Next term")))

(define (has-label label)
  (lambda (obj) (equal? label (send obj get-label))))

(define (enabled? obj)
  (send obj is-enabled?))

(define (click button)
  (waitb)
  (unless (enabled? button)
    (error 'click "button not enabled"))
  (send button command (make-object control-event% 'button)))

(define (click-until-disabled button)
  (define (loop n)
    (if (enabled? button)
        (begin (click button)
               (loop (add1 n)))
        n))
  (loop 0))

(define (click-if-enabled button)
  (when (enabled? button)
    (click button)))

(define (check check-box value)
  (wait)
  (unless (enabled? check-box)
    (error 'check "check box not enabled"))
  (send* check-box
    (set-value value)
    (command (make-object control-event% 'check-box))))

(define (choose choice value)
  (wait)
  (unless (enabled? choice)
    (error 'choice "choice not enabled"))
  (send* choice
    (set-string-selection value)
    (command (make-object control-event% 'choice))))

(define (menu-check menu-item value)
  (wait)
  (unless (enabled? menu-item)
    (error 'menu-check "menu item not enabled"))
  (send* menu-item
    (check value)
    (command (make-object control-event% 'menu))))

(define (set-policy frame policy-symbol)
  (let ([policy (find-object frame choice% (has-label "Macro hiding: "))])
    (case policy-symbol
      ((none)
       (choose policy "Disable")
       #;(check enable #f)
       #;(check hide-mz #f)
       #;(check hide-libs #f))
      ((basic)
       (choose policy "Custom ...")
       (check (find-object frame check-box% (has-label "Enable macro hiding")) #t)
       (check (find-object frame check-box% (has-label "Hide mzscheme syntax")) #f)
       (check (find-object frame check-box% (has-label "Hide library syntax")) #f))
      ((normal)
       (choose policy "Standard")
       #;(check enable #t)
       #;(check hide-mz #t)
       #;(check hide-libs #t)))))

(define (get-menu-item frame menu-path)
  (let ([menu (send frame get-menu-bar)])
    (define (menu-loop path menus)
      (cond [(string? path)
             (let ([item
                    (ormap (lambda (m)
                             (and (is-a? m labelled-menu-item<%>)
                                  (equal? path (send m get-label))
                                  m))
                           menus)])
               (unless item
                 (error 'get-menu-item "no such menu item: ~s" path))
               item)]
            [else
             (let ([menu
                    (ormap (lambda (m) 
                             (and (is-a? m menu%)
                                  (equal? (car path) (send m get-label))
                                  m))
                           menus)])
               (unless menu
                 (error 'get-menu-item "no such menu item: ~s" path))
               (menu-loop (cdr path) (send menu get-items)))]))
    (or (menu-loop menu-path (send menu get-items))
        (error 'get-menu-item "no such menu item"))))

(define (menu-item:one-by-one frame)
  (get-menu-item frame '("Stepper" "Extra options" . "One term at a time")))
(define (menu-item:show-renaming-steps frame)
  (get-menu-item frame '("Stepper" "Extra options" . "Include renaming steps")))
(define (menu-item:highlight-redex/contractum frame)
  (get-menu-item frame '("Stepper" "Extra options" . "Highlight redex/contractum")))

(define (set-mode frame . flags)
  (menu-check (menu-item:one-by-one frame) 
              (memq 'one-by-one flags))
  (menu-check (menu-item:show-renaming-steps frame)
              (memq 'renames flags))
  (menu-check (menu-item:highlight-redex/contractum frame)
              (not (memq 'no-highlight flags))))

(define (run-through start prev next end)
  (begin-with-definitions
    (click-if-enabled start)
    (begin (for-each assert-disabled (list start prev)))
    (define next-clicks (click-until-disabled next))
    (begin (for-each assert-disabled (list next end)))
    (click-if-enabled start)
    (begin (for-each assert-disabled (list start prev)))
    (click-if-enabled end)
    (begin (for-each assert-disabled (list next end)))
    (define prev-clicks (click-until-disabled prev))
    (unless (equal? next-clicks prev-clicks)
      (error 'run-through 
             "pressed next ~s times, pressed prev ~s times"
             next-clicks prev-clicks))
    (begin (for-each assert-disabled (list start prev)))))

(define (assert-enabled obj)
  (unless (enabled? obj) 
    (error 'assert-enabled "assertion failed for ~s" (send obj get-label))))
(define (assert-disabled obj)
  (when (enabled? obj)
    (error 'assert-disabled "assertion failed for ~s" (send obj get-label))))

;; check-threads : (-> any) -> any
;; Runs thunk; raises error if any subthreads created by thunk raise
;; errors, or if a subthread outlives thunk's computation.
(define (check-threads thunk)
  (let* ([main-cust (current-custodian)]
         [sub-cust (make-custodian main-cust)]
         [sub-exns null]
         [old-uncaught-exception-handler (uncaught-exception-handler)])
    (parameterize ((current-custodian sub-cust)
                   (uncaught-exception-handler
                    (lambda (exn)
                      (set! sub-exns (cons exn sub-exns))
                      (old-uncaught-exception-handler exn))))
      (let ([result (thunk)])
        ;; Check that sub-custodian has no living threads.
        (let ([threads-still-going?
               (ormap thread-running?
                      (filter thread?
                              (custodian-managed-list sub-cust main-cust)))])
          (when (pair? sub-exns)
            (raise (car sub-exns))
            #;(error 'nice-threads "child thread raised exception"))
          (when threads-still-going?
            (error 'nice-threads "child thread left still running"))
          result)))))

(define (new-uninitialized-stepper)
  (sleep 1)
  (parameterize ((current-eventspace (make-eventspace)))
    (let ([frame (new macro-stepper-frame%
                      (config (new macro-stepper-config/prefs/readonly%))
                      (director (new macro-stepper-director%)))])
      (send frame show #t)
      frame)))

(define (new-stepper)
  (let ([frame (new-uninitialized-stepper)])
    frame))

(define (add-expansion frame stx)
  (let ([widget (send frame get-widget)])
    (send widget add-deriv (trace stx))))

(define (test-stepper* stxs policies)
  (check-threads
   (lambda ()
     (let ([frame (new-stepper)])
       (let ([start (find:start frame)]
             [prev (find:prev frame)]
             [next (find:next frame)]
             [end (find:end frame)]
             [up (find:up frame)]
             [down (find:down frame)])
         (define (run)
           (run-down)
           (click-until-disabled up))
         (define (run-down)
           (when (enabled? down)
             (run-through start prev next end)
             (click down)
             (run-down)))
         (define (run/all-modes)
           (set-mode frame) ;; normal by default
           (run)
           (set-mode frame 'no-highlight)
           (run)
           (set-mode frame 'renames)
           (run)
           ;;(set-mode frame 'no-highlight 'renames)
           ;;(run)
           ;;(set-mode frame 'one-by-one)
           ;;(run)
           ;;(set-mode frame 'one-by-one 'renames)
           ;;(run)
           (set-mode frame 'one-by-one 'renames 'no-highlight)
           (run)
           (set-mode frame))
         (dynamic-wind
          void
          (lambda ()
            (for-each (lambda (stx) (add-expansion frame stx)) stxs)
            ;; Test different hiding policies
            (for-each (lambda (policy)
                        (set-policy frame policy)
                        (run/all-modes))
                      policies)
            (wait))
          (lambda ()
            (test:close-top-level-window frame)
            (kill-thread
             (eventspace-handler-thread
              (send frame get-eventspace))))))))))

(define (test-stepper expr)
  (test-stepper* (list expr) '(none basic normal)))

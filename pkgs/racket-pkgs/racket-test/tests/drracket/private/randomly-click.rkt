#lang scheme/gui
(require framework)
(provide go)

(define numButtonsToPush 200)
(define the-seed (+ 1 (modulo (current-seconds) (- (expt 2 31) 1))))
(random-seed the-seed)

;;find-all-actions: area -> (listof (-> void))
(define (find-all-actions area)
  (cond
    [(is-a? area area-container<%>)
     (apply append (map find-all-actions (send area get-children)))]
    [(and (is-a? area button%)
          (send area is-enabled?)
          (send area is-shown?))
     (list (case-lambda
             [(x) (format "button ~s" (send area get-label))]
             [() (test:button-push area)]))]
    [(and (is-a? area check-box%)
          (send area is-enabled?))
     (let ([func
            (λ (which-way)
              (case-lambda 
                [(x) (format "checkbox ~s" (send area get-label))]
                [() (test:set-check-box! area which-way)]))])
       (list (func #t) (func #f)))]
    [(and (is-a? area radio-box%)
          (send area is-enabled?))
     (for/list ([i (in-range 0 (send area get-number))])
       (case-lambda
         [(x) (format "radiobox, item ~s" (send area get-item-label i))]
         [() (test:set-radio-box! area i)]))]
    [else '()]))

;;find-random-button: area -> random element of the buttons in area
;;return #f if there is no buttons in area
(define (find-random-action area)
  (define buttons (find-all-actions area))
  (cond
    ;;Area with no buttons
    [(null? buttons) #f]
    [else (list-ref buttons (random (length buttons)))]))

;; Trace the path to the area back to a base-frame
(define (trace-area area base-frame)
  (cond
    [(eq? area base-frame)
     (list base-frame)]
    [else
     (append (trace-area (send area get-parent) base-frame) (list area))]
    ))

;;toy print-label function
(define (print-label area)
  (cond
    [(is-a? area tab-panel%)
     ;(send area get-item-label (send area get-selection))]
     (send area get-item-label 0)]
    [(is-a? area vertical-panel%)
     "Vert-Panel"]
    [(is-a? area horizontal-panel%)
     "Hort-Panel"]
    [(is-a? area vertical-pane%)
     "Vert-Pane"]
    [(is-a? area horizontal-pane%)
     "Hort-Pane"]
    [else
     (send area get-label)]))

(define (g open-dialog)
  (let ((base-window (test:get-active-top-level-window)))
    (open-dialog)
    (wait-for-different-frame base-window)
    (let loop ([n numButtonsToPush]
               [actions '()])
      (cond
        [(zero? n) 
         (printf "\n")
         (exit 0)]
        [else
         
         (printf "~a " n)
         (when (= 1 (modulo n 10)) (printf "\n"))
         (flush-output)
         
         (let ((window (test:get-active-top-level-window)))
           (cond
             ;; Back to base-window is not interesting, Reopen
             [(eq? base-window window)
              (open-dialog)
              (wait-for-different-frame base-window)
              (loop (- n 1) actions)]
             
             [(eq? window #f)
              (sleep .1)
              (loop (- n 1) actions)]
             
             [else
              ;; print out the button before the button is pushed
              ;; Using the toy print-label function
              ;; because some of the parents may not be sent with get-label e.g. vertical-pane%
              ;(print (map print-label (trace-area button window)))
              (let ([action (find-random-action window)])
                (cond
                  [action
                   (with-handlers ((exn:fail? (λ (x) 
                                                (eprintf "\nExecution fail: transcript of ~a clicking follows with seed ~s\n"
                                                         (send window get-label)
                                                         the-seed)
                                                (apply show-log (cons action actions))
                                                (raise x))))
                     ;; pause to make sure all events are flushed from the queue
                     (let ([s (make-semaphore 0)])
                       (queue-callback (λ () (semaphore-post s)) #f)
                       (semaphore-wait s))
                     ;; do the new thing.
                     (action))
                   (loop (- n 1) (cons action actions))]
                  [else
                   (eprintf "\nExists/Meets window with no button: Bug? seed ~s\n"
                            the-seed)
                   (apply show-log actions)
                   (error 'randomly-click.rkt "giving up")]))]))]))))

(define (show-log . actions)
  (for ([action (in-list actions)])
    (eprintf "   ~a\n" (action 'ignored))))

;; the splash screen is in a separate eventspace so wont' show up.
(define (wait-for-first-frame)
  (let loop ()
    (let ([tlw (test:get-active-top-level-window)])
      (cond
        [(not tlw)
         (sleep 1/20)
         (loop)]
        [else tlw]))))


(define (wait-for-different-frame win)
  (let loop ([n 1000])
    (cond
      [(zero? n)
       (error 'wait-for-different-frame "never got that new window, only this one: ~s" win)]
      [else
       (let ([tlw (test:get-active-top-level-window)])
         (when (eq? win tlw)
           (sleep 1/10)
           (loop (- n 1))))])))

(define orig-display-handler (error-display-handler))

(define (go which-dialog)
  (dynamic-require 'drscheme #f)
  
  ;; reset the uncaught exception handler to be sure we kill everything (drscheme sets it)
  (uncaught-exception-handler
   (λ (x)
     (if (exn? x)
         (orig-display-handler (exn-message x) x)
         (eprintf "uncaught exception ~s\n" x))
     (exit 1)))
  
  (void
   (thread
    (λ ()
      (define drs (wait-for-first-frame))
      (case which-dialog
        [(language-dialog)
         (g (λ () (test:menu-select "Language" "Choose Language...")))]
        [(preferences-dialog)
         (g (λ () (preferences:show-dialog)))])))))

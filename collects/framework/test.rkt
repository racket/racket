#lang at-exp racket/base

(require racket/class
         racket/contract/base
         racket/gui/base
         scribble/srcdoc
         (for-syntax racket/base)
         (prefix-in :: framework/private/focus-table))
(generate-delayed-documents) ; enables for-doc--for-label import of `framework'
(require/doc scheme/base scribble/manual
             (for-label framework))

(define (test:top-level-focus-window-has? pred)
  (let ([tlw (test:get-active-top-level-window)])
    (and tlw
         (let loop ([tlw tlw])
           (or (pred tlw)
               (and (is-a? tlw area-container<%>)
                    (ormap loop (send tlw get-children))))))))

(define initial-run-interval 0)  ;; milliseconds

;;
;; The minimum time an action is allowed to run before returning from
;; mred:test:action.  Controls the rate at which actions are started, 
;; and gives some slack time for real events to complete (eg, update).
;; Make-parameter doesn't do what we need across threads.
;; Probably don't need semaphores here (set! is atomic).
;; Units are in milliseconds (as in mred:timer%).
;;

(define run-interval
  (let ([tag  'test:run-interval]
        [msec  initial-run-interval])
    (case-lambda
      [()   msec]
      [(x)  (if (and (integer? x) (exact? x) (<= 0 x))
                (set! msec x)
                (error tag "expects exact, non-negative integer, given: ~e" x))])))

;;
;; How we get into the handler thread, and put fake actions 
;; on the real event queue.
;;

(define install-timer
  (λ (msec thunk)
    (let ([timer (instantiate timer% ()
                   [notify-callback (λ () (thunk))])])
      (send timer start msec #t))))

;;
;; Simple accounting of actions and errors.
;;
;; Keep number of unfinished actions.  An error in the buffer
;; (caught but not-yet-reraised) counts as an unfinished action.
;; (but kept in the-error, not count).
;;
;; Keep buffer of one error, and reraise at first opportunity.
;; Keep just first error, any others are thrown on the floor.
;; Reraising the error flushes the buffer.
;; Store exn in box, so can correctly catch (raise #f).
;; 
;; These values are set in handler thread and read in main thread,
;; so certainly need semaphores here.
;;

(define-values (begin-action  end-action  end-action-with-error
                              get-exn-box   is-exn?     num-actions)
  (let 
      ([sem    (make-semaphore 1)]
       [count      0]     ;; number unfinished actions.
       [the-error  #f])   ;; boxed exn struct, or else #f.
    (letrec
        ([begin-action
           (λ ()
             (semaphore-wait sem)
             (set! count (add1 count))
             (semaphore-post sem))]
         
         [end-action
          (λ ()
            (semaphore-wait sem)
            (set! count (sub1 count))
            (semaphore-post sem))]
         
         [end-action-with-error
          (λ (exn)
            (semaphore-wait sem)
            (set! count (sub1 count))
            (unless the-error
              (set! the-error (box exn)))
            (semaphore-post sem))]
         
         [get-exn-box
          (λ ()
            (semaphore-wait sem)
            (let ([ans  the-error])
              (set! the-error #f)
              (semaphore-post sem)
              ans))]
         
         [is-exn?
          (λ ()
            (semaphore-wait sem)
            (let ([ans  (if the-error #t #f)])
              (semaphore-post sem)
              ans))]
         
         [num-actions
          (λ ()
            (semaphore-wait sem)
            (let ([ans  (+ count (if the-error 1 0))])
              (semaphore-post sem)
              ans))])
      
      (values  begin-action  end-action  end-action-with-error
               get-exn-box   is-exn?     num-actions))))

;; Functions to export, always in main thread.

(define number-pending-actions num-actions)

(define reraise-error
  (λ ()
    (let ([exn-box  (get-exn-box)])
      (if exn-box (raise (unbox exn-box)) (void)))))

;;
;; Start running thunk in handler thread.
;; Don't return until run-interval expires, and thunk finishes, 
;; raises error, or yields (ie, at event boundary).
;; Reraise error (if exists) even from previous action.
;; Note: never more than one timer (of ours) on real event queue.
;; 

(define run-one
  (let ([yield-semaphore (make-semaphore 0)]
        [thread-semaphore (make-semaphore 0)])
    (thread
     (λ ()
       (let loop ()
         (semaphore-wait thread-semaphore)
         (sleep)
         (semaphore-post yield-semaphore)
         (loop))))
    (λ (thunk)
      (let ([sem (make-semaphore 0)])
        (letrec ([start
                  (λ () ;; eventspace main thread
                    
                    ;; guarantee (probably) that some events are handled
                    (semaphore-post thread-semaphore) 
                    (yield yield-semaphore)
                    
                    (install-timer (run-interval) return)
                    (unless (is-exn?)
                      (begin-action)
                      (call-with-exception-handler
                       (λ (exn)
                         (end-action-with-error exn)
                         ((error-escape-handler)))
                       thunk)
                      (end-action)))]
                 
                 [return (λ () (semaphore-post sem))])
          
          (install-timer 0 start)
          (semaphore-wait sem)
          (reraise-error))))))

(define current-get-eventspaces
  (make-parameter (λ () (list (current-eventspace)))))

(define test:use-focus-table (make-parameter #f))

(define (test:get-active-top-level-window)
  (ormap (λ (eventspace)
           (parameterize ([current-eventspace eventspace])
             (cond
               [(test:use-focus-table)
                (define lst (::frame:lookup-focus-table))
                (define focusd (and (not (null? lst)) (car lst)))
                (when (eq? (test:use-focus-table) 'debug)
                  (define f2 (get-top-level-focus-window))
                  (unless (eq? focusd f2)
                    (eprintf "found mismatch focus-table: ~s vs get-top-level-focus-window: ~s\n" 
                             (map (λ (x) (send x get-label)) lst) 
                             (and f2 (list (send f2 get-label))))))
                focusd]
               [else
                (get-top-level-focus-window)])))
         ((current-get-eventspaces))))

(define (get-focused-window)
  (let ([f (test:get-active-top-level-window)])
    (and f
         (send f get-edit-target-window))))

(define time-stamp current-milliseconds)

;;
;; Return list of window's ancestors from root down to window
;; (including window).  Used for on-subwindow-char and on-subwindow-event.
;; get-parent returns #f for no parent.
;; If stop-at-top-level-window? is #t, then the ancestors up to the
;; first top-level-window are returned.
;;

(define ancestor-list
  (λ (window stop-at-top-level-window?)
    (let loop ([w window] [l null])
      (if (or (not w)
              (and stop-at-top-level-window?
                   (is-a? w top-level-window<%>)))
          l
          (loop (send w get-parent) (cons w l))))))

;;
;; Returns #t if window is in active-frame, else #f.
;; get-parent returns () for no parent.
;;

(define (in-active-frame? window)
  (let ([frame  (test:get-active-top-level-window)])
    (let loop ([window  window])
      (cond [(not window) #f]
            [(null? window) #f]  ;; is this test needed?
            [(eq? window frame) #t]
            [else  (loop (send window get-parent))]))))

;;
;; Verify modifier list.
;; l, valid : lists of symbols.
;; returns first item in l *not* in valid, or else #f.
;;

(define verify-list
  (λ (l  valid)
    (cond [(null? l)  #f]
          [(member (car l) valid)  (verify-list (cdr l) valid)]
          [else  (car l)])))

(define verify-item 
  (λ (item valid)
    (verify-list (list item) valid)))

;;;
;;; find-object obj-class b-desc 
;;; returns an object belonging to obj-class, where b-desc
;;; is either an object, or a string
;;;

(define object-tag 'test:find-object)

;; find-object : class (union string regexp (object -> boolean)) -> object
(define (find-object obj-class b-desc)
  (λ ()
    (cond
      [(or (string? b-desc)
           (regexp? b-desc)
           (procedure? b-desc))
       (let* ([active-frame (test:get-active-top-level-window)]
              [_ (unless active-frame
                   (error object-tag
                          "could not find object: ~e, no active frame" 
                          b-desc))]
              [child-matches?
               (λ (child)
                 (cond
                   [(string? b-desc)
                    (equal? (send child get-label) b-desc)]
                   [(regexp? b-desc)
                    (and (send child get-label)
                         (regexp-match? b-desc (send child get-label)))]
                   [(procedure? b-desc)
                    (b-desc child)]))]
              [found
               (let loop ([panel active-frame])
                 (ormap (λ (child)
                          (cond
                            [(and (is-a? child obj-class)
                                  (child-matches? child))
                             child]
                            [(is-a? child area-container-window<%>) 
                             (and (send child is-shown?)
                                  (loop child))]
                            [(is-a? child area-container<%>) 
                             (loop child)]
                            [else #f]))
                        (send panel get-children)))])
         (or found
             (error object-tag 
                    "no object of class ~e named ~e in active frame"
                    obj-class
                    b-desc)))]
      [(is-a? b-desc obj-class) b-desc]
      [else (error 
             object-tag
             "expected either a string or an object of class ~e as input, received: ~e"
             obj-class b-desc)])))


;;; functions specific to various user input

;;; CONTROL functions, to be specialized for individual controls 

(define control-action
  (λ (error-tag event-sym find-ctrl update-control)
    (run-one
     (λ ()
       (let ([event (make-object control-event% event-sym)]
             [ctrl (find-ctrl)])
         (cond
           [(not (send ctrl is-shown?))
            (error error-tag "control ~e is not shown (label ~e)" ctrl (send ctrl get-label))]
           [(not (send ctrl is-enabled?))
            (error error-tag "control ~e is not enabled (label ~e)" ctrl (send ctrl get-label))]
           [(not (in-active-frame? ctrl))
            (error error-tag "control ~e is not in active frame (label ~e)" ctrl (send ctrl get-label))]
           [else
            (update-control ctrl)
            (send ctrl command event)
            (void)]))))))

;;
;; BUTTON
;;

(define (button-push button)
  (control-action
   'test:button-push
   'button
   (find-object button% button)
   void))

;; 
;; CHECK-BOX 
;;

(define (set-check-box! in-cb state) 
  (control-action
   'test:set-check-box!
   'check-box 
   (find-object check-box% in-cb)
   (λ (cb) (send cb set-value state))))

;; 
;; RADIO-BOX 
;;

(define (build-labels radio-box)
  (string-append
   (format "~s" (send radio-box get-item-label 0))
   (let loop ([n (- (send radio-box get-number) 1)])
     (cond
       [(zero? n) ""]
       [else (string-append " "
                            (format "~s"
                                    (send radio-box get-item-label
                                          (- (send radio-box get-number)
                                             n)))
                            (loop (- n 1)))]))))

(define (set-radio-box! in-cb state) 
  (control-action
   'test:set-radio-box!
   'radio-box 
   (find-object radio-box% in-cb)
   (λ (rb) 
     (cond
       [(string? state) 
        (let ([total (send rb get-number)])
          (let loop ([n total])
            (cond
              [(zero? n) (error 'test:set-radio-box!
                                "did not find ~e as a label for ~e; labels: ~a"
                                state in-cb
                                (build-labels rb))]
              [else (let ([i (- total n)])
                      (if (ith-item-matches? rb state i)
                          (if (send rb is-enabled? i)
                              (send rb set-selection i)
                              (error 'test:set-radio-box!
                                     "label ~e is disabled"
                                     state))
                          (loop (- n 1))))])))]
       [(number? state)
        (unless (send rb is-enabled? state)
          (error 'test:set-radio-box! "item ~a is not enabled\n" state))
        (send rb set-selection state)]
       [else (error 'test:set-radio-box!
                    "expected a string or a number as second arg, got: ~e (other arg: ~e)"
                    state in-cb)]))))

(define (ith-item-matches? rb state i)
  (cond
    [(string? state)
     (or (string=? state (send rb get-item-label i))
         (string=? state (send rb get-item-plain-label i)))]
    [(regexp? state)
     (or (regexp-match state (send rb get-item-label i))
         (regexp-match state (send rb get-item-plain-label i)))]))

;; set-radio-box-item! : string -> void
(define (set-radio-box-item! state) 
  (control-action
   'test:set-check-box-state!
   'radio-box
   (find-object radio-box% (entry-matches state))
   (λ (rb) 
     (let ([total (send rb get-number)])
       (let loop ([n total])
         (cond
           [(zero? n) (error 'test:set-radio-box-item! "internal error")]
           [else (let ([i (- total n)])
                   (if (ith-item-matches? rb state i)
                       (if (send rb is-enabled? i)
                           (send rb set-selection i)
                           (error 'test:set-radio-box!
                                  "label ~e is disabled"
                                  state))
                       (loop (- n 1))))]))))))

;; entry-matches : string | regexp -> radio-box -> boolean
(define (entry-matches name)
  (procedure-rename
   (λ (rb)
     (let loop ([n (send rb get-number)])
       (cond
         [(zero? n) #f]
         [else
          (let ([itm (send rb get-item-label (- n 1))]
                [pln-itm (send rb get-item-plain-label (- n 1))])
            (or (cond
                  [(string? name)
                   (or (equal? name itm)
                       (equal? name pln-itm))]
                  [(regexp? name)
                   (or (regexp-match name itm)
                       (regexp-match name pln-itm))])
                (loop (- n 1))))])))
   (string->symbol
    (if (regexp? name)
        (object-name name)
        name))))

;;; CHOICE 

; set-choice! : ((instance in-choice%) (union string number) -> void)
(define (set-choice! in-choice str)
  (control-action
   'test:set-choice!
   'choice
   (find-object choice% in-choice)
   (λ (choice)
     (cond
       [(number? str) (send choice set-selection str)]
       [(string? str) (send choice set-string-selection str)]
       [else (error 'test:set-choice!
                    "expected a string or a number as second arg, got: ~e (other arg: ~e)"
                    str in-choice)]))))

(define (set-list-box! in-lb str)
  (control-action
   'test:set-list-box!
   'list-box
   (find-object list-box% in-lb)
   (λ (lb)
     (cond
       [(number? str) (send lb set-selection str)]
       [(string? str) (send lb set-string-selection str)]
       [else (error 'test:set-list-box!
                    "expected a string or a number as second arg, got: ~e (other arg: ~e)"
                    str in-lb)]))))

;;
;; KEYSTROKES 
;;
;; Give ancestors (from root down) option of handling key event
;; with on-subwindow-char.  If none want it, then send to focused window
;; with (send <window> on-char <wx:key-event>).
;;
;; key: char or integer.
;; optional modifiers: 'alt, 'control, 'meta, 'shift, 
;;   'noalt, 'nocontrol, 'nometa, 'noshift.
;;
;; Window must be shown, in active frame, and either the window has
;; on-char, or else some ancestor must grab key with on-subwindow-char.
;;

(define key-tag 'test:keystroke)
(define legal-keystroke-modifiers
  (list 'alt 'control 'meta 'shift
        'noalt 'nocontrol 'nometa 'noshift))

(define valid-key-symbols
  (list 'escape ;; just trying this for the heck of it -- JBC, 2010-08-13
        'start 'cancel 'clear 'shift 'control 'menu 'pause 'capital
        'prior 'next 'end 'home 'left 'up 'right 'down 'select 'print
        'execute 'snapshot 'insert 'help 'numpad0 'numpad1 'numpad2
        'numpad3 'numpad4 'numpad5 'numpad6 'numpad7 'numpad8 'numpad9
        'multiply 'add 'separator 'subtract 'decimal 'divide 'f1 'f2 'f3
        'f4 'f5 'f6 'f7 'f8 'f9 'f10 'f11 'f12 'f13 'f14 'f15 'f16 'f17
        'f18 'f19 'f20 'f21 'f22 'f23 'f24 'numlock 'scroll))

(define keystroke
  (case-lambda
    [(key) (keystroke key null)]
    [(key modifier-list)
     (cond
       [(not (or (char? key) (memq key valid-key-symbols)))
        (error key-tag "expects char or valid key symbol, given: ~e" key)]
       [(not (list? modifier-list))
        (error key-tag "expected a list as second argument, got: ~e" modifier-list)]
       [(verify-list  modifier-list  legal-keystroke-modifiers)
        => (λ (mod) (error key-tag "unknown key modifier: ~e" mod))]
       [else
        (run-one
         (λ ()
           (let ([window (get-focused-window)])
             (cond
               [(not window)
                (error key-tag "no focused window")]
               [(not (send window is-shown?))
                (error key-tag "focused window is not shown")]
               [(not (send window is-enabled?))
                (error key-tag "focused window is not enabled")]
               [(not (in-active-frame? window))
                (error key-tag "focused window is not in active frame")]
               [else
                (let ([event (make-key-event key window modifier-list)])
                  (send-key-event window event)
                  (void))]))))])]))

;; delay test for on-char until all ancestors decline on-subwindow-char.
(define (send-key-event window event)
  (let loop ([l (ancestor-list window #t)])
    (cond [(null? l)
           (cond
             [(method-in-interface? 'on-char (object-interface window))
              (send window on-char event)]
             [(is-a? window text-field%)
              (send (send window get-editor) on-char event)]
             [else 
              (error
               key-tag
               "focused window is not a text-field% and does not have on-char, ~e" window)])]
          [(send (car l) on-subwindow-char window event) #f]
          [else (loop (cdr l))])))

;; Make full key-event% object.
;; Shift is determined implicitly from key-code.
;; Alt, Meta, Control come from modifier-list.
;; get-alt-down, etc are #f unless explicitly set to #t.
;; WILL WANT TO ADD SET-POSITION WHEN THAT GETS IMPLEMENTED.

(define make-key-event
  (λ (key window modifier-list)
    (let ([event (make-object key-event%)])
      (send event set-key-code key)
      (send event set-time-stamp (time-stamp))
      (set-key-modifiers event key modifier-list)
      event)))

(define set-key-modifiers
  (λ (event key modifier-list)
    (when (shifted? key) (send event set-shift-down #t))
    (let loop ([l  modifier-list])
      (unless (null? l)
        (let ([mod  (car l)])
          (cond
            [(eq? mod 'alt)        (send event set-alt-down     #t)]
            [(eq? mod 'control)    (send event set-control-down #t)]
            [(eq? mod 'meta)       (send event set-meta-down    #t)]
            [(eq? mod 'shift)      (send event set-shift-down   #t)]
            [(eq? mod 'noalt)      (send event set-alt-down     #f)]
            [(eq? mod 'nocontrol)  (send event set-control-down #f)]
            [(eq? mod 'nometa)     (send event set-meta-down    #f)]
            [(eq? mod 'noshift)    (send event set-shift-down   #f)]
            [else  (error key-tag "unknown key modifier: ~e" mod)])
          (loop (cdr l)))))))

(define shifted?
  (let* ([shifted-keys '(#\? #\: #\~ #\\ #\|
                             #\< #\> #\{ #\} #\[ #\] #\( #\)
                             #\! #\@ #\# #\$ #\% #\^ #\& #\* #\_ #\+
                             #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M 
                             #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)])
    (λ (key)
      (memq shifted-keys shifted-keys))))

;;
;; MENU ITEMS 
;;
;; Select menu item with: 
;;   (send <frame> command <menu-item-id>)
;; menu, item: strings
;;
;; DOESN'T HANDLE MENU CHECKBOXES YET.
;;

(define menu-tag 'test:menu-select)

(define (menu-select menu-name . item-names)
  (cond
    [(not (string? menu-name))
     (error menu-tag "expects string, given: ~e" menu-name)]
    [(not (andmap string? item-names))
     (error menu-tag "expects strings, given: ~e" item-names)]
    [else
     (run-one
      (λ ()
        (let* ([frame (test:get-active-top-level-window)]
               [item (get-menu-item frame (cons menu-name item-names))]
               [evt (make-object control-event% 'menu)])
          (send evt set-time-stamp (current-milliseconds))
          (send item command evt))))]))

(define get-menu-item
  (λ (frame item-names)
    (cond
      [(not frame)
       (error menu-tag "no active frame")]
      [(not (method-in-interface? 'get-menu-bar (object-interface frame)))
       (error menu-tag "active frame does not have menu bar")]
      [else
       (let ([menu-bar  (send frame get-menu-bar)])
         (unless menu-bar
           (error menu-tag "active frame does not have menu bar"))
         (send menu-bar on-demand)
         (let* ([items (send menu-bar get-items)])
           (let loop ([items items]
                      [this-name (car item-names)]
                      [wanted-names (cdr item-names)])
             (cond
               [(null? items)
                (error 'menu-select "didn't find a menu: ~e, entire list: ~e" this-name item-names)]
               [else (let ([i (car items)])
                       (cond
                         [(not (is-a? i labelled-menu-item<%>))
                          (loop (cdr items)
                                this-name
                                wanted-names)]
                         [(string=? this-name (send i get-plain-label))
                          (cond
                            [(and (null? wanted-names)
                                  (not (is-a? i menu-item-container<%>)))
                             i]
                            [(and (not (null? wanted-names))
                                  (is-a? i menu-item-container<%>))
                             (loop (send i get-items)
                                   (car wanted-names)
                                   (cdr wanted-names))]
                            [else
                             (error menu-tag "no menu matching ~e" item-names)])]
                         [else
                          (loop (cdr items)
                                this-name
                                wanted-names)]))]))))])))


;;
;; SIMPLE MOUSE EVENTS
;;
;; Simple left-click mouse in current canvas.
;; Sends 3 mouse-events to canvas: motion, down, up.
;;
;; Give ancestors (from root down) option of handling mouse event
;; with pre-on-event.  If none want it, then send to focused window
;; with on-event.
;;
;; NEED TO EXPAND: DRAGGING, DOUBLE-CLICK, MOVING TO OTHER CANVASES,
;; MODIFIER KEYS (SHIFT, META, CONTROL, ALT).
;; 

(define mouse-tag 'test:mouse-action)
(define legal-mouse-buttons (list 'left 'middle 'right))
(define legal-mouse-modifiers
  (list 'alt 'control 'meta 'shift 'noalt 'nocontrol 'nometa 'noshift))

(define mouse-click
  (case-lambda
    [(button x y) (mouse-click button x y null)]
    [(button x y modifier-list)
     (cond 
       [(verify-item button legal-mouse-buttons)
        => (λ (button)
             (error mouse-tag "unknown mouse button: ~e" button))]
       [(not (real? x))
        (error mouse-tag "expected real, given: ~e" x)]
       [(not (real? y))
        (error mouse-tag "expected real, given: ~e" y)]
       [(verify-list modifier-list legal-mouse-modifiers)
        => (λ (mod) 
             (error mouse-tag "unknown mouse modifier: ~e" mod))]
       [else
        (run-one
         (λ ()
           (let ([window  (get-focused-window)])
             (cond 
               [(not window)
                (error mouse-tag "no focused window")]
               [(not (send window is-shown?))
                (error mouse-tag "focused window is not shown")]
               [(not (send window is-enabled?))
                (error mouse-tag "focused window is not enabled")]
               [(not (in-active-frame? window))
                (error mouse-tag "focused window is not in active frame")]
               [else
                (let ([motion  (make-mouse-event 'motion x y modifier-list)]
                      [down    (make-mouse-event (list button 'down) x y modifier-list)]
                      [up      (make-mouse-event (list button 'up) x y modifier-list)])
                  (send-mouse-event window motion)
                  (send-mouse-event window down)
                  (send-mouse-event window up)
                  (void))]))))])]))


;; NEED TO MOVE THE CHECK FOR 'ON-EVENT TO HERE.

(define send-mouse-event
  (λ (window event)
    (let loop ([l  (ancestor-list window #t)])
      (cond
        [(null? l)
         (if (method-in-interface? 'on-event (object-interface window))
             (send window on-event event)
             (error mouse-tag "focused window does not have on-event"))]
        [(and (is-a? (car l) window<%>)
              (send (car l) on-subwindow-event window event))
         #f]
        [else  (loop (cdr l))]))))

;;
;; Make mouse event.
;;

(define make-mouse-event
  (λ (type x y modifier-list)
    (let ([event (make-object mouse-event% (mouse-type-const type))])
      (when (and (pair? type) (not (eq? (cadr type) 'up)))
        (set-mouse-modifiers event (list (car type))))
      (set-mouse-modifiers event modifier-list)
      (send event set-x x)
      (send event set-y y)
      (send event set-time-stamp (time-stamp))
      event)))

(define set-mouse-modifiers
  (λ (event modifier-list)
    (unless (null? modifier-list)
      (let ([mod  (car modifier-list)])
        (cond
          [(eq? mod 'alt)        (send event set-alt-down     #t)]
          [(eq? mod 'control)    (send event set-control-down #t)]
          [(eq? mod 'meta)       (send event set-meta-down    #t)]
          [(eq? mod 'shift)      (send event set-shift-down   #t)]
          [(eq? mod 'left)       (send event set-left-down    #t)]
          [(eq? mod 'middle)     (send event set-middle-down  #t)]
          [(eq? mod 'right)      (send event set-right-down   #t)]
          [(eq? mod 'noalt)      (send event set-alt-down     #f)]
          [(eq? mod 'nocontrol)  (send event set-control-down #f)]
          [(eq? mod 'nometa)     (send event set-meta-down    #f)]
          [(eq? mod 'noshift)    (send event set-shift-down   #f)]
          [else  (error mouse-tag "unknown mouse modifier: ~e" mod)]))
      (set-mouse-modifiers event (cdr modifier-list)))))

(define mouse-type-const
  (λ (type)
    (cond
      [(symbol? type)
       (cond
         [(eq? type 'motion)  'motion]
         [(eq? type 'enter)   'enter]
         [(eq? type 'leave)   'leave]
         [else  (bad-mouse-type type)])]
      [(and (pair? type) (pair? (cdr type)))
       (let ([button (car type)] [action (cadr type)])
         (cond
           [(eq? button 'left)
            (cond 
              [(eq? action 'down)    'left-down]
              [(eq? action 'up)      'left-up]
              [(eq? action 'dclick)  'left-dclick]
              [else  (bad-mouse-type type)])]
           [(eq? button 'middle)
            (cond
              [(eq? action 'down)    'middle-down]
              [(eq? action 'up)      'middle-up]
              [(eq? action 'dclick)  'middle-dclick]
              [else  (bad-mouse-type type)])]
           [(eq? button 'right)
            (cond
              [(eq? action 'down)    'right-down]
              [(eq? action 'up)      'right-up]
              [(eq? action 'dclick)  'right-dclick]
              [else  (bad-mouse-type type)])]
           [else  (bad-mouse-type type)]))]
      [else  (bad-mouse-type type)])))

(define bad-mouse-type
  (λ (type)
    (error mouse-tag "unknown mouse event type: ~e" type)))

;;
;; Move mouse to new window.
;; Implement with three events:
;; leave old window, show top-level frame, enter new window, focus.
;;
;; NEED TO CLEAN UP ACTIONS FOR MOVING TO NEW FRAME.
;;

(define new-window
  (let ([tag  'test:new-window])
    (λ (new-window)
      (cond
        [(not (is-a? new-window window<%>))
         (error tag "new-window is not a window<%>")]
        [else
         (run-one
          (λ ()
            (let
                ([old-window  (get-focused-window)]
                 [leave   (make-object mouse-event% 'leave)]
                 [enter   (make-object mouse-event% 'enter)]
                 [root    (car (ancestor-list new-window #t))])
              (send leave  set-x 0)   (send leave  set-y 0)
              (send enter  set-x 0)   (send enter  set-y 0)
              
              ;; SOME KLUDGES HERE TO WORK AROUND TEXT% PROBLEMS.
              
              (when (and old-window (method-in-interface? 'on-event (object-interface old-window)))
                (send-mouse-event old-window leave))
              (send root show #t)
              (when (method-in-interface? 'on-event (object-interface new-window))
                (send-mouse-event new-window enter))
              (send new-window focus)
              (void))))]))))

(define (close-top-level-window tlw)
  (when (send tlw can-close?)
    (send tlw on-close)
    (send tlw show #f)))

;; manual renaming
(define test:run-interval run-interval)
(define test:number-pending-actions number-pending-actions)
(define test:reraise-error reraise-error)
(define test:run-one run-one)
(define test:current-get-eventspaces current-get-eventspaces)
(define test:close-top-level-window close-top-level-window)
(define test:button-push button-push)
(define test:set-radio-box! set-radio-box!)
(define test:set-radio-box-item! set-radio-box-item!)
(define test:set-check-box! set-check-box!)
(define test:set-choice! set-choice!)
(define test:set-list-box! set-list-box!)
(define test:keystroke keystroke)
(define test:menu-select menu-select)
(define test:mouse-click mouse-click)
(define test:new-window new-window)

(define (label-of-enabled/shown-button-in-top-level-window? str)
  (test:top-level-focus-window-has?
   (λ (c)
     (and (is-a? c button%)
          (string=? (send c get-label) str)
          (send c is-enabled?)
          (send c is-shown?)))))

(define (enabled-shown-button? btn)
  (and (send btn is-enabled?)
       (send btn is-shown?)))

(define (button-in-top-level-focusd-window? btn)
  (test:top-level-focus-window-has?
   (λ (c) (eq? c btn))))

(provide/doc
 (proc-doc/names
  test:button-push
  (-> (or/c (and/c string?
                   label-of-enabled/shown-button-in-top-level-window?)
            (and/c (is-a?/c button%)
                   enabled-shown-button?
                   button-in-top-level-focusd-window?))
      void?)
  (button)
  @{Simulates pushing @racket[button].  If a string is supplied, the
  primitive searches for a button labelled with that string in the
  active frame. Otherwise, it pushes the button argument.})
 
 (proc-doc/names
  test:set-radio-box!
  (-> (or/c string? regexp? (is-a?/c radio-box%)) (or/c string? number?) void?)
  (radio-box state)
  @{Sets the radio-box to the label matching @racket[state]. If @racket[state] is a
         string, this function finds the choice with that label. 
         If it is a regexp, this function finds the first choice whose label matches the regexp.
         If it is a number, it uses the number as an index into the
         state. If the number is out of range or if the label isn't
         in the radio box, an exception is raised.

         If @racket[radio-box] is a string, this function searches for a
         @racket[radio-box%] object with a label matching that string,
         otherwise it uses @racket[radio-box] itself.})
 
 (proc-doc/names
  test:set-radio-box-item!
  (-> (or/c string? regexp?) void?)
  (entry)
  @{Finds a @racket[radio-box%] that has a label matching @racket[entry]
          and sets the radio-box to @racket[entry].})
 
 (proc-doc/names
  test:set-check-box!
  (-> (or/c string? (is-a?/c check-box%)) boolean? void?)
  (check-box state)
  @{Clears the @racket[check-box%] item if @racket[state] is @racket[#f], and sets it
  otherwise.
  
  If @racket[check-box] is a string,
  this function searches for a @racket[check-box%] with a label matching that string,
  otherwise it uses @racket[check-box] itself.})
 
 (proc-doc/names
  test:set-choice!
  (-> (or/c string? (is-a?/c choice%)) (or/c string? (and/c number? exact? integer? positive?))
      void?)
  (choice str)
  @{Selects @racket[choice]'s item @racket[str]. If @racket[choice] is a string,
  this function searches for a @racket[choice%] with a label matching
  that string, otherwise it uses @racket[choice] itself.})
 
 (proc-doc/names
  test:set-list-box!
  (-> (or/c string? (is-a?/c list-box%)) (or/c string? (and/c number? exact? integer? positive?)) 
      void?)
  (choice str)
  @{Selects @racket[list-box]'s item @racket[str]. If @racket[list-box] is a string,
  this function searches for a @racket[list-box%] with a label matching
  that string, otherwise it uses @racket[list-box] itself.})
 
 (proc-doc/names
  test:keystroke
  (->* ((or/c char? symbol?))
       ((listof (or/c 'alt 'control 'meta 'shift 
                      'noalt 'nocontrol 'nometea 'noshift)))
       void?)
  ((key)
   ((modifier-list null)))
  @{This function simulates a user pressing a key. The argument, @racket[key],
  is just like the argument to the
  @method[key-event% get-key-code]
  method of the @racket[key-event%] class. 
  
  @italic{Note:}
  To send the ``Enter'' key, use @racket[#\return],
  not @racket[#\newline].
  
  The @racket['shift] or @racket['noshift] modifier is implicitly set from @racket[key],
  but is overridden by the argument list. The @racket['shift] modifier is
  set for any capitol alpha-numeric letters and any of the following characters:
  @racketblock[
   #\? #\: #\~ #\\ #\|
   #\< #\> #\{ #\} #\[ #\] #\( #\)
   #\! #\@ #\# #\$ #\% #\^ #\& #\* #\_ #\+
  ]
  
  If conflicting modifiers are provided, the ones later in the list are used.})
 
 (proc-doc
  test:menu-select
  (->i ([menu string?]) () #:rest [items (listof string?)] [res void?])
  @{Selects the menu-item named by the @racket[item]s in the menu named @racket[menu].
  
  @italic{Note:}
  The string for the menu item does not include its keyboard equivalent.
  For example, to select ``New'' from the ``File'' menu, 
  use ``New'', not ``New Ctrl+N''.})
 
 (proc-doc/names
  test:mouse-click
  (->*
   ((or/c 'left 'middle 'right)
    (and/c exact? integer?)
    (and/c exact? integer?))
   ((listof (or/c 'alt 'control 'meta 'shift 'noalt
                  'nocontrol 'nometa 'noshift)))
   void?)
  ((button x y)
   ((modifiers null)))
  @{Simulates a mouse click at the coordinate (x,y) in the currently
  focused @racket[window], assuming that it supports the 
  @method[canvas<%> on-event] method.
  Use @racket[test:button-push] to click on a button.
  
  Under Mac OS X, @racket['right] corresponds to holding down the command
  modifier key while clicking and @racket['middle] cannot be generated.
  
  Under Windows, @racket['middle] can only be generated if the user has a
  three button mouse.
  
  The modifiers later in the list @racket[modifiers] take precedence over
  ones that appear earlier.})

 (proc-doc/names
  test:run-interval
  (case->
   (number? . -> . void?)
   (-> number?))
  ((msec) ())
  @{See also @secref{test:actions-completeness}.
  The first case in the case-lambda sets
  the run interval to @racket[msec] milliseconds and the second
  returns the current setting.})
 
 (parameter-doc
  test:current-get-eventspaces
  (parameter/c (-> (listof eventspace?)))
  func
  
  @{This parameter that specifies which evenspaces
         (see also @secref[#:doc '(lib "scribblings/gui/gui.scrbl") "eventspaceinfo"])
  are considered when finding the frontmost frame.
  The first case
  sets the parameter to @racket[func]. The procedure @racket[func] will be
  invoked with no arguments to determine the eventspaces to consider
  when finding the frontmost frame for simulated user events.
  The second case
  returns the current value of the parameter. This will be a procedure
  which, when invoked, returns a list of eventspaces.})
 
 (proc-doc/names
  test:new-window 
  (-> (is-a?/c window<%>) void?)
  (window)
  @{Moves the keyboard focus to a new window within the currently active
          frame.  Unfortunately, neither this function nor any other function in
          the test engine can cause the focus to move from the top-most (active) frame.})
 
 (proc-doc/names 
  test:close-top-level-window
  (-> (is-a?/c top-level-window<%>) void?)
  (tlw)
  @{Use this function to simulate clicking on the close box of a frame.
  Closes @racket[tlw] with this expression:
  @racketblock[
   (when (send tlw can-close?)
     (send tlw on-close)
     (send tlw show #f))]})
 
 (proc-doc/names
  test:top-level-focus-window-has?
  (-> (-> (is-a?/c area<%>) boolean?) boolean?)
  (test)
  @{Calls @racket[test] for each child of the @racket[test:get-active-top-level-window]
          and returns @racket[#t] if @racket[test] ever does, otherwise
          returns @racket[#f]. If there
          is no top-level-focus-window, returns @racket[#f].})
 
 
 (proc-doc
  test:number-pending-actions
  (-> number?)
  @{Returns the number of pending events (those that haven't completed yet)})
 
 (proc-doc
  test:reraise-error
  (-> void?)
  @{See also @secref{test:errors}.})
 
 (proc-doc/names
  test:run-one
  (-> (-> void?) void?)
  (f)
  @{Runs the function @racket[f] as if it was a simulated event.})
 
 (parameter-doc
  test:use-focus-table
  (parameter/c (or/c boolean? 'debug))
  use-focus-table?
  @{If @racket[#t], then the test framework uses @racket[frame:lookup-focus-table] to determine
    which is the focused frame. If @racket[#f], then it uses @racket[get-top-level-focus-window].
    If @racket[test:use-focus-table]'s value is @racket['debug], then it still uses 
    @racket[frame:lookup-focus-table] but it also prints a message to the @racket[current-error-port]
    when the two methods would give different results.})
 
 (proc-doc/names
  test:get-active-top-level-window
  (-> (or/c (is-a?/c frame%) (is-a?/c dialog%) #f))
  ()
  @{Returns the frontmost frame, based on @racket[test:use-focus-table].})
 
 (proc-doc/names
  label-of-enabled/shown-button-in-top-level-window?
  (-> string? boolean?)
  (label)
  @{Returns @racket[#t] when @racket[label] is
            the label of an enabled and shown
            @racket[button%] instance that
            is in the top-level window that currently
            has the focus, using @racket[test:top-level-focus-window-has?].})
 
 (proc-doc/names
  enabled-shown-button?
  (-> (is-a?/c button%) boolean?)
  (button)
  @{Returns @racket[#t] when @racket[button]
            is both enabled and shown.})
 
 (proc-doc/names
  button-in-top-level-focusd-window?
  (-> (is-a?/c button%) boolean?)
  (button)
  @{Returns @racket[#t] when @racket[button] is
            in the top-level focused window.}))

#lang racket/unit

(require string-constants
         racket/class
         racket/contract/base
         racket/path
         "search.rkt"
         "sig.rkt"
         "../preferences.rkt"
         "../gui-utils.rkt"
         "bday.rkt"
         "gen-standard-menus.rkt"
         framework/private/focus-table
         mrlib/close-icon
         mred/mred-sig)

(import mred^
        [prefix group: framework:group^]
        [prefix preferences: framework:preferences^]
        [prefix icon: framework:icon^]
        [prefix handler: framework:handler^]
        [prefix application: framework:application^]
        [prefix panel: framework:panel^]
        [prefix finder: framework:finder^]
        [prefix keymap: framework:keymap^]
        [prefix text: framework:text^]
        [prefix pasteboard: framework:pasteboard^]
        [prefix editor: framework:editor^]
        [prefix canvas: framework:canvas^]
        [prefix menu: framework:menu^]
        [prefix racket: framework:racket^]
        [prefix exit: framework:exit^]
        [prefix comment-box: framework:comment-box^])

(export (rename framework:frame^ 
                [-editor<%> editor<%>]
                [-pasteboard% pasteboard%]
                [-text% text%]))

(init-depend mred^ framework:text^ framework:canvas^)

(define (reorder-menus frame)
  (define items (send (send frame get-menu-bar) get-items))
  (define (find-menu name)
    (ormap (λ (i) (and (string=? (send i get-plain-label) name) i))
           items))
  (let* ([file-menu (find-menu (string-constant file-menu))]
         [edit-menu (find-menu (string-constant edit-menu))]
         [windows-menu (or (find-menu (string-constant windows-menu))
                           (find-menu (string-constant tabs-menu)))]
         [help-menu (find-menu (string-constant help-menu))]
         [other-items
          (remq* (list file-menu edit-menu windows-menu help-menu) items)]
         [re-ordered (filter values `(,file-menu ,edit-menu
                                                 ,@other-items
                                                 ,windows-menu ,help-menu))])
    (for-each (λ (item) (send item delete)) items)
    (for-each (λ (item) (send item restore)) re-ordered)))

(define (remove-empty-menus frame)
  (define menus (send (send frame get-menu-bar) get-items))
  (for-each (λ (menu) (send menu delete)) menus)
  (for-each (λ (menu)
              (when (pair? (send menu get-items)) (send menu restore)))
            menus))

(define add-snip-menu-items
  (lambda (edit-menu c% [func void])
    (let* ([get-edit-target-object
            (λ ()
              (let ([menu-bar
                     (let loop ([p (send edit-menu get-parent)])
                       (cond
                         [(is-a? p menu-bar%)
                          p]
                         [(is-a? p menu%)
                          (loop (send p get-parent))]
                         [else #f]))])
                (and menu-bar
                     (let ([frame (send menu-bar get-frame)])
                       (send frame get-edit-target-object)))))]
           [edit-menu:do 
            (λ (const)
              (λ (menu evt)
                (let ([edit (get-edit-target-object)])
                  (when (and edit
                             (is-a? edit editor<%>))
                    (send edit do-edit-operation const)))
                #t))]
           [on-demand
            (λ (menu-item)
              (let ([edit (get-edit-target-object)])
                (send menu-item enable (and edit (is-a? edit editor<%>)))))]
           [insert-comment-box
            (λ ()
              (let ([text (get-edit-target-object)])
                (when text
                  (let ([snip (make-object comment-box:snip%)])
                    (send text insert snip)
                    (send text set-caret-owner snip 'global)))))])
      
      (let ([item
             (new c%
                  [label (string-constant insert-comment-box-menu-item-label)]
                  [parent edit-menu]
                  [callback (λ (x y) (insert-comment-box))]
                  [demand-callback on-demand])])
        (func item))
      (define plain-insert-callback (edit-menu:do 'insert-image))
      (define lowercase-imgs "*.png;*.jpg;*.jpeg;*.gif;*.xpm;*.bmp")
      (define imgs (string-append
                    lowercase-imgs ";"
                    (string-upcase lowercase-imgs)))
      (let ([item 
             (new c% 
                  [label (string-constant insert-image-item)]
                  [parent edit-menu]
                  [callback (λ (menu evt)
                              (parameterize ([finder:default-filters 
                                              `(["Image" ,imgs]
                                                ["Any" "*.*"])])
                                (plain-insert-callback menu evt)))]
                  [demand-callback on-demand])])
        (func item))
      (void))))

(define frame-width 600)
(define frame-height 650)
(let ([window-trimming-upper-bound-width 20]
      [window-trimming-upper-bound-height 50])
  (let-values ([(w h) (get-display-size)])
    (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
    (set! frame-height (min frame-height (- h window-trimming-upper-bound-height)))))

(define basic<%> (interface ((class->interface frame%))
                   get-area-container%
                   get-area-container
                   get-menu-bar%
                   make-root-area-container
                   close
                   editing-this-file?
                   get-filename
                   make-visible))

(define focus-table<%> (interface (top-level-window<%>)))
(define focus-table-mixin
  (mixin (top-level-window<%>) (focus-table<%>)
    (inherit get-eventspace)
    
    (define/override (show on?)
      (define old (remove this (frame:lookup-focus-table (get-eventspace))))
      (define new (if on? (cons this old) old))
      (frame:set-focus-table (get-eventspace) new)
      (super show on?))
    
    (define/augment (on-close)
      (frame:set-focus-table (get-eventspace) (remove this (frame:lookup-focus-table (get-eventspace))))
      (inner (void) on-close))
    
    (super-new)
    
    (frame:set-focus-table (get-eventspace) (frame:lookup-focus-table (get-eventspace)))))

(define basic-mixin
  (mixin ((class->interface frame%)) (basic<%>)
    
    (define/override (show on?)
      (if on?
          (send (group:get-the-frame-group) insert-frame this)
          (send (group:get-the-frame-group) remove-frame this))
      (super show on?))
    
    (define/override (can-exit?)
      (and (exit:user-oks-exit)
           (begin
             (exit:set-exiting #t)
             (let ([res (exit:can-exit?)])
               (unless res
                 (exit:set-exiting #f))
               res))))
    (define/override (on-exit) 
      (exit:on-exit)
      (queue-callback
       (λ ()
         (exit)
         (exit:set-exiting #f))))
    
    (define/public (make-visible filename) (void))
    (define/public get-filename
      (case-lambda
        [() (get-filename #f)]
        [(b) #f]))
    
    (define/public (editing-this-file? filename) #f)
    
    (define/override (on-superwindow-show shown?)
      (send (group:get-the-frame-group) frame-shown/hidden this)
      (super on-superwindow-show shown?))
    
    (define after-init? #f)
    
    (define/override on-drop-file
      (λ (filename)
        (handler:edit-file filename)))
    
    ;; added call to set label here to hopefully work around a problem in mac mred
    (inherit set-label change-children)
    (define/override after-new-child
      (λ (child)
        (when after-init?
          (change-children (λ (l) (remq child l)))
          (error 'frame:basic-mixin
                 (string-append
                  "do not add children directly to a frame:basic (unless using make-root-area-container); "
                  "use the get-area-container method instead")))))
    
    (define/public get-area-container% (λ () vertical-panel%))
    (define/public get-menu-bar% (λ () menu-bar%))
    (define/public make-root-area-container
      (λ (% parent)
        (make-object % parent)))
    
    (inherit on-close can-close?)
    (define/public (close)
      (when (can-close?)
        (on-close)
        (show #f)))
    
    (inherit accept-drop-files)
    
    (super-new)
    
    (accept-drop-files #t)
    
    (inherit set-icon)
    (let ([icon (current-icon)])
      (when icon
        (if (pair? icon)
            (let ([small (car icon)]
                  [large (cdr icon)])
              (set-icon small (send small get-loaded-mask) 'small)
              (set-icon large (send large get-loaded-mask) 'large))
            (set-icon icon (send icon get-loaded-mask) 'both))))
    
    (group:create-windows-menu (make-object (get-menu-bar%) this))
    
    (reorder-menus this)
    
    [define panel (make-root-area-container (get-area-container%) this)]
    (define/public (get-area-container) panel)
    (set! after-init? #t)))

(define current-icon (make-parameter #f))

(define size-pref<%>
  (interface (basic<%>)
    adjust-size-when-monitor-setup-changes?))

(define-local-member-name monitor-setup-changed)

(define size-pref-mixin
  (mixin (basic<%>) (size-pref<%>)
    (init-field size-preferences-key
                [position-preferences-key #f])
    (inherit is-maximized?)
    (define/override (on-size w h)
      
      ;; if the monitor state is currently fluxuating, then
      ;; don't save preferences values, since the OS is doing
      ;; some adjustments for us.
      (when (or (equal? (compute-current-monitor-information)
                        latest-monitor-information)
                (not (adjust-size-when-monitor-setup-changes?)))
        (define old-table (preferences:get size-preferences-key)) 
        (define new-val
          (cond
            [(is-maximized?)
             (define old (or (hash-ref old-table latest-monitor-information #f)
                             (hash-ref old-table #f)))
             (cons #t (cdr old))]
            [else
             (list #f w h)]))
        (preferences:set size-preferences-key 
                         (hash-set*
                          old-table
                          latest-monitor-information new-val
                          #f new-val)))
      (super on-size w h))
      
    (define on-move-timer-arg-x #f)
    (define on-move-timer-arg-y #f)
    (define on-move-timer-arg-max? #f)
    (define on-move-callback-running? #f)
    (define/override (on-move x y)
      (when position-preferences-key
        (unless on-move-callback-running?
          (set! on-move-callback-running? #t)
          (queue-callback
           (λ () 
             (set! on-move-callback-running? #f)
             (unless on-move-timer-arg-max?
               
               ;; if the monitor state is currently fluxuating, then
               ;; don't save preferences values, since the OS is doing
               ;; some adjustments for us.
               (when (or (equal? (compute-current-monitor-information)
                                 latest-monitor-information)
                         (not (adjust-size-when-monitor-setup-changes?)))
                 (define-values (monitor delta-x delta-y) (find-closest on-move-timer-arg-x on-move-timer-arg-y))
                 (define old-table (preferences:get position-preferences-key))
                 (define val (list monitor delta-x delta-y))
                 (preferences:set position-preferences-key
                                  (hash-set*
                                   old-table
                                   latest-monitor-information val
                                   #f val)))))
           #f))
        (set! on-move-timer-arg-x x)
        (set! on-move-timer-arg-y y)
        (set! on-move-timer-arg-max? (is-maximized?)))
      (super on-move x y))
    
    (define/augment (display-changed)
      (restart-montior-information-timer)
      (inner (void) display-changed))
    
    ;; if all of the offsets have some negative direction, then
    ;; just keep the thing relative to the original montior; otherwise
    ;; make it relative to whatever origin it is closest to.
    (define/private (find-closest x y)
      (define closest 0)
      (define-values (delta-x delta-y dist) (find-distance x y 0))
      
      (for ([m (in-range 1 (get-display-count))])
        (define-values (new-delta-x new-delta-y new-dist) 
          (find-distance x y m))
        (when (and new-delta-x
                   new-delta-y
                   (new-delta-x . >= . 0)
                   (new-delta-y . >= . 0))
          (when (< new-dist dist)
            (set! closest m)
            (set!-values (delta-x delta-y dist) (values new-delta-x new-delta-y new-dist)))))
      
      (values closest delta-x delta-y))
    
    (define/private (find-distance x y mon)
      (define-values (delta-x delta-y)
        (let-values ([(l t) (get-display-left-top-inset #:monitor mon)])
          (values (+ x l) (+ y t))))
      (values delta-x
              delta-y
              (sqrt (+ (* delta-x delta-x)
                       (* delta-y delta-y)))))
    
    (inherit maximize)
    (define/private (get-sizes/maximzed)
      (define size-table (preferences:get size-preferences-key))
      (define-values (maximized? w h) (apply values (or (hash-ref size-table latest-monitor-information #f)
                                                        (hash-ref size-table #f))))
      (define-values (x y origin-still-visible?)
        (cond
          [position-preferences-key
           (define pos-table (preferences:get position-preferences-key))
           (define-values (monitor delta-x delta-y) (apply values (or (hash-ref pos-table latest-monitor-information #f)
                                                                      (hash-ref pos-table #f))))
           (define-values (l t) (get-display-left-top-inset #:monitor monitor))
           (define-values (mw mh) (get-display-size #:monitor monitor))
           (if (and l t mw mh)
               (values (- delta-x l) 
                       (- delta-y t)
                       (and (<= 0 l mw)
                            (<= 0 t mh)))
               (values #f #f #f))]
          [else
           (values #f #f #f)]))
      (define (already-one-there? x y w h)
        (for/or ([fr (in-list (get-top-level-windows))])
          (and (equal? x (send fr get-x))
               (equal? y (send fr get-y))
               (equal? w (send fr get-width))
               (equal? h (send fr get-height))
               (equal? maximized? (send fr is-maximized?)))))
      (cond
        [(or (and (already-one-there? x y w h)
                  (not maximized?))
             (not origin-still-visible?))
         ;; these are the situations where we look for a different position of the window
         (let loop ([n 50]
                    [x 0]
                    [y 0])
           (cond
             [(zero? n)
              (values #f #f #f #f maximized?)]
             [(already-one-there? x y w h)
              (define-values (dw dh) (get-display-size #:monitor 0))
              (define sw (- dw w))
              (define sh (- dh h))
              (if (or (<= sw 0)
                      (<= sh 0))
                (values #f #f #f #f maximized?)
                (loop (- n 1) 
                      (modulo (+ x 20) (- dw w))
                      (modulo (+ y 20) (- dh h))))]
             [else
              (values w h x y maximized?)]))]
        [else
         (values w h x y maximized?)]))
    
    (define/public (adjust-size-when-monitor-setup-changes?) #f)
    
    (inherit begin-container-sequence
             end-container-sequence
             move
             resize)
    (define/public (monitor-setup-changed) 
      (when (adjust-size-when-monitor-setup-changes?)
        (define-values (w h x y maximized?) (get-sizes/maximzed))
        (when (and w h x y)
          (begin-container-sequence)
          (move x y)
          (resize w h)
          (when maximized?
            (maximize #t))
          (end-container-sequence))))
    
    (let-values ([(w h x y maximized?) (get-sizes/maximzed)])
      (cond
        [(and w h x y)
         (super-new [width w] [height h] [x x] [y y])]
        [else
         (super-new)])
      (when maximized?
        (maximize #t)))))

(define (setup-size-pref size-preferences-key w h 
                         #:maximized? [maximized? #f]
                         #:position-preferences [position-preferences-key #f])
  (preferences:set-default size-preferences-key 
                           (hash #f (list maximized? w h))
                           (hash/c (or/c (listof (list/c any/c any/c any/c any/c))
                                         #f)
                                   (list/c boolean?
                                           exact-nonnegative-integer?
                                           exact-nonnegative-integer?)
                                   #:immutable #t
                                   #:flat? #t))
  (when position-preferences-key
    (preferences:set-default position-preferences-key 
                             (hash #f (list 0 0 0))
                             (hash/c (or/c (listof (list/c any/c any/c any/c any/c))
                                           #f)
                                     (list/c exact-nonnegative-integer?
                                             exact-integer?
                                             exact-integer?)
                                     #:immutable #t
                                     #:flat? #t))))

(define (compute-current-monitor-information)
  (filter
   values
   (for/list ([m (in-range (get-display-count))])
     (define-values (left top) (get-display-left-top-inset #:monitor m))
     (define-values (width height) (get-display-size #:monitor m))
     (and left top width height
          (list left top width height)))))

(define latest-monitor-information (compute-current-monitor-information))
(define pending-monitor-information #f)
(define montior-information-timer
  (new timer%
       [notify-callback
        (λ ()
          (define new-monitor-information (compute-current-monitor-information))
          (cond
            [pending-monitor-information
             (cond
               [(equal? pending-monitor-information new-monitor-information)
                (set! pending-monitor-information #f)
                (set! latest-monitor-information new-monitor-information)
                (queue-callback
                 (λ ()
                   (when (editor:get-change-font-size-when-monitors-change?)
                     (editor:set-current-preferred-font-size
                      (editor:get-current-preferred-font-size)))
                   (for ([frame (in-list (get-top-level-windows))])
                     (when (is-a? frame size-pref<%>)
                       (send frame monitor-setup-changed)))
                   #f))]
               [else
                (set! pending-monitor-information new-monitor-information)
                (restart-montior-information-timer)])]
            [else
             (unless (equal? latest-monitor-information new-monitor-information)
               (set! pending-monitor-information new-monitor-information)
               (restart-montior-information-timer))]))]))
(define (restart-montior-information-timer)
  (send montior-information-timer stop)
  (send montior-information-timer start 250 #t))


(define register-group<%> (interface ()))
(define register-group-mixin
  (mixin (basic<%>) (register-group<%>)
    
    (define/augment (can-close?)
      (and (inner #t can-close?)
           (group:can-close-check)))
    (define/augment (on-close)
      (send (group:get-the-frame-group)
            remove-frame
            this)
      (inner (void) on-close)
      (group:on-close-action))
    
    (define/override (on-activate on?)
      (super on-activate on?)
      (when on?
        (send (group:get-the-frame-group) set-active-frame this)))
    
    (super-new)
    (send (group:get-the-frame-group) insert-frame this)))

(define locked-message (string-constant read-only))

(define lock-canvas%
  (class canvas%
    (field [locked? #f])
    (inherit refresh)
    (define/public (set-locked l)
      (unless (eq? locked? l)
        (set! locked? l)
        (setup-sizes)
        (refresh)))
    (inherit get-client-size get-dc)
    (define/override (on-paint)
      (let* ([dc (get-dc)]
             [draw
              (λ (str bg-color bg-style line-color line-style)
                (send dc set-font small-control-font)
                (let-values ([(w h) (get-client-size)]
                             [(tw th _1 _2) (send dc get-text-extent str)])
                  (send dc set-pen (send the-pen-list find-or-create-pen line-color 1 line-style))
                  (send dc set-brush (send the-brush-list find-or-create-brush bg-color bg-style))
                  (send dc draw-rectangle 0 0 w h)
                  (send dc draw-text str
                        (- (/ w 2) (/ tw 2))
                        (- (/ h 2) (/ th 2)))))])
        (when locked?
          (draw locked-message "yellow" 'solid "black" 'solid))))
    
    (inherit get-parent min-width min-height stretchable-width stretchable-height)
    (define/private (setup-sizes)
      (let ([dc (get-dc)])
        (if locked?
            (let-values ([(w h _1 _2) (send dc get-text-extent locked-message)])
              (min-width (inexact->exact (floor (+ w 4))))
              (min-height (inexact->exact (floor (+ h 2)))))
            (begin
              (min-width 0)
              (min-height 0)))))
    
    (super-new [style '(transparent no-focus)])
    
    (send (get-dc) set-font small-control-font)
    (setup-sizes)
    (stretchable-width #f)
    (stretchable-height #t)))

(define status-line<%>
  (interface (basic<%>)
    open-status-line
    close-status-line
    update-status-line))

;; status-line : (make-status-line symbol number)
(define-struct status-line (id count))

;; status-line-msg : (make-status-line-msg (is-a?/c message%) (union symbol #f))
(define-struct status-line-msg (message [id #:mutable]))

(define status-line-mixin
  (mixin (basic<%>) (status-line<%>)
    (field [status-line-container-panel #f]
           
           ;; status-lines : (listof status-line)
           [status-lines null]
           
           ;; status-line-msgs : (listof status-line-msg)
           [status-line-msgs null])
    (define/override (make-root-area-container % parent)
      (let* ([s-root (super make-root-area-container vertical-panel% parent)]
             [r-root (make-object % s-root)])
        (set! status-line-container-panel
              (instantiate vertical-panel% ()
                (parent s-root)
                (stretchable-height #f)))
        r-root))
    (define/public (open-status-line id)
      (do-main-thread
       (λ ()
         (when status-line-container-panel
           (set! status-lines
                 (let loop ([status-lines status-lines])
                   (cond
                     [(null? status-lines)
                      (list (make-status-line id 1))]
                     [else (let ([status-line (car status-lines)])
                             (if (eq? id (status-line-id status-line))
                                 (cons (make-status-line id (+ (status-line-count status-line) 1))
                                       (cdr status-lines))
                                 (cons status-line (loop (cdr status-lines)))))])))))))
    
    (define/public (close-status-line id)
      (do-main-thread
       (λ ()
         (when status-line-container-panel
           
           ;; decrement counter in for status line, or remove it if
           ;; counter goes to zero.
           (set! status-lines
                 (let loop ([status-lines status-lines])
                   (cond
                     [(null? status-lines) (error 'close-status-line "status line not open ~e" id)]
                     [else (let* ([status-line (car status-lines)]
                                  [this-line? (eq? (status-line-id status-line) id)])
                             (cond
                               [(and this-line? (= 1 (status-line-count status-line)))
                                (cdr status-lines)]
                               [this-line?
                                (cons (make-status-line id (- (status-line-count status-line) 1))
                                      (cdr status-lines))]
                               [else (cons status-line (loop (cdr status-lines)))]))])))
           
           ;; make sure that there are only as many messages as different status lines, in total
           (let ([status-line-msg (find-status-line-msg id)])
             (when status-line-msg
               (send (status-line-msg-message status-line-msg) set-label "")
               (set-status-line-msg-id! status-line-msg #f)))
           (let* ([msgs-that-can-be-removed (filter (λ (x) (not (status-line-msg-id x))) status-line-msgs)]
                  [max-to-include (length status-lines)]
                  [msgs-to-remove
                   (let loop ([n max-to-include]
                              [l msgs-that-can-be-removed])
                     (cond
                       [(null? l) l]
                       [(zero? n) l]
                       [else (loop (- n 1) (cdr l))]))])
             (send status-line-container-panel
                   change-children
                   (λ (old-children)
                     (foldl (λ (status-line-msg l)
                              (remq (status-line-msg-message status-line-msg) l))
                            old-children
                            msgs-to-remove)))
             (set! status-line-msgs
                   (let loop ([l msgs-to-remove]
                              [status-line-msgs status-line-msgs])
                     (cond
                       [(null? l) status-line-msgs]
                       [else (loop (cdr l)
                                   (remq (car l) status-line-msgs))]))))))))
    
    ;; update-status-line : symbol (union #f string)
    (define/public (update-status-line id msg-txt)
      (do-main-thread
       (λ ()
         (unless (open-status-line? id)
           (error 'update-status-line "unknown id ~e, other arg ~e" id msg-txt))
         (if msg-txt
             (cond
               [(find-status-line-msg id)
                =>
                (λ (existing-status-line-msg)
                  (let ([msg (status-line-msg-message existing-status-line-msg)])
                    (unless (equal? (send msg get-label) msg-txt)
                      (send msg set-label msg-txt))))]
               [(find-available-status-line-msg)
                =>
                (λ (available-status-line-msg)
                  (send (status-line-msg-message available-status-line-msg) set-label msg-txt)
                  (set-status-line-msg-id! available-status-line-msg id))]
               [else
                (set! status-line-msgs
                      (cons (make-new-status-line-msg id msg-txt)
                            status-line-msgs))])
             (let ([status-line-msg (find-status-line-msg id)])
               (when status-line-msg
                 (send (status-line-msg-message status-line-msg) set-label "")
                 (set-status-line-msg-id! status-line-msg #f)))))))
    
    ;; open-status-line? : symbol -> boolean
    (define/private (open-status-line? id)
      (let loop ([status-lines status-lines])
        (cond
          [(null? status-lines) #f]
          [else
           (let ([status-line (car status-lines)])
             (or (eq? (status-line-id status-line) id)
                 (loop (cdr status-lines))))])))
    
    ;; find-available-status-line-msg : -> (union #f status-line-msg)
    (define/private (find-available-status-line-msg)
      (let loop ([status-line-msgs status-line-msgs])
        (cond
          [(null? status-line-msgs) #f]
          [else (let ([status-line-msg (car status-line-msgs)])
                  (if (status-line-msg-id status-line-msg)
                      (loop (cdr status-line-msgs))
                      status-line-msg))])))
    
    ;; find-status-line-msg : symbol -> (union #f status-line-msg)
    (define/private (find-status-line-msg id)
      (let loop ([status-line-msgs status-line-msgs])
        (cond
          [(null? status-line-msgs) #f]
          [else (let ([status-line-msg (car status-line-msgs)])
                  (if (eq? id (status-line-msg-id status-line-msg))
                      status-line-msg
                      (loop (cdr status-line-msgs))))])))
    
    ;; make-new-status-line-msg : symbol string -> status-line-msg
    (define/private (make-new-status-line-msg id msg-txt)
      (define msg
        (new message% 
             [parent status-line-container-panel]
             [stretchable-width #t]
             [label ""]))
      (send msg set-label msg-txt)
      (make-status-line-msg msg id))
    
    (inherit get-eventspace)
    (define/private (do-main-thread t)
      (let ([c-eventspace (current-eventspace)])
        (if (and (eq? c-eventspace (get-eventspace))
                 (eq? (current-thread) (eventspace-handler-thread c-eventspace)))
            (t)
            (parameterize ([current-eventspace c-eventspace])
              ;; need high priority callbacks to ensure ordering wrt other callbacks
              (queue-callback t #t)))))
    
    (super-new)))

(define info<%> (interface (basic<%>)
                  determine-width
                  lock-status-changed
                  update-info
                  set-info-canvas
                  get-info-canvas
                  get-info-editor
                  get-info-panel
                  show-info
                  hide-info
                  is-info-hidden?))

(define magic-space 25)

(define info-mixin
  (mixin (basic<%>) (info<%>)
    [define rest-panel 'uninitialized-root]
    [define super-root 'uninitialized-super-root]
    (define/override (make-root-area-container % parent)
      (let* ([s-root (super make-root-area-container
                            vertical-panel%
                            parent)]
             [r-root (make-object % s-root)])
        (set! super-root s-root)
        (set! rest-panel r-root)
        r-root))
    
    (define info-canvas #f)
    (define/public (get-info-canvas) info-canvas)
    (define/public (set-info-canvas c) (set! info-canvas c))
    (define/public (get-info-editor)
      (and info-canvas
           (send info-canvas get-editor)))
    
    (define/public (determine-width string canvas edit)
      (send edit set-autowrap-bitmap #f)
      (send canvas call-as-primary-owner
            (λ ()
              (let ([lb (box 0)]
                    [rb (box 0)])
                (send edit erase)
                (send edit insert string)
                (send edit position-location 
                      (send edit last-position)
                      rb)
                (send edit position-location 0 lb)
                (send canvas min-width 
                      (+ (get-client-width/view-delta edit canvas)
                         (- (inexact->exact (floor (unbox rb)))
                            (inexact->exact (floor (unbox lb))))))))))
    
    (define outer-info-panel 'top-info-panel-uninitialized)
    
    ;; this flag is specific to this frame
    ;; the true state of the info panel is
    ;; the combination of this flag and the 
    ;; 'framework:show-status-line preference
    ;; as shown in update-info-visibility
    (define info-hidden? #f)
    (define/public (hide-info)
      (set! info-hidden? #t)
      (update-info-visibility (preferences:get 'framework:show-status-line)))
    (define/public (show-info)
      (set! info-hidden? #f)
      (update-info-visibility (preferences:get 'framework:show-status-line)))
    (define/public (is-info-hidden?) info-hidden?)
    (define/private (update-info-visibility pref-value)
      (cond
        [(or info-hidden? (not pref-value))
         (send super-root change-children
               (λ (l)
                 (if (memq outer-info-panel l)
                     (begin (unregister-collecting-blit gc-canvas)
                            (unregister-pref-save-callback)
                            (list rest-panel))
                     l)))]
        [else
         (send super-root change-children
               (λ (l)
                 (if (memq outer-info-panel l)
                     l
                     (begin
                       (register-gc-blit)
                       (register-pref-save-callback)
                       (list rest-panel outer-info-panel)))))]))
    
    [define close-panel-callback
      (preferences:add-callback
       'framework:show-status-line
       (λ (p v)
         (update-info-visibility v)))]
    (define memory-cleanup void) ;; only for checkouts and nightly build users; used with memory-text
    
    (define/augment (on-close)
      (unregister-collecting-blit gc-canvas)
      (unregister-pref-save-callback)
      (close-panel-callback)
      (memory-cleanup)
      (inner (void) on-close))
    
    (define icon-currently-locked? 'uninit)
    (define/public (lock-status-changed)
      (let ([info-edit (get-info-editor)])
        (cond
          [(not (object? lock-canvas))
           (void)]
          [(is-a? info-edit editor:file<%>)
           (unless (send lock-canvas is-shown?)
             (send lock-canvas show #t))
           (let ([locked-now? (not (send info-edit get-read-write?))])
             (unless (eq? locked-now? icon-currently-locked?)
               (set! icon-currently-locked? locked-now?)
               (when (object? lock-canvas)
                 (send lock-canvas set-locked locked-now?))))]
          [else
           (when (send lock-canvas is-shown?)
             (send lock-canvas show #f))])))
    
    (define/public (update-info) (lock-status-changed))
    
    (super-new)
    (set! outer-info-panel (make-object horizontal-panel% super-root))
    (send outer-info-panel stretchable-height #f)
    
    (define info-panel (new horizontal-panel% [parent outer-info-panel]))
    (new grow-box-spacer-pane% [parent outer-info-panel])
    
    (define/public (get-info-panel) info-panel)
    (define/public (update-memory-text)
      (when show-memory-text?
        (for-each
         (λ (memory-canvas)
           (send memory-canvas set-str (format-number (current-memory-use))))
         memory-canvases)))
    
    (define/private (format-number n)
      (let* ([mbytes (/ n 1024 1024)]
             [before-decimal (floor mbytes)]
             [after-decimal (modulo (floor (* mbytes 100)) 100)])
        (string-append
         (number->string before-decimal)
         "."
         (cond
           [(<= after-decimal 9) (format "0~a" after-decimal)]
           [else (number->string after-decimal)])
         " MB")))
    
    (define/private (pad-to-3 n)
      (cond
        [(<= n 9) (format "00~a" n)]
        [(<= n 99) (format "0~a" n)]
        [else (number->string n)]))
    
    (define pref-save-canvas #f)
    (when checkout-or-nightly?
      (set! pref-save-canvas (new pref-save-canvas% [parent (get-info-panel)])))
    
    [define lock-canvas (make-object lock-canvas% (get-info-panel))]
    
    ; only for checkouts and nightly build users
    (when show-memory-text?
      (let* ([panel (new horizontal-panel%
                         [parent (get-info-panel)]
                         [stretchable-width #f]
                         [stretchable-height #f])]
             [ec (new position-canvas%
                      [parent panel]
                      [button-up
                       (λ (evt)
                         (cond
                           [(or (send evt get-alt-down)
                                (send evt get-control-down))
                            (dynamic-require 'tests/drracket/follow-log #f)]
                           [else
                            (collect-garbage)
                            (update-memory-text)]))]
                      [init-width "99.99 MB"])])
        (set! memory-canvases (cons ec memory-canvases))
        (update-memory-text)
        (set! memory-cleanup
              (λ ()
                (set! memory-canvases (remq ec memory-canvases))))
        (send panel stretchable-width #f)))
    
    (define gc-canvas (new bday-click-canvas% [parent (get-info-panel)] [style '(border no-focus)]))
    (define/private (register-gc-blit)
      (let ([onb (icon:get-gc-on-bitmap)]
            [offb (icon:get-gc-off-bitmap)])
        (when (and (send onb ok?)
                   (send offb ok?))
          (register-collecting-blit gc-canvas 
                                    0 0
                                    (send onb get-width)
                                    (send onb get-height)
                                    onb offb))))
    
    (define pref-save-callback-registration #f)
    (inherit get-eventspace)
    (define/private (register-pref-save-callback)
      (when pref-save-canvas
        (set! pref-save-callback-registration
              (preferences:register-save-callback
               (λ (start?)
                 (cond
                   [(eq? (current-thread) (eventspace-handler-thread (get-eventspace)))
                    (send pref-save-canvas set-on? start?)]
                   [else
                    (queue-callback
                     (λ ()
                       (send pref-save-canvas set-on? start?)))]))))))
    (define/private (unregister-pref-save-callback)
      (when pref-save-callback-registration
        (preferences:unregister-save-callback pref-save-callback-registration)))
    (register-pref-save-callback)
    
    (unless (preferences:get 'framework:show-status-line)
      (send super-root change-children
            (λ (l)
              (list rest-panel))))
    (register-gc-blit)
    
    (let* ([gcb (icon:get-gc-on-bitmap)]
           [gc-width (if (send gcb ok?)
                         (send gcb get-width)
                         10)]
           [gc-height (if (send gcb ok?)
                          (send gcb get-height)
                          10)])
      (send* gc-canvas
        (min-client-width (max (send gc-canvas min-width) gc-width))
        (min-client-height (max (send gc-canvas min-height) gc-height))
        (stretchable-width #f)
        (stretchable-height #f)))
    (send* (get-info-panel) 
      (set-alignment 'right 'center)
      (stretchable-height #f)
      (spacing 3)
      (border 3))))

(define (ensure-enough-width editor-canvas text)
  (send editor-canvas call-as-primary-owner
        (λ ()
          (let ([delta (get-client-width/view-delta text editor-canvas)]
                [lb (box 0)]
                [rb (box 0)])
            (send text position-location 
                  (send text last-position)
                  rb)
            (send text position-location 0 lb)
            (let ([nw
                   (+ delta (- (inexact->exact (floor (unbox rb)))
                               (inexact->exact (floor (unbox lb)))))])
              (when (< (send editor-canvas min-client-width) nw)
                (send editor-canvas min-client-width nw)))))))

(define (get-client-width/view-delta position-edit position-canvas)
  (let ([admin (send position-edit get-admin)]
        [wb (box 0)])
    (send admin get-view #f #f wb #f)
    (let-values ([(cw ch) (send position-canvas get-client-size)])
      (inexact->exact (floor (- cw (unbox wb)))))))

(define position-canvas%
  (class canvas%
    (inherit min-client-height min-client-width get-dc get-client-size refresh)
    (init init-width)
    (init-field [button-up #f])
    (init-field [char-typed void])
    (define str "")
    (define/public (set-str _str)
      (set! str _str)
      (update-client-width str)
      (refresh))
    (define/private (update-client-width str)
      (let ([dc (get-dc)])
        (let-values ([(cw _4) (get-client-size)]
                     [(tw _1 _2 _3) (send dc get-text-extent str normal-control-font)])
          (when (< cw tw)
            (min-client-width (inexact->exact (ceiling tw)))))))
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (send dc set-font normal-control-font)
        (let-values ([(cw ch) (get-client-size)]
                     [(tw th _1 _2) (send dc get-text-extent str)])
          (send dc draw-text str 0 (/ (- ch th) 2)))))
    (define/override (on-event evt)
      (when button-up
        (when (send evt button-up?)
          (let-values ([(cw ch) (get-client-size)])
            (when (and (<= (send evt get-x) cw)
                       (<= (send evt get-y) ch))
              (if (procedure-arity-includes? button-up 1)
                  (button-up evt)
                  (button-up)))))))
    (define/override (on-char evt)
      (char-typed evt))
    (super-new (style '(transparent no-focus)))
    (let ([dc (get-dc)])
      (let-values ([(_1 th _2 _3) (send dc get-text-extent str)])
        (min-client-height (inexact->exact (floor th)))))
    (update-client-width init-width)))

(define text-info<%> (interface (info<%>)
                       set-macro-recording
                       overwrite-status-changed
                       anchor-status-changed
                       editor-position-changed
                       use-file-text-mode-changed
                       add-line-number-menu-items))
(define text-info-mixin
  (mixin (info<%>) (text-info<%>)
    (inherit get-info-editor)
    (define remove-first
      (preferences:add-callback
       'framework:col-offsets
       (λ (p v)
         (editor-position-changed-offset/numbers
          v
          (preferences:get 'framework:display-line-numbers))
         #t)))
    (define remove-second
      (preferences:add-callback
       'framework:display-line-numbers
       (λ (p v)
         (editor-position-changed-offset/numbers
          (preferences:get 'framework:col-offsets)
          v)
         #t)))
    (define/augment (on-close)
      (remove-first)
      (remove-second)
      (inner (void) on-close))
    [define last-start #f]
    [define last-end #f]
    [define last-params #f]
    (define/private (editor-position-changed-offset/numbers offset? line-numbers?)
      (let* ([edit (get-info-editor)]
             [make-one
              (λ (pos)
                (if line-numbers?
                    (let* ([line (send edit position-paragraph pos)]
                           [col (find-col edit line pos)])
                      (format "~a:~a"
                              (add1 line)
                              (if offset?
                                  (add1 col)
                                  col)))
                    (format "~a" pos)))])
        (cond
          [(not (object? position-canvas))
           (void)]
          [edit
           (unless (send position-canvas is-shown?)
             (send position-canvas show #t))
           (let ([start (send edit get-start-position)]
                 [end (send edit get-end-position)])
             (unless (and last-start
                          (equal? last-params (list offset? line-numbers?))
                          (= last-start start)
                          (= last-end end))
               (set! last-params (list offset? line-numbers?))
               (set! last-start start)
               (set! last-end end)
               (when (object? position-canvas)
                 (change-position-edit-contents
                  (if (= start end)
                      (make-one start)
                      (string-append (make-one start)
                                     "-"
                                     (make-one end)))))))]
          [else
           (when (send position-canvas is-shown?)
             (send position-canvas show #f))])))
    
    ;; find-col : text number number -> number
    ;; given a line number and a position, finds the
    ;; column number for that position
    (define/private (find-col text line pos)
      (let ([line-start (send text paragraph-start-position line)])
        (if (= line-start pos)
            0
            (let loop ([col 0]
                       [snip (send text find-snip line-start 'after-or-none)])
              (cond
                [(and snip (is-a? snip tab-snip%))
                 ;; assume cursor isn't in the middle of the tab snip
                 ;; and that there is no tab array
                 (let ([twb (box 0)])
                   (send text get-tabs #f twb #f)
                   (let ([tw (floor (inexact->exact (unbox twb)))])
                     (loop (+ col (- tw (modulo col tw)))
                           (send snip next))))]
                [snip 
                 (let ([snip-position (send text get-snip-position snip)]
                       [snip-length (send snip get-count)])
                   (if (<= snip-position pos (+ snip-position snip-length))
                       (+ col (- pos snip-position))
                       (loop (+ col snip-length)
                             (send snip next))))]
                [else
                 col])))))
    
    
    [define anchor-last-state? #f]
    [define overwrite-last-state? #f]
    
    (field (macro-recording? #f))
    (define/private (update-macro-recording-icon)
      (unless (eq? (send macro-recording-message is-shown?)
                   macro-recording?)
        (if macro-recording?
            (add-uncommon-child macro-recording-message)
            (remove-uncommon-child macro-recording-message))))
    (define/public (set-macro-recording on?)
      (set! macro-recording? on?)
      (update-macro-recording-icon))
    
    (define/public (anchor-status-changed)
      (let ([info-edit (get-info-editor)]
            [failed
             (λ ()
               (unless (eq? anchor-last-state? #f)
                 (set! anchor-last-state? #f)
                 (remove-uncommon-child anchor-message)))])
        (cond
          [info-edit
           (let ([anchor-now? (send info-edit get-anchor)])
             (unless (eq? anchor-now? anchor-last-state?)
               (cond
                 [(object? anchor-message)
                  (if anchor-now?
                      (add-uncommon-child anchor-message)
                      (remove-uncommon-child anchor-message))
                  (set! anchor-last-state? anchor-now?)]
                 [else (failed)])))]
          [else
           (failed)])))
    (define/public (editor-position-changed)
      (editor-position-changed-offset/numbers
       (preferences:get 'framework:col-offsets)
       (preferences:get 'framework:display-line-numbers)))
    (define/public (overwrite-status-changed)
      (let ([info-edit (get-info-editor)]
            [failed
             (λ ()
               (set! overwrite-last-state? #f)
               (remove-uncommon-child overwrite-message))])
        (cond
          [info-edit
           (let ([overwrite-now? (send info-edit get-overwrite-mode)])
             (unless (eq? overwrite-now? overwrite-last-state?)
               (cond
                 [(object? overwrite-message)
                  (if overwrite-now?
                      (add-uncommon-child overwrite-message)
                      (remove-uncommon-child overwrite-message))
                  (set! overwrite-last-state? overwrite-now?)]
                 [else
                  (failed)])))]
          [else
           (failed)])))
    

    (define/public (add-line-number-menu-items menu)
      (void))
    
    (define/public (use-file-text-mode-changed)
      (when (object? file-text-mode-msg)
        (define ed (get-info-editor))
        (send file-text-mode-msg-parent change-children
              (λ (l)
                (if (and (is-a? ed text:info<%>)
                         (eq? (system-type) 'windows)
                         (send ed use-file-text-mode))
                  (list file-text-mode-msg)
                  '())))))
    
    (define/override (update-info)
      (super update-info)
      (update-macro-recording-icon)
      (overwrite-status-changed)
      (anchor-status-changed)
      (editor-position-changed)
      (use-file-text-mode-changed))
    (super-new)
    
    (inherit get-info-panel)
    
    (define position-parent (new click-pref-panel% 
                                 [border 2]
                                 [parent (get-info-panel)]
                                 [stretchable-width #f]
                                 [stretchable-height #f]
                                 [extra-menu-items (λ (menu) (add-line-number-menu-items menu))]))
    (define position-canvas (new position-canvas% 
                                 [parent position-parent] 
                                 [init-width "1:1"]))
    (define/private (change-position-edit-contents str)
      (send position-canvas set-str str))
    
    (send (get-info-panel) change-children
          (λ (l)
            (cons position-parent (remq position-parent l))))

    (define file-text-mode-msg-parent (new horizontal-panel%
                                           [stretchable-width #f]
                                           [stretchable-height #f]
                                           [parent (get-info-panel)]))
    (define file-text-mode-msg (new file-text-mode-msg% [parent file-text-mode-msg-parent]))
    (send file-text-mode-msg-parent change-children (λ (l) '()))
    (send (get-info-panel) change-children
          (λ (l)
            (cons file-text-mode-msg-parent (remq file-text-mode-msg-parent l))))
    
    (define uncommon-parent (new horizontal-panel%
                                 [parent (get-info-panel)]
                                 [stretchable-width #f]))
    
    (send (get-info-panel) change-children
          (λ (l) (cons uncommon-parent (remq uncommon-parent l))))
    (define anchor-message
      (new message%
           [font small-control-font]
           [label (string-constant auto-extend-selection)]
           [parent uncommon-parent]))
    (define overwrite-message 
      (new message%
           [font small-control-font]
           [label (string-constant overwrite)]
           [parent uncommon-parent]))
    (define macro-recording-message
      (new message%
           [label "c-x;("]
           [font small-control-font]
           [parent uncommon-parent]))
    (define/private (remove-uncommon-child c)
      (send uncommon-parent change-children
            (λ (l) (remq c l))))
    (define/private (add-uncommon-child c)
      (define (child->num c)
        (cond
          [(eq? c anchor-message) 0]
          [(eq? c overwrite-message) 1]
          [(eq? c macro-recording-message) 2]))
      (send uncommon-parent change-children
            (λ (l) (sort (cons c (remq c l))
                         <
                         #:key child->num))))

    (inherit determine-width)
    (send uncommon-parent change-children (λ (l) '()))
    (editor-position-changed)
    (use-file-text-mode-changed)))

(define crlf-string "CRLF")
(define file-text-mode-msg%
  (class canvas%
    (inherit min-width min-height get-dc refresh)
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush "orange" 'solid)
      (define-values (w h d a) (send dc get-text-extent crlf-string))
      (send dc draw-rectangle 0 0 (+ w 4) h)
      (send dc draw-text crlf-string 2 0))
    (super-new)
    (inherit stretchable-width)
    (stretchable-width #f)
    (send (get-dc) set-font small-control-font)
    (let ()
      (define-values (w h d a) (send (get-dc) get-text-extent crlf-string))
      (min-width (inexact->exact (ceiling (+ w 4))))
      (min-height (inexact->exact (ceiling h))))))         

(define click-pref-panel%
  (class horizontal-panel%
    (init-field extra-menu-items)
    (inherit popup-menu)
    (define/override (on-subwindow-event receiver evt)
      (cond
        [(send evt button-down?)
         (let ([menu (new popup-menu%)]
               [line-numbers? (preferences:get 'framework:display-line-numbers)])
           (new checkable-menu-item%
                [parent menu]
                [label (string-constant show-line-and-column-numbers)]
                [callback (λ (x y) (preferences:set 'framework:display-line-numbers #t))]
                [checked line-numbers?])
           (new checkable-menu-item%
                [parent menu]
                [label (string-constant show-character-offsets)]
                [callback (λ (x y) (preferences:set 'framework:display-line-numbers #f))]
                [checked (not line-numbers?)])
           (extra-menu-items menu)
           (popup-menu menu 
                       (+ 1 (send evt get-x))
                       (+ 1 (send evt get-y))))
         #t]
        [else
         (super on-subwindow-event receiver evt)]))
    (super-new)))

(define pasteboard-info<%> (interface (info<%>)))
(define pasteboard-info-mixin
  (mixin (basic<%>) (pasteboard-info<%>)
    (super-new)))

(generate-standard-menus-code)

(define -editor<%> (interface (standard-menus<%>)
                     get-entire-label
                     get-label-prefix
                     set-label-prefix

                     get-canvas%
                     get-canvas<%>
                     get-editor%
                     get-editor<%>
                     
                     make-editor
                     revert
                     save
                     save-as
                     get-canvas
                     get-editor))

(define editor-mixin
  (mixin (standard-menus<%>) (-editor<%>)
    (init (filename #f))
    (init-field (editor% #f))
    
    (inherit get-area-container get-client-size 
             show get-edit-target-window get-edit-target-object)
    
    (define/override get-filename
      (case-lambda
        [() (get-filename #f)]
        [(b)
         (let ([e (get-editor)])
           (and e (send e get-filename b)))]))
    
    (define/override (editing-this-file? filename)
      (let ([path-equal?
             (λ (x y)
               (with-handlers ((exn:fail? (λ (x) #f)))
                 (equal? (normal-case-path (normalize-path x))
                         (normal-case-path (normalize-path y)))))])
        (let ([this-fn (get-filename)])
          (and this-fn
               (path-equal? filename (get-filename))))))
    
    (define/augment (on-close)
      (send (get-editor) on-close)
      (inner (void) on-close))
    
    (define/augment (can-close?)
      (and (send (get-editor) can-close?)
           (inner #t can-close?)))
    
    [define label ""]
    [define label-prefix (application:current-app-name)]
    (define/private (do-label)
      (super set-label (gui-utils:trim-string (get-entire-label) 200))
      (send (group:get-the-frame-group) frame-label-changed this))
    
    (define/public (get-entire-label)
      (cond
        [(string=? "" label)
         label-prefix]
        [(string=? "" label-prefix)
         label]
        [else 
         (string-append label " - " label-prefix)]))
    (define/public (get-label-prefix) label-prefix)
    (define/public (set-label-prefix s)
      (when (and (string? s)
                 (not (string=? s label-prefix)))
        (set! label-prefix s)
        (do-label)))
    [define/override get-label (λ () label)]
    [define/override set-label
      (λ (t)
        (when (and (string? t)
                   (not (string=? t label)))
          (set! label t)
          (do-label)))]
    
    (define/public (get-canvas%) editor-canvas%)
    (define/public (get-canvas<%>) (class->interface editor-canvas%))
    (define/public (make-canvas)
      (let ([% (get-canvas%)]
            [<%> (get-canvas<%>)])
        (unless (implementation? % <%>)
          (error 'frame:editor%
                 "result of get-canvas% method must match ~e interface; got: ~e"
                 <%> %))
        (instantiate % () (parent (get-area-container)))))
    (define/public (get-editor%)
      (or editor%
          (error 'editor-frame% "abstract method: no editor% class specified")))
    (define/public (get-editor<%>)
      editor:basic<%>)
    (define/public (make-editor)
      (let ([% (get-editor%)]
            [<%> (get-editor<%>)])
        (unless (implementation? % <%>)
          (error 'frame:editor%
                 "result of get-editor% method must match ~e interface; got: ~e"
                 <%> %))
        (make-object %)))
    
    (define/public save
      (lambda ([format 'same])
        (let* ([ed (get-editor)]
               [filename (send ed get-filename)])
          (if filename
              (send ed save-file/gui-error filename format)
              (save-as format)))))
    
    (define/public save-as
      (lambda ([format 'same])
        (let* ([editor (get-editor)]
               [name (send editor get-filename)])
          (let-values ([(base name)
                        (if name 
                            (let-values ([(base name dir?) (split-path name)])
                              (values base name))
                            (values #f #f))])
            (let ([file (send editor put-file base name)])
              (if file
                  (send editor save-file/gui-error file format)
                  #f))))))
    
    (define/private (basename str)
      (let-values ([(base name dir?) (split-path str)])
        base))
    
    (inherit get-checkable-menu-item% get-menu-item%)
    
    (define/override (file-menu:open-callback item evt)
      (let* ([e (get-editor)]
             [fn (and e (send e get-filename))]
             [dir (and fn
                       (let-values ([(base name dir) (split-path fn)])
                         base))])
        (handler:open-file dir)))
    
    (define/override (file-menu:revert-on-demand item)
      (send item enable (not (send (get-editor) is-locked?))))
    
    (define/override file-menu:revert-callback 
      (λ (item control)
        (let* ([edit (get-editor)]
               [b (box #f)]
               [filename (send edit get-filename b)])
          (if (or (not filename)
                  (unbox b))
              (bell)
              (when (or (not (send (get-editor) is-modified?))
                        (gui-utils:get-choice
                         (string-constant are-you-sure-revert)
                         (string-constant yes)
                         (string-constant no)
                         (string-constant are-you-sure-revert-title)
                         #f
                         this))
                (revert))))
        #t))
    
    (define/public (revert)
      (let* ([edit (get-editor)]
             [b (box #f)]
             [filename (send edit get-filename b)])
        (when (and filename
                   (not (unbox b)))
          (let ([start
                 (if (is-a? edit text%)
                     (send edit get-start-position)
                     #f)])
            (send edit begin-edit-sequence)
            (let ([status (send edit load-file/gui-error
                                filename
                                'guess
                                #f)])
              (if status
                  (begin
                    (when (is-a? edit text%)
                      (send edit set-position start start))
                    (send edit end-edit-sequence))
                  (send edit end-edit-sequence)))))))
    
    (define/override file-menu:create-revert? (λ () #t))
    (define/override file-menu:save-callback
      (λ (item control)
        (save)
        #t))
    
    (define/override file-menu:create-save? (λ () #t))
    (define/override file-menu:save-as-callback (λ (item control) (save-as) #t))
    (define/override file-menu:create-save-as? (λ () #t))
    (define/override file-menu:print-callback (λ (item control)
                                                (send (get-editor) print
                                                      #t
                                                      #t
                                                      (preferences:get 'framework:print-output-mode))
                                                #t))
    (define/override file-menu:create-print? (λ () #t))
    
    (inherit get-top-level-window)
    (define/override (file-menu:between-save-as-and-print file-menu)
      (when (and (can-get-page-setup-from-user?) (file-menu:create-print?))
        (new menu-item% 
             [parent file-menu]
             [label (string-constant page-setup-menu-item)]
             [help-string (string-constant page-setup-info)]
             [callback
              (lambda (item event)
                (let ([s (get-page-setup-from-user #f (get-top-level-window))])
                  (when s
                    (send (current-ps-setup) copy-from s))))])))
    
    (define/override (edit-menu:between-select-all-and-find edit-menu)
      (define c% (get-checkable-menu-item%))
      (define (on-demand menu-item)
        (define edit (get-edit-target-object))
        (cond
          [(and edit (is-a? edit editor<%>))
           (send menu-item enable #t)
           (send menu-item check (send edit auto-wrap))]
          [else 
           (send menu-item check #f)
           (send menu-item enable #f)]))
      (define (callback item event)
        (define edit (get-edit-target-object))
        (when (and edit
                   (is-a? edit editor<%>))
          (define new-pref (not (send edit auto-wrap)))
          (preferences:set 'framework:auto-set-wrap? new-pref)
          (send edit auto-wrap new-pref)))
      (new c%
           [label (string-constant wrap-text-item)]
           [parent edit-menu]
           [callback callback]
           [demand-callback on-demand])
      (make-object separator-menu-item% edit-menu))
    
    (define/override help-menu:about-callback 
      (λ (menu evt) 
        (message-box (application:current-app-name)
                     (format (string-constant welcome-to-something)
                             (application:current-app-name))
                     #f
                     '(ok))))
    (define/override help-menu:about-string (λ () (application:current-app-name)))
    (define/override help-menu:create-about? (λ () #t))
    
    (super-new (label (get-entire-label)))
    
    (define canvas #f)
    (define editor #f)
    (public get-canvas get-editor)
    (define get-canvas 
      (λ () 
        (unless canvas
          (set! canvas (make-canvas))
          (send canvas set-editor (get-editor)))
        canvas))
    (define get-editor 
      (λ () 
        (unless editor
          (set! editor (make-editor))
          (send (get-canvas) set-editor editor))
        editor))
    
    (cond
      [(and filename (file-exists? filename))
       (let ([ed (get-editor)])
         (send ed begin-edit-sequence)
         (send ed load-file/gui-error filename 'guess)
         (send ed end-edit-sequence))]
      [filename
       (send (get-editor) set-filename filename)]
      [else (void)])
    
    (let ([ed-fn (send (get-editor) get-filename)])
      (set! label (or (and ed-fn 
                           (path->string (file-name-from-path ed-fn)))
                      (send (get-editor) get-filename/untitled-name))))
    (do-label)
    (let ([canvas (get-canvas)])
      (when (is-a? canvas editor-canvas%)
        ;; when get-canvas is overridden,
        ;; it might not yet be implemented
        (send canvas focus)))))

(define text<%> (interface (-editor<%>)))
(define text-mixin
  (mixin (-editor<%>) (text<%>)
    (define/override (get-editor<%>) (class->interface text%))
    (init (filename #f) (editor% text:keymap%))
    
    (inherit get-editor)
    (define/override (edit-menu:between-find-and-preferences edit-menu)
      (when (add-find-longest-line-menu-item?)
        (new menu-item%
             [parent edit-menu] 
             [label (string-constant find-longest-line)]
             [callback 
              (λ (x y)
                (define ed (get-editor))
                (define (line-len p)
                  (define ans 
                    (- (send ed paragraph-end-position p #f)
                       (send ed paragraph-start-position p #f)))
                  ans)
                (when ed
                  (let loop ([p (- (send ed last-paragraph) 1)]
                             [longest-size (line-len (send ed last-paragraph))]
                             [longest (send ed last-paragraph)])
                    (cond
                      [(= p -1)
                       (send ed set-position 
                             (send ed paragraph-start-position longest #f)
                             (send ed paragraph-end-position longest #f))]
                      [else
                       (define this-size (line-len p))
                       (cond
                         [(<= longest-size this-size)
                          (loop (- p 1) this-size p)]
                         [else
                          (loop (- p 1) longest-size longest)])]))))]))
      (super edit-menu:between-find-and-preferences edit-menu))
      
    (define/public (add-find-longest-line-menu-item?) #t)
    
    (super-new (filename filename) (editor% editor%))))

(define pasteboard<%> (interface (-editor<%>)))
(define pasteboard-mixin
  (mixin (-editor<%>) (pasteboard<%>)
    (define/override get-editor<%> (λ () (class->interface pasteboard%)))
    (init (filename #f) (editor% pasteboard:keymap%))
    (super-new (filename filename) (editor% editor%))))

(define delegate<%>
  (interface (status-line<%> text<%>)
    delegated-text-shown?
    hide-delegated-text
    show-delegated-text
    delegate-moved
    set-delegated-text
    get-delegated-text))

(define delegatee-editor-canvas%
  (class (canvas:color-mixin canvas:basic%)
    (init-field delegate-frame)
    (inherit get-editor get-dc)
    
    (define/override (on-event evt)
      (super on-event evt)
      (when delegate-frame
        (let ([text (get-editor)])
          (when (and (is-a? text text%)
                     (send delegate-frame delegated-text-shown?))
            (cond
              [(send evt button-down?)
               (let-values ([(editor-x editor-y)
                             (send text dc-location-to-editor-location 
                                   (send evt get-x)
                                   (send evt get-y))])
                 (send delegate-frame click-in-overview 
                       (send text find-position editor-x editor-y)))])))))
    (super-new)))

(define (at-most-200 s)
  (cond
    [(<= (string-length s) 200)
     s]
    [else (substring s 0 200)]))

(define delegatee-text<%>
  (interface ()
    set-start/end-para))

(define delegatee-text%
  (class* text:basic% (delegatee-text<%>)
    (define start-para #f)
    (define end-para #f)
    (define view-x-b (box 0))
    (define view-width-b (box 0))
    (inherit get-admin
             paragraph-start-position paragraph-end-position 
             position-location invalidate-bitmap-cache scroll-to-position
             get-visible-position-range position-paragraph
             last-position)
    
    (define/override (on-new-string-snip)
      (instantiate text:1-pixel-string-snip% ()))
    
    (define/override (on-new-tab-snip)
      (instantiate text:1-pixel-tab-snip% ()))
    
    ;; set-start/end-para : (union (#f #f -> void) (number number -> void))
    (define/public (set-start/end-para _start-para _end-para)
      (unless (and (equal? _start-para start-para)
                   (equal? _end-para end-para))
        (let ([old-start-para start-para]
              [old-end-para end-para])
          (cond
            [else
             (set! start-para _start-para)
             (set! end-para _end-para)])
          
          (when (and start-para end-para)
            
            (let-values ([(v-start v-end) (let ([bs (box 0)]
                                                [bf (box 0)])
                                            (get-visible-position-range bs bf)
                                            (values (unbox bs)
                                                    (unbox bf)))])
              (let ([v-start-para (position-paragraph v-start)]
                    [v-end-para (position-paragraph v-end)])
                (cond
                  [(v-start-para . >= . start-para)
                   (scroll-to-position (paragraph-start-position start-para))]
                  [(v-end-para . <= . end-para)
                   (scroll-to-position (paragraph-end-position end-para))]
                  [else (void)]))))
          (define the-l #f)
          (define the-t #f)
          (define the-r #f)
          (define the-b #f)
          (when (and old-start-para old-end-para)
            (let-values ([(x y w h) (get-rectangle old-start-para old-end-para)])
              (when x
                (set! the-l x)
                (set! the-t y)
                (set! the-r (+ x w))
                (set! the-b (+ y h)))))
          (when (and start-para end-para)
            (let-values ([(x y w h) (get-rectangle start-para end-para)])
              (cond
                [(and x the-l)
                 (set! the-l (min x the-l))
                 (set! the-t (min y the-t))
                 (set! the-r (max the-r (+ x w)))
                 (set! the-b (max the-b (+ y h)))]
                [x
                 (set! the-l x)
                 (set! the-t y)
                 (set! the-r (+ x w))
                 (set! the-b (+ y h))])))
          (when the-l
            (invalidate-bitmap-cache the-l the-t (- the-r the-l) (- the-b the-t))))))
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when (and before?
                 start-para
                 end-para)
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)])
          (send dc set-pen
                (send the-pen-list find-or-create-pen 
                      (preferences:get 'framework:delegatee-overview-color)
                      1
                      'solid))
          (send dc set-brush
                (send the-brush-list find-or-create-brush 
                      (preferences:get 'framework:delegatee-overview-color)
                      'solid))
          (let-values ([(x y w h) (get-rectangle start-para end-para)])
            (when x
              (send dc draw-rectangle 
                    (+ dx x)
                    (+ dy y)
                    w
                    h)))
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)))
      (super on-paint before? dc left top right bottom dx dy draw-caret))
    
    
    ;; get-rectangle : number number -> 
    ;;                (values (union #f number) (union #f number) (union #f number) (union #f number))
    ;; computes the rectangle corresponding the input paragraphs
    (define/private (get-rectangle start-para end-para)
      (let ([start (get-line-y start-para #t)]
            [end (get-line-y end-para #f)]
            [admin (get-admin)])
        (cond
          [(not admin)
           (values #f #f #f #f)]
          [(= 0 (last-position))
           (values #f #f #f #f)]
          [else
           (send admin get-view view-x-b #f view-width-b #f)
           (send admin get-view view-x-b #f view-width-b #f)
           (values (unbox view-x-b)
                   start
                   (unbox view-width-b)
                   (- end start))])))
    
    (define/private (get-line-y para top?)
      (let ([pos (paragraph-start-position para)]
            [b (box 0)])
        (position-location pos #f b top? #f #t)
        (unbox b)))
    (super-new)
    
    (inherit set-cursor)
    (set-cursor (make-object cursor% 'arrow))
    
    (inherit set-line-spacing)
    (set-line-spacing 0)))

(define delegate-mixin
  (mixin (status-line<%> text<%>) (delegate<%>)
    
    (define delegated-text #f)
    (define/public-final (get-delegated-text) delegated-text)
    (define/public-final (set-delegated-text t) 
      (unless (or (not t) (is-a? t text:delegate<%>))
        (error 'set-delegated-text 
               "expected either #f or a text:delegate<%> object, got ~e" 
               t))
      (unless (eq? delegated-text t)
        (set! delegated-text t)
        (when shown?
          (unless (send (get-delegated-text) get-delegate)
            (send (get-delegated-text) set-delegate 
                  (new delegatee-text%)))
          (send delegate-ec set-editor (send (get-delegated-text) get-delegate)))))
    
    [define rest-panel 'uninitialized-root]
    [define super-root 'uninitialized-super-root]
    [define/override make-root-area-container
      (λ (% parent)
        (let* ([s-root (super make-root-area-container
                              horizontal-panel%
                              parent)]
               [r-root (make-object % s-root)])
          (set! super-root s-root)
          (set! rest-panel r-root)
          r-root))]
    
    (define/override (get-editor<%>)
      text:delegate<%>)
    
    (define/override (get-editor%)
      (text:delegate-mixin (super get-editor%)))
    
    (field (shown? (preferences:get 'framework:show-delegate?)))
    (define/public (delegated-text-shown?) 
      shown?)
    
    (inherit close-status-line open-status-line)
    (define/public (hide-delegated-text)
      (set! shown? #f)
      (when delegated-text 
        (send delegated-text set-delegate #f))
      (send delegate-ec set-editor #f)
      (send super-root change-children
            (λ (l) (list rest-panel))))
    (define/public (show-delegated-text)
      (set! shown? #t)
      (when delegated-text
        (unless (send delegated-text get-delegate)
          (send delegated-text set-delegate 
                (new delegatee-text%)))
        (send delegate-ec set-editor (send (get-delegated-text) get-delegate)))
      (send super-root change-children
            (λ (l) (list rest-panel delegate-ec))))
    
    (define/public (click-in-overview pos)
      (when shown?
        (let* ([d-canvas (send delegated-text get-canvas)]
               [bx (box 0)]
               [by (box 0)])
          (let-values ([(cw ch) (send d-canvas get-client-size)])
            (send delegated-text position-location pos bx by)
            (send d-canvas focus)
            (send d-canvas scroll-to 
                  (- (unbox bx) (/ cw 2))
                  (- (unbox by) (/ ch 2))
                  cw
                  ch
                  #t)
            (send delegated-text set-position pos)))))
    
    (define/public (delegate-moved)
      (define delegatee (send delegate-ec get-editor))
      (when delegatee
        (let ([startb (box 0)]
              [endb (box 0)])
          (send delegated-text get-visible-position-range startb endb #f)
          (send delegatee set-start/end-para
                (send delegated-text position-paragraph (unbox startb)) 
                (send delegated-text position-paragraph (unbox endb))))))
    
    (define/public (get-delegatee) (send delegate-ec get-editor))
        
    (super-new)
    
    (define delegate-ec (new delegatee-editor-canvas%
                             [parent super-root]
                             [delegate-frame this]
                             [min-width 150]
                             [stretchable-width #f]))
    (inherit get-editor)
    (if (preferences:get 'framework:show-delegate?)
        (show-delegated-text)
        (hide-delegated-text))))

(define searchable<%> (interface (basic<%>)
                        search
                        search-replace
                        replace-all
                        
                        get-text-to-search
                        set-text-to-search
                        
                        search-hidden?
                        hide-search
                        unhide-search

                        get-case-sensitive-search?
                        search-hits-changed
                        ))

(define old-search-highlight void)
(define (clear-search-highlight)
  (old-search-highlight)
  (set! old-search-highlight void))

(define find/replace-text%
  (class text:keymap%
    (init-field pref-sym)
    (inherit get-canvas get-text last-position insert find-first-snip
             get-admin invalidate-bitmap-cache
             begin-edit-sequence end-edit-sequence get-top-level-window)
    (define/augment (after-insert x y)
      (update-prefs)
      (inner (void) after-insert x y))
    (define/augment (after-delete x y)
      (update-prefs)
      (inner (void) after-delete x y))
    (define timer #f)
    (define/private (update-prefs)
      (unless timer
        (set! timer (new timer%
                         [notify-callback
                          (λ ()
                            (preferences:set pref-sym
                                             (let loop ([snip (find-first-snip)])
                                               (cond
                                                 [(not snip) '()]
                                                 [(is-a? snip string-snip%)
                                                  (cons (send snip get-text 0 (send snip get-count))
                                                        (loop (send snip next)))]
                                                 [else (cons snip (loop (send snip next)))]))))])))
      (send timer stop)
      (send timer start 150 #t))
    (define/override (get-keymaps)
      (editor:add-after-user-keymap search/replace-keymap (super get-keymaps)))
    (super-new)
    (inherit set-styles-fixed)
    (set-styles-fixed #t)
    (begin-edit-sequence)
    (for-each
     (λ (x) (insert x (last-position) (last-position)))
     (preferences:get pref-sym))
    (end-edit-sequence)
    
    (define pref-callback 
      (λ (p v)
        (let ([c (get-canvas)])
          (when (and c (send c get-line-count))
            (send c set-editor (send c get-editor))))))
    
    (preferences:add-callback 'framework:standard-style-list:font-size pref-callback #t)))

(define find-text%
  (class find/replace-text%
    (inherit get-canvas get-text last-position insert find-first-snip
             get-admin invalidate-bitmap-cache run-after-edit-sequence
             begin-edit-sequence end-edit-sequence get-top-level-window)
    
    (define/private (get-case-sensitive-search?)
      (let ([frame (get-top-level-window)])
        (and frame
             (send frame get-case-sensitive-search?))))

    (define/override (on-focus on?)  
      (let ([frame (get-top-level-window)])
        (when frame
          (let ([text-to-search (send frame get-text-to-search)])
            (when text-to-search
              (when on?
                (send text-to-search set-search-anchor (send text-to-search get-start-position)))))))
      (super on-focus on?))
    
    (define/augment (after-insert x y)
      (update-searching-str/trigger-jump)
      (inner (void) after-insert x y))
    (define/augment (after-delete x y)
      (update-searching-str/trigger-jump)
      (inner (void) after-delete x y))
    (define/private (update-searching-str/trigger-jump)
      (let ([tlw (get-top-level-window)])
        (when tlw
          (send tlw search-string-changed)))
      
      ;; trigger-jump
      (when (preferences:get 'framework:anchored-search)
        (let ([frame (get-top-level-window)])
          (when frame
            (let ([text-to-search (send frame get-text-to-search)])
              (when text-to-search
                (let ([anchor-pos (send text-to-search get-anchor-pos)])
                  (when anchor-pos
                    (cond
                      [(equal? "" (get-text))
                       (send text-to-search set-position anchor-pos anchor-pos)]
                      [else
                       (search 'forward #t #t #f anchor-pos)])))))))))

    
    (define/private (get-searching-text)
      (let ([frame (get-top-level-window)])
        (and frame
             (send frame get-text-to-search))))
    (define/public (search [searching-direction 'forward] 
                           [beep? #t]
                           [wrap? #t]
                           [move-anchor? #t]
                           [search-start-position #f])
      (let* ([string (get-text)]
             [top-searching-edit (get-searching-text)])
        (when top-searching-edit
          (let ([searching-edit (let ([focus-snip (send top-searching-edit get-focus-snip)])
                                  (if (and focus-snip (is-a? focus-snip editor-snip%))
                                      (send focus-snip get-editor)
                                      top-searching-edit))]
                
                [not-found
                 (λ (found-edit skip-beep?)
                   (when (and beep?
                              (not skip-beep?))
                     (bell))
                   #f)]
                [found
                 (λ (text first-pos)
                   (define (thunk)
                     (define last-pos ((if (eq? searching-direction 'forward) + -)
                                       first-pos (string-length string)))
                     (define start-pos (min first-pos last-pos))
                     (define end-pos (max first-pos last-pos))
                     
                     (send text begin-edit-sequence #t #f)
                     (send text set-caret-owner #f 'display)
                     (send text set-position start-pos end-pos #f #f 'local)
                     
                     
                     ;; scroll to the middle if the search result isn't already visible
                     (let ([search-result-line (send text position-line (send text get-start-position))]
                           [bt (box 0)]
                           [bb (box 0)])
                       (send text get-visible-line-range bt bb #f)
                       (unless (< (unbox bt) search-result-line (unbox bb))
                         (let* ([half (sub1 (quotient (- (unbox bb) (unbox bt)) 2))]
                                [last-pos (send text position-line (send text last-position))]
                                [top-pos (send text line-start-position 
                                               (max (min (- search-result-line half) last-pos) 0))]
                                [bottom-pos (send text line-start-position 
                                                  (max 0
                                                       (min (+ search-result-line half)
                                                            last-pos)))])
                           (send text scroll-to-position 
                                 top-pos
                                 #f
                                 bottom-pos))))
                     
                     (when move-anchor?
                       (when (is-a? text text:searching<%>)
                         (send text set-search-anchor
                               (if (eq? searching-direction 'forward)
                                   end-pos
                                   start-pos))))
                     
                     (send text end-edit-sequence))
                   
                   (define owner (or (send text get-active-canvas)
                                     (send text get-canvas)))
                   (if owner
                       (send owner call-as-primary-owner thunk)
                       (thunk))
                   #t)])
            
            (if (string=? string "")
                (not-found top-searching-edit #t)
                (let-values ([(found-edit first-pos)
                              (find-string-embedded
                               (if search-start-position 
                                   top-searching-edit
                                   searching-edit)
                               string
                               searching-direction
                               (or search-start-position
                                   (if (eq? 'forward searching-direction)
                                       (send searching-edit get-end-position)
                                       (send searching-edit get-start-position)))
                               'eof #t
                               (get-case-sensitive-search?)
                               #t)])
                  (cond
                    [(not first-pos)
                     (if wrap?
                         (begin
                           (let-values ([(found-edit pos)
                                         (find-string-embedded
                                          top-searching-edit
                                          string 
                                          searching-direction
                                          (if (eq? 'forward searching-direction)
                                              0
                                              (send searching-edit last-position))
                                          'eof #t
                                          (get-case-sensitive-search?)
                                          #f)])
                             (if (not pos)
                                 (not-found found-edit #f)
                                 (found found-edit pos))))
                         (not-found found-edit #f))]
                    [else
                     (found found-edit first-pos)])))))))

    (define/override (on-paint before dc left top right bottom dx dy draw-caret?)
      (super on-paint before dc left top right bottom dx dy draw-caret?)
      (when before
        (let ([canvas (get-canvas)])
          (when (and canvas
                     (send canvas is-red?))
            (let-values ([(view-x view-y view-width view-height)
                          (let ([b1 (box 0)]
                                [b2 (box 0)]
                                [b3 (box 0)]
                                [b4 (box 0)])
                            (send (get-admin) get-view b1 b2 b3 b4)
                            (values (unbox b1)
                                    (unbox b2)
                                    (unbox b3)
                                    (unbox b4)))])
              (let ([pen (send dc get-pen)]
                    [brush (send dc get-brush)])
                (send dc set-pen "black" 1 'transparent)
                (send dc set-brush "pink" 'solid)
                (send dc draw-rectangle (+ dx view-x) (+ view-y dy) view-width view-height)
                (send dc set-pen pen)
                (send dc set-brush brush)))))))
    
    (super-new [pref-sym 'framework:search-string])))

(define replace-text%
  (class find/replace-text%
    (inherit set-styles-fixed)
    (super-new [pref-sym 'framework:replace-string])
    (define/override (get-keymaps)
      (editor:add-after-user-keymap search/replace-keymap (super get-keymaps)))
    (set-styles-fixed #t)))

(define search/replace-keymap (new keymap%))
(send search/replace-keymap map-function "tab" "switch-between-search-and-replace")
(send search/replace-keymap add-function "switch-between-search-and-replace"
      (λ (text evt)
        (let ([find-txt (send (send text get-top-level-window) get-find-edit)]
              [replace-txt (send (send text get-top-level-window) get-replace-edit)])
          (cond
            [(eq? find-txt text)
             (send replace-txt set-position 0 (send replace-txt last-position))
             (send (send replace-txt get-canvas) focus)]
            [(eq? replace-txt text)
             (send find-txt set-position 0 (send find-txt last-position))
             (send (send find-txt get-canvas) focus)]))))

(send search/replace-keymap map-function "return" "next")
(send search/replace-keymap add-function "next"
      (λ (text evt)
        (send (send text get-top-level-window) search 'forward)))

(send search/replace-keymap map-function "s:return" "prev")
(send search/replace-keymap add-function "prev"
      (λ (text evt)
        (send (send text get-top-level-window) search 'backward)))

(send search/replace-keymap map-function "c:return" "insert-return")
(send search/replace-keymap map-function "a:return" "insert-return")
(send search/replace-keymap add-function "insert-return"
      (λ (text evt)
        (send text insert "\n")))

(send search/replace-keymap map-function "esc" "hide-search")
(send search/replace-keymap add-function "hide-search"
      (λ (text evt)
        (let ([tlw (send text get-top-level-window)])
          (when tlw
            (send tlw hide-search)))))

(send search/replace-keymap map-function "c:g" "hide-search-and-snap-back")
(send search/replace-keymap add-function "hide-search-and-snap-back"
      (λ (text evt)
        (let ([tlw (send text get-top-level-window)])
          (when tlw
            (when (preferences:get 'framework:anchored-search)
              (let ([text-to-search (send tlw get-text-to-search)])
                (when text-to-search
                  (let ([anchor-pos (send text-to-search get-anchor-pos)])
                    (when anchor-pos 
                      (send text-to-search set-position anchor-pos))))))
            (send tlw hide-search)))))

(send search/replace-keymap map-function "f3" "unhide-search-and-toggle-focus")
(send search/replace-keymap add-function "unhide-search-and-toggle-focus"
      (λ (text evt)
        (let ([tlw (send text get-top-level-window)])
          (when tlw
            (send tlw unhide-search-and-toggle-focus)))))

(define searchable-canvas% 
  (class editor-canvas% 
    (inherit refresh get-dc get-client-size)
    (define red? #f)
    (define/public (is-red?) red?)
    (define/public (set-red r?)
      (unless (equal? red? r?)
        (set! red? r?)
        (refresh)))
    (define/override (on-paint)
      (when red?
        (let ([dc (get-dc)])
          (let-values ([(cw ch) (get-client-size)])
            (let ([pen (send dc get-pen)]
                  [brush (send dc get-brush)])
              (send dc set-pen "black" 1 'transparent)
              (send dc set-brush "pink" 'solid)
              (send dc draw-rectangle 0 0 cw ch)
              (send dc set-pen pen)
              (send dc set-brush brush)))))
      (super on-paint))
    (super-new)))

(define-local-member-name 
  update-matches 
  get-find-edit
  get-replace-edit
  search-string-changed)

(define searchable-mixin
  (mixin (standard-menus<%>) (searchable<%>)
    (inherit edit-menu:get-show/hide-replace-item)
    
    (define super-root 'unitiaialized-super-root)
    
    (define case-sensitive-search? (preferences:get 'framework:case-sensitive-search?))
    (define/public (get-case-sensitive-search?) case-sensitive-search?)
    (define replace-visible? (preferences:get 'framework:replace-visible?))
  
    (define/override (edit-menu:find-callback menu evt) (unhide-search-and-toggle-focus) #t)
    (define/override (edit-menu:create-find?) #t)

    (define/override (edit-menu:find-from-selection-callback menu evt) 
      (unhide-search-and-toggle-focus #:new-search-string-from-selection? #t)
      #t)
    (define/override (edit-menu:find-from-selection-on-demand item)
      (define t (get-text-to-search))
      (send item enable (and t (not (= (send t get-start-position)
                                       (send t get-end-position))))))
    (define/override (edit-menu:create-find-from-selection?) #t)
    
    (define/override (edit-menu:find-next-callback menu evt) (search 'forward) #t)
    (define/override (edit-menu:create-find-next?) #t)
    
    (define/override (edit-menu:find-previous-callback menu evt) (search 'backward) #t)
    (define/override (edit-menu:create-find-previous?) #t)
    
    (define/override (edit-menu:create-show/hide-replace?) #t)
    (define/override (edit-menu:show/hide-replace-callback a b) 
      (cond
        [hidden?
         (unhide-search #f)
         (set-replace-visible? #t)
         (send replace-edit set-position 0 (send replace-edit last-position))
         (send (send replace-edit get-canvas) focus)]
        [else
         (set-replace-visible? (not replace-visible?))
         (when replace-visible?
           (send replace-edit set-position 0 (send replace-edit last-position))
           (send (send replace-edit get-canvas) focus))])
      #t)
    (define/override (edit-menu:show/hide-replace-string)
      (if replace-visible?
          (string-constant hide-replace-menu-item)
          (string-constant show-replace-menu-item)))
    (define/override (edit-menu:show/hide-replace-on-demand item) 
      (send item set-label
            (if (and (not hidden?) replace-visible?)
                (string-constant hide-replace-menu-item)
                (string-constant show-replace-menu-item))))
    
    (define/override (edit-menu:replace-callback a b) (search-replace) #t)
    (define/override (edit-menu:create-replace?) #t)
    (define/override (edit-menu:replace-on-demand item) 
      (send item enable (and (not hidden?) replace-visible?)))

    (define/override (edit-menu:find-case-sensitive-callback menu evt) 
      (set! case-sensitive-search? (not case-sensitive-search?))
      (preferences:set 'framework:case-sensitive-search? case-sensitive-search?)
      (when find-edit
        (unless hidden?
          (search-string-changed))))
    (define/override (edit-menu:find-case-sensitive-on-demand item) (send item check case-sensitive-search?))
    (define/override (edit-menu:create-find-case-sensitive?) #t)

    (define/override (edit-menu:replace-all-callback menu evt) (replace-all) #t)
    (define/override (edit-menu:replace-all-on-demand item) (send item enable (not hidden?)))
    (define/override (edit-menu:create-replace-all?) #t)

    (define/override make-root-area-container
      (λ (% parent)
        (let* ([s-root (super make-root-area-container
                              vertical-panel%
                              parent)]
               [root (make-object % s-root)])
          (set! super-root s-root)
          root)))
    
    (define text-to-search #f)
    (define/public-final (get-text-to-search) text-to-search)

    (define/public-final (set-text-to-search new)
      (unless (eq? new text-to-search)
        (let ([old text-to-search])
          (set! text-to-search new)
          (unless hidden?
            (when find-edit
              (when old
                (send old set-searching-state #f #f #f #f)
                (send old set-search-anchor #f))
              (when new
                (send new set-search-anchor (send new get-start-position))
                (search-parameters-changed)))))))
    
    ;; called by the text-to-search when it finishes the search
    (define/public-final (search-hits-changed)
      (when find-edit
        (when text-to-search
          (let-values ([(before-caret-new-hits new-hits) (send text-to-search get-search-hit-count)])
            (update-matches before-caret-new-hits new-hits)
            (let ([is-red? (and (zero? new-hits) 
                                (not (zero? (send find-edit last-position))))])
              (send find-canvas set-red is-red?))))))

    (define/public-final (search-string-changed) (search-parameters-changed))
    (define/private (search-parameters-changed)
      (let ([str (send find-edit get-text)])
        (send text-to-search set-searching-state
              (if (equal? str "") #f str)
              case-sensitive-search?
              replace-visible?
              #t)))
    
    (define/public (search-hidden?) hidden?)
    
    (define/public (hide-search)
      (set! hidden? #t)
      (when search-gui-built?
        (when text-to-search
          (send text-to-search set-searching-state #f #f #f #f))
        (send super-root change-children
              (λ (l)
                (remove search/replace-panel l)))
        (clear-search-highlight)
        (when text-to-search
          (send text-to-search set-search-anchor #f)
          (let ([canvas (send text-to-search get-canvas)])
            (when canvas
              (send canvas focus))))))
    
    (define/public (unhide-search focus? #:new-search-string-from-selection? [new-search-string-from-selection? #f])
      (when hidden?
        (set! hidden? #f)
        (build-search-gui-in-frame)
        (unless (memq search/replace-panel (send super-root get-children))
          (send super-root add-child search/replace-panel))
        (search-parameters-changed)
        (send find-edit begin-edit-sequence #t #f)
        (when new-search-string-from-selection?
          (update-search-string-from-selection))
        (when focus?
          (send find-edit set-position 0 (send find-edit last-position))
          (send (send find-edit get-canvas) focus))
        (send find-edit end-edit-sequence)))
    
    (define/public (unhide-search-and-toggle-focus #:new-search-string-from-selection? [new-search-string-from-selection? #f])
      (if hidden?
        (unhide-search #t #:new-search-string-from-selection? new-search-string-from-selection?)
        (let ([canvas (and text-to-search (send text-to-search get-canvas))])
          (cond
            [(or (not text-to-search) (and canvas (send canvas has-focus?)))
             (send find-edit begin-edit-sequence #t #f)
             (when new-search-string-from-selection?
               (update-search-string-from-selection))
             (send find-edit set-position 0 (send find-edit last-position))
             (send find-canvas focus)
             (send find-edit end-edit-sequence)]
            [canvas
             (send canvas focus)]))))
    
    (define/private (update-search-string-from-selection)
      (when (and text-to-search
                 (not (= (send text-to-search get-start-position)
                         (send text-to-search get-end-position))))
        (send find-edit delete 0 (send find-edit last-position))
        (send text-to-search move/copy-to-edit 
              find-edit 
              (send text-to-search get-start-position)
              (send text-to-search get-end-position)
              0
              #:try-to-move? #f)))
    
    (define/public (search searching-direction)
      (unhide-search #f)
      (send find-edit search searching-direction #t))
    
    (define/public (search-replace) 
      (let ([text-to-search (get-text-to-search)])
        (when text-to-search
          (let ([replacee-start (send text-to-search get-replace-search-hit)])
            (when replacee-start
              (let ([replacee-end (+ replacee-start (send find-edit last-position))])
                (send text-to-search begin-edit-sequence)
                (send text-to-search set-position replacee-end replacee-end)
                (send text-to-search delete replacee-start replacee-end)
                (copy-over replace-edit 0 (send replace-edit last-position) text-to-search replacee-start)
                (search 'forward)
                (send text-to-search end-edit-sequence)))))))
      
    (define/private (copy-over src-txt src-start src-end dest-txt dest-pos)
      (send src-txt split-snip src-start)
      (send src-txt split-snip src-end)
      (let loop ([snip (send src-txt find-snip src-end 'before)])
        (cond
          [(or (not snip) (< (send src-txt get-snip-position snip) src-start))
           (void)]
          [else
           (let ([prev (send snip previous)])
             (send dest-txt insert (send snip copy) dest-pos dest-pos)
             (loop prev))])))
    
    (define/public (replace-all)
      (unhide-search #f)
      (let ([txt (get-text-to-search)])
        (when txt
          (let ([search-str (send find-edit get-text)]
                [ht (make-hasheq)])
            (send txt begin-edit-sequence)
            (hash-set! ht txt #t)
            (let loop ([txt (pop-all-the-way-out txt)]
                       [pos 0])
              (let-values ([(found-txt found-pos) (find-string-embedded txt
                                                                        search-str
                                                                        'forward
                                                                        pos
                                                                        'eof
                                                                        #f
                                                                        case-sensitive-search?
                                                                        #t)])
                (when found-pos
                  (unless (hash-ref ht found-txt #f)
                    (hash-set! ht found-txt #t)
                    (send found-txt begin-edit-sequence))
                  (let ([start (- found-pos (send find-edit last-position))])
                    (send found-txt delete start found-pos)
                    (copy-over replace-edit 0 (send replace-edit last-position) found-txt start)
                    (loop found-txt (+ start (send replace-edit last-position)))))))
            (hash-for-each ht (λ (txt _) (send txt end-edit-sequence)))))))
                             
    (define/private (pop-all-the-way-out txt)
      (let ([admin (send txt get-admin)])
        (if (is-a? admin editor-snip-editor-admin<%>)
            (let* ([snip (send admin get-snip)]
                   [edit-above (send (send snip get-admin) get-editor)])
              (pop-all-the-way-out edit-above))
            txt)))
    
    (define/private (find-embedded-focus-editor editor)
      (let loop ([editor editor])
        (let ([s (send editor get-focus-snip)])
          (cond
            [(and s (is-a? s editor-snip%))
             (let ([next-ed (send s get-editor)])
               (if next-ed 
                   (loop next-ed)
                   editor))]
            [else editor]))))
    
    (define search-gui-built? #f)
    (define search/replace-panel #f)
    (define find-canvas #f)
    (define replace-canvas #f)
    (define hidden? #t)
    (field [update-matches void])
    (define find-edit #f)
    (define replace-edit #f)
    
    (define/public (get-find-edit) find-edit)
    (define/public (get-replace-edit) replace-edit)
    
    (inherit begin-container-sequence end-container-sequence)
    
    (define/public (get-replace-visible?) replace-visible?)
    (define/public (set-replace-visible? r?) 
      (unless (equal? replace-visible? r?)
        (set! replace-visible? r?)
        (preferences:set 'framework:replace-visible? r?)
        (show/hide-replace)
        (send (edit-menu:get-show/hide-replace-item) set-label
              (if replace-visible?
                  (string-constant hide-replace-menu-item)
                  (string-constant show-replace-menu-item)))
        (search-parameters-changed)))
    
    (define show/hide-replace void)
    
    (define/private (build-search-gui-in-frame)
      (unless search-gui-built?
        (set! search-gui-built? #t)
        (begin-container-sequence)
        (set! find-edit (new find-text%))
        (set! replace-edit (new replace-text%))
        (set! search/replace-panel (new horizontal-panel% 
                                        [parent super-root]
                                        [stretchable-height #f]))
        (define search-panel
          (new horizontal-panel% 
               [parent search/replace-panel]
               [stretchable-height #f]))
        (define replace-panel
          (new horizontal-panel%
               [parent search/replace-panel]
               [stretchable-height #f]))
        (set! find-canvas (new searchable-canvas%
                               [style '(hide-hscroll hide-vscroll)]
                               [vertical-inset 2]
                               [parent search-panel]
                               [editor find-edit]
                               [line-count 1]
                               [stretchable-height #f]
                               [stretchable-width #t]))
        (set! replace-canvas (new searchable-canvas%
                                  [style '(hide-hscroll hide-vscroll)]
                                  [vertical-inset 2]
                                  [parent replace-panel]
                                  [editor replace-edit]
                                  [line-count 1]
                                  [stretchable-height #f]
                                  [stretchable-width #t]))
        
        (define search-button (new button% 
                                   [label (string-constant search-next)]
                                   [vert-margin 0]
                                   [parent search-panel]
                                   [callback (λ (x y) (search 'forward))]
                                   [font small-control-font]))
        (define search-prev-button (new button% 
                                        [label (string-constant search-previous)]
                                        [vert-margin 0]
                                        [parent search-panel]
                                        [callback (λ (x y) (search 'backward))]
                                        [font small-control-font]))
        
        (define hits-panel (new vertical-panel%
                                [parent search-panel]
                                [alignment '(left center)]
                                [stretchable-height #f]
                                [stretchable-width #f]))
        
        (define num-msg (new message% 
                             [label "0"] 
                             [vert-margin 0]
                             [auto-resize #t]
                             [font tiny-control-font]
                             [parent hits-panel]))
        (define matches-msg (new message% 
                                 [label (string-constant search-matches)]
                                 [vert-margin 0]
                                 [font tiny-control-font]
                                 [parent hits-panel]))
        
        (define _6 (set! update-matches
                         (λ (before-caret-m m) 
                           (cond
                             [(zero? m)
                              (send num-msg set-label "0")]
                             [else
                              (let ([number (number->str/comma m)]
                                    [bc-number (number->str/comma before-caret-m)])
                                (send num-msg set-label (format "~a/~a" bc-number number)))])
                           (send matches-msg set-label (if (= m 1) 
                                                           (string-constant search-match)
                                                           (string-constant search-matches))))))
        
        (define replace-button
          (new button% 
               [label (string-constant search-replace)]
               [vert-margin 0]
               [parent replace-panel]
               [font small-control-font]
               [callback (λ (x y) (search-replace))]))
        (define skip-button
          (new button% 
               [label (string-constant search-skip)]
               [vert-margin 0]
               [parent replace-panel]
               [font small-control-font]
               [callback (λ (x y) (search 'forward))]))
        
        (define show-replace-button
          (new button%
               [label (string-constant search-show-replace)]
               [font small-control-font]
               [callback (λ (a b) (set-replace-visible? #t))]
               [parent replace-panel]))
        (define hide-replace-button
          (new button%
               [label (string-constant search-hide-replace)]
               [font small-control-font]
               [callback (λ (a b) (set-replace-visible? #f))]
               [parent replace-panel]))
        
        (set! show/hide-replace
              (λ ()
                (send replace-panel begin-container-sequence)
                (cond
                  [replace-visible?
                   (send replace-panel change-children (λ (l) all-replace-children))
                   (send replace-panel stretchable-width #t)]
                  [else
                   (send replace-panel change-children (λ (l) (list show-replace-button)))
                   (send replace-panel stretchable-width #f)])
                (send replace-panel end-container-sequence)))
        
        (define all-replace-children
          (list replace-canvas
                replace-button
                skip-button
                hide-replace-button))
        
        (define hide-button
          (new close-icon%
               [callback (λ () (hide-search))]
               [vertical-pad 0]
               [parent search/replace-panel]))
        
        (show/hide-replace)
        (end-container-sequence)))
    
    (super-new)))

(define (number->str/comma m)
  (list->string
   (reverse
    (let loop ([chars (reverse (string->list (format "~a" m)))])
      (cond
        [(<= (length chars) 3)
         chars]
        [else (list* (list-ref chars 0)
                     (list-ref chars 1)
                     (list-ref chars 2)
                     #\,
                     (loop (cdddr chars)))])))))

(define searchable-text<%> (interface (searchable<%> text<%>)))

(define searchable-text-mixin
  (mixin (text<%> searchable<%>) (searchable-text<%>)
    (inherit get-editor)
    (define/override (get-editor<%>) text:searching<%>)
    (define/override (get-editor%) (text:searching-mixin (super get-editor%)))
    (super-new)))

;; code copied to drracket/private/unit.rkt
(define checkout-or-nightly?
  (or (with-handlers ([exn:fail:filesystem? (λ (x) #f)])
        (directory-exists? (collection-path "repo-time-stamp")))
      (with-handlers ([exn:fail:filesystem? (λ (x) #f)])
        (let ([fw (collection-path "framework")])
          (directory-exists? (build-path fw 'up 'up ".git"))))))

(define memory-canvases '())
(define show-memory-text? checkout-or-nightly?)

(define bday-click-canvas%
  (class canvas%
    (define/override (on-event evt)
      (cond
        [(and (mrf-bday?)
              (send evt button-up?))
         (message-box (string-constant drscheme)
                      (string-constant happy-birthday-matthew))]
        [else (super on-event evt)]))
    (super-new)))

(define pref-save-canvas%
  (class canvas%
    (define on? #f)
    
    (define mouse-over? #f)
    (define mouse-down? #f)
    
    (define/private (update-mouse-over? mo?)
      (unless (eq? mouse-over? mo?)
        (set! mouse-over? mo?)
        (refresh)))
    (define/private (update-mouse-down? md?)
      (unless (eq? mouse-down? md?)
        (set! mouse-down? md?)
        (refresh)))
    
    (define indicator "P")
    
    (inherit refresh)
    (define/override (on-event evt)
      (cond
        [(send evt entering?)
         (update-mouse-over? #t)]
        [(send evt leaving?)
         (update-mouse-over? #f)])
      (cond
        [(send evt button-down?)
         (update-mouse-down? #t)]
        [(send evt button-up?)
         (update-mouse-down? #f)
         (let-values ([(cw ch) (get-client-size)])
           (when (and (<= 0 (send evt get-x) cw)
                      (<= 0 (send evt get-y) ch))
             (show-prefs-stats)))]))
    
    (define/private (show-prefs-stats)
      (define f (new frame% 
                     [label (format "~a - Preferences Stats" (string-constant drscheme))]
                     [width 600]
                     [height 400]))
      (define t (new text%))
      (define ec (new editor-canvas% [parent f] [editor t]))
      (send f reflow-container)
      (send t begin-edit-sequence)
      (define tp (open-output-text-editor t))
      (define prefs-file (find-system-path 'pref-file))
      (fprintf tp "prefs file:\n  ~a\n\n" (path->string prefs-file))
      (fprintf tp "setting a preference:\n  ")
      (preferences:set 'drracket:prefs-debug #f)
      (parameterize ([current-output-port tp])
        (time (preferences:set 'drracket:prefs-debug #t)))
      (define file-contents (call-with-input-file prefs-file read))
      (fprintf tp "\n~s preference keys\n\n" (length file-contents))
      
      (fprintf tp "preferences taking the most space:\n")
      (define sizes (map 
                     (λ (x) 
                       (list 
                        (car x)
                        (bytes-length (string->bytes/utf-8 (format "~s" x)))))
                     file-contents))
      (for ([frame (in-list (sort sizes > #:key cadr))]
            [x (in-range 0 10)])
        (define key (list-ref frame 0))
        (define size (list-ref frame 1))
        (fprintf tp "  ~s (~s bytes)\n" key size))
      (send t auto-wrap #t)
      (send t set-position 0 0)
      (send t lock #t)
      (send t end-edit-sequence)
      (send f show #t))
    
    (define/override (on-paint)
      (define-values (cw ch) (get-client-size))
      (define dc (get-dc))
      (define (draw-p)
        (send dc set-font small-control-font)
        (send dc draw-text indicator
              (- (/ cw 2) (/ indicator-width 2))
              (- (/ ch 2) (/ indicator-height 2))))
      (cond
        [on?
         (send dc set-text-foreground (send the-color-database find-color "black"))
         (draw-p)]
        [mouse-over?
         (send dc set-brush (if mouse-down? "blue" "skyblue") 'solid)
         (send dc set-pen "black" 1 'transparent)
         (send dc draw-rectangle 0 0 cw ch)
         (send dc set-text-foreground (send the-color-database find-color "white"))
         (draw-p)]))
    
    (define/public (set-on? new-on?)
      (set! on? new-on?)
      (send (get-dc) erase)
      (on-paint)
      (flush))
    
    (inherit get-dc flush get-client-size min-width min-height)
    (super-new [stretchable-width #f]
               [stretchable-height #f]
               [style '(transparent no-focus)])
    
    (send (get-dc) set-smoothing 'smoothed)
    (define-values (indicator-width indicator-height)
      (let-values ([(tw th _1 _2) (send (get-dc) get-text-extent indicator small-control-font)])
        (values tw th)))
    (min-width (+ (inexact->exact (ceiling indicator-width)) 4))
    (min-height (+ (inexact->exact (ceiling indicator-height)) 4))))

(define basic% (focus-table-mixin (register-group-mixin (basic-mixin frame%))))
(define size-pref% (size-pref-mixin basic%))
(define info% (info-mixin basic%))
(define text-info% (text-info-mixin info%))
(define pasteboard-info% (pasteboard-info-mixin text-info%))
(define status-line% (status-line-mixin text-info%))
(define standard-menus% (standard-menus-mixin status-line%))
(define editor% (editor-mixin standard-menus%))

(define -text% (text-mixin editor%))
(define searchable% (searchable-text-mixin (searchable-mixin -text%)))
(define delegate% (delegate-mixin searchable%))

(define -pasteboard% (pasteboard-mixin editor%))

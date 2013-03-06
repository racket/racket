#lang scheme/unit

  (require string-constants
           racket/class
           "sig.rkt"
           "../preferences.rkt"
           "../gui-utils.rkt"
           mred/mred-sig
           racket/path)
  
  (import mred^
          [prefix application: framework:application^]
          [prefix frame: framework:frame^]
          [prefix text: framework:text^]
          [prefix canvas: framework:canvas^]
          [prefix menu: framework:menu^]
          [prefix exit: framework:exit^])
  (export framework:group^)
  
  (define-struct frame (frame id))
  
  (define mdi-parent #f)
  
  (define extra-windows-menus-proc void)
  (define (add-to-windows-menu f)
    (let ([old extra-windows-menus-proc])
      (set! extra-windows-menus-proc
            (λ (menu)
              (f menu)
              (old menu)))))
  
  (define windows-menu-label
    (case (system-type)
      [(macosx) (string-constant windows-menu-label)]
      [else (string-constant tabs-menu-label)]))
  
  (define-local-member-name update-windows-menu)
  
  (define %
    (class object%
      
      [define active-frame #f]
      [define most-recent-window-box (make-weak-box #f)]
      [define frame-counter 0]
      [define frames null]
      [define todo-to-new-frames void]
      
      [define windows-menus null]
      
      ;; get-windows-menu : (is-a?/c frame%) -> (union false? (is-a?/c menu%))
      (define/private (get-windows-menu frame)
        (let ([menu-bar (send frame get-menu-bar)])
          (and menu-bar
               (let ([menus (send menu-bar get-items)])
                 (ormap (λ (x)
                          (if (or (string=? (string-constant windows-menu)
                                            (send x get-plain-label))
                                  (string=? (string-constant tabs-menu)
                                            (send x get-plain-label)))
                              x
                              #f))
                        menus)))))
      
      (define/private (insert-windows-menu frame)
        (let ([menu (get-windows-menu frame)])
          (when menu
            (set! windows-menus (cons menu windows-menus)))))
      
      (define/private (remove-windows-menu frame)
        (let ([menu (get-windows-menu frame)])
          
          (when menu
            ;; to help the gc.
            (for-each (λ (i) (send i delete)) (send menu get-items))
            
            (set! windows-menus
                  (remove
                   menu
                   windows-menus
                   eq?)))))
      
      (define/public (update-windows-menu menu)
        (let* ([windows (length windows-menus)]
               [default-name (string-constant untitled)]
               [get-name 
                (λ (frame)
                  (let ([label (send frame get-label)])
                    (if (string=? label "")
                        (if (method-in-interface? 'get-entire-label (object-interface frame))
                            (let ([label (send frame get-entire-label)])
                              (if (string=? label "")
                                  default-name
                                  label))
                            default-name)
                        label)))]
               [sorted/visible-frames
                (sort
                 (filter (λ (x) (send (frame-frame x) is-shown?)) frames)
                 (λ (f1 f2)
                   (string-ci<=? (get-name (frame-frame f1))
                                 (get-name (frame-frame f2)))))])
          (for-each (λ (item) (send item delete)) (send menu get-items))
          (when (eq? (system-type) 'macosx)
            (new menu:can-restore-menu-item%
                 [label (string-constant minimize)]
                 [parent menu]
                 [callback (λ (x y) (send (send (send menu get-parent) get-frame) iconize #t))]
                 [shortcut #\m])
            (new menu:can-restore-menu-item%
                 [label (string-constant zoom)]
                 [parent menu]
                 [callback (λ (x y) 
                             (let ([frame (send (send menu get-parent) get-frame)])
                               (send frame maximize (not (send frame is-maximized?)))))]) 
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant bring-frame-to-front...))
              (parent menu)
              (callback (λ (x y) (choose-a-frame (send (send menu get-parent) get-frame))))
              (shortcut #\j))
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant most-recent-window))
              (parent menu)
              (callback (λ (x y) (most-recent-window-to-front)))
              (shortcut #\'))
            (make-object separator-menu-item% menu))
          
          (extra-windows-menus-proc menu)
          
          (when (eq? (system-type) 'macosx)
            (for-each
             (λ (frame)
               (let ([frame (frame-frame frame)])
                 (make-object menu-item% 
                   (regexp-replace*
                    #rx"&"
                    (gui-utils:trim-string (get-name frame) 200)
                    "&&")
                   menu
                   (λ (_1 _2)
                     (send frame show #t)))))
             sorted/visible-frames))))
      
      ;; most-recent-window-to-front : -> void?
      ;; brings the most recent window to the front
      (define/private (most-recent-window-to-front)
        (let ([most-recent-window (weak-box-value most-recent-window-box)])
          (when most-recent-window
            (send most-recent-window show #t))))                              
      
      (define/private (update-close-menu-item-state)
        (let* ([set-close-menu-item-state! 
                (λ (frame state)
                  (when (is-a? frame frame:standard-menus<%>)
                    (let ([close-menu-item (send frame file-menu:get-close-menu)])
                      (when close-menu-item
                        (send close-menu-item enable state)))))])
          (if (eq? (length frames) 1)
              (set-close-menu-item-state! (car frames) #f)
              (for-each (λ (a-frame)
                          (set-close-menu-item-state! a-frame #t))
                        frames))))
      
      (define/public (get-mdi-parent)
        (when (and (eq? (system-type) 'windows)
                   (preferences:get 'framework:windows-mdi)
                   (not mdi-parent))
          (set! mdi-parent (make-object frame% (application:current-app-name)
                             #f #f #f #f #f
                             '(mdi-parent)))
          (send mdi-parent show #t))
        mdi-parent)
      
      (define/public (get-frames) (map frame-frame frames))
      
      (define/public (frame-label-changed frame)
        (void))
      
      (define/public (frame-shown/hidden frame)
        (void))
      
      (define/public (for-each-frame f)
        (for-each (λ (x) (f (frame-frame x))) frames)
        (set! todo-to-new-frames
              (let ([old todo-to-new-frames])
                (λ (frame) (old frame) (f frame)))))
      
      (define/public (get-active-frame)
        (cond
          [active-frame active-frame]
          [(null? frames) #f]
          [else (frame-frame (car frames))]))
      
      (define/public (set-active-frame f)
        (when (and active-frame
                   (not (eq? active-frame f)))
          (set! most-recent-window-box (make-weak-box active-frame)))
        (set! active-frame f))
      
      (define/public (insert-frame new-frame)
        (unless (memf (λ (fr) (eq? (frame-frame fr) new-frame))
                      frames)
          (set! frame-counter (add1 frame-counter))
          (let ([new-frames (cons (make-frame new-frame frame-counter)
                                  frames)])
            (set! frames new-frames)
            (update-close-menu-item-state)
            (insert-windows-menu new-frame))
          (todo-to-new-frames new-frame)))
      
      (define/public (remove-frame f)
        (when (eq? f active-frame)
          (set! active-frame #f))
        (let ([new-frames
               (remove
                f frames
                (λ (f fr) (eq? f (frame-frame fr))))])
          (set! frames new-frames)
          (update-close-menu-item-state)
          (remove-windows-menu f)))
      
      (define/public (clear)
        (set! frames null)
        #t)
      
      (define/public (on-close-all)
        (for-each (λ (f)
                    (let ([frame (frame-frame f)])
                      (send frame on-close) 
                      (send frame show #f)))
                  frames))
      
      (define/public (can-close-all?)
        (andmap (λ (f)
                  (let ([frame (frame-frame f)])
                    (send frame can-close?)))
                frames))
      
      (define/public (locate-file name)
        (let* ([normalized
                ;; allow for the possibility of filenames that are urls
                (with-handlers ([exn:fail?
                                 (λ (x) name)])
                  (normal-case-path
                   (normalize-path name)))]
               [test-frame
                (λ (frame)
                  (and (is-a? frame frame:basic<%>)
                       (send frame editing-this-file? normalized)))])
          (let loop ([frames frames])
            (cond
              [(null? frames) #f]
              [else
               (let* ([frame (frame-frame (car frames))])
                 (if (test-frame frame)
                     frame
                     (loop (cdr frames))))]))))
      
      (super-new)))

  (define (create-windows-menu mb)
    (new menu:can-restore-underscore-menu%
         [label windows-menu-label]
         [demand-callback (λ (menu) (send (get-the-frame-group) update-windows-menu menu))]
         [parent mb]))
  
  (define (can-close-check)
    (let ([number-of-frames (length (send (get-the-frame-group) get-frames))])
      (or (not (preferences:get 'framework:exit-when-no-frames))
          (exit:exiting?)
          (not (= 1 number-of-frames))
          (current-eventspace-has-standard-menus?)
          (exit:user-oks-exit))))

  (define (on-close-action)
    (when (preferences:get 'framework:exit-when-no-frames)
      (unless (exit:exiting?)
        (when (and (null? (send (get-the-frame-group) get-frames))
                   (not (current-eventspace-has-standard-menus?)))
          (exit:exit)))))
  
  (define (choose-a-frame parent)
    (letrec-values ([(sorted-frames)
                     (sort
                      (send (get-the-frame-group) get-frames)
                      (λ (x y) (string-ci<=? (send x get-label) (send y get-label))))]
                    [(d) (make-object dialog% (string-constant bring-frame-to-front) parent 400 600)]
                    [(lb) (instantiate list-box% () 
                            (label #f)
                            (choices (map (λ (x) (gui-utils:trim-string (send x get-label) 200)) sorted-frames))
                            (callback (λ (x y) (listbox-callback y)))
                            (parent d))]
                    [(t) (instantiate text:hide-caret/selection% ())]
                    [(ec) (instantiate canvas:basic% ()
                            (parent d)
                            (stretchable-height #f))]
                    [(bp) (instantiate horizontal-panel% ()
                            (parent d)
                            (stretchable-height #f)
                            (alignment '(right center)))]
                    [(cancelled?) #t]
                    [(listbox-callback)
                     (λ (evt)
                       (case (send evt get-event-type)
                         [(list-box)
                          
                          (send ok enable (pair? (send lb get-selections)))
                          
                          (let ([full-name
                                 (let ([sels (send lb get-selections)])
                                   (and (pair? sels)
                                        (let ([fr (list-ref sorted-frames (car sels))])
                                          (and (is-a? fr frame:basic%)
                                               (send fr get-filename)))))])
                            (send t begin-edit-sequence)
                            (send t erase)
                            (when full-name
                              (send t insert (path->string full-name)))
                            (send t end-edit-sequence))]
                         [(list-box-dclick)
                          (set! cancelled? #f)
                          (send d show #f)]))]
                    [(ok cancel)
                     (gui-utils:ok/cancel-buttons
                      bp
                      (λ (x y)
                        (set! cancelled? #f)
                        (send d show #f))
                      (λ (x y)
                        (send d show #f)))])
      (send ec set-line-count 3)
      (send ec set-editor t)
      (send t auto-wrap #t)
      (let ([fr (car sorted-frames)])
        (when (and (is-a? fr frame:basic<%>)
                   (send fr get-filename))
          (send t insert (path->string (send (car sorted-frames) get-filename))))
        (send lb set-selection 0))
      (send d show #t)
      (unless cancelled?
        (let ([sels (send lb get-selections)])
          (unless (null? sels)
            (send (list-ref sorted-frames (car sels)) show #t))))))
  
  
  (define (internal-get-the-frame-group)
    (let ([the-frame-group (make-object %)])
      (set! internal-get-the-frame-group (λ () the-frame-group))
      (internal-get-the-frame-group)))
  
  (define (get-the-frame-group)
    (internal-get-the-frame-group))

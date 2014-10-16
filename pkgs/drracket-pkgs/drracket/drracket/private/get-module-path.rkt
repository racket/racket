#lang racket/base
(require racket/class
         racket/contract
         racket/gui/base
         string-constants
         setup/path-to-relative
         framework
         "find-completions.rkt")

(provide
 (contract-out
  [get-module-path-from-user
   (->i ()
        (#:init [init string?] 
         #:pref [pref symbol?]
         #:dir? [dir? boolean?]
         #:current-directory [current-directory (or/c path-string? #f)])
        [res (dir?)
             (if (or (not dir?)
                     (unsupplied-arg? dir?))
                 (or/c path? #f)
                 (or/c (listof path?) #f))])]))

(define (get-module-path-from-user #:init [init-value ""] 
                                   #:pref [pref-sym #f]
                                   #:dir? [dir? #f]
                                   #:current-directory 
                                   [_the-current-directory #f])
  (define the-current-directory (or _the-current-directory (current-directory)))
  (define dlg%
    (class dialog%
      (define/override (on-subwindow-char receiver event)
        (cond
          [(member (send event get-key-code) '(up down))
           (up/down-callback (send event get-key-code))]
          [else (super on-subwindow-char receiver event)]))
      (define/public (new-clcl/clcp clcl/clcp)
        (update-list-of-paths))
      (define/override (on-size w h)
        (preferences:set 'drracket:get-module-path-from-user-size (list w h)))
      (super-new)))
  
  (define dlg (new dlg%
                   [style '(resize-border)]
                   [label (string-constant drracket)]
                   [width (car (preferences:get 'drracket:get-module-path-from-user-size))]
                   [height (cadr (preferences:get 'drracket:get-module-path-from-user-size))]))
  (define tf (new text-field% [parent dlg] [label #f]
                  [init-value init-value]
                  [callback (λ (tf evt) (tf-callback))]))
  (send (send tf get-editor) set-position 0 (send (send tf get-editor) last-position))
  (define lb (new list-box% 
                  [style (if dir? '(extended) '(single))]
                  [parent dlg] [choices '()] [label #f]
                  [callback (λ (lb evt)
                              (cond
                                [(equal? (send evt get-event-type) 'list-box-dclick)
                                 (handle-list-box-double-click)]
                                [else
                                 (update-buttons)]))]))
                  
  (define different-racket-panel
    (new vertical-panel% 
         [parent dlg]
         [stretchable-height #f]
         [alignment '(left center)]))
  (define racket-path-cb
    (new check-box%
         [label (string-constant use-a-different-racket)]
         [value (list-ref (preferences:get racket-binary-pref) 0)]
         [callback (λ (_1 _2) (racket-path-cb-callback))]
         [parent different-racket-panel]))
  (define racket-path-tf
    (new text-field% 
         [parent different-racket-panel]
         [label (string-constant path-to-racket-binary)]
         [init-value (list-ref (preferences:get racket-binary-pref) 1)]
         [callback (λ (_1 _2) (racket-path-tf-callback))]))

  (define bp (new horizontal-panel% 
                  [parent dlg]
                  [stretchable-height #f]
                  [alignment '(right center)]))
  
  (define enter-sub-button 
    (and (not dir?)
         (new button% 
              [parent bp]
              [style '(border)]
              [label (string-constant enter-subcollection)]
              [callback (λ (_1 _2) (enter-sub))])))
  
  (define-values (ok-button cancel-button) 
    (gui-utils:ok/cancel-buttons 
     bp 
     (λ (_1 _2) (ok))
     (λ (_1 _2) (cancel))))
  
  (new grow-box-spacer-pane% [parent bp])
  
  (define (tf-callback)
    (when pref-sym
      (preferences:set pref-sym (send tf get-value)))
    (update-list-of-paths))
  
  (define (up/down-callback key)
    (define up? (equal? key 'up))
    (define old-sel (send lb get-selection))
    (define dir (if up? -1 1))
    (unless (= 0 (send lb get-number))
      (send lb set-selection 
            (cond
              [old-sel 
               (modulo (+ old-sel dir)
                       (send lb get-number))]
              [up?
               (- (send lb get-number) 1)]
              [else
               0])))
    (update-buttons))
  
  (define (racket-path-tf-callback)
    (preferences:set racket-binary-pref 
                     (list (list-ref (preferences:get racket-binary-pref) 0)
                           (send racket-path-tf get-value)))
    (update-list-of-paths)
    (maybe-turn-racket-path-pink)
    (new-alternate-racket (send racket-path-tf get-value) dlg))
  
  (define (racket-path-cb-callback)
    (define nv (send racket-path-cb get-value))
    (preferences:set racket-binary-pref
                     (list nv (list-ref (preferences:get racket-binary-pref) 1)))
    (update-different-racket-gui)
    (update-list-of-paths)
    (when nv
      (send racket-path-tf focus)))
  
  (define (update-list-of-paths)
    (adjust-lb)
    (update-buttons))
  
  (define p->r-s/l-cache (make-hash))
  (define (path->rel-string p alt-racket-info)
    (if alt-racket-info
        (path->string p)
        (path->relative-string/library p #:cache p->r-s/l-cache)))
  
  (define (adjust-lb)
    (send lb clear)
    (unless (equal? (send tf get-value) "")
      (define alt-racket-info 
        (and (send racket-path-cb get-value)
             (get-clcl/clcp)))
      (define the-completions 
        (find-completions (send tf get-value) 
                          the-current-directory
                          #:alternate-racket alt-racket-info))
      (for ([i (in-list (if dir?
                            (filter (λ (i) (directory-exists? (list-ref i 1)))
                                    the-completions)
                            the-completions))]
            [n (in-naturals)])
        (send lb append (path->rel-string (list-ref i 1) alt-racket-info))
        ;; data holds a path => open the file
        ;; data holds a string => add that past the last / in 'tf'
        ;; when dir?=#t, then data always holds a path
        (cond
          [(or dir? (file-exists? (list-ref i 1)))
           (send lb set-data n (list-ref i 1))]
          [else
           (send lb set-data n (list-ref i 0))]))
      (when (= 1 (send lb get-number))
        (send lb set-selection 0))))
  
  (define (maybe-turn-racket-path-pink)
    (define pth (send racket-path-tf get-value))
    (define bkg
      (cond
        [(and (path-string? pth)
              (file-exists? pth) 
              (member 'execute (file-or-directory-permissions pth)))
         "white"]
        [else "yellow"]))
    (send racket-path-tf set-field-background 
          (send the-color-database find-color bkg)))
  
  (define (update-different-racket-gui)
    (send different-racket-panel
          change-children
          (λ (l)
            (if (list-ref (preferences:get racket-binary-pref) 0)
                (list racket-path-cb racket-path-tf)
                (list racket-path-cb)))))
  
  (define (forward-new-alternate-racket)
    (cond
      [(send racket-path-cb get-value)
       (define s (send racket-path-tf get-value))
       (and (not (equal? s ""))
            (not (regexp-match? #rx"\0" s))
            s)]
      [else #f]))
  
  (define cancelled? #t)
  
  (define (ok) 
    (set! cancelled? #f)
    (send dlg show #f))
  (define (cancel) (send dlg show #f))
  (define (enter-sub)
    (define item-to-act-on (get-item-to-act-on))
    (define mtch (regexp-match #rx"(^.*/)[^/]*$" (send tf get-value)))
    (define prefix 
      (if mtch
          (list-ref mtch 1)
          ""))

    (send tf set-value (string-append prefix
                                      (send lb get-data item-to-act-on)
                                      "/"))
    (update-list-of-paths))
  
  (define (update-buttons)
    (cond
      [dir?
       (send ok-button enable #t)]
      [else
       (define item-to-act-on (get-item-to-act-on))
       (cond
         [item-to-act-on
          (define datum (send lb get-data item-to-act-on))
          (cond
            [(path? datum)
             (send ok-button enable #t)
             (send enter-sub-button enable #f)]
            [(string? datum)
             (send ok-button enable #f)
             (send enter-sub-button enable #t)])]
         [else
          (send ok-button enable #f)
          (send enter-sub-button enable #f)])]))
  
  (define (handle-list-box-double-click)
    (cond
      [dir?
       (ok)]
      [else
       (define item-to-act-on (get-item-to-act-on))
       (cond
         [item-to-act-on 
          (define datum (send lb get-data item-to-act-on))
          (cond
            [(path? datum)
             (ok)]
            [(string? datum)
             (enter-sub)])]
         [else
          (void)])]))
  
  (define (get-item-to-act-on)
    (or (send lb get-selection)
        (and (<= 1 (send lb get-number))
             0)))
    
  (update-list-of-paths)
  (update-different-racket-gui)
  (maybe-turn-racket-path-pink)
  (send tf focus)
  (send dlg show #t)
  (cond
    [cancelled? #f]
    [dir?
     (define selections (send lb get-selections))
     (for/list ([i (if (null? selections)
                       (in-range (send lb get-number))
                       (in-list selections))])
       (send lb get-data i))]
    [else (send lb get-data (get-item-to-act-on))]))

(define racket-binary-pref 'drracket:different-racket-for-open-collection-path)
(preferences:set-default racket-binary-pref (list #f "") (list/c boolean? string?))


;; the thread always holds the value of the clcp/clcf
;; for (list-ref (preferences:get racket-binary-pref) 1),
;; even if (list-ref (preferences:get racket-binary-pref) 0)
;; is #f (in which case, no one asks for the value inside the thread)

(define (new-alternate-racket str dlg)
  (init-alternate-racket-thread)
  (channel-put new-alternate-racket-chan (list str dlg)))
(define new-alternate-racket-chan (make-channel))

(define (get-clcl/clcp)
  (init-alternate-racket-thread)
  (channel-get current-alternate-racket-chan))
(define current-alternate-racket-chan (make-channel))

(define (init-alternate-racket-thread)
  (unless thd
    (define pref-val (preferences:get racket-binary-pref))
    (set! thd
          (thread (alternate-racket-thread-loop (list-ref pref-val 1))))))
(define thd #f)

(define (fire-off-alternate-racket-call str+dlg)
  (define new-clcl-thread-pending-chan (make-channel))
  (thread
   (λ () 
     (define-values (a b) 
       (if (path-string? (list-ref str+dlg 0))
           (alternate-racket-clcl/clcp (list-ref str+dlg 0))
           (values (current-library-collection-links)
                   (current-library-collection-paths))))
     (channel-put new-clcl-thread-pending-chan
                  (list (list a b)
                        (list-ref str+dlg 1)))))
  new-clcl-thread-pending-chan)

(define (alternate-racket-thread-loop initial-alternate-racket)
  (λ ()
    (let loop ([clcl-thread-pending-chan #f]
               
               ;; (cons/c string? (is-a?/c dialog%))
               [pending-str+dlg #f]
               [clcl/clcp (if (path-string? initial-alternate-racket)
                              (let-values ([(a b) (alternate-racket-clcl/clcp 
                                                   initial-alternate-racket)])
                                (list a b))
                              (list (current-library-collection-links)
                                    (current-library-collection-paths)))])
      (sync
       (handle-evt 
        new-alternate-racket-chan
        (λ (str+dlg)
          (cond
            [clcl-thread-pending-chan
             (loop clcl-thread-pending-chan
                   str+dlg
                   clcl/clcp)]
            [else
             (define new-clcl-thread-pending-chan 
               (fire-off-alternate-racket-call str+dlg))
             (loop new-clcl-thread-pending-chan
                   #f
                   clcl/clcp)])))
       (handle-evt 
        (channel-put-evt current-alternate-racket-chan clcl/clcp)
        (λ (c)
          (loop clcl-thread-pending-chan 
                pending-str+dlg
                clcl/clcp)))
       (if clcl-thread-pending-chan
           (handle-evt
            clcl-thread-pending-chan
            (λ (new-clcl/clcp+dlg)
              (cond
                [pending-str+dlg
                 (loop (fire-off-alternate-racket-call pending-str+dlg)
                       #f
                       clcl/clcp)]
                [else
                 (define new-clcl/clcp (list-ref new-clcl/clcp+dlg 0))
                 (define dlg (list-ref new-clcl/clcp+dlg 1))
                 (parameterize ([current-eventspace (send dlg get-eventspace)])
                   (queue-callback
                    (λ ()
                      (send dlg new-clcl/clcp new-clcl/clcp))))
                 (loop #f
                       #f  
                       new-clcl/clcp)])))
           never-evt)))))

(preferences:set-default 'drracket:get-module-path-from-user-size 
                         (list 600 600)
                         (list/c exact-nonnegative-integer? exact-nonnegative-integer?))


(module+ main
  (get-module-path-from-user))

#lang racket/base

(require racket/class
         racket/string
         (prefix-in wx: "kernel.rkt")
         "const.rkt"
         "check.rkt"
         "helper.rkt"
         "editor.rkt"
         "mrtop.rkt"
         "mrcanvas.rkt"
         "mrpopup.rkt"
         "mrmenu.rkt"
         "mritem.rkt"
         "mrpanel.rkt")

(provide message-box
         message-box/custom
         message+check-box
         message+check-box/custom)

(define do-message-box/custom
  (lambda (who title message
               button1 button2 button3
               parent style close-result
               check? two-results? check-message
               dialog-mixin)
    (check-label-string who title)
    (check-string/false who message)
    (when check?
      (check-label-string who check-message))
    (check-label-string-or-bitmap/false who button1)
    (check-label-string-or-bitmap/false who button2)
    (check-label-string-or-bitmap/false who button3)
    (check-top-level-parent/false who parent)
    (check-style who
                 '(default=1 default=2 default=3 no-default)
                 (let ([l '(disallow-close number-order caution stop no-icon)])
                   (if check?
                       (cons 'checked l)
                       l))
                 style)

    (let ([go (lambda ()
                (create-message-box/custom
                 who title message
                 button1 button2 button3
                 parent style close-result
                 check? two-results? check-message
                 dialog-mixin))]
          [es (if parent
                  (send parent get-eventspace)
                  (wx:current-eventspace))])
      (if (eq? (current-thread) (wx:eventspace-handler-thread es))
          ;; In the right thread:
          (go)
          ;; Not in the right thread:
          (let ([ch (make-channel)])
            (parameterize ([wx:current-eventspace es])
              (wx:queue-callback
               (lambda ()
                 (channel-put ch (call-with-values go list)))))
            (apply values (channel-get ch)))))))

(define create-message-box/custom
  (lambda (who title message
               button1 button2 button3
               parent style close-result
               check? two-results? check-message
               dialog-mixin)
    (let* ([strings (regexp-split #rx"\n" message)]
           [single? (and (< (length strings) 10)
                         (andmap (lambda (s) (< (string-length s) 60)) strings))]
           [f (make-object (dialog-mixin
                            (class dialog%
                              (public*
                               [get-message
                                (lambda () message)])
                              (augment*
                               [can-close? (lambda ()
                                             (if (memq 'disallow-close style)
                                                 (begin
                                                   (wx:bell)
                                                   #f)
                                                 #t))])
                              (override*
                               [on-subwindow-event
                                (lambda (w e)
                                  (if (send e button-down?)
                                      (if (is-a? w button%)
                                          #f
                                          (if (or (is-a? w message%)
                                                  (and
                                                   (is-a? w editor-canvas%)
                                                   (let-values ([(w h) (send w get-client-size)])
                                                     (< (send e get-x) w))))
                                              (begin
                                                (send w popup-menu
                                                      (let ([m (make-object popup-menu%)])
                                                        (make-object menu-item%
                                                                     "Copy Message"
                                                                     m
                                                                     (lambda (i e)
                                                                       (send (wx:get-the-clipboard)
                                                                             set-clipboard-string
                                                                             message
                                                                             (send e get-time-stamp))))
                                                        m)
                                                      (send e get-x)
                                                      (send e get-y))
                                                #t)
                                              #f))
                                      #f))])
                              (super-make-object title parent box-width))))]
           [result close-result]
           [icon-id (cond
                     [(memq 'no-icon style) #f]
                     [(memq 'stop style) 'stop]
                     [(memq 'caution style) 'caution]
                     [else 'app])])
      (let-values ([(msg-pnl btn-pnl cb-pnl extra-width btn-h-align msg-h-align msg-v-align)
                    (case (system-type)
                      [(macosx) (let ([p (make-object horizontal-pane% f)])
                                  (send f min-width 300)
                                  (send p set-alignment 'center 'top)
                                  (when icon-id
                                    (let ([m (make-object message% icon-id p)])
                                      (send m horiz-margin 16)
                                      (send m vert-margin 16)))
                                  (let* ([rhs-pnl (make-object vertical-pane% p)]
                                         [msg-pnl (make-object vertical-pane% rhs-pnl)]
                                         [btn-pnl (make-object vertical-pane% rhs-pnl)])
                                    (send msg-pnl vert-margin 16)
                                    (when single?
                                      (send msg-pnl horiz-margin 8))
                                    (send btn-pnl vert-margin 8)
                                    (send msg-pnl min-height 40)
                                    (send msg-pnl min-width 300)
                                    (send btn-pnl stretchable-height #f)
                                    (values msg-pnl btn-pnl btn-pnl 96 'right 'left 'top)))]
                      [else (let ([p (new horizontal-pane% [parent f] [alignment '(center top)])])
                              (let ([icon-msg (and icon-id (make-object message% icon-id p))]
                                    [msg-pnl (new vertical-pane% [parent p])])
                                (values (if (= 1 (length strings))
                                            (new horizontal-pane%
                                                 [parent msg-pnl]
                                                 [alignment '(center top)]
                                                 [min-height (if icon-msg
                                                                 (send icon-msg min-height)
                                                                 1)])
                                            msg-pnl)
                                        f msg-pnl 0 'center 'center 'center)))])])
        (if single?
            (begin
              (send msg-pnl set-alignment (if (= (length strings) 1) msg-h-align 'left) msg-v-align)
              (for-each (lambda (s) (make-object message% (protect& s) msg-pnl)) strings)
              (send f stretchable-width #f)
              (send f stretchable-height #f))
            ;; Try without scrollbar, then add one if necessary:
            (let loop ([scroll? #f])
              (let* ([e (make-object text%)]
                     [c (make-object editor-canvas% msg-pnl e (if scroll?
                                                                  '(no-hscroll)
                                                                  '(no-hscroll no-vscroll transparent no-border)))])
                (send c min-width 400)
                (send c set-line-count 5)
                (send c allow-tab-exit #t)
                (send f reflow-container)
                (send e auto-wrap #t)
                (send e insert message)
                (send e set-position 0)
                (send e hide-caret #t)
                (send e set-cursor (make-object wx:cursor% 'arrow) #t)
                (send e lock #t)
                (when (not scroll?)
                  ;; Check whether it actually fits
                  (let ([vh (box 0)]
                        [eh (box 0)])
                    (send e get-view-size #f vh)
                    (send e get-extent #f eh)
                    (unless ((unbox eh) . <= . (unbox vh))
                      (send c show #f)
                      (send msg-pnl delete-child c)
                      (loop #t)))))))
        (let ([check (and check?
                          (let ([p (new vertical-pane% [parent cb-pnl]
                                        [stretchable-height #f]
                                        [alignment '(left center)])])
                            (when (and single?
                                       (eq? 'macosx (system-type)))
                              ;; Match text-panel margin:
                              (send p horiz-margin 8))
                            (new check-box%
                                 [label check-message]
                                 [parent p]
                                 [callback void]
                                 [value (memq 'checked style)])))])
          (let* ([p (make-object horizontal-pane% btn-pnl)]
                 [mk-button (lambda (title v default?)
                              (let ([b (make-object button% title p (lambda (b e) (set! result v) (send f show #f))
                                                    (if default? '(border) null))])
                                (when default? (send b focus))))])
            (send p set-alignment btn-h-align 'center)
            (send p stretchable-height #f)
            (send p stretchable-width #t) ; to get panel's centering
            (let ([mk-1 (lambda ()
                          (when button1
                            (mk-button button1 1 (memq 'default=1 style))))]
                  [mk-2 (lambda ()
                          (when button2
                            (mk-button button2 2 (memq 'default=2 style))))]
                  [mk-3 (lambda ()
                          (when button3
                            (mk-button button3 3 (memq 'default=3 style))))])
              (cond
               [(or (memq 'number-order style)
                    (memq (system-type) '(windows)))
                (mk-1)
                (mk-2)
                (mk-3)]
               [else
                (mk-3)
                (make-object horizontal-pane% p)
                (mk-2)
                (mk-1)])))
          (send f center)
          (send f show #t)
          (if two-results?
              (values result (and check? (send check get-value)))
              result))))))

(define message-box/custom
  (lambda (title message
                 button1
                 button2
                 button3
                 [parent #f]
                 [style '(no-default)]
                 [close-result #f]
                 #:dialog-mixin [dialog-mixin values])
    (do-message-box/custom 'message-box/custom
                           title message button1 button2 button3
                           parent style close-result
                           #f #f #f dialog-mixin)))

(define do-message-box
  (lambda (who title message parent style check? check-message dialog-mixin)
    (check-label-string who title)
    (check-string/false who message)
    (when check?
      (check-label-string who check-message))
    (check-top-level-parent/false who parent)
    (check-style who
                 '(ok ok-cancel yes-no)
                 (let ([l '(caution stop no-icon)])
                   (if check?
                       (cons 'checked l)
                       l))
                 style)

    (let-values ([(one two one-v two-v close-val default)
                  (cond
                   [(memq 'ok style)
                    (values "OK" #f 'ok #f 1 'default=1)]
                   [(memq 'ok-cancel style)
                    (values "OK" "Cancel" 'ok 'cancel 2 'default=1)]
                   [(memq 'yes-no style)
                    (values "&Yes" "&No" 'yes 'no #f 'no-default)])])
      (let-values ([(result checked?)
                    (do-message-box/custom who
                                           title message
                                           one two #f
                                           parent
                                           (append
                                            (cond
                                             [(memq 'checked style) '(checked)]
                                             [else null])
                                            (cond
                                             [(memq 'no-icon style) '(no-icon)]
                                             [(memq 'stop style) '(stop)]
                                             [(memq 'caution style) '(caution)]
                                             [else null])
                                            (if close-val
                                                (list default)
                                                (list default 'disallow-close)))
                                           close-val
                                           check? #t check-message
                                           dialog-mixin)])
        (let ([result (case result
                        [(1) one-v]
                        [(2) two-v])])
          (if check?
              (values result checked?)
              result))))))

(define message-box
  (lambda (title message [parent #f] [style '(ok)] #:dialog-mixin [dialog-mixin values])
    (do-message-box 'message-box title message parent style #f #f dialog-mixin)))

(define message+check-box/custom
  (lambda (title message
                 checkbox-message
                 button1
                 button2
                 button3
                 [parent #f]
                 [style '(no-default)]
                 [close-result #f]
                 #:dialog-mixin [dialog-mixin values])
    (do-message-box/custom 'message+check-box/custom
                           title message button1 button2 button3
                           parent style close-result
                           #t #t checkbox-message
                           dialog-mixin)))

(define message+check-box
  (lambda (title message check-message [parent #f] [style '(ok)] #:dialog-mixin [dialog-mixin values])
    (do-message-box 'message-box title message parent style #t check-message dialog-mixin)))

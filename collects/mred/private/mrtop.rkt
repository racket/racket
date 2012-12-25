#lang racket/base

(require racket/class
         racket/list
         (prefix-in wx: "kernel.rkt")
         "lock.rkt"
         "helper.rkt"
         "const.rkt"
         "check.rkt"
         "wx.rkt"
         "wxtop.rkt"
         "wxpanel.rkt"
         "wxitem.rkt"
         "mrwindow.rkt"
         "mrcontainer.rkt"
         "app.rkt")

(provide top-level-window<%>
         frame%
         dialog%
         (protect-out root-menu-frame)
         get-top-level-windows
         get-top-level-focus-window
         get-top-level-edit-target-window
         send-message-to-window
         (protect-out check-top-level-parent/false
                      check-frame-parent/false))

(define top-level-window<%>
  (interface (area-container-window<%>)
    get-eventspace
    on-activate on-traverse-char on-system-menu-char
    can-close? on-close
    can-exit? on-exit
    get-focus-window get-edit-target-window
    get-focus-object get-edit-target-object
    center move resize
    on-message
    display-changed))

(define-local-member-name
  do-create-status-line
  do-set-status-text)

(define basic-top-level-window%
  (class* (make-area-container-window% (make-window% #t (make-container% area%))) (top-level-window<%>)
    (init mk-wx mismatches label parent)
    (init-rest)
    (inherit show)
    (rename-super [super-set-label set-label])
    (private*
     [wx-object->proxy
      (lambda (o)
        (if (is-a? o wx:window%)
            (wx->proxy o)
            o))])
    (override*
     [set-label (entry-point
                 (lambda (l)
                   (check-label-string/false '(method top-level-window<%> set-label) l)
                   (send wx set-title (or l ""))
                   (super-set-label l)))])
    (public*
     [on-traverse-char (entry-point
                        (lambda (e)
                          (check-instance '(method top-level-window<%> on-traverse-char)
                                          wx:key-event% 'key-event% #f e)
                          (send wx handle-traverse-key e)))]
     [on-system-menu-char (entry-point
                           (lambda (e)
                             (check-instance '(method top-level-window<%> on-system-menu-char)
                                             wx:key-event% 'key-event% #f e)
                             (and (eq? #\space (send e get-key-code))
                                  (send e get-meta-down)
                                  (eq? 'windows (system-type))
                                  (send wx system-menu) #t)))]
     [get-eventspace (entry-point (lambda () (send wx get-eventspace)))])
    (pubment*
     [can-close? (lambda () (inner #t can-close?))]
     [on-close (lambda () (inner (void) on-close))]
     [display-changed (λ () (inner (void) display-changed))])
    (public*
     [can-exit? (lambda () (can-close?))]
     [on-exit (lambda () (on-close) (show #f))]
     [on-activate (lambda (x) (void))]
     [set-icon (case-lambda
                 [(i) (send wx set-icon i)]
                 [(i b) (send wx set-icon i b)]
                 [(i b l?) (send wx set-icon i b l?)])]
     [center (entry-point
              (case-lambda
                [() (send wx center 'both)]
                [(dir) (send wx center dir)]))]
     [move (entry-point
            (lambda (x y)
              (check-slider-integer '(method top-level-window<%> move) x)
              (check-slider-integer '(method top-level-window<%> move) y)
              (send wx move x y)))]
     [resize (entry-point
              (lambda (w h)
                (check-range-integer '(method top-level-window<%> resize) w)
                (check-range-integer '(method top-level-window<%> resize) h)
                (send wx set-size -11111 -11111 w h)))]

     [get-focus-window (entry-point
                        (lambda () (let ([w (send wx get-focus-window)])
                                     (and w (wx->proxy w)))))]
     [get-edit-target-window (entry-point
                              (lambda () (let ([w (send wx get-edit-target-window)])
                                           (and w (wx->proxy w)))))]
     [get-focus-object (entry-point
                        (lambda () (let ([o (send wx get-focus-object)])
                                     (and o (wx-object->proxy o)))))]
     [get-edit-target-object (entry-point
                              (lambda () (let ([o (send wx get-edit-target-object)])
                                           (and o (wx-object->proxy o)))))]

     [on-message (lambda (m) (void))])
    (define wx #f)
    (define mid-panel #f) ;; supports status line
    (define wx-panel #f)
    (define status-message #f)
    (define finish (entry-point
                    (lambda (top-level hide-panel?)
                      (set! mid-panel (make-object wx-vertical-panel% #f this top-level null #f))
                      (send mid-panel skip-subwindow-events? #t)
		      (send mid-panel skip-enter-leave-events #t)
                      (send (send mid-panel area-parent) add-child mid-panel)
                      (set! wx-panel (make-object wx-vertical-panel% #f this mid-panel null #f))
                      (send wx-panel skip-subwindow-events? #t)
		      (send wx-panel skip-enter-leave-events #t)
                      (send (send wx-panel area-parent) add-child wx-panel)
                      (send top-level set-container wx-panel)
                      (when hide-panel?
                        (send mid-panel show #f))
                      top-level)))
    (public*
     [do-create-status-line (lambda ()
                              (unless status-message
                                (set! status-message (make-object wx-message% this this mid-panel "" -1 -1 null #f))
                                (send status-message stretchable-in-x #t)))]
     [do-set-status-text (lambda (s)
                           (when status-message
                             (send status-message set-label s)))])
    (override*
     [get-client-handle (lambda () (send wx-panel get-client-handle))])
    (super-make-object (lambda () (set! wx (mk-wx finish)) wx)
                       (lambda () wx-panel) (lambda () mid-panel)
                       mismatches label parent arrow-cursor)))


(define frame%
  (class basic-top-level-window%
    (init label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null]
          ;; for inherited keywords
          [enabled #t]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (inherit on-traverse-char on-system-menu-char
             do-create-status-line do-set-status-text)
    (let ([cwho '(constructor frame)])
      (check-label-string cwho label)
      (check-frame-parent/false cwho parent)
      (check-dimension cwho width)
      (check-dimension cwho height)
      (check-init-pos-integer cwho x)
      (check-init-pos-integer cwho y)
      (check-style cwho #f '(no-resize-border no-caption no-system-menu
                                              toolbar-button hide-menu-bar float metal)
                   style))
    (rename-super [super-on-subwindow-char on-subwindow-char])
    (define wx #f)
    (define status-line? #f)
    (define modified? #f)
    (override*
     [on-subwindow-char (lambda (w event)
                          (super-on-subwindow-char w event)
                          (or (on-menu-char event)
                              (on-system-menu-char event)
                              (on-traverse-char event)))])
    (public*
     [on-menu-char (entry-point
                    (lambda (e)
                      (check-instance '(method frame% on-menu-char) wx:key-event% 'key-event% #f e)
                      (send wx handle-menu-key e)))]
     [on-toolbar-button-click (lambda () (void))]
     [create-status-line (entry-point (lambda () (unless status-line? (do-create-status-line) (set! status-line? #t))))]
     [set-status-text (lambda (s) (do-set-status-text s))]
     [has-status-line? (lambda () status-line?)]
     [iconize (entry-point (lambda (on?) (send wx iconize on?)))]
     [is-iconized? (entry-point (lambda () (send wx iconized?)))]
     [maximize (entry-point (lambda (on?) (send wx position-for-initial-show) (send wx maximize on?)))]
     [is-maximized? (entry-point (lambda () (send wx is-maximized?)))]
     [get-menu-bar (entry-point (lambda () (let ([mb (send wx get-the-menu-bar)])
                                             (and mb (wx->mred mb)))))]
     [modified (entry-point
                (case-lambda
                  [() modified?]
                  [(m)
                   (set! modified? m)
                   (send wx set-modified m)]))])
    (as-entry
     (lambda ()
       (super-new
        [mk-wx
         (lambda (finish)
           (set! wx (finish (make-object wx-frame% this this
                                         (and parent (mred->wx parent)) label
                                         (or x -11111) (or y -11111)
                                         (or width -1) (or height -1)
                                         style)
                            #f))
           (send wx set-mdi-parent #f)
           wx)]
        [mismatches
         (lambda ()
           (let ([cwho '(constructor frame)])
             (check-container-ready cwho parent)))]
        [label label]
        [parent parent]
        ;; for inherited inits
        [enabled enabled]
        [border border]
        [spacing spacing]
        [alignment alignment]
        [min-width min-width]
        [min-height min-height]
        [stretchable-width stretchable-width]
        [stretchable-height stretchable-height])))))

(define dialog%
  (class basic-top-level-window%
    (init label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null]
          ;; for inherited keywords
          [enabled #t]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (inherit on-traverse-char on-system-menu-char center)
    (let ([cwho '(constructor dialog)])
      (check-label-string cwho label)
      (check-top-level-parent/false cwho parent)
      (for-each (lambda (x) (check-dimension cwho x)) (list width height x y))
      (check-style cwho #f '(no-caption resize-border no-sheet close-button) style))
    (rename-super [super-on-subwindow-char on-subwindow-char])
    (define wx #f)
    (public*
     [show-without-yield (lambda ()
                           (as-entry
                            (lambda ()
                              (send wx call-show #t (lambda () (send wx show-without-yield))))))])
    (override*
     [on-subwindow-char (lambda (w event)
                          (super-on-subwindow-char w event)
                          (or (on-system-menu-char event)
                              (on-traverse-char event)))])
    (as-entry
     (lambda ()
       (super-new
        [mk-wx
         (lambda (finish)
           (set! wx (finish (make-object wx-dialog% this this
                                         (and parent (mred->wx parent)) label
                                         (or x -11111) (or y -11111) (or width 0) (or height 0)
                                         style)
                            #f))
           wx)]
        [mismatches
         (lambda ()
           (let ([cwho '(constructor dialog)])
             (check-container-ready cwho parent)))]
        [label label]
        [parent parent]
        ;; for inherited inits
        [enabled enabled]
        [border border]
        [spacing spacing]
        [alignment alignment]
        [min-width min-width]
        [min-height min-height]
        [stretchable-width stretchable-width]
        [stretchable-height stretchable-height])))))

(define (get-top-level-windows)
  (remq root-menu-frame (map wx->mred (wx:get-top-level-windows))))

(define (get-top-level-focus-window)
  (ormap (lambda (f) (and (send f is-act-on?)
                          (let ([f (wx->mred f)])
                            (and f
                                 (not (eq? f root-menu-frame))
                                 f))))
         (wx:get-top-level-windows)))

(define (get-top-level-edit-target-window)
  (let loop ([l (wx:get-top-level-windows)][f #f][s 0][ms 0])
    (if (null? l)
        f
        (let* ([f2 (car l)]
               [f2m (wx->mred f2)]
               [s2 (send f2 get-act-date/seconds)]
               [ms2 (send f2 get-act-date/milliseconds)])
          (if (and (or (not f)
                       (> s2 s)
                       (and (= s2 s) (> ms2 ms)))
                   (not (eq? f2m root-menu-frame)))
              (loop (cdr l) f2m s2 ms2)
              (loop (cdr l) f s ms))))))

(define (send-message-to-window x y m)
  (check-slider-integer 'send-message-to-window x)
  (check-slider-integer 'send-message-to-window y)
  (let ([w (wx:location->window x y)])
    (and w (let ([f (wx->proxy w)])
             (and f
                  (not (eq? f root-menu-frame))
                  (send f on-message m))))))

(define (check-top-level-parent/false who p)
  (unless (or (not p) (is-a? p frame%) (is-a? p dialog%))
    (raise-argument-error (who->name who) "(or/c (is-a?/c frame%) (is-a?/c dialog%) #f)" p)))

(define (check-frame-parent/false who p)
  (unless (or (not p) (is-a? p frame%))
    (raise-argument-error (who->name who) "(or/c (is-a?/c frame%) #f)" p)))

(define root-menu-frame
  (and (current-eventspace-has-menu-root?)
       ;; The very first frame shown is somehow sticky under Cocoa,
       ;;  so create the root frame, show it , and hide it.
       (let* ([f (make-object (class frame%
                                (define/override (on-exit)
                                  (exit))
                                (super-make-object "Root" #f 0 0 -9000 -9000
                                                   '(no-resize-border no-caption))))]
              [wx (mred->wx f)])
         (set-root-menu-wx-frame! wx)
         (send wx designate-root-frame)
         f)))

(module wxwindow racket/base
  (require racket/class
           (prefix-in wx: "kernel.rkt")
           "te.rkt"
           "lock.rkt"
           "helper.rkt"
           "const.rkt"
           "wx.rkt")

  (provide (protect-out wx-make-window%
                        make-window-glue%))

  (define wx-make-window%
    (lambda (% top?)
      (class %
        (init-rest args)
	(inherit is-shown-to-root? is-enabled-to-root?)
        (define top-level #f)
        (define focus? #f)
        (define container this)
        (define visible? #f)
        (define active? #f)
        (define skip-sub-events? #f)
	(public*
         [on-visible
          (lambda ()
            (let ([vis? (is-shown-to-root?)])
              (unless (eq? vis? visible?)
                (set! visible? vis?)
                (unless skip-sub-events?
                  (as-exit
                   (lambda ()
                     (send (wx->proxy this) on-superwindow-show vis?)))))))]
         [queue-visible
          (lambda ()
            (parameterize ([wx:current-eventspace (send (get-top-level) get-eventspace)])
              (wx:queue-callback (entry-point (lambda () (on-visible))) wx:middle-queue-key)))]
         [skip-subwindow-events?
          (case-lambda
            [() skip-sub-events?]
            [(skip?) (set! skip-sub-events? skip?)])])
	(public*
         [get-text-extent (lambda (s wb hb db ab font)
                            (let-values ([(w h d a) (get-window-text-extent* s font #t)])
                              (let ([set (lambda (b v)
                                           (when b (set-box! b (inexact->exact (ceiling v)))))])
                                (set wb w)
                                (set hb h)
                                (set db d)
                                (set ab a))))]
         [on-active
          (lambda ()
            (let ([act? (is-enabled-to-root?)])
              (unless (eq? act? active?)
                (set! active? act?)
                (unless skip-sub-events?
                  (as-exit
                   (lambda ()
                     (send (wx->proxy this) on-superwindow-enable act?)))))))]
         [queue-active
          (lambda ()
            (parameterize ([wx:current-eventspace (send (get-top-level) get-eventspace)])
              (wx:queue-callback (entry-point (lambda () (on-active))) wx:middle-queue-key)))]

         ;; Needed for radio boxes:
         [orig-enable
          (lambda args (super-enable . args))])
	(rename-super [super-enable enable])
	
        (define can-accept-drag? #f)
        (define fake-shown? #f)

	(public*
         [accept-drag? (lambda () can-accept-drag?)]
         [get-container (lambda () container)]
         [set-container (lambda (c) (set! container c))]
         [get-window (lambda () this)]
         [tabbing-position (lambda (x y w h) (list this x y w h))]
         [has-tabbing-children? (lambda () #f)]
         [dx (lambda () 0)]
         [dy (lambda () 0)]
         [ext-dx (lambda () (dx))]
         [ext-dy (lambda () (dy))]
         [handles-key-code (lambda (x alpha? meta?) #f)]
         [char-to (lambda () (void))]
         [get-top-level
          (lambda ()
            (unless top-level
              (let loop ([window this])
                (cond
                 [(or (is-a? window wx:frame%)
                      (is-a? window wx:dialog%)) 
                  (set! top-level window)]
                 [else (loop (send window get-parent))])))
            top-level)]
         [ensure-forgotten
          (lambda ()
            ;; This value or its ancestor is going to be adopted elsewhere,
            ;; so really forget it, make sure it's hidden, etc.
            (send (get-top-level) forget-child this))])
        (public*
         [really-show
          (lambda (on?)
            (set! fake-shown? #f)
            (super show on?))]
         [fake-show
          (lambda (on?)
            (set! fake-shown? on?))])
	(override*
         [show
          (lambda (on?)
            (queue-visible)
            (send (get-top-level) show-control this on?))]
         [is-shown?
          (lambda ()
            (or fake-shown?
                (super is-shown?)))]

         [enable
          (lambda (on?)
            (queue-active)
            (super enable on?))]

         [drag-accept-files
          (lambda (on?)
            (set! can-accept-drag? (and on? #t))
            (super drag-accept-files on?))]
         [on-set-focus
          (entry-point
           (lambda ()
             (send (get-top-level) set-focus-window this)
             (set! focus? #t)
             (as-exit (lambda () (super on-set-focus)))))]
         [on-kill-focus
          (entry-point
           (lambda ()
             (send (get-top-level) set-focus-window #f)
             (set! focus? #f)
             (as-exit (lambda () (super on-kill-focus)))))])
	(public*
         [has-focus? (lambda () focus?)])
        (apply super-make-object args)
        (unless top?
          (set! visible? (is-shown-to-root?))
          (set! active? (is-enabled-to-root?))))))

  (define (make-window-glue% %)  ; implies make-glue%
    (class (make-glue% %)
      (init mred proxy)
      (init-rest  args)
      (inherit get-x get-y get-width get-height area-parent get-mred get-proxy skip-subwindow-events? get-parent)
      (define/private (pre-wx->proxy orig-w e k)
        ;; MacOS: w may not be something the user knows
        ;; Look for a parent, and shift coordinates
        (let loop ([w orig-w])
          (if w
              (if (is-a? w wx/proxy<%>)
                  (if (eq? w orig-w)
                      (k (wx->proxy w) e)
                      (let ([bx (box (send e get-x))]
                            [by (box (send e get-y))])
                        (send orig-w client-to-screen bx by)
                        (send w screen-to-client bx by)
                        (let ([new-e (if (e . is-a? . wx:key-event%)
                                         (instantiate wx:key-event% ()
                                           [key-code (send e get-key-code)])
                                         (instantiate wx:mouse-event% ()
                                           [event-type (send e get-event-type)]
                                           [left-down (send e get-left-down)]
                                           [right-down (send e get-right-down)]
                                           [middle-down (send e get-middle-down)]))])
                          (when (e . is-a? . wx:key-event%)
                            (send new-e set-key-release-code (send e get-key-release-code)))
                          (send new-e set-time-stamp (send e get-time-stamp))
                          (send new-e set-alt-down (send e get-alt-down))
                          (send new-e set-control-down (send e get-control-down))
                          (send new-e set-meta-down (send e get-meta-down))
                          (send new-e set-shift-down (send e get-shift-down))
                          (send new-e set-x (unbox bx))
                          (send new-e set-y (unbox by))
                          (k (wx->proxy w) new-e))))
                  (loop (send w get-parent)))
              #f)))
      (define old-w -1)
      (define old-h -1)
      (define old-x -1)
      (define old-y -1)
      (define expose-focus? #t)
      (public*
       [set-no-expose-focus (lambda () (set! expose-focus? #f))])
      (override*
       [on-drop-file (entry-point
                      (lambda (f)
                        (as-exit
                         (lambda ()
                           (send (get-proxy) on-drop-file f)))))]
       [queue-on-size
        (lambda ()
          (super queue-on-size)
          (queue-window-callback
           this
           (entry-point
            (lambda ()
              (let ([mred (get-mred)])
                (when mred 
                  (let* ([w (get-width)]
                         [h (get-height)])
                    (when (not (and (= w old-w) (= h old-h)))
                      (set! old-w w)
                      (set! old-h h)
                      (as-exit (lambda () (send mred on-size w h)))))
                  (let* ([p (area-parent)]
                         [x (max (- WIN-SIZE-MAX) (min WIN-SIZE-MAX (- (get-x) (or (and p (send p dx)) 0))))]
                         [y (max (- WIN-SIZE-MAX) (min WIN-SIZE-MAX (- (get-y) (or (and p (send p dy)) 0))))])
                    (when (not (and (= x old-x) (= y old-y)))
                      (set! old-x x)
                      (set! old-y y)
                      (as-exit (lambda () (send mred on-move x y)))))))))))]
       [on-set-focus (lambda () 
                       (super on-set-focus)
                       (when expose-focus? 
                         (let ([p (get-proxy)])
                           (send p on-focus #t)
                           (on-subwindow-focus p #t))))]
       [on-kill-focus (lambda () 
                        (super on-kill-focus)
                        (when expose-focus? 
                          (let ([p (get-proxy)])
                            (send p on-focus #f)
                            (on-subwindow-focus p #f))))]
       [pre-on-char (lambda (w e)
                      (or (super pre-on-char w e)
                          (if (skip-subwindow-events?)
                              #f
                              (as-entry
                               (lambda ()
                                 (pre-wx->proxy w e
                                                (lambda (m e)
                                                  (as-exit (lambda () 
                                                             (send (get-proxy) on-subwindow-char m e))))))))))]
       [pre-on-event (entry-point
                      (lambda (w e)
                        (if (skip-subwindow-events?)
			    #f
                            (pre-wx->proxy w e
                                           (lambda (m e) 
                                             (as-exit (lambda () 
                                                        (send (get-proxy) on-subwindow-event m e))))))))])
      (public*
       [on-subwindow-focus (lambda (win on?)
                             (unless (or (is-a? this wx:frame%)
                                         (is-a? this wx:dialog%))
                               (send (get-parent) on-subwindow-focus win on?))
                             (unless (skip-subwindow-events?)
                               (send (get-proxy) on-subwindow-focus win on?)))])
      (apply super-make-object mred proxy args))))

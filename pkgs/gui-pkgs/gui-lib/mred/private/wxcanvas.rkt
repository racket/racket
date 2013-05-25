(module wxcanvas racket/base
  (require racket/class
           (prefix-in wx: "kernel.rkt")
           (prefix-in wx: "wxme/text.rkt")
           (prefix-in wx: "wxme/editor-canvas.rkt")
           "lock.rkt"
           "helper.rkt"
           "wx.rkt"
           "wxwindow.rkt"
           "wxitem.rkt")

  (provide (protect-out make-canvas-glue%
                        wx-canvas%
                        wx-editor-canvas%))

  (define (make-canvas-glue% default-tabable? %) ; implies make-window-glue%
    (class (make-window-glue% %)
      (init mred proxy)
      (init-rest args)
      (inherit get-mred get-top-level clear-margins)
      (public*
       [do-on-char (lambda (e) (super on-char e))]
       [do-on-event (lambda (e) (super on-event e))]
       [do-on-scroll (lambda (e) (super on-scroll e))]
       [do-on-paint (lambda () (super on-paint))])
      (define tabable? default-tabable?)
      (define on-popup-callback void)
      (public*
       [get-tab-focus (lambda () tabable?)]
       [set-tab-focus (lambda (v) (set! tabable? v))]
       [on-tab-in (lambda () 
                    (let ([mred (wx->mred this)])
                      (when mred
                        (send mred on-tab-in))))]
       [set-on-popup (lambda (proc) (set! on-popup-callback proc))])
      (override*
       [gets-focus? (lambda () tabable?)]
       [handles-key-code
        (lambda (code alpha? meta?)
          (if default-tabable?
              (super handles-key-code code alpha? meta?)
              (or meta? (not tabable?))))])
      (define clear-and-on-paint
        (lambda (mred)
          (clear-margins)
          (send mred on-paint)))
      (override*
       [on-char (entry-point
                 (lambda (e)
                   (let ([mred (get-mred)])
                     (if mred
                         (as-exit (lambda () (send mred on-char e)))
                         (super on-char e)))))]
       [on-event (entry-point
                  (lambda (e)
                    (let ([mred (get-mred)])
                      (if mred
                          (as-exit (lambda () (send mred on-event e)))
                          (as-exit (lambda () (super on-event e)))))))]
       ;; only called for canvas%, not editor-canvas%:
       [on-scroll (entry-point
                   (lambda (e)
                     (let ([mred (get-mred)])
                       (if mred
                           (send mred on-scroll e)
                           (as-exit (lambda () (super on-scroll e)))))))]
       [on-paint (entry-point
                  (lambda ()
                    (let ([mred (get-mred)])
                      (if mred
                          (as-exit (lambda () (clear-and-on-paint mred)))
                          (as-exit (lambda () (clear-margins) (super on-paint)))))))]
       ;; for 'combo canvases:
       [on-popup (lambda () (on-popup-callback))])
      (apply super-make-object mred proxy args)))

  (define wx-canvas% 
    (make-canvas-glue%
     #f
     (class (make-control% wx:canvas% 0 0 #t #t)
       (init parent x y w h style gl-config)
       (inherit get-top-level)
       (public*
        [clear-margins (lambda () (void))])
       (super-make-object style parent x y w h (cons 'deleted style) "canvas" gl-config)
       (unless (memq 'deleted style)
         (send (get-top-level) show-control this #t)))))

  (define (make-editor-canvas% %)
    (class %
      (init parent x y w h name style spp init-buffer)
      (inherit get-editor force-redraw
	       call-as-primary-owner min-height get-size
	       get-hard-minimum-size set-min-height
               get-top-level)
      (define fixed-height? #f)
      (define fixed-height-lines 0)
      (define orig-hard #f)
      (define single-line-canvas? #f)
      (define tabable? #f)
      (override*
       [on-container-resize (lambda ()
                              (let ([edit (get-editor)])
                                (when edit
                                  (as-exit (lambda () (send edit on-display-size-when-ready))))))]
       [on-scroll-on-change (lambda ()
                              (queue-window-callback
                               this
                               (lambda ()
                                 (let ([edit (get-editor)])
                                   (when edit
                                     (send edit on-display-size-when-ready))))))]
       [on-set-focus
        (entry-point
         (lambda ()
           (as-exit (lambda () (super on-set-focus)))
           (let ([m (get-editor)])
             (when m 
               (let ([mred (wx->mred this)])
                 (when mred
                   (as-exit (lambda () (send m set-active-canvas mred)))))))))]
       [set-editor
        (letrec ([l (case-lambda
		      [(edit) (l edit #t)]
		      [(edit redraw?)
		       (let ([old-edit (get-editor)])
                         ;; An exception here means we end up in a bad state:
			 (as-exit (lambda () 
                                    ;; set-editor can invoke callbacks:
                                    (super set-editor edit redraw?)))
			 
			 (let ([mred (wx->mred this)])
			   (when mred
			     (when old-edit
			       (as-exit
				(lambda () (send old-edit remove-canvas mred))))
			     (when edit
			       (as-exit
				(lambda () (send edit add-canvas mred))))))

			 (update-size)
			 
			 ;; force-redraw causes on-container-resize to be called,
			 ;;  but only when the size of the canvas really matters
			 ;;  (i.e., when it is shown)
			 (force-redraw))])])
          l)]
       [handles-key-code 
        (lambda (x alpha? meta?)
          (case x
            [(#\tab #\return escape) (and (not tabable?)
                                          (not single-line-canvas?))]
            [else (not meta?)]))]


       [popup-for-editor (entry-point
                          (lambda (e m)
                            (let ([mwx (mred->wx m)])
                              (and (send mwx popup-grab e)
                                   (as-exit (lambda () (send m on-demand) #t))
                                   mwx))))])
      (public*
       [set-tabable (lambda (on?) (set! tabable? on?))]
       [is-tabable? (lambda () tabable?)]
       [set-single-line (lambda () (set! single-line-canvas? #t))]
       [is-single-line? (lambda () single-line-canvas?)]
       [set-line-count (lambda (n)
                         (if n
                             (begin
                               (unless orig-hard
                                 (let-values ([(hmw hmh) (get-hard-minimum-size)])
                                   (set! orig-hard hmh)))
                               (set! fixed-height? #t)
                               (set! fixed-height-lines n))
                             (when orig-hard
                               (set! fixed-height? #f)
                               (set-min-height orig-hard)))
                         (update-size))]
       [get-line-count (lambda () (and fixed-height? fixed-height-lines))]
       [update-size
        (lambda ()
          (let ([edit (get-editor)])
            (when (and edit fixed-height?)
              (let* ([top (if (is-a? edit wx:text%)
                              (send edit line-location 0 #t)
                              0)]
                     [bottom (if (is-a? edit wx:text%)
                                 (send edit line-location 0 #f)
                                 14)]
                     [height (- bottom top)])
                (let* ([ch (box 0)]
                       [h (box 0)])
                  (call-as-primary-owner
                   (lambda ()
                     (send (send edit get-admin) 
                           get-view #f #f #f ch)))
                  (get-size (box 0) h)
                  (let ([new-min-height (+ (* fixed-height-lines height) 
                                           (- (unbox h) (unbox ch)))])
                    (set-min-height (inexact->exact (round new-min-height)))
                    (force-redraw)))))))])
      (override*
       [set-y-margin (lambda (m)
                       (super set-y-margin m)
                       (when fixed-height? (update-size)))])
      
      (super-make-object style parent x y w h (or name "") (cons 'deleted style) spp init-buffer)
      (unless (memq 'deleted style)
        (send (get-top-level) show-control this #t))
      (when init-buffer
        (let ([mred (wx->mred this)])
          (when mred
            (as-exit (lambda () (send init-buffer add-canvas mred))))))))

  (define wx-editor-canvas% 
    (class (make-canvas-glue%
            #t
            (make-editor-canvas% (make-control% wx:editor-canvas%
                                                0 0 #t #t)))
      (inherit editor-canvas-on-scroll
               set-no-expose-focus)
      (define/override (on-scroll e)
        (editor-canvas-on-scroll))
      (super-new)
      #;(set-no-expose-focus))))

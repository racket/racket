(module gdi mzscheme
  (require mzlib/class
	   mzlib/class100
           mzlib/list
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "check.ss"
	   "wx.ss"
           "te.rkt"
	   "mrtop.ss"
	   "mrcanvas.ss"
           "syntax.rkt")

  (provide register-collecting-blit
	   unregister-collecting-blit
	   printer-dc%
	   get-window-text-extent
	   normal-control-font
	   small-control-font
	   tiny-control-font
	   view-control-font
	   menu-control-font)

  (define register-collecting-blit
    (case-lambda
     [(canvas x y w h on off) (register-collecting-blit canvas x y w h on off 0 0 0 0)]
     [(canvas x y w h on off on-x) (register-collecting-blit canvas x y w h on off on-x 0 0 0)]
     [(canvas x y w h on off on-x on-y) (register-collecting-blit canvas x y w h on off on-x on-y 0 0)]
     [(canvas x y w h on off on-x on-y off-x) (register-collecting-blit canvas x y w h on off on-x on-y off-x 0)]
     [(canvas x y w h on off on-x on-y off-x off-y)
      (check-instance 'register-collecting-blit canvas% 'canvas% #f canvas)
      ((check-bounded-integer -10000 10000 #f) 'register-collecting-blit x)
      ((check-bounded-integer -10000 10000 #f) 'register-collecting-blit y)
      ((check-bounded-integer 0 10000 #f) 'register-collecting-blit w)
      ((check-bounded-integer 0 10000 #f) 'register-collecting-blit h)
      (check-instance 'register-collecting-blit wx:bitmap% 'bitmap% #f on)
      (check-instance 'register-collecting-blit wx:bitmap% 'bitmap% #f off)
      ((check-bounded-integer -10000 10000 #f) 'register-collecting-blit on-x)
      ((check-bounded-integer -10000 10000 #f) 'register-collecting-blit on-y)
      ((check-bounded-integer -10000 10000 #f) 'register-collecting-blit off-x)
      ((check-bounded-integer -10000 10000 #f) 'register-collecting-blit off-y)
      (wx:register-collecting-blit (mred->wx canvas) x y w h on off on-x on-y off-x off-y)]))

  (define unregister-collecting-blit
    (lambda (canvas)
      (check-instance 'unregister-collecting-blit canvas% 'canvas% #f canvas)
      (wx:unregister-collecting-blit (mred->wx canvas))))

  (define-syntax check-page-active
    (syntax-rules ()
      [(_ check-page-status (id . args) ...) (begin (check-one-page-active check-page-status id args) ...)]))
  (define-syntax check-one-page-active
    (syntax-rules ()
      [(_ check-page-status id simple ... (arg ... [opt ...]))
       (check-one-page-active 
        check-page-status id 
        simple ...
        (arg ...)
        (arg ... opt ...))]
      [(_ check-page-status id (arg ...) ...)
       (define/override id
         (case-lambda
          [(arg ...) (check-page-status 'id) (super id arg ...)]
          ...))]))

  (define-local-member-name multiple-pages-ok?)

  (define (doc+page-check-mixin % class-name)
    (class %
      (define status #f)
      (define did-one-page? #f)

      (define/public (multiple-pages-ok?) #t)

      (define/override (start-doc s)
        (when status
          (raise-mismatch-error (who->name (list 'method class-name 'start-doc))
                                (case status
                                  [(done)
                                   "document has already been ended: "]
                                  [else
                                   "document has already been started: "])
                                this))
        (set! status 'doc)
        (super start-doc s))

      (define/override (end-doc)
        (unless (eq? status 'doc)
          (raise-mismatch-error (who->name (list 'method class-name 'end-doc))
                                (case status
                                  [(page)
                                   "current page has not been ended: "]
                                  [(done)
                                   "document is already ended: "]
                                  [(#f)
                                   "document is not started: "])
                                this))
        (set! status 'done)
        (super end-doc))

      (define/override (start-page)
        (unless (eq? status 'doc)
          (raise-mismatch-error (who->name (list 'method class-name 'start-page))
                                (if (eq? status 'page)
                                    "current page has not been ended: "
                                    "document is not started (use the `start-doc' method): ")
                                this))
        (when did-one-page?
          (unless (multiple-pages-ok?)
            (raise-mismatch-error (who->name (list 'method class-name 'start-page))
                                  "cannot create multiple pages for EPS output: "
                                  this)))
        (set! status 'page)
        (set! did-one-page? #t)
        (super start-page))

      (define/override (end-page)
        (unless (eq? status 'page)
          (raise-mismatch-error (who->name (list 'method class-name 'end-page))
                                "no page is currently started: "
                                this))
        (set! status 'doc)
        (super end-page))

      (define/private (check-page-status method-name)
        (unless (eq? status 'page)
          (raise-mismatch-error (who->name (list 'method class-name method-name))
                                "no page is currently started (use `start-doc' and `start-page' before drawing): "
                                this)))

      (check-page-active
       check-page-status 
       (draw-bitmap source dest-x dest-y [style [color [mask]]])
       (draw-bitmap-section source dest-x dest-y src-x src-y src-width src-height [style [color [mask]]])
       (set-text-foreground c)
       (set-text-background c)
       (set-brush b/c [style])
       (set-pen p/c [width style])
       (set-font f)
       (set-background c)
       (set-clipping-region r)
       (set-clipping-rect x y w h)
       (draw-polygon pts [x [y [fill]]])
       (draw-lines pts [x [y]])
       (draw-path path [x [y [fill]]])
       (draw-ellipse x y w h)
       (draw-arc x y w h s e)
       (draw-text txt x y [combine? [offset [angle]]])
       (draw-spline x1 y1 x2 y2 x3 y3)
       (draw-rounded-rectangle x y w h [r])
       (draw-rectangle x y w h)
       (draw-point x y)
       (draw-line x1 y1 x2 y2)
       (clear))

      (super-new)))

  (define printer-dc%
    (class100 (doc+page-check-mixin wx:printer-dc% 'printer-dc%) ([parent #f])
      (sequence
	(check-top-level-parent/false '(constructor printer-dc) parent)
	(as-entry
	 (lambda ()
	   (let ([p (and parent (mred->wx parent))])
	     (as-exit (lambda () (super-init p)))))))))

  (define get-window-text-extent
    (case-lambda
     [(string font)
      (get-window-text-extent string font #f)]
     [(string font combine?)
      (check-string 'get-window-text-extent string)
      (check-instance 'get-window-text-extent wx:font% 'font% #f font)
      (let-values ([(w h d a) (get-window-text-extent* string font combine?)])
        (values (inexact->exact (ceiling w)) (inexact->exact (ceiling h))))]))

  (define small-delta (case (system-type)
			[(windows) 0]
			[(macosx) 2]
			[else 1]))
  (define tiny-delta (case (system-type)
			[(windows) 1]
			[else 2]))

  (define normal-control-font (make-object wx:font% (wx:get-control-font-size) 
					   (wx:get-control-font-face) 'system
                                           'normal 'normal #f 'default
                                           (wx:get-control-font-size-in-pixels?)))
  (define small-control-font (make-object wx:font% (- (wx:get-control-font-size) small-delta) 
					  (wx:get-control-font-face) 'system
					  'normal 'normal #f 'default
					  (wx:get-control-font-size-in-pixels?)))
  (define tiny-control-font (make-object wx:font% (- (wx:get-control-font-size) tiny-delta small-delta)
					 (wx:get-control-font-face) 'system
					 'normal 'normal #f 'default
					 (wx:get-control-font-size-in-pixels?)))
  (define view-control-font (if (eq? 'macosx (system-type))
				(make-object wx:font% (- (wx:get-control-font-size) 1) 
					     (wx:get-control-font-face) 'system)
				normal-control-font))
  (define menu-control-font (if (eq? 'macosx (system-type))
				(make-object wx:font% (+ (wx:get-control-font-size) 1) 
					     (wx:get-control-font-face) 'system)
				normal-control-font)))

(module gdi mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "check.ss"
	   "wx.ss"
	   "mrtop.ss"
	   "mrcanvas.ss")

  (provide register-collecting-blit
	   unregister-collecting-blit
	   bitmap-dc%
	   post-script-dc%
	   printer-dc%
	   get-window-text-extent
	   get-family-builtin-face
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
      (wx:register-collecting-blit (mred->wx canvas) x y w h on off on-x on-y off-x off-y)]))

  (define unregister-collecting-blit
    (lambda (canvas)
      (check-instance 'unregister-collecting-blit canvas% 'canvas% #f canvas)
      (wx:unregister-collecting-blit (mred->wx canvas))))

  (define bitmap-dc%
    (class100 wx:bitmap-dc% ([bitmap #f])
      (inherit set-bitmap)
      (sequence
	(super-init)
	(when bitmap
	  (set-bitmap bitmap)))))

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

  (define post-script-dc%
    (class (doc+page-check-mixin wx:post-script-dc% 'post-script-dc%) 
      (init [interactive #t][parent #f][use-paper-bbox #f][as-eps #t])

      (check-top-level-parent/false '(constructor post-script-dc) parent)
      
      (define is-eps? (and as-eps #t))
      (define/override (multiple-pages-ok?) (not is-eps?))

      (as-entry
       (lambda ()
         (let ([p (and parent (mred->wx parent))])
           (as-exit (lambda () (super-make-object interactive p use-paper-bbox as-eps))))))))

  (define printer-dc%
    (class100 (doc+page-check-mixin wx:printer-dc% 'printer-dc%) ([parent #f])
      (sequence
	(check-top-level-parent/false '(constructor printer-dc) parent)
	(as-entry
	 (lambda ()
	   (let ([p (and parent (mred->wx parent))])
	     (as-exit (lambda () (super-init p)))))))))

  (define get-window-text-extent
    (let ([bm #f][dc #f])
      (case-lambda
       [(string font)
	(check-string 'get-window-text-extent string)
	(check-instance 'get-window-text-extent wx:font% 'font% #f font)
	(unless bm
	  (set! bm (make-object wx:bitmap% 2 2))
	  (set! dc (make-object wx:bitmap-dc%))
	  (send dc set-bitmap bm))
	(unless (send bm ok?)
	  (error 'get-window-text-extent "couldn't allocate sizing bitmap"))
	(let-values ([(w h d a) (send dc get-text-extent string font)])
	  (values (inexact->exact w) (inexact->exact h)))])))

  (define x-has-xft? 'unknown)
  (define mswin-system #f)
  (define mswin-default #f)
  (define (look-for-font name)
    (if (ormap (lambda (n) (string-ci=? name n)) (wx:get-face-list))
	name
	"MS San Serif"))

  (define (get-family-builtin-face family)
    (unless (memq family '(default decorative roman script swiss modern system symbol))
      (raise-type-error 'get-family-builtin-face "family symbol" family))
    (case (system-type)
      [(unix)
       ;; Detect Xft by looking for a font with a space in front of its name:
       (when (eq? x-has-xft? 'unknown)
	 (set! x-has-xft? (ormap (lambda (s) (regexp-match #rx"^ " s)) (wx:get-face-list))))
       (if x-has-xft?
	   (case family
	     [(system) " Sans"]
	     [(default) " Sans"]
	     [(roman) " Serif"]
	     [(decorative) " Nimbus Sans L"]
	     [(modern) " Monospace"]
	     [(swiss) " Nimbus Sans L"]
	     [(script) " URW Chancery L"]
	     [(symbol) " Standard Symbols L,Nimbus Sans L"])
	   (case family
	     [(system) "-b&h-lucida"]
	     [(default) "-b&h-lucida"]
	     [(roman) "-adobe-times"]
	     [(decorative) "-adobe-helvetica"]
	     [(modern) "-adobe-courier"]
	     [(swiss) "-b&h-lucida"]
	     [(script) "-itc-zapfchancery"]
	     [(symbol) "-adobe-symbol"]))]
      [(windows)
       (case family
	 [(system) 
	  (unless mswin-system 
	    (set! mswin-system (look-for-font "Tahoma")))
	  mswin-system]
	 [(default) 
	  (unless mswin-default 
	    (set! mswin-default (look-for-font "Microsoft Sans Serif")))
	  mswin-default]
	 [(default) "MS Sans Serif"]
	 [(roman) "Times New Roman"]
	 [(decorative) "Arial"]
	 [(modern) "Courier New"]
	 [(swiss) "Arial"]
	 [(script) "Arial"]
	 [(symbol) "Symbol"])]
      [(macos)
       (case family
	 [(system) "systemfont"]
	 [(default) "applicationfont"]
	 [(roman) "Times"]
	 [(decorative) "Geneva"]
	 [(modern) "Monaco"]
	 [(swiss) "Helvetica"]
	 [(script) "Zaph Chancery"]
	 [(symbol) "Symbol"])]
      [(macosx)
       (case family
	 [(system) "systemfont"]
	 [(default) "applicationfont"]
	 [(roman) "Times"]
	 [(decorative) "Arial"]
	 [(modern) "Courier New"]
	 [(swiss) "Helvetica"]
	 [(script) "Apple Chancery"]
	 [(symbol) "Symbol"])]))

  (define small-delta (case (system-type)
			[(windows) 0]
			[(macosx) 2]
			[else 1]))
  (define tiny-delta (case (system-type)
			[(windows) 1]
			[else 0]))

  (define normal-control-font (make-object wx:font% (wx:get-control-font-size) 'system))
  (define small-control-font (make-object wx:font% (- (wx:get-control-font-size) small-delta) 'system))
  (define tiny-control-font (make-object wx:font% (- (wx:get-control-font-size) tiny-delta small-delta) 'system))
  (define view-control-font (if (eq? 'macosx (system-type))
				(make-object wx:font% (- (wx:get-control-font-size) 1) 'system)
				normal-control-font))
  (define menu-control-font (if (eq? 'macosx (system-type))
				(make-object wx:font% (+ (wx:get-control-font-size) 1) 'system)
				normal-control-font)))

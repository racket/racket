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

  (define post-script-dc%
    (class100 wx:post-script-dc% ([interactive #t][parent #f][use-paper-bbox #f][as-eps #t])
      (sequence
	(check-top-level-parent/false '(constructor post-script-dc) parent)
	(as-entry
	 (lambda ()
	   (let ([p (and parent (mred->wx parent))])
	     (as-exit (lambda () (super-init interactive p use-paper-bbox as-eps)))))))))

  (define printer-dc%
    (class100 wx:printer-dc% ([parent #f])
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

  (define normal-control-font (make-object wx:font% (wx:get-control-font-size) 'system))
  (define small-control-font (make-object wx:font% (- (wx:get-control-font-size) small-delta) 'system))
  (define tiny-control-font (make-object wx:font% (- (wx:get-control-font-size) 2 small-delta) 'system))
  (define view-control-font (if (eq? 'macosx (system-type))
				(make-object wx:font% (- (wx:get-control-font-size) 1) 'system)
				normal-control-font))
  (define menu-control-font (if (eq? 'macosx (system-type))
				(make-object wx:font% (+ (wx:get-control-font-size) 1) 'system)
				normal-control-font)))

(module gdi mzscheme
  (require mzlib/class
           mzlib/list
           racket/draw/private/page-dc
           (prefix wx: "kernel.rkt")
           (rename "wxme/cycle.rkt" wx:set-printer-dc%! set-printer-dc%!)
           "lock.rkt"
           "check.rkt"
           "wx.rkt"
           "te.rkt"
           "mrtop.rkt"
           "mrcanvas.rkt"
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
      (define (check-real who v)
        (unless (real? v) (raise-argument-error who "real?" v)))
      (check-instance 'register-collecting-blit canvas% 'canvas% #f canvas)
      (check-position 'register-collecting-blit x)
      (check-position 'register-collecting-blit y)
      (check-dimension 'register-collecting-blit w)
      (check-dimension 'register-collecting-blit h)
      (check-instance 'register-collecting-blit wx:bitmap% 'bitmap% #f on)
      (check-instance 'register-collecting-blit wx:bitmap% 'bitmap% #f off)
      (check-real 'register-collecting-blit on-x)
      (check-real 'register-collecting-blit on-y)
      (check-real 'register-collecting-blit off-x)
      (check-real 'register-collecting-blit off-y)
      (wx:register-collecting-blit (mred->wx canvas) x y w h on off on-x on-y off-x off-y)]))

  (define unregister-collecting-blit
    (lambda (canvas)
      (check-instance 'unregister-collecting-blit canvas% 'canvas% #f canvas)
      (wx:unregister-collecting-blit (mred->wx canvas))))

  (define printer-dc%
    (class (doc+page-check-mixin (class wx:printer-dc% 
                                   (define/public (multiple-pages-ok?) #t)
                                   (super-new))
                                 'printer-dc%) 
      (init [parent #f])

      (let ([prim? (or (parent . is-a? . wx:frame%)
                       (parent . is-a? . wx:dialog%))])
        (unless prim?
          (check-top-level-parent/false '(constructor printer-dc) parent))
        (as-entry
         (lambda ()
           (let ([p (if prim?
                        parent
                        (and parent (mred->wx parent)))])
             (as-exit (lambda () (super-new [parent p])))))))))
  (wx:set-printer-dc%! printer-dc%)

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

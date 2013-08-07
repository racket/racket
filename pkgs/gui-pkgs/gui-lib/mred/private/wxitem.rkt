(module wxitem racket/base
  (require racket/class
           racket/function
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "helper.rkt"
           "const.rkt"
           "wx.rkt"
           "check.rkt"
           "wxwindow.rkt")

  (provide (protect-out make-item%
                        make-control%
                        make-simple-control%
                        wx-button%
                        wx-check-box%
                        wx-message%))

  ;; make-item%: creates items which are suitable for placing into
  ;;  containers.
  ;; input: item%: a wx:item% descendant (but see below) from which the
  ;;          new class will be derived.
  ;;        stretch-x/stretch-y: booleans which specify the default
  ;;          stretchability behavior for the new class.
  ;; returns: a class, descended from wx:item%, which is suitable for
  ;;            placing in a container.
  ;; Note: the item% parameter does not necessarily HAVE to be a
  ;; descendant of wx:item%, so long as it contains the identifiers in the
  ;; inherit section below.  You will note below that I ran wx:panel%
  ;; through this function to create panel%.

  (define make-item%
    (lambda (item% x-margin-w y-margin-h stretch-x stretch-y)
      (class (wx-make-window% item% #f)
        (init window-style)
        (init-rest args)
	(inherit get-width get-height get-x get-y
		 get-parent get-client-size get-top-level)
	(define enabled? #t)
	(override*
         [enable
          (lambda (b)
            (set! enabled? (and b #t))
            (super enable b))]

         ;; set-size: caches calls to set-size to avoid unnecessary work,
         ;;           and works with windowsless panels
         ;; input: x/y: new position for object
         ;;        width/height: new size for object
         ;; returns: nothing
         ;; effect: if arguments mark a different geometry than the object's
         ;;   current geometry, passes args to super-class's set-size.
         ;;   Otherwise, does nothing.
         [set-size
          (lambda (x y width height)
	    (set! x (if (not x)
			(get-x)
			(+ x (send (area-parent) dx))))
	    (set! y (if (not y)
			(get-y)
			(+ y (send (area-parent) dy))))
            (unless (and (same-dimension? x (get-x))
                         (same-dimension? y (get-y))
                         (same-dimension? width (get-width))
                         (same-dimension? height (get-height)))
              (super set-size x y width height)))])

	(public*
         [is-enabled? (lambda () enabled?)])

	;; Store minimum size of item.  
	;; This will never change after the item is created.
	(define hard-min-width (void))
        (define hard-min-height (void))
	(public*
         [set-min-height (lambda (v) (set! hard-min-height v) (min-height v))]
         [set-min-width (lambda (v) (set! hard-min-width v) (min-width v))]
         [get-hard-minimum-size (lambda () (values hard-min-width hard-min-height))]
	  
         [client-inset
          (lambda (h?)
            (let ([h #f][w #f])
              (unless h
                (let ([w-box (box 0)]
                      [h-box (box 0)])
                  (get-client-size w-box h-box)
                  (set! h (- (get-height) (unbox h-box)))
                  (set! w (- (get-width) (unbox w-box)))))
              (if h? h w)))]

         ;; gets/sets user's requirement for minimum width.  Errors out
         ;; if new value is not a non-negative real number.  Forces a
         ;; redraw upon a set.
         [min-client-width
          (case-lambda 
	    [() (- (min-width) (client-inset #f))]
	    [(new-width)
	     (check-dimension '(method canvas<%> min-client-width) new-width)
	     (min-width (+ new-width (client-inset #f)))])]
         [min-client-height
          (case-lambda 
	    [() (- (min-height) (client-inset #t))]
	    [(new-height) 
	     (check-dimension '(method canvas<%> min-client-height) new-height)
	     (min-height (+ new-height (client-inset #t)))])])

        (define -mw 0)
        (define -mh 0)
        (define -xm x-margin-w)
        (define -ym y-margin-h)
        (define -sx stretch-x)
        (define -sy stretch-y)
        (define first-arg (car args))

	(public*
         [min-width
          (mk-param
           -mw identity
           (lambda (v)
             (check-dimension '(method area<%> min-width) v))
           force-redraw)]
         [min-height
          (mk-param
           -mh identity
           (lambda (v)
             (check-dimension '(method area<%> min-height) v))
           force-redraw)]
	  
         [x-margin
          (mk-param
           -xm identity
           (lambda (v)
             (check-margin-integer '(method subarea<%> horiz-margin) v)
             v)
           force-redraw)]
         [y-margin
          (mk-param
           -ym identity
           (lambda (v) 
             (check-margin-integer '(method subarea<%> vert-margin) v)
             v)
           force-redraw)]

         [stretchable-in-x
          (mk-param -sx (lambda (x) (and x #t)) void force-redraw)]
         [stretchable-in-y
          (mk-param -sy (lambda (x) (and x #t)) void force-redraw)]
	  
         ;; get-info: passes necessary info up to parent.
         ;; input: none
         ;; returns: child-info struct containing the info about this
         ;;   item.
         ;; intended to be called by item's parent upon resize.
         [get-info
          (lambda ()
            (let* ([min-size (get-min-size)]
                   [result (make-child-info (car min-size) (cadr min-size)
                                            (x-margin) (y-margin)
                                            (stretchable-in-x)
                                            (stretchable-in-y))])
              result))]
	  
         [area-parent (lambda () first-arg)]

         ;; force-redraw: unconditionally trigger redraw.
         ;; input: none
         ;; returns: nothing
         ;; effects: forces the item's parent (if it exists) to redraw
         ;;   itself. This will recompute the min-size cache if it is
         ;;   invalid.
         [force-redraw
          (lambda ()
            (let ([parent (area-parent)])
              (when parent
                (send parent child-redraw-request this))))]
	  
         [on-container-resize (lambda () (void))] ; This object doesn't contain anything

         [init-min (lambda (x) x)]
	  
         ;; get-min-size: computes the minimum size the item can
         ;;   reasonably assume.
         ;; input: none
         ;; returns: a list containing the minimum width & height.
         [get-min-size
          (lambda ()
            (let ([w (+ (* 2 (x-margin)) (max hard-min-width (min-width)))]
                  [h (+ (* 2 (y-margin)) (max hard-min-height (min-height)))])
              (list w h)))])

        (public*
         [set-area-parent (lambda (p) (set! first-arg p))])
	
        (apply super-make-object (send (car args) get-window) (cdr args))
        (set-min-width (init-min (get-width)))
        (set-min-height (init-min (get-height)))

        (unless (memq 'deleted window-style)
          (send (get-top-level) show-control this #t)
          ;; For a pane[l], the creator must call the equivalent of the following,
          ;;  delaying to let the panel's wx field get initialized before
          ;;  panel-sizing methods are called
          (unless (is-a? this wx-basic-panel<%>)
            (send (area-parent) add-child this))))))

  ;; make-control% - for non-panel items
  (define (make-control% item% x-margin y-margin
			 stretch-x stretch-y)
    (class (make-item% item% x-margin y-margin stretch-x stretch-y)
      (init-rest args)
      (inherit get-parent)
      (apply super-make-object args)
      (send (get-parent) set-item-cursor 0 0)))

  (define (make-simple-control% item% [x-m const-default-x-margin] [y-m const-default-y-margin])
    (make-control% item% x-m y-m #f #f))

  (define wx-button% (make-window-glue% 
		      (class (make-simple-control% wx:button%)
                        (init parent cb label x y w h style font)
			(inherit command set-border get-top-level)
                        (define border? (memq 'border style))
                        (define border-on? border?)
			(public*
                         [defaulting (lambda (on?)
                                       (set! border-on? on?)
                                       (set-border border-on?))] 
                         [has-border? (lambda () border-on?)])
			(override*
                         [char-to (lambda ()
                                    (as-exit
                                     (lambda ()
                                       (command (make-object wx:control-event% 'button)))))])
			(super-make-object style parent cb label x y w h (cons 'deleted style) font)
                        (when border?
                          (send (get-top-level) add-border-button this)))))
  (define wx-check-box% (class (make-window-glue% (make-simple-control% wx:check-box%))
                          (init mred proxy parent cb label x y w h style font)
			  (inherit set-value get-value command)
			  (override*
                           [char-to (lambda ()
                                      (as-exit
                                       (lambda ()
                                         (set-value (not (get-value)))
                                         (command (make-object wx:control-event% 'check-box)))))])
			  (super-make-object mred proxy style parent cb label x y w h (cons 'deleted style) font)))

  (define wx-message% (class (make-window-glue% (make-simple-control% wx:message%))
                        (init mred proxy parent label x y style font)
			(override* [gets-focus? (lambda () #f)])
			(super-make-object mred proxy style parent label x y (cons 'deleted style) font))))


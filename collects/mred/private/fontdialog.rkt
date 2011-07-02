(module fontdialog mzscheme
  (require mzlib/class
           mzlib/etc
           mzlib/list
           (prefix wx: "kernel.rkt")
           "lock.rkt"
           "wx.rkt"
           "cycle.rkt"
           "check.rkt"
           "helper.rkt"
           "gdi.rkt"
           "editor.rkt"
           "mrtop.rkt"
           "mrcanvas.rkt"
           "mrpopup.rkt"
           "mrmenu.rkt"
           "mritem.rkt"
           "mrpanel.rkt"
           "mrtextfield.rkt")

  (provide get-font-from-user)

  (define get-font-from-user 
    (case-lambda
     [() (get-font-from-user #f #f #f null)]
     [(message) (get-font-from-user message #f #f null)]
     [(message parent) (get-font-from-user message parent #f null)]
     [(message parent font) (get-font-from-user message parent font null)]
     [(message parent font style)
      (check-label-string/false 'get-font-from-user message)
      (check-top-level-parent/false 'get-font-from-user parent)
      (check-instance 'get-font-from-user wx:font% 'font% #t font)
      (check-style 'get-font-from-user #f null style)
      (letrec ([ok? #f]
	       [f (make-object dialog% "Choose Font" parent 500 300)]
	       [refresh-sample (lambda (b e) (let ([f (get-font)])
					       (send ok-button enable f)
					       (when f
						 (let ([s (send (send edit get-style-list) find-named-style "Standard")])
						   (send s set-delta (font->delta f))))))]
	       [p (make-object horizontal-pane% f)]
	       [face (make-object list-box% #f (wx:get-face-list) p refresh-sample)]
	       [p2 (make-object vertical-pane% p)]
	       [p3 (instantiate horizontal-pane% (p2) [stretchable-width #f])]
	       [style (let ([pnl (instantiate group-box-panel% ("Style" p3) [stretchable-height #f] [stretchable-width #f])])
			(make-object radio-box% #f '("Normal" "Italic" "Slant") pnl refresh-sample))]
	       [weight (let ([pnl (instantiate group-box-panel% ("Weight" p3) [stretchable-height #f] [stretchable-width #f])])
			 (make-object radio-box% #f '("Normal" "Bold" "Light") pnl refresh-sample))]
	       [p4 (instantiate vertical-pane% (p3) [alignment '(left center)])]
	       [underlined (make-object check-box% "Underlined" p4 refresh-sample)]
	       [smoothing (make-object choice% "Smoothing:" '("Default" "Some" "Full" "None") p4 refresh-sample)]
	       [sip (make-object check-box% "Size in Pixels" p4 refresh-sample)]
	       [sym (make-object check-box% "Map as Symbol" p4 refresh-sample)]
	       [size (make-object slider% "Size:" 4 127 p2 refresh-sample 12)]
	       [sample (make-object text-field% "Sample" f void 
                                    "The quick brown fox jumped over the lazy dog\n(\u3bb (x) x)\n" 
                                    '(multiple))]
	       [edit (send sample get-editor)]
	       [done (lambda (ok) (lambda (b e) (set! ok? ok) (send f show #f)))]
	       [get-font (lambda () (let ([face (send face get-string-selection)])
				      (and face
					   (make-object wx:font% (send size get-value) face 
							(if (send sym get-value)
							    'symbol
							    'default)
							(case (send style get-selection) [(0) 'normal] [(1) 'italic] [(2) 'slant])
							(case (send weight get-selection) [(0) 'normal] [(1) 'bold] [(2) 'light])
							(send underlined get-value)
							(case (send smoothing get-selection) 
							  [(0) 'default] 
							  [(1) 'partly-smoothed]
							  [(2) 'smoothed]
							  [(3) 'unsmoothed])
							(send sip get-value)))))]
	       [bp (instantiate horizontal-pane% (f) [stretchable-height #f])]
	       [ms-button (if (eq? (wx:font-from-user-platform-mode) 'dialog)
			      (begin0
			       (make-object button% "Use System Dialog..." bp
					    (lambda (b e)
					      (let ([new-font (wx:get-font-from-user 
							       message 
							       (mred->wx f)
							       (get-font))])
						(when new-font
						  (reset-font new-font)))))
			       ;; Spacer:
			       (make-object pane% bp))
			      (void))]
               [ok+cancel (call-with-values
                              (lambda () 
                                (ok-cancel
                                 (lambda () (make-object button% "OK" bp (done #t) '(border)))
                                 (lambda () (make-object button% "Cancel" bp (done #f)))))
                            cons)]
	       [ok-button (car ok+cancel)]
	       [cancel-button (cdr ok+cancel)]
	       [reset-font 
		(lambda (font)
		  (let* ([facen (if font
				    (send font get-face)
				    (wx:get-family-builtin-face 'default))]
			 [f (and facen (send face find-string facen))])
		    (and f (>= f 0) (send face set-selection f)))
		  (when font
		    (send style set-selection (case (send font get-style) [(normal) 0] [(italic) 1] [(slant) 2]))
		    (send weight set-selection (case (send font get-weight) [(normal) 0] [(bold) 1] [(light) 2]))
		    (send underlined set-value (send font get-underlined))
		    (send size set-value (send font get-point-size))
		    (send sip set-value (send font get-size-in-pixels)))
		  (refresh-sample (void) (void)))])
	(send bp set-alignment 'right 'center)
	(send face min-width (max 200 (let-values ([(w h) (send face get-graphical-min-size)]) w)))
	(reset-font font)
	(send f center)
	(send f show #t)
	(and ok? (get-font)))]))

  (set-get-font-from-user! get-font-from-user))

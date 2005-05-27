
(module toolbar mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss")

  ;; These modules implement snips for the various
  ;;  kinds of windows and controls.
  (require "base.ss"
	   "panel.ss"
	   "simple-control.ss"
	   "text-field.ss"
	   "multiple-choice.ss"
	   "slider-guage.ss"
	   "canvas.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Frame
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-struct tool (icon callback active?))

  (define lg-pen (send mred:the-pen-list find-or-create-pen 
		       (make-object mred:color% 200 200 200) 0 'solid))

  (define dg-pen (send mred:the-pen-list find-or-create-pen 
		       (make-object mred:color% 140 140 140) 0 'solid))

  (define icons (make-hash-table))

  (define toolbar%
    (class mred:canvas%
      (inherit min-height stretchable-height get-dc)
      (private-field
       [margin 2]
       [icon-size 16]
       [tools null]
       [active-tool #f])
      (private*
       [deactivate-tool
	(lambda ()
	  (when active-tool
		(set-tool-active?! active-tool #f)
		(set! active-tool #f)
		(on-paint)))]
       [activate-tool
	(lambda (mx my only)
	  (let ([y 0]
		[h (+ icon-size (* 2 margin))]
		[w (+ icon-size (* 2 margin))])
	    (let loop ([l tools][x 0])
	      (unless (null? l)
		 (if (and (<= x mx (+ x w)) (<= y my (+ y h))
			  (or (not only) (eq? (car l) only)))
		     (begin
		       (set! active-tool (car l))
		       (set-tool-active?! active-tool #t)
		       (on-paint))
		     (loop (cdr l) (+ x w)))))))])
      (private-field
       [can-drag #f])
      (override*
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)]
		[y 0]
		[h (+ icon-size (* 2 margin))]
		[w (+ icon-size (* 2 margin))])
	    (let loop ([l tools][x 0])
	      (unless (null? l)
		(let ([tool (car l)])
		  (let ([p (send dc get-pen)]
			[on? (tool-active? tool)])
		    (send dc set-pen (if on? dg-pen lg-pen))
		    (send dc draw-line x y (+ x w -1) y)
		    (send dc draw-line x y x (+ y h -1))
		    (send dc draw-line x (add1 y) (+ x w -2) (add1 y))
		    (send dc draw-line (add1 x) y (add1 x) (+ y h -2))
		    (send dc set-pen (if on? lg-pen dg-pen))
		    (send dc draw-line (+ x 1) (+ y h -1) (+ x w -1) (+ y h -1))
		    (send dc draw-line (+ x w -1) (+ y 1) (+ x w -1) (+ y h -1))
		    (send dc draw-line (+ x 2) (+ y h -2) (+ x w -2) (+ y h -2))
		    (send dc draw-line (+ x w -2) (+ y 2) (+ x w -2) (+ y h -2))
		    (send dc set-pen p))
		  (if (tool-icon tool)
		      (send dc draw-bitmap (tool-icon tool) (+ x margin) margin)
		      (send dc draw-rectangle (+ x margin) margin icon-size icon-size)))
		(loop (cdr l) (+ x w))))))]
       [on-event
	(lambda (e)
	  (cond
	   [(send e button-down?)
	    (deactivate-tool)
	    (activate-tool (send e get-x) (send e get-y) #f)
	    (set! can-drag active-tool)]
	   [(send e button-up?)
	    (set! can-drag #f)
	    (when active-tool
		  (let ([cb (tool-callback active-tool)])
		    (deactivate-tool)
		    (cb #f #f)))]
	   [(send e dragging?)
	    (when can-drag
		  (let ([old-active active-tool])
		    (set! active-tool #f)
		    (activate-tool (send e get-x) (send e get-y) can-drag)
		    (when (and (not active-tool) old-active)
			  (set-tool-active?! old-active #f)
			  (on-paint))))]
	   [else (set! can-drag #f) 
		 (deactivate-tool)]))])
      (public*
       [append-tool
	(lambda (icon-name cb)
	  (let* ([name (string->symbol icon-name)]
		 [icon
		  (hash-table-get 
		   icons name
		   (lambda ()
		     (let* ([icon (make-object mred:bitmap% 
					       (build-path (collection-path "guibuilder") 
							   icon-name))])
		       (if (send icon ok?) 
			   icon
			   #f))))])
	    (hash-table-put! icons name icon)
	    (set! tools (append tools (list (make-tool icon cb #f))))))])
      (super-new)
      (min-height (+ icon-size (* margin 2)))
      (stretchable-height #f)))

  (define (add-tools toolbar emenu insert-element)
    (let* ([append-element-type
	    (lambda (name icon c%)
	      (let ([maker (lambda (i e) (insert-element c%))])
		(when toolbar
		  (send toolbar append-tool icon maker))
		(when emenu
		  (make-object mred:menu-item% name emenu maker))))])
      (append-element-type "New Vertical Panel" "vpanel.xpm" gb:vertical-panel-snip%)
      (append-element-type "New Horizontal Panel" "hpanel.xpm" gb:horizontal-panel-snip%)
      (append-element-type "New Message Label" "message.xpm" gb:message-snip%)
      (append-element-type "New Button" "button.xpm" gb:button-snip%)
      (append-element-type "New Checkbox" "checkbox.xpm" gb:check-box-snip%)
      (append-element-type "New Text Field" "text.xpm" gb:text-snip%)
      (append-element-type "New List" "list.xpm" gb:list-box-snip%)
      (append-element-type "New Radiobox" "radiobox.xpm" gb:radio-box-snip%)
      (append-element-type "New Choice" "choice.xpm" gb:choice-snip%)
      (append-element-type "New Slider" "slider.xpm" gb:slider-snip%)
      (append-element-type "New Gauge" "gauge.xpm" gb:gauge-snip%)
      (append-element-type "New Canvas" "canvas.xpm" gb:canvas-snip%)
      (append-element-type "New Editor Canvas" "mcanvas.xpm" gb:editor-canvas-snip%)))

  (provide toolbar%
	   add-tools))

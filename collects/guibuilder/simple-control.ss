
(module simple-control mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss"
	   "base.ss"
	   "feature.ss")

  (define gb:make-message-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h)
	(inherit get-label get-label-size draw-label)
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "message"))]
	  [gb-get-min-size
	   (lambda (dc)
	     (get-label-size dc))]
	  [draw
	   (lambda (dc x y . other)
	     (draw-label dc x y))]
	  [gb-get-default-class (lambda () 'message%)])
	(super-new))))
  
  (define gb:message-snip% (gb:make-message-snip%
			    (gb:make-text-label-snip% gb:atomic-snip% 
						      "Message")
			    "gb:message"))
  
  (register-class gb:message-snip% "gb:message")
  
  (define gb:make-button-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h)
	(inherit get-label get-label-size get-callback-names draw-label)
	(private-field
	  [m 5])
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "button"))]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)])
	       (values (+ (* 2 m) x) (+ (* 2 m) y))))]
	  [draw
	   (lambda (dc x y . other)
	     (send dc draw-rounded-rectangle x y w h 3)
	     (let-values ([(lw lh) (get-label-size dc)])
	       (draw-label dc 
			   (+ (+ x m) (/ (- w lw (* 2 m)) 2))
			   (+ (+ y m) (/ (- h lh (* 2 m)) 2)))))]
	  [gb-get-default-class (lambda () 'button%)])
	(super-new))))
  
  (define gb:make-check-box-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h)
	(inherit get-style get-label get-callback-names get-label-size draw-label)
	(private-field
	  [hspace 2]
	  [boxsize 12])
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "checkbox"))]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)])
	       (values (+ boxsize hspace x) (max boxsize y))))]
	  [draw
	   (lambda (dc x y . other)
	     (let-values ([(lx ly) (get-label-size dc)])
	       (send dc draw-rectangle x (+ y (/ (- h boxsize) 2)) boxsize boxsize)
	       (draw-label dc (+ x boxsize hspace) (+ y (/ (- h ly) 2)))))]
	  [gb-get-default-class (lambda () 'check-box%)])
	(super-new))))
  
  (define gb:button-snip% (gb:make-button-snip%
			   (gb:make-callback-snip%
			    (gb:make-text-label-snip% gb:atomic-snip% 
						      "Button"))
			   "gb:button"))
  
  (define gb:check-box-snip% (gb:make-check-box-snip%
			      (gb:make-callback-snip%
			       (gb:make-text-label-snip% gb:atomic-snip% 
							 "Checkbox"))
			      "gb:checkbox"))
  
  (register-class gb:button-snip% "gb:button")
  (register-class gb:check-box-snip% "gb:checkbox")
  

  (provide gb:message-snip%
	   gb:button-snip%
	   gb:check-box-snip%))

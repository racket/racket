#lang racket/gui

(require racket/class
	 mzlib/etc)

(define my-txt #f)
(define my-lb #f)
(define noisy? #f)

(define mdi-frame #f)
(define (mdi)
  (set! mdi-frame (make-object frame% "Item Test" #f
			       #f #f #f #f
			       '(mdi-parent)))
  (send mdi-frame maximize #t)
  (send mdi-frame show #t))

(define default-parent-frame #f)
(define (parent-frame)
  (set! default-parent-frame (make-object frame% "Item Test Parent" #f
			       100 100))
  (send default-parent-frame show #t))

(when (namespace-variable-value 'mdi? #t (lambda () #f))
  (mdi))

(define (add-frame-style style)
  (let* ([style (if use-metal?
		    (cons 'metal style)
		    style)]
	 [style (if float-frame?
		    (cons 'float style)
		    style)]
	 [style (if no-caption?
		    (cons 'no-caption style)
		    style)])
    style))

(define make-frame
  (opt-lambda (% name [parent #f] [w #f] [h #f] [x #f] [y #f] [style '()])
    (make-object % name
		 (or parent mdi-frame default-parent-frame)
		 w h x y 
		 (if mdi-frame
		     (cons 'mdi-child style)
		     (add-frame-style style)))))

(define special-font (send the-font-list find-or-create-font
			   20 'decorative 
			   'normal 'bold
			   #f))
(define italic-font (send the-font-list find-or-create-font
                          13 'roman
                          'italic 'normal
                          #f))
(define ($ font) (or font normal-control-font))

(define (make-h&s cp f)
  (make-object button% "Hide and Show" cp
	       (lambda (b e) (send f show #f) (send f show #t))))

(define (add-hide name w cp)
  (let ([c (make-object check-box% (format "Show ~a" name) cp
			(lambda (c e) (send w show (send c get-value))))])
    (send c set-value #t)))

(define (add-disable name w ep)
  (let ([c (make-object check-box% (format "Enable ~a" name) ep
			(lambda (c e) (send w enable (send c get-value))))])
    (send c set-value (send w is-enabled?))))

(define (add-disable-radio name w i ep)
  (let ([c (make-object check-box% (format "Enable ~a" name) ep
			(lambda (c e) (send w enable i (send c get-value))))])
    (send c set-value #t)))

(define (add-change-label name w lp orig other)
  (make-object button% (format "Relabel ~a" name) lp
	       (let ([orig-name (if orig orig (send w get-label))]
		     [changed? #f])
		 (lambda (b e)
		   (if changed?
		       (unless (null? orig-name)
			 (send w set-label orig-name))
		       (send w set-label other))
		   (set! changed? (not changed?))))))

(define (add-focus-note frame panel)
  (define m (make-object message% "focus: ??????????????????????????????" panel))
  (send
   (make-object
    (class timer%
      (super-new)
      (inherit start)
      (override*
	[notify
	 (lambda ()
	   (when (send frame is-shown?)
	     (send m set-label
		   (let* ([w (with-handlers ([void (lambda (x) #f)])
			       (let ([f (get-top-level-focus-window)])
				 (and f (send f get-focus-window))))]
			  [l (and w (send w get-label))])
		     (let ([s (format "focus: ~a ~a" (or l "") w)])
		       (substring s 0 (min 200 (string-length s))))))
	     (start 1000 #t)))])))
   start 1000 #t))

(define (add-pre-note frame panel)
  (define m (make-object message% "pre: ??????????????????????????????" panel))
  (define cm (make-object check-box% "Drop Mouse Events" panel void))
  (define ck (make-object check-box% "Drop Key Events" panel void))
  (lambda (win e)
    (let ([m? (is-a? e mouse-event%)])
      (send m set-label
	    (let ([s (format "pre: ~a ~a ~a,~a"
			     (if m? "mouse" "key")
			     (let ([l (send win get-label)])
			       (if (not l)
				   win
				   l))
			     (send e get-x)
			     (send e get-y))])
	      (substring s 0 (min 200 (string-length s)))))
      (and (not (or (eq? win cm) (eq? win ck)))
	   (or (and m? (send cm get-value))
	       (and (not m?) (send ck get-value)))))))

(define (add-enter/leave-note frame panel)
  (define m (make-object message% "enter: ??????????????????????????????" panel))
  (lambda (win e)
    (when (memq (send e get-event-type) '(enter leave))
      (let ([s (format "~a: ~a"
		    (send e get-event-type)
		    (let ([l (send win get-label)])
		      (if (not l)
			  win
			  l)))])
	(when noisy? (printf "~a\n" s))
	(send m set-label (substring s 0 (min 200 (string-length s))))))))

(define (add-click-intercept frame panel)
  (define cp (make-object check-box% "Popup on Click" panel void))
  (lambda (win e)
    (if (and (send e button-down?)
	     (not (eq? cp win))
	     (send cp get-value))
	(let ([m (make-object popup-menu%)])
	  (make-object menu-item% (format "Click on ~a" win)
		       m (lambda (i e)
			   (unless (eq? (send m get-popup-target) win)
			     (printf "Wrong owner!\n"))))
	  (send win popup-menu m 
		(inexact->exact (send e get-x))
		(inexact->exact (send e get-y)))
	  #t)
	#f)))

(define (add-cursors frame panel ctls)
  (let ([old #f]
	[f-old #f]
	[bc (make-object cursor% 'bullseye)]
	[cc (make-object cursor% 'cross)])
    (make-object check-box% "Control Bullseye Cursors" panel
		 (lambda (c e)
		   (printf "~a\n" e)
		   (if (send c get-value)
		       (set! old 
			     (map (lambda (b) 
				    (begin0
				     (send b get-cursor)
				     (send b set-cursor bc)))
				  ctls))
		       (map (lambda (b c) (send b set-cursor c))
			    ctls old))))
    (make-object check-box% "Frame Cross Cursor" panel
		 (lambda (c e)
		   (if (send c get-value)
		       (begin
			 (set! f-old (send frame get-cursor))
			 (send frame set-cursor cc))
		       (send frame set-cursor f-old))))
    (make-object check-box% "Busy Cursor" panel
		 (lambda (c e)
		   (if (send c get-value)
		       (begin-busy-cursor)
		       (end-busy-cursor))))))
		       
(define OTHER-LABEL "XXXXXXXXXXXXXXXXXXXXXX")

(define-values (icons-path local-path)
  (let ([d (this-expression-source-directory)])
    (values
     (lambda (n)
       (collection-file-path n "icons"))
     (lambda (n)
       (build-path d n)))))

(define on-demand-menu-item%
  (class menu-item%
    (init -name)
    (init-rest args)
    (define name -name)
    (override*
      [on-demand
       (lambda ()
	 (printf "Menu item ~a demanded\n" name))])
    (apply super-make-object name args)))

(define popup-test-canvas%
  (class canvas%
    (init -objects -names)
    (init-rest args)
    (inherit popup-menu get-dc refresh)
    (define objects -objects)
    (define names -names)
    (define tab-in? #f)
    (define last-m null)
    (define last-choice #f)
    (override*
     [on-paint
      (lambda ()
        (let ([dc (get-dc)])
          (send dc clear)
          (send dc draw-text "Left: popup hide state" 0 0)
          (send dc draw-text "Right: popup previous" 0 20)
          (send dc draw-text (format "Last pick: ~s" last-choice) 0 40)
          (when tab-in?
            (send dc draw-text "Tab in" 0 60))))]
     [on-event
      (lambda (e)
        (when (send e button-down?)
          (let ([x (send e get-x)]
                [y (send e get-y)]
                [m (if (or (null? last-m)
                           (send e button-down? 'left)
                           (send e button-down? 'middle))
                       (let ([m (make-object popup-menu% "T&itle"
                                             (lambda (m e)
                                               (unless (is-a? m popup-menu%)
                                                 (error "bad menu object"))
                                               (unless (and (is-a? e control-event%)
                                                            (memq (send e get-event-type)
                                                                  '(menu-popdown menu-popdown-none)))
                                                 (error "bad event object"))
                                               (printf "popdown ok\n")))]
                             [make-callback 
                              (let ([id 0])
                                (lambda ()
                                  (set! id (add1 id))
                                  (let ([id id])
                                    (lambda (m e)
                                      (set! last-choice id)
                                      (on-paint)))))])
                         (for-each
                          (lambda (obj name)
                            (make-object menu-item%
                                         (string-append
                                          name ": "
                                          (if (send obj is-shown?)
                                              "SHOWN"
                                              "<h i d d e n>"))
                                         m
                                         (make-callback)))
                          objects names)
                         (make-object on-demand-menu-item%
                                      "[on-demand hook]"
                                      m
                                      void)
                         (make-object menu-item%
                                      "6 && Half-D&ozen"
                                      m
                                      void)
                         (make-object menu-item%
                                      "&&_A"
                                      m
                                      void)
                         (let mloop ([m m][sub-at-50? #t])
                           (let ([sm (if (and sub-at-50?
                                              (send e button-down? 'middle))
                                         m
                                         (make-object menu% "Too Tall" m))])
                             (let loop ([n 1])
                               (unless (= n 101)
                                 (if (and sub-at-50? (= n 50))
                                     (let ([m (make-object menu% "Item 50" sm)])
                                       (mloop m #f))
                                     (make-object menu-item% (format "Item ~a" n) sm void))
                                 (when (zero? (modulo (- n 5) 10))
                                   (make-object separator-menu-item% sm))
                                 (loop (add1 n))))))
                         m)
                       last-m)])
            (set! last-m m)
            (popup-menu m (inexact->exact x) (inexact->exact y)))))]
     [on-tab-in (lambda () (set! tab-in? #t) (refresh))]
     [on-focus (lambda (on?)
                 (when (and tab-in? (not on?))
                   (set! tab-in? #f)
                   (refresh)))])
    (apply super-make-object args)))

(define prev-frame #f)

(define bitmap2%
  (class bitmap%
    (init-rest args)
    (inherit ok?)
    (apply super-make-object args)
    (unless (ok?)
      (printf "bitmap failure: ~s\n" args))))

(define (active-mixin %)
  (class %
    (define pre-on void)
    (define click-i void)
    (define el void)
    (override* [on-subwindow-event (lambda args 
                                     (apply el args)
                                     (or (apply pre-on args)
                                         (apply click-i args)
                                         (super on-subwindow-event . args)))]
               [on-subwindow-char (lambda args 
                                    (or (apply pre-on args)
                                        (super on-subwindow-char . args)))]
               [on-subwindow-focus (lambda (win on?)
                                     (printf "focus ~s ~s\n" (send win get-label) on?))]
               [on-activate (lambda (on?) (printf "active: ~a\n" on?))]
               [on-move (lambda (x y) (printf "moved: ~a ~a\n" x y))]
               [on-size (lambda (x y) (printf "sized: ~a ~a\n" x y))])
    (public* [set-info
              (lambda (ep)
                (set! pre-on (add-pre-note this ep))
                (set! click-i (add-click-intercept this ep))
                (set! el (add-enter/leave-note this ep)))])
    (super-new)))

(define active-frame% (active-mixin frame%))
(define active-dialog% (active-mixin dialog%))

(define (trace-mixin c%)
  (class c%
    (init -name)
    (init-rest args)
    (define name -name)
    (override*
      [on-superwindow-show
       (lambda (on?)
	 (printf "~a ~a\n" name (if on? "show" "hide")))]
      [on-superwindow-enable
       (lambda (on?)
	 (printf "~a ~a\n" name (if on? "on" "off")))])
    (apply super-make-object name args)))

(define (auto-mixin c% v)
  (class c%
    (super-new [auto-resize v])))

(define return-bmp 
  (make-object bitmap2% (icons-path "return.xbm") 'xbm))
(define bb-bmp
  (read-bitmap (icons-path "bomb-32x32.png") #:try-@2x? #t))
(define mred-bmp
  (make-object bitmap2% (icons-path "mred.xbm") 'xbm))
(define nruter-bmp
  (make-object bitmap2% (local-path "nruter.xbm") 'xbm))
(define gc-bmp
  (read-bitmap (icons-path "recycle.png") #:try-@2x? #t))

(define (add-label-direction label-h? l)
  (if (not label-h?)
      (cons 'vertical-label l)
      l))

(define (make-ctls ip cp lp add-testers ep radio-h? label-h? null-label? stretchy? alt-inits? msg-auto? font)
  
  (define-values (l il)
    (let ([p (make-object horizontal-panel% ip)])
      (send p stretchable-width stretchy?)
      (send p stretchable-height stretchy?)
      
      (let ()
	(define l (make-object (trace-mixin (auto-mixin message% msg-auto?)) "L\u03B9&st" p null ($ font)))
	(define il (make-object (trace-mixin (auto-mixin message% msg-auto?)) return-bmp p null ($ font)))
	
	(add-testers "Message" l)
	(add-change-label "Message" l lp #f OTHER-LABEL)
	
	(add-testers "Image Message" il)
	(add-change-label "Image Message" il lp return-bmp nruter-bmp)
	
	(values l il))))
  
  (define b (make-object (trace-mixin button%)
			 "H\u03A3&llo" ip ; \u03A3 is eta
			 (lambda (b e)
			   (send b enable #f)
			   (sleep/yield 5)
			   (send b enable #t))
			 null ($ font)))
  
  (define ib (make-object (trace-mixin button%) bb-bmp ip void null ($ font)))
  
  ; (define ib2 (make-object button% return-bmp ip void))
  
  (define lb (make-object (trace-mixin list-box%)
			  (if null-label? #f "L\u03B9&st") ; \u03B9 is iota
			  '("Appl\u03A3" "Banana" "Coconut & Donuts" "Eclair" "French Fries" "Gatorade" "Huevos Rancheros") ; \u03A3 is eta
			  ip void
			  (add-label-direction label-h? '(single))
			  (if alt-inits? 2 #f)
			  (or font view-control-font) ($ font)))
  
  (define cb (make-object (trace-mixin check-box%) "C&h\u03A3ck" ip void null alt-inits? ($ font))) ; \u03A3 is eta
  
  (define icb (make-object (trace-mixin check-box%) mred-bmp ip void null alt-inits? ($ font)))
  
  (define rb (make-object (trace-mixin radio-box%)
			  (if null-label? #f "R&ad\u03B9o") ; \u03B9 is iota
			  '("F\u03B9rst" "Dos" "T&rio")
			  ip void 
			  (add-label-direction 
			   label-h? 
			   (if radio-h?
			       '(horizontal)
			       '(vertical)))
			  (if alt-inits? 2 0)
			  ($ font)))
  
  (define irb (make-object (trace-mixin radio-box%)
			   (if null-label? #f "Image Ra&dio")
			   (list return-bmp nruter-bmp)
			    ip void 
			    (add-label-direction 
			     label-h? 
			     (if radio-h?
				 '(horizontal)
				 '(vertical)))
			  (if alt-inits? 1 0)
			  ($ font)))
  
  (define ch (make-object (trace-mixin choice%)
			  (if null-label? #f "Ch&o\u03B9ce") ; \u03B9 is iota
			  '("Alpha" "Beta" "Gamma" "Delta & R\u03A3st") ; \u03A3 is eta
			  ip void
			  (add-label-direction label-h? null)
			  (if alt-inits? 3 0)
			  ($ font)))
  
  (define txt (make-object (trace-mixin text-field%)
			   (if null-label? #f "T\u03A3&xt") ; \u03A3 is eta
			   ip void
			   "initial & starting"
			   (add-label-direction label-h? '(single))
			   ($ font)))
  
  (set! my-txt txt)
  (set! my-lb lb)

  (add-testers "Button" b)
  (add-change-label "Button" b lp #f OTHER-LABEL)
  
  (add-testers "Image Button" ib)
  (add-change-label "Image Button" ib lp bb-bmp return-bmp)
  
  (add-testers "List" lb)
  (add-change-label "List" lb lp #f OTHER-LABEL)
  
  (add-testers "Checkbox" cb)
  (add-change-label "Checkbox" cb lp #f OTHER-LABEL)
  
  (add-testers "Image Checkbox" icb)
  (add-change-label "Image Checkbox" icb lp mred-bmp bb-bmp)
  
  (add-testers "Radiobox" rb)
  (add-disable-radio "Radio Item `First'" rb 0 ep)
  (add-disable-radio "Radio Item `Dos'" rb 1 ep)
  (add-disable-radio "Radio Item `Trio'" rb 2 ep)
  (add-change-label "Radiobox" rb lp #f OTHER-LABEL)
  
  (add-testers "Image Radiobox" irb)
  (add-disable-radio "Radio Image Item 1" irb 0 ep)
  (add-disable-radio "Radio Image Item 2" irb 1 ep)
  (add-change-label "Image Radiobox" irb lp #f OTHER-LABEL)
  
  (add-testers "Choice" ch)
  (add-change-label "Choice" ch lp #f OTHER-LABEL)
  
  (add-testers "Text" txt)
  (add-change-label "Text" txt lp #f OTHER-LABEL)
  
  (let ([items (list ip
                     l il 
		     b ib 
		     lb
		     cb icb 
		     rb irb 
		     ch
		     txt)]
	[names (list "panel"
                     "label" "image label"
		     "button" "image button"
		     "list box"
		     "checkbox" "image checkbox"
		     "radio box" "image radiobox"
		     "choice"
		     "text")])
    (make-object choice%
		 "Set Focus"
		 (cons "..." names)
		 lp
		 (lambda (c e)
		   (let ([v (send c get-selection)])
		     (when (positive? v)
		       (send (list-ref items (sub1 v)) focus)
		       (send c set-selection 0)))))
    (make-object choice%
		 "Reparent"
		 (cons "..." names)
		 lp
		 (lambda (c e)
		   (let ([v (send c get-selection)])
		     (when (positive? v)
                       (define f (new frame% [label "New Parent"]))
                       (define p (if (zero? (random 2))
                                     (new vertical-pane% [parent f])
                                     f))
		       (send (list-ref items (sub1 v)) reparent p)
                       (send f show #t)
		       (send c set-selection 0)))))
    (cons (make-object popup-test-canvas% 
		       items
		       names
		       cp)
	  items)))

(define (add-deleted-adds panel l)
  (define v #f)
  
  (make-object choice% "New Deleted"
	       (list*
		"..."
		"*Activate Last*"
		(map car l))
	       panel
	       (lambda (c e)
		 (let ([i (send c get-selection)])
		   (send c set-selection 0)
		   (case i
		     [(0) (void)]
		     [(1) (send (send v get-parent) add-child v)]
		     [else (set! v ((cadr (list-ref l (- i 2)))))])))))

(define (add-big-deleted-adds panel)
  (add-deleted-adds
   panel
   (list (list "Message" (lambda () (instantiate message% ("Hello" panel) [style '(deleted)])))
	 (list "Bitmap Message" (lambda () (instantiate message% (bb-bmp panel) [style '(deleted)])))
	 (list "Icon Message" (lambda () (instantiate message% ('app panel) [style '(deleted)])))
	 (list "Button" (lambda () (instantiate button% ("Hello" panel void) [style '(deleted)])))
	 (list "Bitmap Button" (lambda () (instantiate button% (bb-bmp panel void) [style '(deleted)])))
	 (list "Checkbox" (lambda () (instantiate check-box% ("Hello" panel void) [style '(deleted)])))
	 (list "Bitmap Checkbox" (lambda () (instantiate check-box% (bb-bmp panel void) [style '(deleted)])))
	 (list "Radio Box" (lambda () (instantiate radio-box% ("Hello" (list "A" "B" "C") panel void) [style '(vertical deleted)])))
	 (list "Bitmap Radio Box" (lambda () (instantiate radio-box% ("Hello" (list bb-bmp bb-bmp) panel void) [style '(vertical deleted)]))))))

(define (add-med-deleted-adds panel)
  (add-deleted-adds
   panel
   (list (list "Canvas" (lambda () (instantiate canvas% (panel) [style '(deleted)])))
	 (list "Editor Canvas" (lambda () (instantiate editor-canvas% (panel) [style '(deleted)])))
	 (list "Slider" (lambda () (instantiate slider% ("Hello" 1 3 panel void) [style '(deleted vertical)])))
	 (list "Gauge" (lambda () (instantiate gauge% ("Hello" 3 panel) [style '(deleted vertical)])))
	 (list "Tab Panel" (lambda () (instantiate tab-panel% ('("Hello" "Bye") panel void) [style '(deleted)])))
	 (list "Group Box Panel" (lambda () (instantiate group-box-panel% ('"Hello" panel) [style '(deleted)])))
	 (list "Panel" (lambda () (instantiate panel% (panel) [style '(deleted border)]))))))

(define use-dialogs? #f)
(define use-metal? #f)
(define float-frame? #f)
(define no-caption? #f)

(define (big-frame h-radio? v-label? null-label? stretchy? font initially-disabled? 
                   alternate-init? msg-auto? panel-style)
  (define f (make-frame (if use-dialogs?
			    active-dialog%
			    active-frame%)
			"T\u03A3ster"  ; \u03A3 is eta
                        #f #f 100))
  
  (define hp (make-object horizontal-panel% f))
  
  (define ip (new vertical-panel% [parent hp] [style panel-style]))
  (define cp (new vertical-panel% [parent hp] [style panel-style]))
  (define ep (new vertical-panel% [parent hp] [style panel-style]))
  (define lp (new vertical-panel% [parent hp] [style panel-style]))
  
  (define (basic-add-testers name w)
    (add-hide name w cp)
    (add-disable name w ep))
  
  (define add-testers
    (if stretchy?
	(lambda (name control)
	  (send control stretchable-width #t)
	  (send control stretchable-height #t)
	  (basic-add-testers name control))
	basic-add-testers))
  
  (define fp (make-object vertical-panel% ip))
  
  (define tp 
    (if #f
	(make-object group-box-panel% "Sub" fp null (or font small-control-font))
	(make-object tab-panel% '("Sub" "Panel") fp void '(no-border) ($ font))))

  (when initially-disabled?
    (send tp enable #f))
    
  (make-h&s cp f)
  
  (add-testers "Sub-panel" fp)
  
  (send tp set-label "Sub-sub panel")
  (add-testers "Sub-sub-panel" tp)

  (let ([ctls (make-ctls tp cp lp add-testers ep h-radio? v-label? null-label? stretchy? alternate-init? msg-auto? font)])
    (add-focus-note f ep)
    (send f set-info ep)
    
    (add-cursors f lp ctls)

    (add-big-deleted-adds lp))

  (send f show #t)
  (set! prev-frame f)
  f)

(define (med-frame plain-slider? label-h? null-label? stretchy? font initially-disabled? 
                   alternate-init? msg-auto? panel-style)
  (define f2 (make-frame (if use-dialogs?
			    active-dialog%
			    active-frame%)
			 "Tester2"
                         #f #f 100))

  (define hp2 (make-object horizontal-panel% f2))
  
  (define ip2-0 (new vertical-panel% [parent hp2] [style panel-style]))
  (define cp2 (new vertical-panel% [parent hp2] [style panel-style]))
  (define ep2 (new vertical-panel% [parent hp2] [style panel-style]))
  (define lp2 (new vertical-panel% [parent hp2] [style panel-style]))
  
  (define (basic-add-testers2 name w)
    (add-hide name w cp2)
    (add-disable name w ep2))
  
  (define add-testers2
    (if stretchy?
	(lambda (name control)
	  (send control stretchable-width #t)
	  (send control stretchable-height #t)
	  (basic-add-testers2 name control))
	basic-add-testers2))

  (define fp2 (make-object vertical-panel% ip2-0))
  (define ip2 (make-object group-box-panel% "Sub" fp2))

  (when initially-disabled?
    (send ip2 enable #f))

  (make-h&s cp2 f2)
  
  (add-testers2 "Sub-panel" fp2)
  (send ip2 set-label "Sub-sub panel")
  (add-testers2 "Sub-sub-panel" ip2)
  
  (make-object text-field% #f ip2 void "start focus here")

  (when prev-frame
    (add-disable "Previous Tester Frame" prev-frame ep2))
  
  (let ()
    (define co
      (make-object combo-field% "Greet:" '("Hola" "Ni Hao") ip2 void "hello" null ($ font)))

    (define sh (make-object slider% 
			    (if null-label? #f "H S&lid\u03A3r") 0 10 ip2
			    (lambda (s e)
			      (send gh set-value (* 10 (send sh get-value))))
			    5
			    (add-label-direction 
			     label-h? 
			     (if plain-slider? '(horizontal plain) '(horizontal)))
			    ($ font)))
    
    (define sv (make-object slider% 
			    (if null-label? #f "V Sl&id\u03A3r") 0 10 ip2 
			    (lambda (s e)
			      (send gv set-value (* 10 (send sv get-value))))
			    5
			    (add-label-direction 
			     label-h? 
			     (if plain-slider? '(vertical plain) '(vertical)))
			    ($ font)))
    
    (define gh (make-object gauge% 
			    (if null-label? #f "H G&aug\u03A3") 100 ip2
			    (add-label-direction 
			     label-h? 
			     '(horizontal))
			    ($ font)))
    
    (define gv (make-object gauge% 
			    (if null-label? #f "V Ga&ug\u03A3") 100 ip2
			    (add-label-direction 
			     label-h? 
			     '(vertical))
			    ($ font)))
    
    (define txt (make-object text-field% 
			     (if null-label? #f "T&ext \u7238") ; \u7238 is Chinese "father"
			     ip2 void 
			     "initial & starting"
			     (add-label-direction 
			      label-h? 
			      '(multiple))
			     ($ font)))

    (define tab (make-object tab-panel% 
			     '("Appl\u03A3" "B&anana") ip2 void
			     null
			     ($ font)))

    (define grp (make-object group-box-panel% 
			     "Group\u03A3" ip2
			     null (or font small-control-font)))

    (make-object button% "OK" tab void)
    (make-object button% "Cancel" grp void)

    (add-testers2 "Combo" co)
    (add-testers2 "Horiz Slider" sh)
    (add-testers2 "Vert Slider" sv)
    (add-testers2 "Horiz Gauge" gh)
    (add-testers2 "Vert Gauge" gv)
    ; (add-testers2 "Text Message" cmt)
    ; (add-testers2 "Image Message" cmi)
    (add-testers2 "Text" txt)
    (add-testers2 "Tab" tab)
    (add-testers2 "Group" grp)
    
    (add-change-label "Combo" co lp2 #f OTHER-LABEL)
    (add-change-label "Horiz Slider" sh lp2 #f OTHER-LABEL)
    (add-change-label "Vert Slider" sv lp2 #f OTHER-LABEL)
    (add-change-label "Horiz Gauge" gh lp2 #f OTHER-LABEL)
    (add-change-label "Vert Gauge" gv lp2 #f OTHER-LABEL)
    (add-change-label "Text" txt lp2 #f OTHER-LABEL)
    (add-change-label "Group" grp lp2 #f OTHER-LABEL)
    
    (let* ([items (list co
			sh sv
			gh gv
			; cmt cmi
			txt
			tab grp)]
	   [canvas  (make-object popup-test-canvas% 
				 items
				 (list "combo"
                                       "h slider" "v slider"
				       "v gauge" "v gauge"
				       ; "text msg" "image msg"
				       "text"
				       "tab" "group")
				 cp2 '(hscroll vscroll))])
      (send canvas accept-tab-focus #t)
      (send canvas init-auto-scrollbars 300 300 0.0 0.0)
      (add-disable "Canvas" canvas ep2)

      (add-focus-note f2 ep2)
      (send f2 set-info ep2)
      
      (add-cursors f2 lp2 (cons canvas items))

      (add-med-deleted-adds lp2))

    (unless use-dialogs?
      (send f2 create-status-line)
      (send f2 set-status-text "This is the status line"))
    (send f2 show #t)
    (set! prev-frame f2)
    f2))

; Need: check, check-test, and enable via menubar
; All operations on Submenus
(define f%
  (class frame%
    (init-rest args)
    (define ADD-APPLE (void))
    (define ADD-BANANA (void))
    (define ADD-COCONUT (void))
    (define DELETE-APPLE (void))
    (define DELETE-EXTRA-BANANA (void))
    (define DELETE-BANANA (void))
    (define DELETE-COCONUT-0 (void))
    (define DELETE-COCONUT (void))
    (define DELETE-COCONUT-2 (void))
    (define COCONUT-ID (void))
    (define DELETE-ONCE (void))
    (define APPLE-CHECK-ID (void))
    (define CHINESE (void))
    (define menu-bar (void))
    (define main-menu (void))
    (define apple-menu (void))
    (define banana-menu (void))
    (define coconut-menu (void))
    (define baseball-ids (void))
    (define hockey-ids (void))
    (define enable-item (void))
    (apply super-make-object args)
    (public*
     [make-menu-bar
      (lambda ()
        (let* ([mb (make-object menu-bar% this)]
               [menu (make-object menu% "&Tester" mb)]
               [new (case-lambda 
		      [(l help parent) (make-object menu-item% l parent (lambda (o e) (callback o e)) #f help)]
		      [(l help) (make-object menu-item% l menu (lambda (o e) (callback o e)) #f help)]
		      [(l) (make-object menu-item% l menu (lambda (o e) (callback o e)))])]
               [sep (lambda () (make-object separator-menu-item% menu))])
          (set! menu-bar mb)
          (set! main-menu menu)

          (set! ADD-APPLE (new "Add Apple" "Adds the Apple menu"))
          (set! ADD-BANANA (new "Add Banana"))
          (set! ADD-COCONUT (new "Add Coconut"))
	   
          (make-object on-demand-menu-item% "Append Donut" menu
                       (lambda (m e) 
                         (make-object menu-item% "Donut" apple-menu void)))
          (sep)
          (set! DELETE-COCONUT-0 (new "Delete Coconut"))
          (make-object menu-item% "Delete Apple" menu
                       (lambda (m e) 
                         (send apple-menu delete)
                         (set! apple-installed? #f)))
	   
          (sep)
          (set! enable-item
                (make-object checkable-menu-item% "Apple Once Disabled" menu
                             (lambda (m e)
                               (send DELETE-ONCE enable
                                     (not (send enable-item is-checked?))))))
	   
          (let ([mk-enable (lambda (on?)
                             (lambda (m e)
                               (let ([l (send menu-bar get-items)])
                                 (unless (null? (cdr l))
                                   (send (cadr l) enable on?)))))])
            (make-object menu-item% "Disable Second" menu (mk-enable #f))
            (make-object menu-item% "Enable Second" menu (mk-enable #t)))

          (set! CHINESE (make-object menu-item% "Chinese: \U7238" menu void))
	   
          (let ([make-menu
                 (opt-lambda (title parent help-string)
                   (let ([m (make-object menu% title parent help-string)])
                     (send m delete)
                     m))])
            (set! apple-menu (make-menu "Apple" mb #f))
            (set! banana-menu (make-menu "Banana" mb #f))
            (set! coconut-menu (make-menu "Coconut" apple-menu "Submenu")))
	   
          (set! COCONUT-ID coconut-menu)

          (set! DELETE-ONCE (new "Delete Once" #f apple-menu))
          (set! DELETE-APPLE (new "Delete Apple" "Deletes the Apple menu" apple-menu))
          (set! APPLE-CHECK-ID (make-object checkable-menu-item% "Checkable" apple-menu void))

          (set! DELETE-BANANA (new "Delete Banana" #f banana-menu))
          (set! DELETE-EXTRA-BANANA (new "Delete First Banana Item" #f banana-menu))

          (set! DELETE-COCONUT (new "Delete Coconut" #f coconut-menu))
          (set! DELETE-COCONUT-2 (new "Delete Coconut By Position" #f coconut-menu))))]
      
     [callback
      (lambda (op ev)
        (cond
         [(eq? op ADD-APPLE)
          (send apple-menu restore)
          (set! apple-installed? #t)]
         [(eq? op ADD-BANANA)
          (send banana-menu restore)]
         [(eq? op ADD-COCONUT)
          (send coconut-menu restore)]
         [(eq? op DELETE-ONCE)
          (send DELETE-ONCE delete)]
         [(eq? op DELETE-APPLE)
          (send apple-menu delete)
          (set! apple-installed? #f)]
         [(eq? op DELETE-BANANA)
          (send banana-menu delete)]
         [(eq? op DELETE-EXTRA-BANANA)
          (send (car (send banana-menu get-items)) delete)]
         [(or (eq? op DELETE-COCONUT) (eq? op DELETE-COCONUT-0))
          (send COCONUT-ID delete)]
         [(eq? op DELETE-COCONUT-2)
          (send (list-ref (send apple-menu get-items) 3) delete)]))])
    (define mfp (make-object vertical-panel% this))
    (define mc (make-object editor-canvas% mfp))
    (define restp (make-object vertical-panel% mfp))
    (define sbp (make-object horizontal-panel% restp))
    (define mfbp (make-object horizontal-panel% restp))
    (define lblp (make-object horizontal-panel% restp))
    (define badp (make-object horizontal-panel% restp))
    (define e (make-object text%))
    (send restp stretchable-height #f)
    (send mc min-height 250)
    (send mc set-editor e)
    (send e load-file (local-path "menu-steps.txt"))
    (public*
     [make-test-button
      (lambda (name pnl menu id)
        (make-object button%
                     (format "Test ~a" name) pnl 
                     (lambda (b e)
                       (message-box
                        "Checked?"
                        (if (send id is-checked?)
                            "yes"
                            "no")))))]
     [compare
      (lambda (expect v kind)
        (unless (or (and (string? expect) (string? v)
                         (string=? expect v))
                    (eq? expect v))
          (error 'test-compare "~a mismatch: ~s != ~s" kind expect v)))]
     [check-parent
      (lambda (menu id)
        (unless use-menubar?
          (unless (eq? (send id get-parent) menu)
            (error 'check-parent "parent mismatch: for ~a, ~a != ~a"
                   (send id get-label)
                   (send menu get-label)
                   (send (send (send id get-parent) get-item) get-label)))))]
     [label-test
      (lambda (menu id expect)
        (check-parent menu id)
        (let ([v (send id get-label)])
          (compare expect v "label")))]
     [top-label-test
      (lambda (pos expect)
        (let ([i (send menu-bar get-items)])
          (and (> (length i) pos)
               (let ([v (send (list-ref i pos) get-label)])
                 (compare expect v "top label")))))]
     [help-string-test
      (lambda (menu id expect)
        (check-parent menu id)
        (let ([v (send id get-help-string)])
          (compare expect v "help string")))]
     [find-test
      (lambda (menu title expect string)
        (letrec ([find
                  (lambda (menu str)
                    (let ([items (send menu get-items)])
                      (ormap (lambda (i)
                               (and (is-a? i labelled-menu-item<%>)
                                    (equal? (send i get-plain-label) str)
                                    i))
                             items)))]
                 [find-item
                  (lambda (menu str)
                    (or (find menu str)
                        (let ([items (send menu get-items)])
                          (ormap (lambda (i)
                                   (and (is-a? i menu%)
                                        (find-item i str)))
                                 items))))]
                 [v (if use-menubar? 
                        (let ([item (find menu-bar title)])
                          (if item
                              (find-item item string)
                              -1))
                        (find-item menu string))])
          (compare expect v (format "label search: ~a" string))))]
     [tell-ok
      (lambda ()
        (printf "ok\n"))])
    (define temp-labels? #f)
    (define use-menubar? #f)
    (define apple-installed? #f)
    (public*
     [via (lambda (menu) (if use-menubar? menu-bar menu))]
     [tmp-pick (lambda (a b) (if temp-labels? a b))]
     [apple-pick (lambda (x a b) (if (and use-menubar? (not apple-installed?))
                                     x
                                     (tmp-pick a b)))])
     (make-menu-bar)

     (send apple-menu restore)

     (make-object button%
                  "Delete Tester" sbp 
                  (lambda args
                    (send main-menu delete)))
     (make-object button%
                  "Delete First Menu" sbp
                  (lambda args
                    (send (car (send menu-bar get-items)) delete)))
     (make-object button%
                  "Add Tester" sbp
                  (lambda args
                    (send main-menu restore)))
     (make-object button%
                  "Add Delete Banana" sbp
                  (lambda args
                    (send DELETE-BANANA restore)))
     (make-object button%
                  "Counts" sbp
                  (lambda args
                    (message-box
                     "Counts"
                     (format "MB: ~a; T: ~a; A: ~a; B: ~a"
                             (length (send menu-bar get-items))
                             (length (send main-menu get-items))
                             (length (send apple-menu get-items))
                             (length (send banana-menu get-items))))))

     (make-test-button "Apple Item" mfbp apple-menu APPLE-CHECK-ID)
     (make-object button%
                  "Check in Apple" mfbp
                  (lambda args
                    (send APPLE-CHECK-ID check #t)))
     (make-object button%
                  "Delete/Restore Check" mfbp
                  (lambda args
                    (if (send APPLE-CHECK-ID is-deleted?)
                        (send APPLE-CHECK-ID restore)
                        (send APPLE-CHECK-ID delete))))
     (make-object button%
                  "Toggle Menubar Enable" mfbp
                  (lambda args
                    (send menu-bar enable (not (send menu-bar is-enabled?)))))
     (make-object button%
                  "Toggle Apple Enable" mfbp
                  (lambda args
                    (send apple-menu enable (not (send apple-menu is-enabled?)))))
       
     (make-object button%
                  "Test Labels" lblp 
                  (lambda args
                    (label-test (via main-menu) ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
                    (help-string-test (via main-menu) ADD-APPLE (tmp-pick "ADDER" "Adds the Apple menu"))
                    (label-test (via apple-menu) DELETE-APPLE (apple-pick #f "Apple Deleter" "Delete Apple"))
                    (help-string-test (via apple-menu) DELETE-APPLE (apple-pick #f "DELETER"
                                                                                "Deletes the Apple menu"))
                    (label-test (via apple-menu) COCONUT-ID (apple-pick #f "Coconut!" "Coconut"))
                    (help-string-test (via apple-menu) COCONUT-ID (apple-pick #f "SUBMENU" "Submenu"))
                    (label-test (via coconut-menu) DELETE-COCONUT (apple-pick #f "Coconut Deleter" "Delete Coconut")) ; submenu test
                    (help-string-test (via coconut-menu) DELETE-COCONUT (apple-pick #f "CDELETER" #f))
                    (top-label-test 0 (if temp-labels? "Hi" "&Tester"))
                    (top-label-test 1 (if apple-installed? "Apple" #f))
                    (tell-ok)))
     (make-object button%
                  "Find Labels" lblp
                  (lambda args
                    (find-test main-menu (tmp-pick "Hi" "&Tester")
                               ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
                    (find-test apple-menu "Apple" (apple-pick -1 DELETE-APPLE DELETE-APPLE)
                               (tmp-pick "Apple Deleter" "Delete Apple"))
                    (find-test apple-menu "Apple" (apple-pick -1 COCONUT-ID COCONUT-ID)
                               (tmp-pick "Coconut!" "Coconut"))
                    (find-test apple-menu "Apple" (apple-pick -1 DELETE-COCONUT DELETE-COCONUT)
                               (tmp-pick "Coconut Deleter" "Delete Coconut"))
                    (tell-ok)))
     (make-object button%
                  "Toggle Labels" lblp
                  (lambda args
                    (set! temp-labels? (not temp-labels?))
                    (let ([menu (via main-menu)])
                      (send ADD-APPLE set-label (tmp-pick "Apple Adder" "Add Apple"))
                      (send DELETE-APPLE set-label (tmp-pick "Apple Deleter" "Delete Apple"))
                      (send COCONUT-ID set-label (tmp-pick "Coconut!" "Coconut"))
                      (send DELETE-COCONUT set-label (tmp-pick "Coconut Deleter" "Delete Coconut"))
                      (send ADD-APPLE set-help-string (tmp-pick "ADDER" "Adds the Apple menu"))
                      (send DELETE-APPLE set-help-string (tmp-pick "DELETER" "Deletes the Apple menu"))
                      (send COCONUT-ID set-help-string (tmp-pick "SUBMENU" "Submenu"))
                      (send DELETE-COCONUT set-help-string (tmp-pick "CDELETER" #f))
                      (send CHINESE set-label (tmp-pick "Chinese: \U7239" "Chinese: \U7238"))
                      (send CHINESE set-shortcut (tmp-pick #\C #\K))
                      (send main-menu set-label (if temp-labels? "Hi" "&Tester")))))
     (letrec ([by-bar (make-object check-box%
                                   "Via Menubar" lblp
                                   (lambda args
                                     (set! use-menubar? (send by-bar get-value))))])
       by-bar)
       
     #f))

(define (menu-frame)
  (define mf (make-frame f% "Menu Test"))
  (set! prev-frame mf)
  (send mf show #t)
  mf)

(define (panel-frame)
  (define make-p% 
    (lambda (panel%)
      (class panel%
        (init parent)
	(override*
	  [container-size
	   (lambda (l)
	     (values (apply + (map car l))
		     (apply + (map cadr l))))]
	  [place-children
	   (lambda (l w h)
	     (let-values ([(mw mh) (container-size l)])
	       (let* ([num-x-stretch (apply + (map (lambda (x) (if (caddr x) 1 0)) l))]
		      [num-y-stretch (apply + (map (lambda (x) (if (cadddr x) 1 0)) l))]
		      [dx (floor (/ (- w mw) num-x-stretch))]
		      [dy (floor (/ (- h mh) num-y-stretch))])
		 (let loop ([l l][r null][x 0][y 0])
		   (if (null? l)
		       (reverse r)
		       (let ([w (+ (caar l) (if (caddr (car l)) dx 0))]
			     [h (+ (cadar l) (if (cadddr (car l)) dy 0))])
			 (loop (cdr l)
			       (cons (list x y w h) r)
			       (+ x w) (+ y h))))))))])
        (super-make-object parent))))
  (define f (make-frame frame% "Panel Tests"))
  (define h (make-object horizontal-panel% f))
  (define kind (begin
		 (send h set-alignment 'center 'top)
		 (make-object radio-box%
			      "Kind"
			      '("Panel" "Pane")
			      h
			      void)))
  (define direction (make-object radio-box%
				 "Direction"
				 '("Horionztal" "Vertical" "Diagonal" "None")
				 h
				 void))
  (define h-align (make-object radio-box%
			       "H Alignment"
			       '("Left" "Center" "Right")
			       h
			       void))
  (define v-align (make-object radio-box%
			       "V Alignment"
			       '("Top" "Center" "Bottom")
			       h
			       void))
  (make-object button% "Make Container" f
	       (lambda (b e) (do-panel-frame
			      (let ([kind (send kind get-selection)]
				    [direction (send direction get-selection)])
				(case kind
				  [(0) (case direction
					 [(0) horizontal-panel%]
					 [(1) vertical-panel%]
					 [(2) (make-p% panel%)]
					 [else panel%])]
				  [(1) (case direction
					 [(0) horizontal-pane%]
					 [(1) vertical-pane%]
					 [(2) (make-p% pane%)]
					 [else pane%])]))
			      (case (send h-align get-selection)
				[(0) 'left]
				[(1) 'center]
				[(2) 'right])
			      (case (send v-align get-selection)
				[(0) 'top]
				[(1) 'center]
				[(2) 'bottom]))))
  (send f show #t))

(define (do-panel-frame p% va ha)
  (define f (make-frame frame% "Container Test"))
  (define p (make-object p% f))
  (define b (make-object button% "Add List or Bad" p
			 (lambda (b e)
			   (send p add-child 
				 (if (send c get-value)
				     m1
				     l)))))
  (define c (make-object check-box% "Remove List" p
			 (lambda (c e)
			   (if (send c get-value)
			       (send p delete-child l)
			       (send p add-child l)))))
  (define l (make-object list-box% "List Box" '("A" "B" "C") p
			 (lambda (l e)
			   (if (eq? (send e get-event-type) 'list-box)
			       (send p get-children)
			       (send p change-children reverse)))))
  (define p2 (make-object vertical-panel% p '(border)))
  (define m1 (make-object message% "1" p2))
  (define m2 (make-object message% "2" p2))
  (send p set-alignment va ha)
  (send f show #t))

(define (check-callback-event orig got e types silent?)
  (unless (eq? orig got)
    (error "object not the same"))
  (unless (is-a? e control-event%)
    (error "bad event object"))
  (let ([type (send e get-event-type)])
    (unless (memq type types)
      (error (format "bad event type: ~a" type))))
  (unless silent?
    (printf "Callback Ok\n")))

(define (instructions v-panel file)
  (define c (make-object editor-canvas% v-panel))
  (define m (make-object text%))
  (send c set-editor m)
  (send m load-file (local-path file))
  (send m lock #t)
  (send c min-width 520)
  (send c min-height 200))

(define (open-file file)
  (define f (make-object frame% file #f 300 300))
  (instructions f file)
  (send f show #t))

(define (button-frame frame% style)
  (define f (make-frame frame% "Button Test"))
  (define p (make-object vertical-panel% f))
  (define old-list null)
  (define commands (list 'button))
  (define hit? #f)
  (define b (make-object button%
			 "Hit Me" p
			 (lambda (bx e)
			   (set! hit? #t)
			   (set! old-list (cons e old-list))
			   (check-callback-event b bx e commands #f))
			 style))
  (define c (make-object button%
			 "Check" p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event b b e commands #t))
			    old-list)
			   (printf "All Ok\n"))))
  (define e (make-object button%
			 "Disable Test" p
			 (lambda (c e)
			   (sleep 1)
			   (set! hit? #f)
			   (let ([sema (make-semaphore)])
			     (send b enable #f)
			     (thread (lambda () (sleep 0.5) (semaphore-post sema)))
			     (yield sema)
			     (when hit?
			       (printf "un-oh\n"))
			     (send b enable #t)))))
  (instructions p "button-steps.txt")
  (send f show #t))

(define (image-button-frame)
  (define f (make-frame frame% "Image Button Test"))
  (define pt (make-object vertical-panel% f))
  (define pm (make-object horizontal-panel% f))
  (define pb (make-object vertical-panel% f))
  (define pc (make-object horizontal-panel% f))
  (define bt (new button% [parent pt]
                  [label (list (read-bitmap
                                (collection-file-path "foot.png" "icons"))
                               "Top"
                               'top)]))
  (define bl (new button% [parent pm]
                  [label (list (read-bitmap
                                (collection-file-path "b-wait.png" "icons"))
                               "Left"
                               'left)]))
  (define br (new button% [parent pm]
                  [label (list (read-bitmap
                                (collection-file-path "b-run.png" "icons"))
                               "Right"
                               'right)]))
  (define bb (new button% [parent pb]
                  [label (list (read-bitmap
                                (collection-file-path "bug09.png" "icons"))
                               "Bottom"
                               'bottom)]))
  (new button% [parent pc]
       [label "Strings"]
       [callback (lambda (b e)
                   (for ([b (in-list (list bt bl br bb))])
                     (send b set-label (list->string
                                        (reverse
                                         (string->list
                                          (cadr (send b get-label))))))))])
  (new button% [parent pc]
       [label "Bitmaps"]
       [callback (lambda (b e)
                   (for ([b (in-list (list bt bl br bb))])
                     (send b set-label (let ([bm (car (send b get-label))])
                                         (let* ([bm2 (make-bitmap (send bm get-width)
                                                                  (send bm get-height))]
                                                [dc (make-object bitmap-dc% bm2)])
                                           (send dc scale 1 -1)
                                           (send dc translate 0 (send bm get-height))
                                           (send dc draw-bitmap bm 0 0)
                                           (send dc set-bitmap #f)
                                           bm2)))))])
  (send f show #t))

(define (checkbox-frame)
  (define f (make-frame frame% "Checkbox Test"))
  (define p f)
  (define old-list null)
  (define commands (list 'check-box))
  (define cb (make-object check-box%
			  "On" p
			  (lambda (cx e)
			    (set! old-list (cons e old-list))
			    (check-callback-event cb cx e commands #f))))
  (define t (make-object button%
			 "Toggle" p
			 (lambda (t e)
			   (let ([on? (send cb get-value)])
			     (send cb set-value (not on?))))))
  (define t2 (make-object button%
			  "Simulation Toggle" p
			  (lambda (t e)
			    (let ([on? (send cb get-value)]
				  [e (make-object control-event% 'check-box)])
			      (send cb set-value (not on?))
			      (send cb command e)))))
  (define c (make-object button%
			 "Check" p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event cb cb e commands #t))
			    old-list)
			   (printf "All Ok\n"))))
  (instructions p "checkbox-steps.txt")
  (send f show #t))

(define (radiobox-frame)
  (define f (make-frame frame% "Radiobox Test"))
  (define p f)
  (define old-list null)
  (define commands (list 'radio-box))
  (define hp (make-object horizontal-panel% p))
  (define _ (send hp stretchable-height #f))
  (define callback (lambda (rb e)
		     (set! old-list (cons (cons rb e) old-list))
		     (check-callback-event rb rb e commands #f)))
  (define rb1-l (list "Singleton"))
  (define rb1 (make-object radio-box% "&Left" rb1-l hp callback))
  (define rb2-l (list "First" "Last"))
  (define rb2 (make-object radio-box% "&Center" rb2-l hp callback))
  (define rb3-l (list "&Top" "&Middle" "&Bottom"))
  (define rb3 (make-object radio-box% "&Right" rb3-l hp callback))

  (define rbs (list rb1 rb2 rb3))
  (define rbls (list rb1-l rb2-l rb3-l))
  (define normal-sel (lambda (rb p) (send rb set-selection p)))
  (define simulate-sel (lambda (rb p)
			 (let ([e (make-object control-event% 'radio-box)])
			   (send rb set-selection p)
			   (send rb command e))))
  (define (mk-err exn?)
    (lambda (f)
      (lambda (rb p)
	(with-handlers ([exn? void])
	  (f rb p)
	  (error "no exn raisd")))))
  (define type-err (mk-err exn:fail:contract?))
  (define mismatch-err (mk-err exn:fail:contract?))

  (define do-sel (lambda (sel n) (for-each (lambda (rb) (sel rb (n rb))) rbs)))
  (define sel-false (lambda (sel) (do-sel sel (lambda (rb) #f))))
  (define sel-minus (lambda (sel) (do-sel (type-err sel) (lambda (rb) -1))))
  (define sel-first (lambda (sel) (do-sel sel (lambda (rb) 0))))
  (define sel-middle (lambda (sel) (do-sel sel (lambda (rb) (floor (/ (send rb get-number) 2))))))
  (define sel-last (lambda (sel) (do-sel sel (lambda (rb) (sub1 (send rb get-number))))))
  (define sel-N (lambda (sel) (do-sel (mismatch-err sel) (lambda (rb) (send rb get-number)))))
  (define (make-selectors title sel)
    (define hp2 (make-object horizontal-panel% p))
    (send hp2 stretchable-height #f)
    (make-object button% (format "Select -1~a" title) hp2 (lambda (b e) (sel-minus sel)))
    (make-object button% (format "Select First~a" title) hp2 (lambda (b e) (sel-first sel)))
    (make-object button% (format "Select Middle ~a" title) hp2 (lambda (b e) (sel-middle sel)))
    (make-object button% (format "Select Last~a" title) hp2 (lambda (b e) (sel-last sel)))
    (make-object button% (format "Select N~a" title) hp2 (lambda (b e) (sel-N sel)))
    (when (equal? title "")
      (make-object button% (format "Select #f~a" title) hp2 (lambda (b e) (sel-false sel)))))
  (make-selectors "" normal-sel)
  (make-selectors " by Simulate" simulate-sel)
  (make-object button% "Check" p
	       (lambda (c e)
		 (for-each
		  (lambda (rb l)
		    (let loop ([n 0][l l])
		      (unless (null? l)
			(let ([a (car l)]
			      [b (send rb get-item-label n)])
			  (unless (string=? a b)
			    (error "item name mismatch: ~s != ~s" a b)))
			(loop (add1 n) (cdr l)))))
		  rbs rbls)
		 (for-each
		  (lambda (rbe)
		    (check-callback-event (car rbe) (car rbe) (cdr rbe) commands #t))
		  old-list)
		 (printf "All Ok\n")))
  (instructions p "radiobox-steps.txt")
  (send f show #t))

(define (choice-or-list-frame list? list-style empty? 
                              [columns '("Choice")] [more-styles '()] [column-order #f])
  (define f (make-frame frame% (if list? "List Test" "Choice Test")))
  (define p f)
  (define-values (actual-content actual-user-data)
    (if empty?
	(values null null)
	(values '("Alpha" "Beta" "Gamma")
		(list #f #f #f))))
  (define commands 
    (if list?
	(append (list 'list-box 'list-box-dclick)
                (if (memq 'clickable-headers more-styles)
                    (list 'list-box-column)
                    null))
	(list 'choice)))
  (define old-list null)
  (define multi? (or (memq 'multiple list-style)
		     (memq 'extended list-style)))
  (define callback
    (lambda (cx e)
      (when (zero? (send c get-number))
	    (error "Callback for empty choice/list"))
      (set! old-list (cons e old-list))
      (cond
       [(eq? (send e get-event-type) 'list-box-dclick)
	; double-click
	(printf "Double-click\n")
	(unless (send cx get-selection)
	  (error "no selection for dclick"))]
       [(eq? (send e get-event-type) 'list-box-column)
        (printf "Column: ~a\n" (send e get-column))]
       [else
	; misc multi-selection
	(printf "Changed: ~a\n" (if list?
				    (send cx get-selections)
				    (send cx get-selection)))])
      (check-callback-event c cx e commands #f)))
  (define c (if list?
		(new list-box% [label "Tester"]
                     [choices actual-content]
                     [parent p]
                     [callback callback]
                     [style (append list-style more-styles)]
                     [columns columns]
                     [column-order column-order])
		(make-object choice% "Tester" actual-content p callback)))
  (define counter 0)
  (define append-with-user-data? #f)
  (define ap (new horizontal-panel% [parent p]
                  [stretchable-width #f]
                  [stretchable-height #f]))
  (define ab (make-object button%
			  "Append" ap
			  (lambda (b e)
			    (set! counter (add1 counter))
			    (let ([naya (format "~aExtra ~a" 
						(if (= counter 10)
						    (string-append
						     "This is a Really Long Named Item That Would Have Used the Short Name, Yes "
						     "This is a Really Long Named Item That Would Have Used the Short Name ")
						    "")
						counter)]
				  [naya-data (box 0)])
			      (set! actual-content (append actual-content (list naya)))
			      (set! actual-user-data (append actual-user-data (list naya-data)))
			      (if (and list? append-with-user-data?)
				  (send c append naya naya-data)
				  (begin
				    (send c append naya)
				    (when list?
					  (send c set-data 
						(sub1 (send c get-number))
						naya-data))))
			      (set! append-with-user-data?
				    (not append-with-user-data?))))))
  (new button% 
       [label "Add Column"]
       [parent ap]
       [callback (lambda (b e)
                   (let ([s (format "New ~a" (length columns))])
                     (send c append-column s)
                     (set! columns (append columns (list s)))))])
  (new button% 
       [label "Delete Right Column"]
       [parent ap]
       [callback (lambda (b e)
                   (let ([pos (last (send c get-column-order))])
                     (send c delete-column pos)
                     (set! columns (send c get-column-labels))))])
  (when list? 
    (let ([hp (new horizontal-panel% 
                   [parent p]
                   [stretchable-width #f]
                   [stretchable-height #f])])
      (make-object button%
                   "Visible Indices" hp
                   (lambda (b e)
                     (printf "top: ~a\nvisible count: ~a\n"
                             (send c get-first-visible-item)
                             (send c number-of-visible-items))))
      (define (mk which pos)
        (new button% [label (format "Set ~a Top" which)]
             [parent hp]
             [callback (lambda (b e) (send c set-first-visible-item pos))]))
      (mk "First" 0)
      (mk "Third" 2)
      (mk "Tenth" 9)
      (new button% [label "Reverse Columns"]
           [parent hp]
           [callback (lambda (b e) (send c set-column-order (reverse (send c get-column-order))))])
      (new button% [label "Set Column Label"]
           [parent hp]
           [callback (lambda (b e) 
                       (send c set-column-label (sub1 (length columns)) "Last")
                       (send c set-column-label 0 "First"))])
      (new button% [label "Set Column Size"]
           [parent hp]
           [callback (lambda (b e) 
                       (send c set-column-size 0 50 10 100)
                       (unless (= 1 (length columns))
                         (let-values ([(w mn mx) (send c get-column-size 0)])
                           (send c set-column-size (sub1 (length columns)) w mn mx))))])))
  (define cdp (new horizontal-panel% [parent p]
                   [stretchable-height #f]))
  (define rb (make-object button% "Clear" cdp
			  (lambda (b e)
			    (set! actual-content null)
			    (set! actual-user-data null)
			    (send c clear))))
  (define (gone l n)
    (if (zero? n)
        (cdr l)
        (cons (car l) (gone (cdr l) (sub1 n)))))
  (define (delete p)
    (send c delete p)
    (when (<= 0 p (sub1 (length actual-content)))
      (set! actual-content (gone actual-content p))
      (set! actual-user-data (gone actual-user-data p))))
  (define db (make-object button%
                          "Delete" cdp
                          (lambda (b e)
                            (let ([p (send c get-selection)])
                              (delete p)))))
  (define dab (if list?
		  (make-object button%
			       "Delete Above" cdp
			       (lambda (b e)
				 (let ([p (send c get-selection)])
				   (delete (sub1 p)))))
		  null))
  (define dbb (if list?
		  (make-object button%
			       "Delete Below" cdp
			       (lambda (b e)
				 (let ([p (send c get-selection)])
				   (delete (add1 p)))))
		  null))
  (define setb (if list?
		   (make-object button%
				"Reset" cdp
				(lambda (b e)
                                  (let ([extras (for/list ([in-list (cdr columns)]
                                                           [col (in-naturals 1)])
                                                  (for/list ([i (in-range 3)])
                                                    (format "~a, ~a" col i)))])
                                    (send c set '("Alpha" "Beta" "Gamma") . extras))
				  (set! actual-content '("Alpha" "Beta" "Gamma"))
				  (set! actual-user-data (list #f #f #f))))
		   null))
  (define sel (if list?
		  (make-object button%
			       "Add Select First" cdp
			       (lambda (b e)
				 (send c select 0 #t)))
		  null))
  (define unsel (if list?
		    (make-object button%
				 "Unselect" cdp
				 (lambda (b e)
				   (send c select (send c get-selection) #f)))
		    null))
  (define change-button-name (if list? 
                                 (make-object button%
                                              "Change Name" cdp
                                              (lambda (b e)
						(let ([p (send c get-selection)])
						  (when p
						    (send c set-string p "New Name")
                                                    (for ([in-list (cdr columns)]
                                                          [col (in-naturals 1)])
                                                      (send c set-string p 
                                                            (format "new ~a" col)
                                                            col))
						    (set! actual-content
							  (let loop ([ac actual-content][p p])
							    (if (zero? p)
								(cons "New Name" (cdr ac))
								(cons (car ac)
								      (loop (cdr ac) (sub1 p))))))))))
                                 null))
  (define (make-selectors method mname numerical?)
    (define p2 (make-object horizontal-panel% p))
    (send p2 stretchable-height #f)
    (when numerical?
	  (make-object button%
		       (string-append "Select Bad -1" mname) p2
		       (lambda (b e)
			 (with-handlers ([exn:fail:contract? void])
			   (method -1)
			   (error "expected a type exception")))))
    (make-object button%
		 (string-append "Select First" mname) p2
		 (lambda (b e)
		   (method 0)))
    (make-object button%
		 (string-append "Select Middle" mname) p2
		 (lambda (b e)
		   (method (floor (/ (send c get-number) 2)))))
    (make-object button%
		 (string-append  "Select Last" mname) p2
		 (lambda (b e)
		   (method (sub1 (send c get-number)))))
    (make-object button%
		 (string-append "Select Bad X" mname) p2
		 (lambda (b e)
		   (with-handlers ([exn:fail:contract? void]) 
		     (method (if numerical?
				 (send c get-number)
				 #f))
		     (error "expected a mismatch exception"))))
    #f)
  (define dummy-1 (make-selectors (lambda (v) (send c set-selection v)) "" #t))
  (define dummy-2 (make-selectors (lambda (p) 
				    (if p
					(when (positive? (length actual-content))
					      (send c set-string-selection 
						    (list-ref actual-content p)))
					(send c set-string-selection "nada")))
				  " by Name"
				  #f))
  (define dummy-3 (make-selectors (lambda (p)
				    (let ([e (make-object control-event% (if list? 'list-box 'choice))])
				      (send c set-selection p)
				      (when list? (send c set-first-visible-item p))
				      (send c command e)))
				  " by Simulate" #t))
  (define tb (make-object button%
			  "Check" p
			  (lambda (b e)
			    (let ([c (send c get-number)])
			      (unless (= c (length actual-content))
				(error "bad number response")))
			    (let loop ([n 0][l actual-content][lud actual-user-data])
			      (unless (null? l)
				      (let ([s (car l)]
					    [sud (car lud)]
					    [sv (send c get-string n)]
					    [sudv (if list?
						      (send c get-data n)
						      #f)])
					(unless (string=? s sv)
					  (error "get-string mismatch"))
					(unless (or (not list?) (eq? sud sudv))
					  (error 'get-data "mismatch at ~a: ~s != ~s"
						 n sud sudv))
					(unless (= n (send c find-string s))
					  (error "bad find-string result")))
				      (loop (add1 n) (cdr l) (cdr lud))))
			    (let ([bad (lambda (exn? i)
					 (with-handlers ([exn? void])
					   (send c get-string i)
					   (error "out-of-bounds: no exn")))])
			      (bad exn:fail:contract? -1)
			      (bad exn:fail:contract? (send c get-number)))
			    (unless (not (send c find-string "nada"))
			      (error "find-string of nada wasn't #f"))
			    (for-each
			     (lambda (e)
			       (check-callback-event c c e commands #t))
			     old-list)
			    (printf "content: ~s\n" actual-content)
			    (when multi?
			      (printf "selections: ~s\n" (send c get-selections))))))
  (send c stretchable-width #t)
  (instructions p "choice-list-steps.txt")
  (send f show #t))

(define (combo-frame empty?)
  (define f (make-frame frame% "Combo Test"))
  (define p f)
  (define actual-content (if empty?
                             null
                             '("Apple" "Banana")))
  (define (callback c e) (void))
  (define c (make-object (class combo-field% 
                           (define/override (on-popup e)
                             (printf "Popup!\n"))
                           (super-new))
                         "Tester" actual-content p callback))
  (define counter 0)
  (define append-with-user-data? #f)
  (define ab (make-object button%
			  "Append" p
			  (lambda (b e)
			    (set! counter (add1 counter))
			    (let ([naya (format "~aExtra ~a" 
						(if (= counter 10)
						    (string-append
						     "This is a Really Long Named Item That Would Have Used the Short Name, Yes "
						     "This is a Really Long Named Item That Would Have Used the Short Name ")
						    "")
						counter)]
				  [naya-data (box 0)])
			      (set! actual-content (append actual-content (list naya)))
			      (send c append naya)))))
  (define asb (make-object button%
                           "Append Separator" p
                           (lambda (b e)
                             (set! counter (add1 counter))
                             (new separator-menu-item% [parent (send c get-menu)]))))
  (define cdp (make-object horizontal-panel% p))
  (define (clear)
    (for ([i (send (send c get-menu) get-items)])
      (send i delete)))
  (define rb (make-object button% "Clear" cdp
                          (lambda (b e) (clear))))
  (define (gone l n)
    (if (zero? n)
        (cdr l)
        (cons (car l) (gone (cdr l) (sub1 n)))))
  (define (delete p)
    (send (list-ref (send (send c get-menu) get-items) p) delete)
    (when (<= 0 p (sub1 (length actual-content)))
      (set! actual-content (gone actual-content p))))
  (define db (make-object button%
                          "Delete First" cdp
                          (lambda (b e)
                            (unless (null? actual-content)
                              (delete 0)))))
  (define dbe (make-object button%
                           "Delete Last" cdp
                           (lambda (b e)
                             (unless (null? actual-content)
                               (delete (sub1 (length actual-content)))))))
  (define setb (make-object button%
                            "Reset" cdp
                            (lambda (b e)
                              (clear)
                              (let ([m (send c get-menu)])
                                (for ([i '("Alpha" "Beta" "Gamma")])
                                  (new menu-item% [parent m] [label i]
                                       [callback (lambda (itm e)
                                                   (send c set-value
                                                         (format "~a from Reset" i)))]))))))
  (define tb (make-object button%
			  "Check" p
			  (lambda (b e)
                            (void))))
  (send c stretchable-width #t)
  (instructions p "combo-steps.txt")
  (send f show #t))

(define (slider-frame style)
  (define f (make-frame frame% "Slider Test"))
  (define p (make-object vertical-panel% f))
  (define old-list null)
  (define commands (list 'slider))
  (define s (make-object slider% "Slide Me" -1 11 p
			 (lambda (sl e)
			   (check-callback-event s sl e commands #f)
			   (printf "slid: ~a\n" (send s get-value)))
			 3
                         (cons 'horizontal style)))
  (define c (make-object button% "Check" p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event s s e commands #t))
			    old-list)
			   (printf "All Ok\n"))))
  (define (simulate v)
    (let ([e (make-object control-event% 'slider)])
      (send s set-value v)
      (send s command e)))
  (define p2 (make-object horizontal-panel% p))
  (define p3 (make-object horizontal-panel% p))
  (send p3 stretchable-height #f)
  (make-object button%
	       "Up" p2
	       (lambda (c e)
		 (send s set-value (add1 (send s get-value)))))
  (make-object button%
	       "Down" p2
	       (lambda (c e)
		 (send s set-value (sub1 (send s get-value)))))
  (make-object button%
	       "Simulate Up" p2
	       (lambda (c e)
		 (simulate (add1 (send s get-value)))))
  (make-object button%
	       "Simulate Down" p2
	       (lambda (c e)
		 (simulate (sub1 (send s get-value)))))
  (make-object check-box%
	       "Disabled" p2
	       (lambda (c e)
		 (send s enable (not (send c get-value)))))
  (instructions p "slider-steps.txt")
  (send f show #t))

(define (gauge-frame)
  (define f (make-frame frame% "Gauge Test"))
  (define p (make-object vertical-panel% f))
  (define g (make-object gauge% "Tester" 10 p))
  (define (move d name)
    (make-object button%
		 name p
		 (lambda (c e)
		   (send g set-value (+ d (send g get-value))))))
  (define (size d name)
    (make-object button%
		 name p
		 (lambda (c e)
		   (send g set-range (+ d (send g get-range))))))
  (move 1 "+")
  (move -1 "-")
  (size 1 "Bigger")
  (size -1 "Smaller")
  (instructions p "gauge-steps.txt")
  (send f show #t))

(define (text-frame style)
  (define (handler get-this)
    (lambda (c e)
      (unless (eq? c (get-this))
	(printf "callback: bad item: ~a\n" c))
      (let ([t (send e get-event-type)])
	(cond
	 [(eq? t 'text-field)
	  (printf "Changed: ~a\n" (send c get-value))]
	 [(eq? t 'text-field-enter)
	  (printf "Return: ~a\n" (send c get-value))]))))

  (define f (make-frame frame% "Text Test"))
  (define p (make-object vertical-panel% f))
  (define t1 (make-object text-field% #f p (handler (lambda () t1)) "This should just fit!" style))
  (define t2 (make-object text-field% "Another" p (handler (lambda () t2)) "This too!" style))
  (define t3 (make-object text-field% "Catch Returns" p (handler (lambda () t3)) "And, yes, this!"
			  (cons 'hscroll style)))
  (send t1 stretchable-width #f)
  (send t2 stretchable-width #f)
  (send t3 stretchable-width #f)
  (send f show #t))

(define (canvas-frame flags)
  (define f (make-frame frame% "Canvas Test" #f #f 250))
  (define p (make-object vertical-panel% f))
  (define c% (class canvas%
               (init -name -swapped-name p)
	       (inherit get-dc get-scroll-pos get-scroll-range get-scroll-page
			get-client-size get-virtual-size get-view-start)
	       (define name -name)
               (define swapped-name -swapped-name)
               (define auto? #f)
               (define incremental? #f)
               (define vw 10)
               (define vh 10)
	       (public*
		 [inc-mode (lambda (x) (set! incremental? x))]
		 [set-vsize (lambda (w h) (set! vw w) (set! vh h))])
	       (override*
		[on-paint
		 (lambda ()
		   (let ([s (format "V: p: ~s r: ~s g: ~s H: ~s ~s ~s"
				    (get-scroll-pos 'vertical)
				    (get-scroll-range 'vertical)
				    (get-scroll-page 'vertical)
				    (get-scroll-pos 'horizontal)
				    (get-scroll-range 'horizontal)
				    (get-scroll-page 'horizontal))]
                         [dc (get-dc)])
		     (let-values ([(w h) (get-client-size)]
				  [(w2 h2) (get-virtual-size)]
				  [(x y) (get-view-start)])
		       ; (send dc set-clipping-region 0 0 w2 h2)
		       (unless incremental? (send dc clear))
		       (send dc draw-text (if (send ck-w get-value) swapped-name name) 3 3)
		       ; (draw-line 3 12 40 12)
		       (send dc draw-text s 3 15)
		       (send dc draw-text (format "client: ~s x ~s  virtual: ~s x ~s  view: ~s x ~s" 
						  w h
						  w2 h2
						  x y)
			     3 27)
		       (send dc draw-line 0 vh vw vh)
		       (send dc draw-line vw 0 vw vh))))]
		[on-event
		 (lambda (e)
		   (let ([s (format "~a ~a" (send e get-x) (send e get-y))])
		     (send f set-status-text s)))]
		[on-scroll
		 (lambda (e) 
		   (when auto? (printf "Hey - on-scroll called for auto scrollbars\n"))
		   (unless incremental? (on-paint)))]
		[init-auto-scrollbars (lambda x
					(set! auto? #t)
					(super init-auto-scrollbars . x))]
		[init-manual-scrollbars (lambda x
					  (set! auto? #f)
					  (super init-manual-scrollbars . x))])
               (super-make-object p flags)))
  (define un-name "Unmanaged scroll")
  (define m-name "Automanaged scroll")
  (define c1 (make-object c% un-name m-name p))
  (define c2 (make-object c% m-name un-name p))
  (define (reset-scrolls for-small?)
    (let* ([h? (send ck-h get-value)]
	   [v? (send ck-v get-value)]
	   [small? (send ck-s get-value)]
	   [swap? (send ck-w get-value)])
      (send c1 set-vsize 10 10)
      (if swap?
	  (send c1 init-auto-scrollbars (and h? 10) (and v? 10) .1 .1)
	  (send c1 init-manual-scrollbars (and h? 10) (and v? 10) 3 3 1 1))
      ; (send c1 set-scrollbars (and h? 1) (and v? 1) 10 10 3 3 1 1 swap?)
      (send c2 set-vsize (if small? 50 500) (if small? 20 200))
      (if swap?
	  (send c2 init-manual-scrollbars (if small? 2 20) (if small? 2 20) 3 3 1 1)
	  (send c2 init-auto-scrollbars (and h? (if small? 50 500)) (and v? (if small? 20 200)) .2 .2))
      ; (send c2 set-scrollbars (and h? 25) (and v? 10) (if small? 2 20) (if small? 2 20) 3 3 1 1 (not swap?))
      (if for-small?
	  ; Specifically refresh the bottom canvas
	  (send c2 refresh)
	  ; Otherwise, we have to specifically refresh the unmanaged canvas
	  (send (if swap? c2 c1) refresh))))
  (define (reset-show)
    (for-each
     (lambda (c)
       (send c show-scrollbars 
	     (and (not (send sh-h get-value)) (memq 'hscroll flags))
	     (and (not (send sh-v get-value)) (memq 'vscroll flags))))
     (list c1 c2)))
  (define p2 (make-object horizontal-panel% p))
  (define junk (send p2 stretchable-height #f))
  (define ck-v (make-object check-box% "Vertical Scroll" p2 (lambda (b e) (reset-scrolls #f))))
  (define ck-h (make-object check-box% "Horizontal Scroll" p2 (lambda (b e) (reset-scrolls #f))))
  (define ck-s (make-object check-box% "Small" p2 (lambda (b e) (reset-scrolls #t))))
  (define ck-w (make-object check-box% "Swap" p2 (lambda (b e) (reset-scrolls #f))))
  (define p3 (make-object horizontal-panel% p))
  (define junk2 (send p3 stretchable-height #f))
  (define sh-v (make-object check-box% "Hide Vertical" p3 (lambda (b e) (reset-show))))
  (define sh-h (make-object check-box% "Hide Horizontal" p3 (lambda (b e) (reset-show))))
  (define ip (make-object horizontal-panel% p))
  (send ip stretchable-height #f)
  (make-object button%
	       "Get Instructions" ip
	       (lambda (b e) (open-file "canvas-steps.txt")))
  (make-object button%
	       "&1/5 Scroll" ip
	       (lambda (b e) (send c2 scroll 0.2 0.2)))
  (make-object button%
	       "&4/5 Scroll" ip
	       (lambda (b e) (send c2 scroll 0.8 0.8)))
  (make-object check-box%
	       "Inc" ip
	       (lambda (c e) 
		 (send c1 inc-mode (send c get-value))
		 (send c2 inc-mode (send c get-value))))
  (make-object check-box%
	       "x2" ip
	       (lambda (c e) 
		 (let ([s (if (send c get-value)
			      2
			      1)])
		   (send (send c2 get-dc) set-scale s s)
		   (send c2 refresh))))
  (send c1 set-vsize 10 10)
  (send c2 set-vsize 500 200)
  (send f create-status-line)
  (send f show #t))

(define (no-clear-canvas-frame)
  (define f (new frame% 
		 [label "No-Clear Canvas Test"]
		 [height 250]
		 [width 300]
		 [style (add-frame-style null)]))
  (define p (make-object vertical-panel% f))
  (define c% (class canvas%
               (inherit get-dc refresh)
	       (define delta 0)
	       (define/override (on-paint)
		 (let ([red (send the-brush-list find-or-create-brush "RED" 'solid)]
		       [blue (send the-brush-list find-or-create-brush "BLUE" 'solid)]
		       [dc (get-dc)])
		   (let loop ([x 0])
		     (unless (= x 500)
		       (send dc set-brush red)
		       (send dc draw-rectangle (- x delta) 0 25 30)
		       (send dc draw-rectangle (- x delta) 40 25 390)
		       (send dc set-brush blue)
		       (send dc draw-rectangle (- (+ x 25) delta) 0 25 30)
		       (send dc draw-rectangle (- (+ x 25) delta) 40 25 390)
		       (loop (+ x 50))))))
	       (define/override (on-event evt)
		 (when (send evt dragging?)
		   (set! delta (modulo (add1 delta) 100))
		   (refresh)))
	       (super-new)))
  (new c% [parent p][style '(border)])
  (new c% [parent p][style '(transparent)])
  (new c% [parent p][style '(no-autoclear border)])
  (send f show #t)
  f)

(define (editor-frame canvas-style canvas-bg)
  (define f (new frame% 
		 [label "No-Clear Canvas Test"]
		 [height 250]
		 [width 300]
		 [style (add-frame-style null)]))
  (define c (new editor-canvas%
		 [parent f]
		 [style canvas-style]))
  (define mb (make-object menu-bar% f))
  (define edit-menu (make-object menu% "Edit" mb))
  (define font-menu (make-object menu% "Font" mb))

  (when canvas-bg
    (send c set-canvas-background (make-object color% canvas-bg)))
  (send c set-editor (new text%))

  (append-editor-operation-menu-items edit-menu #f)
  (append-editor-font-menu-items font-menu)

  (send f show #t))

(define (editor-canvas-oneline-frame)
  (define f (make-frame frame% "x" #f 200 #f))
  
  (define (try flags)
    (define c (make-object editor-canvas% f #f flags))
    
    (define e (make-object text%))
    
    (send e insert "Xy!")
    
    (send c set-line-count 1)
    
    (send c set-editor e)
    (send c stretchable-height #f))

  (send f show #t)
  
  (try '(no-hscroll no-vscroll))
  (try '(no-vscroll))
  (try '(no-hscroll))
  (try '()))

(define (minsize-frame)
  (define f (make-frame frame% "x"))
  
  (define bp (make-object horizontal-panel% f))
  (define tb (make-object button% "Toggle Stretch" bp
			  (lambda (b e)
			    (for-each
			     (lambda (p)
			       (send p stretchable-width (not (send p stretchable-width)))
			       (send p stretchable-height (not (send p stretchable-height))))
			     containers))))
  (define ps (make-object button% "Print Sizes" bp
			  (lambda (b e)
			    (newline)
			    (for-each
			     (lambda (p)
			       (let ([c (car (send p get-children))])
				 (let-values ([(w h) (send c get-size)]
					      [(cw ch) (send c get-client-size)])
				   (printf "~a: (~a x ~a) client[~a x ~a] diff<~a x ~a> min{~a x ~a}\n"
					   c w h cw ch
					   (- w cw) (- h ch)
					   (send c min-width) (send c min-height)))))
			     (reverse containers))
			    (newline))))
  
  (define containers null)

  (define (make-container p)
    (let ([p (make-object vertical-panel% p '())])
      (send p stretchable-width #f)
      (send p stretchable-height #f)
      (set! containers (cons p containers))
      p))
  
  (define hp0 (make-object horizontal-panel% f))

  (define p (make-object panel% (make-container hp0)))
  (define pb (make-object panel% (make-container hp0) '(border)))

  (define hp1 (make-object horizontal-panel% f))

  (define c (make-object canvas% (make-container hp1)))
  (define cb (make-object canvas% (make-container hp1) '(border)))
  (define ch (make-object canvas% (make-container hp1) '(hscroll)))
  (define cv (make-object canvas% (make-container hp1) '(vscroll)))
  (define chv (make-object canvas% (make-container hp1) '(hscroll vscroll)))
  (define cbhv (make-object canvas% (make-container hp1) '(border hscroll vscroll)))

  (define hp2 (make-object horizontal-panel% f))

  (define ec (make-object editor-canvas% (make-container hp2) #f '(no-hscroll no-vscroll)))
  (define ech (make-object editor-canvas% (make-container hp2) #f '(no-vscroll)))
  (define ecv (make-object editor-canvas% (make-container hp2) #f '(no-hscroll)))
  (define echv (make-object editor-canvas% (make-container hp2) #f '()))

  (define hp3 (make-object horizontal-panel% f))
  
  (define pec (make-object editor-canvas% (make-container hp3) #f '(no-border no-hscroll no-vscroll)))
  (define pech (make-object editor-canvas% (make-container hp3) #f '(no-border no-vscroll)))
  (define pecv (make-object editor-canvas% (make-container hp3) #f '(no-border no-hscroll)))
  (define pechv (make-object editor-canvas% (make-container hp3) #f '(no-border )))

  (define hp4 (make-object horizontal-panel% f))
  (define chvh (make-object canvas% (make-container hp4) '(border hscroll vscroll)))
  (define chvv (make-object canvas% (make-container hp4) '(border hscroll vscroll)))
  (send chvh show-scrollbars #t #f)
  (send chvv show-scrollbars #f #t)

  (send f show #t))

;----------------------------------------------------------------------

(define (test-tab-panel no-border?)
  (define f (make-object frame% "Tabby"))
  (define p (make-object tab-panel% '("App&le" "B&anana" "Co&conut") 
			 f
			 (lambda (p e)
			   (send m set-label (format "Selected: ~a" (send p get-selection))))
			 (if no-border?
			     '(no-border)
			     '())))
  (define p2 (if no-border?
		 (new vertical-panel% [parent f])
		 p))
  (define count 3)
  (define on? #t)
  (define m (make-object message% (format "Selected: ~a" (send p get-selection)) p2))

  (when no-border?
    (make-object vertical-pane% p2))

  (make-object button% "Append" p2 (lambda (b e) 
				    (send p append (format "N&ew ~a" count))
				    (set! count (add1 count))))
  (make-object button% "Delete" p2 (lambda (b e)
				     (send p delete 0)))
  (make-object button% "First" p2 (lambda (b e)
				    (send p set-selection 0)))
  (make-object button% "Last" p2 (lambda (b e)
				   (send p set-selection (sub1 (send p get-number)))))
  (make-object button% "Rename" p2 (lambda (b e)
				     (send p set-item-label (quotient (send p get-number) 2) "Do&nut")))
  (make-object button% "Labels" p2 (lambda (b e)
				     (printf "~s\n"
					     (reverse
					      (let loop ([i (send p get-number)])
						(if (zero? i)
						    null
						    (cons (send p get-item-label (sub1 i)) (loop (sub1 i)))))))))
  (make-object button% "Set" p2 (lambda (b e)
				  (send p set '("New One" "New Second" "New Third"))))
  (when no-border?
    (make-object button% "Toggle" p2 (lambda (b e)
				       (if on?
					   (send f delete-child p)
					   (send f change-children
						 (lambda (l) (cons p l))))
				       (set! on? (not on?)))))

  (send f show #t))

;----------------------------------------------------------------------

(define (test-modified-frame)
  (define f (new (class frame% 
		   (define/override (on-toolbar-button-click)
		     (send f modified (not (send f modified))))
		   (super-make-object))
		 [label "Modifiable"]
		 [style '(toolbar-button)]))

  (make-object button% "Toggle" f (lambda (b e)
				    (send f on-toolbar-button-click)))
  (make-object message% "Mac OS X: toolbar button also toggles" f)
  (send f show #t))

;----------------------------------------------------------------------

(define (message-boxes parent)
  (define (check expected got)
    (unless (eq? expected got)
      (eprintf "bad result: - expected ~e, got ~e\n" expected got)))
  (define (big s)
    (format "~a\n~a\n~a\n~a\n" s
            (make-string 500 #\x)
            (make-string 500 #\x)
            (make-string 500 #\x)))

  (check 'ok (message-box "Title" "Message OK!" parent '(ok)))
  (check 'ok (message-box "Title" (big "Message OK!") parent '(ok)))

  (check 'cancel (message-box "Title" "Cancel Me" parent '(ok-cancel)))
  (check 'ok (message-box "Title" "Ok Me" parent '(ok-cancel)))
  (check 'ok (message-box "Title" (big "Ok Me") parent '(ok-cancel)))

  (check 'yes (message-box "Title" "Yes, please" parent '(yes-no)))
  (check 'no (message-box "Title" "No, please" parent '(yes-no)))

  (check 'yes (message-box "Title" "Caution sign?" parent '(yes-no caution)))
  (check 'yes (message-box "Title" "Stop sign?" parent '(yes-no stop)))

  (check 1 (message-box/custom "Title" "Hello"
				"Hi" #f #f
				parent
				'(default=1)))
  (check 2 (message-box/custom "Title" "Hello"
			       #f "Howdy" #f
			       parent))
  (check 3 (message-box/custom "Title" "Hello (response should be on left for Mac OS)"
				#f #f "Howdy"
				parent))
  (check #f (message-box/custom "Title" "Escape to close, please"
				 "Hi" #f #f
				 parent))
  (check 'closed (message-box/custom "Title" "Escape to close, again, please"
				     "Hi" #f #f
				     parent
				     '(default=1)
				     'closed))
  (check 'closed (message-box/custom "Title" "Escape to close, again, please"
				     #f #f #f
				     parent
				     '(default=1)
				     'closed))
  (check 1 (message-box/custom "Title" "Try to escape to close"
				"I can't" #f #f
				parent
				'(default=1 disallow-close)
				'closed))

  (message-box/custom "Title" "Buttons out of order in Mac OS"
		       "One" "Two" "Three"
		       parent)
  (message-box/custom "Title" "Buttons in order on all platforms"
		       "One" "Two" "Three"
		       parent
		       '(default=1 number-order))
		       
  )

;----------------------------------------------------------------------

(define (cursors)
  (define f (make-object frame% "Cursors"))
  (for-each (lambda (s)
	      (make-object button%
			   (format "~a" s)
			   f
			   (lambda (b e)
			     (send f set-cursor (make-object cursor% s)))))
	    '(arrow bullseye cross hand ibeam watch blank size-n/s size-e/w size-ne/sw size-nw/se))
  (send f show #t))

;----------------------------------------------------------------------

(define (mouse)
  (define f (new frame% 
                 [label "Mouse"]
                 [width 300]
                 [height 200]))
  (define m (new message%
                 [parent f]
                 [label ""]
                 [stretchable-width #t]))
  (send f show #t)
  (thread (lambda ()
            (let loop ()
              (when (send f is-shown?)
                (sleep 0.1)
                (define-values (pos keys) (get-current-mouse-state))
                (queue-callback
                 (lambda () (send m set-label
                                  (format "~a,~a ~a" 
                                          (send pos get-x)
                                          (send pos get-y)
                                          keys))))
                (loop))))))

;----------------------------------------------------------------------

(define selector (make-frame frame% "Test Selector"))
(define ap (make-object vertical-panel% selector))

; Test timers while we're at it. And create the "Instructions" button.
(let ([clockp (make-object horizontal-panel% ap)]
      [selector selector])
  (make-object button% "Get Instructions" clockp
	       (lambda (b e) 
		 (open-file "frame-steps.txt")))
  (make-object check-box% "Use Dialogs" clockp
	       (lambda (c e)
		 (set! use-dialogs? (send c get-value))))
  (make-object check-box% "Metal" clockp
	       (lambda (c e)
		 (set! use-metal? (send c get-value))))
  (make-object check-box% "Float" clockp
	       (lambda (c e)
		 (set! float-frame? (send c get-value))))
  (make-object check-box% "No Title" clockp
	       (lambda (c e)
		 (set! no-caption? (send c get-value))))
  (make-object vertical-panel% clockp) ; filler
  (let ([time (make-object message% "XX:XX:XX" clockp)])
    (make-object
     (class timer%
       (inherit start)
       (override*
	 [notify
	  (lambda ()
	    (let* ([now (seconds->date (current-seconds))]
		   [pad (lambda (pc d)
			  (let ([s (number->string d)])
			    (if (= 1 (string-length s))
				(string-append pc s)
				s)))]
		   [s (format "~a:~a:~a"
			      (pad " " (let ([h (modulo (date-hour now) 12)])
					 (if (zero? h)
					     12
					     h)))
			      (pad "0" (date-minute now))
			      (pad "0" (date-second now)))])
	      (send time set-label s)
	      (when (send selector is-shown?)
		(start 1000 #t))))])
       (super-make-object)
       (start 1000 #t)))))

(define bp0 (make-object vertical-panel% ap '(border)))
(define bp1 (make-object horizontal-panel% bp0))
(define bp2 (make-object horizontal-pane% bp0))
(define mp (make-object vertical-panel% ap '(border)))
(define mp1 (make-object horizontal-panel% mp))
(define mp2 (make-object horizontal-pane% mp))

(define pp (make-object horizontal-pane% ap))
(send bp0 stretchable-height #f)
(make-object button% "Make Menus Frame" pp (lambda (b e) (menu-frame)))
(make-object horizontal-pane% pp)
(make-object button% "Make Panel Frame" pp (lambda (b e) (panel-frame)))
(make-object horizontal-pane% pp)
(make-object button% "Editor Canvas One-liners" pp (lambda (b e) (editor-canvas-oneline-frame)))
(make-object horizontal-pane% pp)
(make-object button% "Minsize Windows" pp (lambda (b e) (minsize-frame)))
(define bp (make-object horizontal-pane% ap))
(send bp stretchable-width #f)
(make-object button% "Make Button Frame" bp (lambda (b e) (button-frame frame% null)))
(make-object button% "Make Default Button Frame" bp (lambda (b e) (button-frame frame% '(border))))
(make-object button% "Make Button Dialog" bp (lambda (b e) (button-frame dialog% null)))
(make-object button% "Make Image Buttons" bp (lambda (b e) (image-button-frame)))
(define crp (make-object horizontal-pane% ap))
(send crp stretchable-height #f)
(make-object button% "Make Checkbox Frame" crp (lambda (b e) (checkbox-frame)))
(make-object vertical-pane% crp) ; filler
(make-object button% "Message Boxes" crp (lambda (b e) (message-boxes #f)))
(make-object vertical-pane% crp) ; filler
(make-object button% "Cursors" crp (lambda (b e) (cursors)))
(make-object vertical-pane% crp) ; filler
(make-object button% "Mouse" crp (lambda (b e) (mouse)))
(make-object vertical-pane% crp) ; filler
(make-object button% "Make Radiobox Frame" crp (lambda (b e) (radiobox-frame)))
(define cp (make-object horizontal-pane% ap))
(send cp stretchable-width #f)
(make-object button% "Make Choice Frame" cp (lambda (b e) (choice-or-list-frame #f null #f)))
(make-object button% "Make Empty Choice Frame" cp (lambda (b e) (choice-or-list-frame #f null #t)))
(make-object button% "Make Combo Frame" cp (lambda (b e) (combo-frame #f)))
(make-object button% "Make Empty Combo Frame" cp (lambda (b e) (combo-frame #t)))
(define lcp (make-object horizontal-pane% ap))
(send lcp stretchable-width #f)
(define list-columns-choice (new choice% 
                                 [parent lcp]
                                 [label "List Type"]
                                 [choices '("Single Column"
                                            "Multiple Columns")]))
(define (get-columns) (if (zero? (send list-columns-choice get-selection))
                          '("Column")
                          '("Main Entry" "Extra" "Final")))
(define list-headers-choice (new check-box%
                                 [parent lcp]
                                 [label "Show Columns"]))
(define (get-headers) (if (send list-headers-choice get-value)
                          '(column-headers clickable-headers reorderable-headers variable-columns)
                          '()))
(define list-order-choice (new check-box%
                               [parent lcp]
                               [label "Swap Last Two"]))
(define (get-order) (if (and (positive? (send list-columns-choice get-selection))
                             (send list-order-choice get-value))
                        '(0 2 1)
                        #f))
(define lp (make-object horizontal-pane% ap))
(send lp stretchable-width #f)
(make-object button% "Make List Frame" lp 
             (lambda (b e) (choice-or-list-frame #t '(single) #f (get-columns) (get-headers) (get-order))))
(make-object button% "Make Empty List Frame" lp 
             (lambda (b e) (choice-or-list-frame #t '(single) #t (get-columns) (get-headers) (get-order))))
(make-object button% "Make MultiList Frame" lp 
             (lambda (b e) (choice-or-list-frame #t '(multiple) #f (get-columns) (get-headers) (get-order))))
(make-object button% "Make MultiExtendList Frame" lp 
             (lambda (b e) (choice-or-list-frame #t '(extended) #f (get-columns) (get-headers) (get-order))))
(define gsp (make-object horizontal-pane% ap))
(send gsp stretchable-height #f)
(make-object button% "Make Gauge Frame" gsp (lambda (b e) (gauge-frame)))
(make-object vertical-pane% gsp) ; filler
(make-object button% "Make Slider Frame" gsp (lambda (b e) (slider-frame null)))
(make-object button% "Make Plain Slider Frame" gsp (lambda (b e) (slider-frame '(plain))))
(make-object vertical-pane% gsp) ; filler
(make-object button% "Make Tab Panel" gsp (lambda (b e) (test-tab-panel #f)))
(make-object button% "Make Tabs" gsp (lambda (b e) (test-tab-panel #t)))

(define tp (make-object horizontal-pane% ap))
(send tp stretchable-width #f)
(make-object button% "Make Text Frame" tp (lambda (b e) (text-frame '(single))))
(make-object button% "Make Multitext Frame" tp (lambda (b e) (text-frame '(multiple))))
(make-object vertical-pane% tp) ; filler
(make-object button% "Make Modified Frame" tp (lambda (b e) (test-modified-frame)))

(define cnp (make-object horizontal-pane% ap))
(send cnp stretchable-width #t)
(send cnp set-alignment 'right 'center)
(let ([mkf (lambda (flags name)
	     (make-object button%
			  (format "Make ~aCanvas Frame" name) cnp 
			  (lambda (b e) (canvas-frame flags))))])
  (mkf '(hscroll vscroll) "HV")
  (mkf '(hscroll) "H")
  (mkf '(vscroll) "V")
  (mkf null "")
  (make-object grow-box-spacer-pane% cnp))
(make-object button%
	     "Make No-Clear Canvas" cnp 
	     (lambda (b e) (no-clear-canvas-frame)))

(define edp (new horizontal-pane% 
		 [parent ap]
		 [alignment '(center center)]))
(make-object button%
	     "Make Editor" edp 
	     (lambda (b e) (editor-frame null #f)))
(make-object button%
	     "Make Transparent Editor" edp 
	     (lambda (b e) (editor-frame '(transparent) #f)))
(make-object button%
	     "Make Blue Editor" edp 
	     (lambda (b e) (editor-frame null "blue")))
(new horizontal-panel% [parent edp])
(make-object button%
	     "Warp Pointer" edp 
	     (lambda (b e)
               (send selector warp-pointer 5 5)))
(let* ([w (send gc-bmp get-width)]
       [h (send gc-bmp get-height)]
       [c (new (class canvas%
                 (super-new)
                 (define/override (on-event e)
                   (when (send e button-down?)
                     (collect-garbage))))
               [parent edp]
               [stretchable-width #f]
               [stretchable-height #f]
               [vert-margin 2]
               [horiz-margin 2]
               [min-width w]
               [min-height h])])
  (register-collecting-blit c
                            0 0 (send gc-bmp get-width) (send gc-bmp get-height)
                            gc-bmp (make-bitmap w h #f)))
  
              

(define (choose-next radios)
  (let loop ([l radios])
    (let* ([c (car l)]
	   [rest (cdr l)]
	   [n (send c get-number)]
	   [v (send c get-selection)])
      (if (< v (sub1 n))
	  (send c set-selection (add1 v))
	  (if (null? rest)
	      (map (lambda (c) (send c set-selection 0)) radios)
	      (begin
		(send c set-selection 0)
		(loop rest)))))))

(define make-next-button
  (lambda (p l)
    (make-object button%
		 "Next Configuration" p
		 (lambda (b e) (choose-next l)))))

(define make-selector-and-runner
  (lambda (p1 p2 radios? msg? size maker)
    (define (make-radio-box lbl choices panel cb)
      (let ([g (instantiate group-box-panel% (lbl panel))])
	(if (= (length choices) 2)
	    (make-object radio-box% #f choices g cb)
	    (make-object choice% #f choices g cb))))
    (define radio-h-radio
      (make-radio-box 
       (if radios? "Radio Box Orientation" "Slider Style")
       (if radios? '("Vertical" "Horizontal") '("Numbers" "Plain"))
       p1 void))
    (define label-h-radio
      (make-radio-box "Label Orientation" '("Vertical" "Horizontal")
		      p1 void))
    (define label-null-radio
      (make-radio-box "Optional Labels" '("Use Label" "No Label")
		      p1 void))
    (define stretchy-radio
      (make-radio-box "Stretchiness" '("Normal" "All Stretchy")
		      p1 void))
    (define font-radio
      (make-radio-box "Label Font" '("Normal" "Small" "Tiny" "Big" "Italic")
		      p1 void))
    (define enabled-radio
      (make-radio-box "Initially" '("Enabled" "Disabled")
		      p1 void))
    (define selection-radio
      (make-radio-box "Selection" '("Default" "Alternate")
		      p1 void))
    (define next-button
      (make-next-button p2 (list radio-h-radio label-h-radio label-null-radio 
				 stretchy-radio font-radio
				 enabled-radio selection-radio)))
    (define go-button
      (make-object button% (format "Make ~a Frame" size) p2
		   (lambda (b e)
		     (maker
		      (positive? (send radio-h-radio get-selection))
		      (positive? (send label-h-radio get-selection))
		      (positive? (send label-null-radio get-selection))
		      (positive? (send stretchy-radio get-selection))
		      (list-ref (list #f
				      small-control-font
				      tiny-control-font
				      special-font
                                      italic-font)
				(send font-radio get-selection))
		      (positive? (send enabled-radio get-selection))
		      (positive? (send selection-radio get-selection))
                      (and message-auto
                           (send message-auto get-value))
                      (append
                       (case (send panel-h-mode get-selection)
                         [(0) '()]
                         [(1) '(hscroll)]
                         [(2) '(auto-hscroll)])
                       (case (send panel-v-mode get-selection)
                         [(0) '()]
                         [(1) '(vscroll)]
                         [(2) '(auto-vscroll)]))))))

    (define message-auto
      (and msg?
           (new check-box% 
                [parent p2]
                [label "Auto-Size Message"])))

    (define panel-h-mode
      (new choice% 
           [parent p2]
           [label "Panels"]
           [choices '("No HScroll" "HScroll" "Auto HScroll")]))
    (define panel-v-mode
      (new choice% 
           [parent p2]
           [label "Panels"]
           [choices '("No VScroll" "VScroll" "Auto VScroll")]))
    
    #t))

(make-selector-and-runner bp1 bp2 #t #t "Big" big-frame)
(make-selector-and-runner mp1 mp2 #f #f "Medium" med-frame)

(send selector show #t)

;; For test mode, check that we can at least start,
;; but exit right away:
(module+ test 
  (queue-callback (lambda () (exit)) #f))

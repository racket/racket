#lang racket/base
  (require racket/class
           (prefix-in wx: "kernel.rkt")
	   (prefix-in wx: racket/snip/private/style)
           "lock.rkt"
           "const.rkt"
           "check.rkt"
           "wx.rkt"
           "helper.rkt"
           "mrtop.rkt"
           "mrcanvas.rkt"
           "mritem.rkt"
           "mrpanel.rkt"
           "mrtextfield.rkt")

  (provide get-ps-setup-from-user
           get-page-setup-from-user
           can-get-page-setup-from-user?
           get-text-from-user
           get-choices-from-user
           get-color-from-user)

  (define (number->string* n)
    (let ([s (number->string n)])
      (regexp-replace #rx"[.]([0-9][0-9][0-9])[0-9]*$"
		      s
		      ".\\1")))

  (define get-ps-setup-from-user
    (case-lambda
     [() (get-ps-setup-from-user #f #f #f null)]
     [(message) (get-ps-setup-from-user message #f #f null)]
     [(message parent) (get-ps-setup-from-user message parent #f null)]
     [(message parent pss) (get-ps-setup-from-user message parent pss null)]
     [(message parent pss-in style)
      (define _
	(begin
	  ;; Calls from C++ have wrong kind of window:
	  (when (is-a? parent wx:window%)
	    (set! parent (as-entry (lambda () (wx->mred parent)))))

	  (check-label-string/false 'get-ps-setup-from-user message)
	  (check-top-level-parent/false 'get-ps-setup-from-user parent)
	  (check-instance 'get-ps-setup-from-user wx:ps-setup% 'ps-setup% #t pss-in)
	  (check-style 'get-ps-setup-from-user #f null style)))
      
      (define bad-fields null)
      (define number-callback
	(lambda (f ev)
	  (let ([e (send f get-editor)]
		[ok? (real? (string->number (send f get-value)))])
	    (send e change-style 
		  (send (make-object wx:style-delta%) 
			set-delta-background 
			(if ok? "white" "yellow"))
		  0 (send e last-position))
	    (set! bad-fields (remq f bad-fields))
	    (unless ok?
	      (set! bad-fields (cons f bad-fields)))
	    (send ok enable (null? bad-fields)))))

      (define pss (or pss-in (wx:current-ps-setup)))
      (define f (make-object dialog% "PostScript Setup" parent))
      (define papers 
	'("A4 210 x 297 mm" "A3 297 x 420 mm" "Letter 8 1/2 x 11 in" "Legal 8 1/2 x 14 in"))
      (define p (make-object horizontal-pane% f))
      (define paper (make-object choice% #f papers p void))
      (define _0 (make-object vertical-pane% p))
      (define-values (ok cancel)
        (ok-cancel
         (lambda () (make-object button% "OK" p (lambda (b e) (done #t)) '(border)))
         (lambda () (make-object button% "Cancel" p (lambda (b e) (done #f))))))
      (define unix? (eq? (system-type) 'unix))
      (define dp (make-object horizontal-pane% f))
      (define orientation (make-object radio-box% "Orientation:" '("Portrait" "Landscape") dp void))
      (define destination (and unix? (make-object radio-box% "Destination:" 
						  '("Printer" "Preview" "File") dp void)))
      (define ssp (make-object horizontal-pane% f))
      (define sp (make-object vertical-pane% ssp))
      (define def-scale "0100.000")
      (define def-offset "0000.000")
      (define def-margin "0016.000")
      (define xscale (make-object text-field% "Horizontal Scale:" sp number-callback def-scale))
      (define xoffset (make-object text-field% "Horizontal Translation:" sp number-callback def-offset))
      (define xmargin (make-object text-field% "Horizontal Margin:" sp number-callback def-margin))
      (define sp2 (make-object vertical-pane% ssp))
      (define yscale (make-object text-field% "Vertical Scale:" sp2 number-callback def-scale))
      (define yoffset (make-object text-field% "Vertical Translation:" sp2 number-callback def-offset))
      (define ymargin (make-object text-field% "Vertical Margin:" sp2 number-callback def-margin))

      (define l2 (make-object check-box% "PostScript Level 2" f void))

      (define cp (and unix? (make-object horizontal-pane% f)))
      (define command (and unix? (make-object text-field% "Print Command:" cp void)))
      (define vcommand (and unix? (make-object text-field% "Preview Command:" f void)))

      (define ok? #f)
      (define (done ?)
	(send f show #f)
	(set! ok? ?))

      (define-values (xsb ysb xtb ytb xmb ymb) 
	(values (box 0) (box 0) (box 0) (box 0) (box 0) (box 0)))

      (send paper set-selection (or (find-pos papers (send pss get-paper-name) equal?) 0))
      (send orientation set-selection (if (eq? (send pss get-orientation) 'landscape) 1 0))
      (when unix?
	(send destination set-selection (case (send pss get-mode)
					  [(printer) 0] [(preview) 1] [(file) 2]))
	(send command set-value (send pss get-command))
	(send vcommand set-value (send pss get-preview-command)))

      (send sp set-alignment 'right 'top)
      (send sp2 set-alignment 'right 'top)
      (send pss get-scaling xsb ysb)
      (send xscale set-value (number->string* (unbox xsb)))
      (send yscale set-value (number->string* (unbox ysb)))
      (send pss get-translation xtb ytb)
      (send xoffset set-value (number->string* (unbox xtb)))
      (send yoffset set-value (number->string* (unbox ytb)))
      (send pss get-margin xmb ymb)
      (send xmargin set-value (number->string* (unbox xmb)))
      (send ymargin set-value (number->string* (unbox ymb)))
      (send xscale stretchable-width #f)
      (send yscale stretchable-width #f)
      (send xoffset stretchable-width #f)
      (send yoffset stretchable-width #f)
      (send xmargin stretchable-width #f)
      (send ymargin stretchable-width #f)

      (send l2 set-value (send pss get-level-2))

      (send f set-alignment 'center 'top)

      (map no-stretch (list f xscale yscale xoffset yoffset xmargin ymargin dp))

      (send f center)

      (send f show #t)

      (if ok?
	  (let ([s (make-object wx:ps-setup%)]
		[gv (lambda (c b)
		      (or (string->number (send c get-value)) (unbox b)))])
	    (send s set-paper-name (send paper get-string-selection))
	    (send s set-orientation (if (positive? (send orientation get-selection))
					'landscape
					'portrait))
	    (when unix?
	      (send s set-mode (case (send destination get-selection)
				 [(0) 'printer]
				 [(1) 'preview]
				 [(2) 'file])))
	    (send s set-scaling (gv xscale xsb) (gv yscale ysb))
	    (send s set-translation (gv xoffset xtb) (gv yoffset ytb))
	    (send s set-margin (gv xmargin xmb) (gv ymargin ymb))
	    (send s set-level-2 (send l2 get-value))
	    
	    (when (eq? (system-type) 'unix)
	      (send s set-command (send command get-value))
	      (send s set-preview-command (send vcommand get-value)))

	    s)
	  #f)]))

  (define get-page-setup-from-user
    (case-lambda
     [() (get-page-setup-from-user #f #f #f null)]
     [(message) (get-page-setup-from-user message #f #f null)]
     [(message parent) (get-page-setup-from-user message parent #f null)]
     [(message parent pss) (get-page-setup-from-user message parent pss null)]
     [(message parent pss-in style)
      (check-label-string/false 'get-page-setup-from-user message)
      (check-top-level-parent/false 'get-page-setup-from-user parent)
      (check-instance 'get-page-setup-from-user wx:ps-setup% 'ps-setup% #t pss-in)
      (check-style 'get-page-setup-from-user #f null style)

      (and (wx:can-show-print-setup?)
	   (let ([s (make-object wx:ps-setup%)])
	     (send s copy-from (or pss-in (wx:current-ps-setup)))
	     (and (parameterize ([wx:current-ps-setup s])
		    (wx:show-print-setup (and parent (mred->wx parent))))
		  s)))]))

  (define (can-get-page-setup-from-user?)
    (wx:can-show-print-setup?))

  (define (get-text-from-user title message
                              [parent #f]
                              [init-val ""]
                              [style null]
                              #:dialog-mixin [dialog-mixin values]
                              #:validate [validate (λ (x) #t)])
    (check-label-string 'get-text-from-user title)
    (check-label-string/false 'get-text-from-user message)
    (check-top-level-parent/false 'get-text-from-user parent)
    (check-string 'get-text-from-user init-val)
    (check-style 'get-text-from-user #f '(password disallow-invalid) style)
    (define f (make-object (dialog-mixin dialog%) title parent box-width))
    (define ok? #f)
    (define (done ?) (set! ok? ?) (send f show #f))
    (define t (new text-field% 
                   [label message]
                   [parent f]
                   [callback (λ (t e) 
                               (cond
                                 [(eq? (send e get-event-type) 'text-field-enter)
                                  (done #t)]
                                 [else (do-validation)]))]
                   [init-value init-val]
                   [style (list* 'single 'vertical-label
                                 (if (memq 'password style)
                                     '(password)
                                     '()))]))
    (define default-background (send t get-field-background))
    (define (do-validation)
      (define valid? (validate (send t get-value)))
      (send t set-field-background 
            (if valid?
                default-background
                (send wx:the-color-database find-color "pink")))
      (when (memq 'disallow-invalid style)
        (send ok-button enable valid?)))
    (define p (make-object horizontal-pane% f))
    (send p set-alignment 'right 'center)
    (send f stretchable-height #f)
    (define-values (ok-button cancel-button)
      (ok-cancel
       (lambda () (make-object button% "OK" p (λ (b e) (done #t)) '(border)))
       (lambda () (make-object button% "Cancel" p (λ (b e) (done #f))))))
    (send (send t get-editor) select-all)
    (send t focus)
    (send f center)
    (do-validation)
    (send f show #t)
    (and ok? (send t get-value)))

  (define get-choices-from-user
    (case-lambda
     [(title message choices) (get-choices-from-user title message choices #f null '(single))]
     [(title message choices parent) (get-choices-from-user title message choices parent null '(single))]
     [(title message choices parent init-vals) (get-choices-from-user title message choices parent init-vals '(single))]
     [(title message choices parent init-vals style)
      (check-label-string 'get-choices-from-user title)
      (check-label-string/false 'get-choices-from-user message)
      (unless (and (list? choices) (andmap label-string? choices))
	(raise-argument-error 'get-choices-from-user "(listof label-string?)" choices))
      (check-top-level-parent/false 'get-choices-from-user parent)
      (unless (and (list? init-vals) (andmap exact-nonnegative-integer? init-vals))
	(raise-argument-error 'get-choices-from-user "(listof exact-nonnegative-integer?)" init-vals))
      (check-style 'get-choices-from-user '(single multiple extended) null style)
      (when (and (memq 'single style) (> (length init-vals) 1))
	(raise-arguments-error 'get-choices-from-user 
                               "multiple initial-selection indices provided with 'single style"
                               "indices" init-vals))
      (let* ([f (make-object dialog% title parent box-width (min 300 (max 150 (* 14 (length choices)))))]
	     [ok-button #f]
	     [update-ok (lambda (l) (send ok-button enable (not (null? (send l get-selections)))))]
	     [ok? #f]
	     [done (lambda (?) (lambda (b e) (set! ok? ?) (send f show #f)))])
	(let ([l (make-object list-box% message choices f
			      (lambda (l e)
				(update-ok l)
				(when (eq? (send e get-event-type) 'list-box-dclick)
				  ((done #t) #f #f)))
			      (cons 'vertical-label style))]
	      [p (make-object horizontal-pane% f)])
	  (for-each (lambda (i) 
		      (when (>= i (send l get-number))
			(raise-arguments-error 
                         'get-choices-from-user 
                         "out of range;\n inital-selection list specifies an out-of-range index"
                         "index" i
                         "provided choices" (send l get-number)
                         "list..." init-vals))
		      (send l select i #t)) init-vals)
	  (send p set-alignment 'right 'center)
	  (send p stretchable-height #f)
          (ok-cancel (lambda ()
                       (set! ok-button (make-object button% "OK" p (done #t) '(border))))
                     (lambda ()
                       (make-object button% "Cancel" p (done #f))))
	  (update-ok l)
	  (send f center)
	  (when (and (pair? init-vals)
		     ((car init-vals) . > . 1))
	    ;; Make sure initial selection is visible:
	    (send f reflow-container)
	    (send l set-first-visible-item (sub1 (car init-vals))))
	  (send f show #t)
	  (and ok? (send l get-selections))))]))

  (define get-color-from-user 
    (case-lambda
      [() (get-color-from-user #f #f #f null)]
      [(message) (get-color-from-user message #f #f null)]
      [(message parent) (get-color-from-user message parent #f null)]
      [(message parent color) (get-color-from-user message parent color null)]
      [(message parent in-color style)
       (check-label-string/false 'get-color-from-user message)
       (check-top-level-parent/false 'get-color-from-user parent)
       (check-instance 'get-color-from-user wx:color% 'color% #t in-color)
       (check-style 'get-color-from-user #f '(alpha) style)
       (cond
         [(eq? (wx:color-from-user-platform-mode) 'dialog)
          (wx:get-color-from-user message (and parent (mred->wx parent)) in-color)]
         [else
          (define color (if (member 'alpha style)
                            in-color
                            (make-object wx:color%
                              (send in-color red)
                              (send in-color green)
                              (send in-color blue)
                              1.0)))
          (define ok? #f)
          (define f (make-object dialog% "Choose Color" parent))
          (define (done ok) (lambda (b e) (set! ok? ok) (send f show #f)))
          (define canvas (make-object (class canvas%
                                        (define/override (on-paint)
                                          (repaint void))
                                        (super-new [parent f]))))
          (define platform-p (and (string? (wx:color-from-user-platform-mode))
                                  (new horizontal-panel%
                                       [parent f]
                                       [alignment '(right center)])))
          (define p (make-object vertical-pane% f))
          (define (repaint ext)
            (let ([c (get-current-color)])
              (ext c)
              (wx:fill-private-color (send canvas get-dc) c)))
          (define (update-and-repaint s e)
            (repaint
             (lambda (c)
               (when platform-p
                 (wx:get-color-from-user c)))))
          (define (make-color-slider l) (make-object slider% l 0 255 p update-and-repaint))
          (define red (make-color-slider "Red:"))
          (define green (make-color-slider "Green:"))
          (define blue (make-color-slider "Blue:"))
          (define alpha (and (member 'alpha style)
                             (new text-field% 
                                  [parent p]
                                  [label "Alpha:"]
                                  [callback
                                   (λ (_1 _2) 
                                     (update-ok-button-and-background))])))
          (define (update-ok-button-and-background)
            (when alpha
              (define n (string->number (send alpha get-value)))
              (define ok? (and n (real? n) (<= 0 n 1)))
              (send ok-button enable ok?)
              (send alpha set-field-background
                    (send wx:the-color-database find-color
                          (if ok? "white" "pink")))))
          (define bp (make-object horizontal-pane% f))
          (define (get-current-color)
            (make-object wx:color% 
              (send red get-value)
              (send green get-value)
              (send blue get-value)
              (if alpha
                  (string->number (send alpha get-value))
                  1.0)))
          (define (install-color color)
            (send red set-value (send color red))
            (send green set-value (send color green))
            (send blue set-value (send color blue))
            (when alpha (send alpha set-value (format "~a" (send color alpha))))
            (send canvas refresh))
          (when platform-p
            (new button%
                 [parent platform-p]
                 [label (wx:color-from-user-platform-mode)]
                 [callback (lambda (b e) (wx:get-color-from-user 'show))])
            (wx:get-color-from-user (or color
                                        (make-object wx:color% 0 0 0)))
            (send (mred->wx f) set-color-callback (lambda ()
                                                    (install-color
                                                     (wx:get-color-from-user 'get)))))
          (when color (install-color color))
          (define-values (ok-button cancel-button)
            (ok-cancel
             (lambda ()
               (make-object button% "OK" bp (done #t) '(border)))
             (lambda ()
               (make-object button% "Cancel" bp (done #f)))))
          (send ok-button focus)
          (update-ok-button-and-background)
          (send bp set-alignment 'right 'center)
          (send p set-alignment 'right 'center)
          (send p stretchable-height #f)
          (send canvas min-height 50)
          (send f center)
          (send f show #t)
          (and ok?
               (get-current-color))])]))

#|

WARNING: printf is rebound in the body of the unit to always
         print to the original output port.

|#

(module text mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "match.ss")
	   "sig.ss"
	   "../gui-utils.ss"
	   (lib "mred-sig.ss" "mred")
           (lib "interactive-value-port.ss" "mrlib")
	   (lib "list.ss")
	   (lib "etc.ss"))
  (provide text@)

  (define text@
    (unit/sig framework:text^
      (import mred^
              [icon : framework:icon^]
              [editor : framework:editor^]
              [preferences : framework:preferences^]
              [keymap : framework:keymap^]
              [color-model : framework:color-model^]
              [frame : framework:frame^]
              [scheme : framework:scheme^]
              [number-snip : framework:number-snip^])
      
      (rename [-keymap% keymap%])
      
      (define original-output-port (current-output-port))
      (define (printf . args) 
        (apply fprintf original-output-port args)
        (void))
      
      (define-struct range (start end b/w-bitmap color caret-space?))
      (define-struct rectangle (left top right bottom b/w-bitmap color))
      
      ;; wx: `default-wrapping?', add as the initial value for auto-wrap bitmap,
      ;; unless matthew makes it primitive
      
      (define basic<%>
        (interface (editor:basic<%> (class->interface text%))
          highlight-range
          get-highlighted-ranges
          get-styles-fixed
          get-fixed-style
          set-styles-fixed
          move/copy-to-edit
          initial-autowrap-bitmap))
      
      (define basic-mixin
        (mixin (editor:basic<%> (class->interface text%)) (basic<%>)
          (inherit get-canvases get-admin split-snip get-snip-position
                   begin-edit-sequence end-edit-sequence
                   set-autowrap-bitmap
                   delete find-snip invalidate-bitmap-cache
                   set-file-format get-file-format
                   get-style-list is-modified? change-style set-modified
                   position-location get-extent)
          
          (define highlight-pen #f)
          (define highlight-brush #f)

          (define range-rectangles null)
          (define ranges null)
          
          (define/public-final (get-highlighted-ranges) ranges)
          (define/public (get-fixed-style)
            (send (get-style-list) find-named-style "Standard"))
          
          (define/private (invalidate-rectangles rectangles)
            (let ([b1 (box 0)]
                  [b2 (box 0)]
                  [b3 (box 0)]
                  [b4 (box 0)]
                  [canvases (get-canvases)])
              (let-values ([(min-left max-right)
                            (cond
                              [(null? canvases)
                               (let ([admin (get-admin)])
                                 (if admin
                                     (begin
                                       (send admin get-view b1 b2 b3 b4)
                                       (let* ([this-left (unbox b1)]
                                              [this-width (unbox b3)]
                                              [this-right (+ this-left this-width)])
                                         (values this-left
                                                 this-right)))
                                     (values #f #f)))]
                              [else 
                               (let loop ([left #f]
                                          [right #f]
                                          [canvases canvases])
                                 (cond
                                   [(null? canvases)
                                    (values left right)]
                                   [else
                                    (let-values ([(this-left this-right)
                                                  (send (car canvases)
                                                        call-as-primary-owner
                                                        (λ ()
                                                          (send (get-admin) get-view b1 b2 b3 b4)
                                                          (let* ([this-left (unbox b1)]
                                                                 [this-width (unbox b3)]
                                                                 [this-right (+ this-left this-width)])
                                                            (values this-left
                                                                    this-right))))])
                                      (if (and left right)
                                          (loop (min this-left left)
                                                (max this-right right)
                                                (cdr canvases))
                                          (loop this-left
                                                this-right
                                                (cdr canvases))))]))])])
                (when (and min-left max-right)
                  (let loop ([left #f]
                             [top #f]
                             [right #f]
                             [bottom #f]
                             [rectangles rectangles])
                    (cond
                      [(null? rectangles)
                       (when left
                         (let ([width (- right left)]
                               [height (- bottom top)])
                           (when (and (> width 0)
                                      (> height 0))
                             (invalidate-bitmap-cache left top width height))))]
                      [else (let* ([r (car rectangles)]
                                   
                                   [rleft (rectangle-left r)]
                                   [rright (rectangle-right r)]
                                   [rtop (rectangle-top r)]
                                   [rbottom (rectangle-bottom r)]
                                   
                                   [this-left (if (number? rleft)
                                                  rleft
                                                  min-left)]
                                   [this-right (if (number? rright)
                                                   rright
                                                   max-right)]
                                   [this-bottom rbottom]
                                   [this-top rtop])
                              (if (and left top right bottom)
                                  (loop (min this-left left)
                                        (min this-top top)
                                        (max this-right right)
                                        (max this-bottom bottom)
                                        (cdr rectangles))
                                  (loop this-left 
                                        this-top
                                        this-right
                                        this-bottom
                                        (cdr rectangles))))]))))))
          
          (define/private (recompute-range-rectangles)
            (let* ([b1 (box 0)]
                   [b2 (box 0)]
                   [new-rectangles
                    (λ (range)
                      (let* ([start (range-start range)]
                             [end (range-end range)]
                             [b/w-bitmap (range-b/w-bitmap range)]
                             [color (range-color range)]
                             [caret-space? (range-caret-space? range)]
                             [start-eol? #f]
                             [end-eol? (if (= start end)
                                           start-eol?
                                           #t)])
                        (let-values ([(start-x top-start-y)
                                      (begin 
                                        (position-location start b1 b2 #t start-eol? #t)
                                        (values (if caret-space?
                                                    (+ 1 (unbox b1))
                                                    (unbox b1))
                                                (unbox b2)))]
                                     [(end-x top-end-y)
                                      (begin (position-location end b1 b2 #t end-eol? #t)
                                             (values (unbox b1) (unbox b2)))]
                                     [(bottom-start-y)
                                      (begin (position-location start b1 b2 #f start-eol? #t)
                                             (unbox b2))]
                                     [(bottom-end-y)
                                      (begin (position-location end b1 b2 #f end-eol? #t)
                                             (unbox b2))])
                          (cond
                            [(= top-start-y top-end-y)
                             (list 
                              (make-rectangle start-x
                                              top-start-y
                                              (if (= end-x start-x)
                                                  (+ end-x 1)
                                                  end-x)
                                              bottom-start-y
                                              b/w-bitmap 
                                              color))]
                            [else
                             (list
                              (make-rectangle start-x
                                              top-start-y
                                              'right-edge
                                              bottom-start-y
                                              b/w-bitmap
                                              color)
                              (make-rectangle 'left-edge
                                              bottom-start-y
                                              'max-width
                                              top-end-y
                                              b/w-bitmap
                                              color)
                              (make-rectangle 'left-edge
                                              top-end-y
                                              end-x
                                              bottom-end-y
                                              b/w-bitmap
                                              color))]))))]
                   [old-rectangles range-rectangles])
              
              (set! range-rectangles 
                    (foldl (λ (x l) (append (new-rectangles x) l))
                           null ranges))))
          
          (define/public highlight-range
            (opt-lambda (start end color [bitmap #f] [caret-space? #f] [priority 'low])
              (unless (let ([exact-pos-int?
                             (λ (x) (and (integer? x) (exact? x) (x . >= . 0)))])
                        (and (exact-pos-int? start)
                             (exact-pos-int? end)))
                (error 'highlight-range "expected first two arguments to be non-negative exact integers, got: ~e ~e"
                       start end))
              (unless (or (eq? priority 'high) (eq? priority 'low))
                (error 'highlight-range "expected last argument to be either 'high or 'low, got: ~e"
                       priority))
              (let ([l (make-range start end bitmap color caret-space?)])
                (invalidate-rectangles range-rectangles)
                (set! ranges (if (eq? priority 'high) (cons l ranges) (append ranges (list l))))
                (recompute-range-rectangles)
                (invalidate-rectangles range-rectangles)
                (λ ()
                  (let ([old-rectangles range-rectangles])
                    (set! ranges
                          (let loop ([r ranges])
                            (cond
                              [(null? r) r]
                              [else (if (eq? (car r) l)
                                        (cdr r)
                                        (cons (car r) (loop (cdr r))))])))
                    (recompute-range-rectangles)                    
                    (invalidate-rectangles old-rectangles))))))
          (define/override (on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
            (super on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
            (recompute-range-rectangles)
            (let ([b1 (box 0)]
                  [b2 (box 0)]
                  [b3 (box 0)]
                  [b4 (box 0)])
              (for-each
               (λ (rectangle)
                 (let-values ([(view-x view-y view-width view-height)
                               (begin 
                                 (send (get-admin) get-view b1 b2 b3 b4)
                                 (values (unbox b1)
                                         (unbox b2)
                                         (unbox b3)
                                         (unbox b4)))])
                   (let* ([old-pen (send dc get-pen)]
                          [old-brush (send dc get-brush)]
                          [b/w-bitmap (rectangle-b/w-bitmap rectangle)]
                          [color (let* ([rc (rectangle-color rectangle)]
                                        [tmpc (make-object color% 0 0 0)])
                                   (if rc
                                       (begin (send dc try-color rc tmpc)
                                              (if (<= (color-model:rgb-color-distance
                                                       (send rc red)
                                                       (send rc green)
                                                       (send rc blue)
                                                       (send tmpc red)
                                                       (send tmpc green)
                                                       (send tmpc blue))
                                                      18)
                                                  rc
                                                  #f))
                                       rc))]
                          [first-number (λ (x y) (if (number? x) x y))]
                          [left (max left-margin (first-number (rectangle-left rectangle) view-x))]
                          [top (max top-margin (rectangle-top rectangle))]
                          [right (min right-margin
                                      (first-number 
                                       (rectangle-right rectangle)
                                       (+ view-x view-width)))]
                          [bottom (min bottom-margin (rectangle-bottom rectangle))]
                          [width (max 0 (- right left))]
                          [height (max 0 (- bottom top))])
                     (let/ec k
                       (cond
                         [(and before color)
                          (send dc set-pen (send the-pen-list find-or-create-pen color 0 'solid))
                          (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))]
                         [(and (not before) (not color) b/w-bitmap)
                          (unless highlight-pen
                            (set! highlight-pen (make-object pen% "BLACK" 0 'solid)))
                          (unless highlight-brush
                            (set! highlight-brush (make-object brush% "black" 'solid)))
                          (send highlight-pen set-stipple b/w-bitmap)
                          (send highlight-brush set-stipple b/w-bitmap)
                          (send dc set-pen highlight-pen)
                          (send dc set-brush highlight-brush)]
                         [else (k (void))])
                       (send dc draw-rectangle (+ left dx) (+ top dy) width height)
                       (send dc set-pen old-pen)
                       (send dc set-brush old-brush)))))
               range-rectangles)))
          
          (define styles-fixed? #f)
          (public get-styles-fixed set-styles-fixed)
          (define (get-styles-fixed) styles-fixed?)
          (define (set-styles-fixed b) (set! styles-fixed? b))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence)
            (inner (void) on-insert start len))
          (define/augment (after-insert start len)
            (when styles-fixed?
              (change-style (get-fixed-style) start (+ start len) #f))
            (inner (void) after-insert start len)
            (end-edit-sequence))
          
          (public move/copy-to-edit)
          (define (move/copy-to-edit dest-edit start end dest-position)
            (split-snip start)
            (split-snip end)
            (let loop ([snip (find-snip end 'before)])
              (cond
                [(or (not snip) (< (get-snip-position snip) start))
                 (void)]
                [else
                 (let ([prev (send snip previous)]
                       [released/copied (if (send snip release-from-owner)
                                            snip
                                            (let* ([copy (send snip copy)]
                                                   [snip-start (get-snip-position snip)]
                                                   [snip-end (+ snip-start (send snip get-count))])
                                              (delete snip-start snip-end)
                                              snip))])
                   (send dest-edit insert released/copied dest-position dest-position)
                   (loop prev))])))
          
          (public initial-autowrap-bitmap)
          (define (initial-autowrap-bitmap) (icon:get-autowrap-bitmap))
          
          (super-instantiate ())
          (set-autowrap-bitmap (initial-autowrap-bitmap))))
      
      (define foreground-color<%>
        (interface (basic<%> editor:standard-style-list<%>)
          ))
          
      (define foreground-color-mixin
        (mixin (basic<%> editor:standard-style-list<%>) (foreground-color<%>)
          (inherit begin-edit-sequence end-edit-sequence change-style get-style-list)
	  (define/override (get-fixed-style)
            (send (editor:get-standard-style-list)
		  find-named-style
		  (editor:get-default-color-style-name)))
          (super-new)))
      
      (define hide-caret/selection<%> (interface (basic<%>)))
      (define hide-caret/selection-mixin
        (mixin (basic<%>) (hide-caret/selection<%>)
          (inherit get-start-position get-end-position hide-caret)
          (define/augment (after-set-position)
            (hide-caret (= (get-start-position) (get-end-position)))
	    (inner (void) after-set-position))
          (super-instantiate ())))

      (define nbsp->space<%> (interface ((class->interface text%))))
      (define nbsp->space-mixin
        (mixin ((class->interface text%)) (nbsp->space<%>)
          (field [rewriting #f])
          (inherit begin-edit-sequence end-edit-sequence delete insert get-character)
          (define/augment (on-insert start len)
            (inner (void) on-insert start len)
            (begin-edit-sequence))
	  (inherit find-string)
          (define/augment (after-insert start len)
            (unless rewriting
              (set! rewriting #t)
	      (let ([str (string (integer->char 160))]
		    [last-pos (+ start len)])
		(let loop ([pos start])
		  (when (< pos last-pos)
		    (let ([next-pos (find-string str 'forward pos last-pos)])
		      (when next-pos
			(delete next-pos (+ next-pos 1) #f)
			(insert " " next-pos next-pos #f)
			(loop (+ next-pos 1)))))))
	      (set! rewriting #f))
            (end-edit-sequence)
            (inner (void) after-insert start len))
          (super-instantiate ())))
      
      (define searching<%> (interface (editor:keymap<%> basic<%>)))
      (define searching-mixin
        (mixin (editor:keymap<%> basic<%>) (searching<%>)
          (define/override (get-keymaps)
            (cons (keymap:get-search) (super get-keymaps)))
          (super-instantiate ())))
      
      (define return<%> (interface ((class->interface text%))))
      (define return-mixin
        (mixin ((class->interface text%)) (return<%>) 
          (init-field return)
          (define/override (on-local-char key)
            (let ([cr-code #\return]
                  [lf-code #\newline]
                  [code (send key get-key-code)])
              (or (and (char? code)
                       (or (char=? lf-code code)
                           (char=? cr-code code))
                       (return))
                  (super on-local-char key))))
          (super-new)))
      
      (define wide-snip<%>
        (interface (basic<%>)
          add-wide-snip
          add-tall-snip))
      
      (define wide-snip-mixin
        (mixin (basic<%>) (wide-snip<%>)
          (define wide-snips '())
          (define tall-snips '())
          (define/public (add-wide-snip s) (set! wide-snips (cons s wide-snips)))
          (define/public (get-wide-snips) wide-snips)
          (define/public (add-tall-snip s) (set! tall-snips (cons s tall-snips)))
          (define/public (get-tall-snips) tall-snips)
          (super-new)))
      
      (define delegate<%> (interface (basic<%>) 
                            get-delegate
			    set-delegate))

      (define small-version-of-snip%
        (class snip%
          (init-field big-snip)
          (define width 0)
          (define height 0)
          (define/override (get-extent dc x y wb hb db sb lb rb)
            (set/f! db 0)
            (set/f! sb 0)
            (set/f! lb 0)
            (set/f! rb 0)
            (let ([bwb (box 0)]
                  [bhb (box 0)])
              (send big-snip get-extent dc x y bwb bhb #f #f #f #f)
              (let* ([cw (send dc get-char-width)]
                     [ch (send dc get-char-height)]
                     [w (floor (/ (unbox bwb) cw))]
                     [h (floor (/ (unbox bhb) ch))])
                (set/f! wb w)
                (set/f! hb h)
                (set! width w)
                (set! height h))))
              
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (send dc draw-rectangle x y width height))
          (define/override (copy) (instantiate small-version-of-snip% () (big-snip big-snip)))
          (super-instantiate ())))
          
      (define 1-pixel-string-snip%
        (class string-snip%
          (init-rest args)
          (inherit get-text get-count set-count get-flags)
          (define/override (split position first second)
            (let* ([str (get-text 0 (get-count))]
                   [new-second (make-object 1-pixel-string-snip%
                                 (substring str position (string-length str)))])
              (set-box! first this)
              (set-box! second new-second)
              (set-count position)
              (void)))
          (define/override (copy)
            (let ([cpy (make-object 1-pixel-string-snip%
                         (get-text 0 (get-count)))])
              (send cpy set-flags (get-flags))))
          (define/override (get-extent dc x y wb hb db sb lb rb)
            (cond
              [(memq 'invisible (get-flags))
               (set/f! wb 0)]
              [else
               (set/f! wb (get-count))])
            (set/f! hb 1)
            (set/f! db 0)
            (set/f! sb 0)
            (set/f! lb 0)
            (set/f! rb 0))
          
	  (define cache-function #f)

          (define/override (insert s len pos)
            (set! cache-function #f)
            (super insert s len pos))

	  ;; for-each/sections : string -> dc number number -> void
          (define/private (for-each/sections str)
            (let loop ([n (string-length str)]
                       [len 0]
                       [blank? #t])
              (cond
                [(zero? n)
                 (if blank?
		     (λ (dc x y) (void))
		     (λ (dc x y)
                       (send dc draw-line (+ x n) y (+ x n (- len 1)) y)))]
                [else
                 (let ([white? (char-whitespace? (string-ref str (- n 1)))])
                   (cond
                     [(eq? white? blank?)
                      (loop (- n 1) (+ len 1) blank?)]
                     [else
		      (let ([res (loop (- n 1) 1 (not blank?))])
			(if blank?
			    res
			    (λ (dc x y)
			      (send dc draw-line (+ x n) y (+ x n (- len 1)) y)
			      (res dc x y))))]))])))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (let ([str (get-text 0 (get-count))])
	      (unless cache-function
		(set! cache-function (for-each/sections str)))
              (when (<= top y bottom)
		(cache-function dc x y))))
          (apply super-make-object args)))
      
      (define 1-pixel-tab-snip%
        (class tab-snip%
          (init-rest args)
          (inherit get-text get-count set-count get-flags)
          (define/override (split position first second)
            (let* ([str (get-text 0 (get-count))]
                   [new-second (make-object 1-pixel-string-snip%
                                 (substring str position (string-length str)))])
              (set-box! first this)
              (set-box! second new-second)
              (set-count position)
              (void)))
          (define/override (copy)
            (let ([cpy (make-object 1-pixel-tab-snip%)])
              (send cpy set-flags (get-flags))))
          
          (inherit get-admin)
          (define/override (get-extent dc x y wb hb db sb lb rb)
            (set/f! wb 0)
            (let ([admin (get-admin)])
              (when admin
                (let ([ed (send admin get-editor)])
                  (when (is-a? ed text%)
                    (let ([len-b (box 0)]
                          [tab-width-b (box 0)]
                          [in-units-b (box #f)])
                      (send ed get-tabs len-b tab-width-b in-units-b)
                      (when (and (or (equal? (unbox len-b) 0)
                                     (equal? (unbox len-b) null))
                                 (not (unbox in-units-b)))
                        (let ([tabspace (unbox tab-width-b)])
                          (set/f! wb (tabspace . - . (x . modulo . tabspace))))))))))
            
            (set/f! hb 0)
            (set/f! db 0)
            (set/f! sb 0)
            (set/f! lb 0)
            (set/f! rb 0))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (void))
          (apply super-make-object args)))

      (define (set/f! b n)
        (when (box? b)
          (set-box! b n)))

      (define delegate-mixin
        (mixin (basic<%>) (delegate<%>) 
          (inherit split-snip find-snip get-snip-position
                   find-first-snip get-style-list set-tabs)
          
          (define linked-snips #f)
          
          (define/private (copy snip)
            (let ([new-snip
                   (cond
                     [(is-a? snip tab-snip%)
                      (let ([new-snip (make-object 1-pixel-tab-snip%)])
                        (send new-snip insert (string #\tab) 1)
                        new-snip)]
                     [(is-a? snip string-snip%)
                      (make-object 1-pixel-string-snip%
                        (send snip get-text 0 (send snip get-count)))]
                     [else 
                      (let ([new-snip
                             (instantiate small-version-of-snip% ()
                               (big-snip snip))])
                        (hash-table-put! linked-snips snip new-snip)
                        new-snip)])])
              (send new-snip set-flags (send snip get-flags))
              (send new-snip set-style (send snip get-style))
              new-snip))

          (define delegate #f)
          (inherit get-highlighted-ranges)
          (define/public-final (get-delegate) delegate)
          (define/public-final (set-delegate _d)
            (set! delegate _d)
            (set! linked-snips (if _d
                                   (make-hash-table)
                                   #f))
            (refresh-delegate))
          
          (define/private (refresh-delegate)
            (when delegate
              (send delegate begin-edit-sequence)
              (send delegate lock #f)
              (when (is-a? this scheme:text<%>)
                (send delegate set-tabs null (send this get-tab-size) #f))
              (send delegate hide-caret #t)
              (send delegate erase)
              (send delegate set-style-list (get-style-list))
              (let loop ([snip (find-first-snip)])
                (when snip
                  (let ([copy-of-snip (copy snip)])
                    (send delegate insert
                          copy-of-snip
                          (send delegate last-position)
                          (send delegate last-position))
                    (loop (send snip next)))))
              (for-each
               (λ (range)
                 (send delegate highlight-range 
                       (range-start range)
                       (range-end range)
                       (range-color range)
                       (range-b/w-bitmap range)
                       (range-caret-space? range)
                       'high))
               (reverse (get-highlighted-ranges)))
              (send delegate lock #t)
              (send delegate end-edit-sequence)))
          
          (define/override highlight-range
            (opt-lambda (start end color [bitmap #f] [caret-space? #f] [priority 'low])
              (let ([res (super highlight-range start end color bitmap caret-space? priority)])
                (if delegate
                    (let ([delegate-res (send delegate highlight-range 
                                              start end color bitmap caret-space? priority)]) 
                      (λ ()
                        (res)
                        (delegate-res)))
                    res))))
          
          (inherit get-canvases get-active-canvas has-focus?)
          (define/override (on-paint before? dc left top right bottom dx dy draw-caret?)
            (super on-paint before? dc left top right bottom dx dy draw-caret?)
            (unless before?
              (let ([active-canvas (get-active-canvas)])
                (when active-canvas
                  (send (send active-canvas get-top-level-window) delegate-moved)))))

          (define/augment (on-edit-sequence)
            (when delegate
              (send delegate begin-edit-sequence))
            (inner (void) on-edit-sequence))
          
          (define/augment (after-edit-sequence)
            (when delegate
              (send delegate end-edit-sequence))
            (inner (void) after-edit-sequence))

          (define/override (resized snip redraw-now?)
            (super resized snip redraw-now?)
            (when (and delegate
                       linked-snips
                       (not (is-a? snip string-snip%)))
              (let ([delegate-copy (hash-table-get linked-snips snip (λ () #f))])
                (when delegate-copy
                  (send delegate resized delegate-copy redraw-now?)))))
          
          (define/augment (after-insert start len)
            (when delegate
              (send delegate begin-edit-sequence)
              (send delegate lock #f)
              (split-snip start)
              (split-snip (+ start len))
              (let loop ([snip (find-snip (+ start len) 'before)])
                (when snip
                  (unless ((get-snip-position snip) . < . start)
                    (send delegate insert (copy snip) start start)
                    (loop (send snip previous)))))
              (send delegate lock #t)
              (send delegate end-edit-sequence))
            (inner (void) after-insert start len))
          
          (define/augment (after-delete start len)
            (when delegate
              (send delegate lock #f)
              (send delegate begin-edit-sequence)
              (send delegate delete start (+ start len))
              (send delegate end-edit-sequence)
              (send delegate lock #t))
            (inner (void) after-delete start len))
          
          (define/augment (after-change-style start len)
            (when delegate
              (send delegate begin-edit-sequence)
              (send delegate lock #f)
              (split-snip start)
              (let* ([snip (find-snip start 'after)]
                     [style (send snip get-style)]
                     [other-style 
                      '(send (send delegate get-style-list) find-or-create-style
                             style delegate-style-delta)])
                (send delegate change-style style start (+ start len)))
              (send delegate lock #f)
              (send delegate end-edit-sequence))
            (inner (void) after-change-style start len))
          
          (define filename #f)
          (define format #f)
          (define/augment (on-load-file _filename _format)
            (set! filename _filename)
            (set! format _format)
            (inner (void) on-load-file _filename _format))
          (define/augment (after-load-file success?)
            (when success?
              (refresh-delegate))
            (inner (void) after-load-file success?))
          (super-instantiate ())))
    
      (define info<%> (interface (basic<%>)))
      
      (define info-mixin
        (mixin (editor:keymap<%> basic<%>) (info<%>)
          (inherit get-start-position get-end-position get-canvas
                   run-after-edit-sequence)
          (define/private (enqueue-for-frame call-method tag)
            (run-after-edit-sequence
             (rec from-enqueue-for-frame
               (λ ()
                 (call-with-frame call-method)))
             tag))
          
          ;; call-with-frame : ((is-a?/c frame:text-info<%>) -> void) -> void
          ;; calls the argument thunk with the frame showing this editor.
          (define/private (call-with-frame call-method)
            (let ([canvas (get-canvas)])
              (when canvas
                (let ([frame (send canvas get-top-level-window)])
                  (when (is-a? frame frame:text-info<%>)
                    (call-method frame))))))
          
          (define/override (set-anchor x)
            (super set-anchor x)
            (enqueue-for-frame 
             (λ (x) (send x anchor-status-changed))
             'framework:anchor-status-changed))
          (define/override (set-overwrite-mode x)
            (super set-overwrite-mode x)
            (enqueue-for-frame
             (λ (x) (send x overwrite-status-changed))
             'framework:overwrite-status-changed))
          (define/augment (after-set-position)
            (maybe-queue-editor-position-update)
            (inner (void) after-set-position))
          
          ;; maybe-queue-editor-position-update : -> void
          ;; updates the editor-position in the frame,
          ;; but delays it until the next low-priority event occurs.
          (define callback-running? #f)
          (define/private (maybe-queue-editor-position-update)
            (enqueue-for-frame 
             (λ (frame) 
               (unless callback-running?
                 (set! callback-running? #t)
                 (queue-callback
                  (λ ()
                    (send frame editor-position-changed)
                    (set! callback-running? #f))
                  #f)))
             'framework:info-frame:update-editor-position))
          
          (define/augment (after-insert start len)
            (maybe-queue-editor-position-update)
            (inner (void) after-insert start len))
          (define/augment (after-delete start len)
            (maybe-queue-editor-position-update)
            (inner (void) after-delete start len))
          (super-new)))
      
      (define clever-file-format<%> (interface ((class->interface text%))))
      
      (define clever-file-format-mixin
        (mixin ((class->interface text%)) (clever-file-format<%>)
          (inherit get-file-format set-file-format find-first-snip)
          (define/private (all-string-snips)
            (let loop ([s (find-first-snip)])
              (cond
                [(not s) #t]
                [(is-a? s string-snip%)
                 (loop (send s next))]
                [else #f])))
          (define/augment (on-save-file name format)
            (let ([all-strings? (all-string-snips)])
              (cond
                [(and all-strings?
                      (eq? format 'same)
                      (eq? 'standard (get-file-format))
                      (or (not (preferences:get 'framework:verify-change-format))
                          (gui-utils:get-choice
                           (string-constant save-as-plain-text) 
                           (string-constant yes)
                           (string-constant no))))
                 (set-file-format 'text)]
                [(and (not all-strings?)
                      (eq? format 'same)
                      (eq? 'text (get-file-format))
                      (or (not (preferences:get 'framework:verify-change-format))
                          (gui-utils:get-choice
                           (string-constant save-in-drs-format)
                           (string-constant yes)
                           (string-constant no))))
                 (set-file-format 'standard)]
                [else (void)]))
            (inner (void) on-save-file name format))
          (super-instantiate ())))
      
      
      (define file<%>
        (interface (editor:file<%> basic<%>)
          get-read-write?))
      
      (define file-mixin
        (mixin (editor:file<%> basic<%>) (file<%>)
          (inherit get-filename)
          (define read-write? #t)
          (define/public (get-read-write?) read-write?)
          (define/private (check-lock)
            (let* ([filename (get-filename)]
                   [can-edit? (if (and filename
                                       (file-exists? filename))
                                  (and (member 'write (file-or-directory-permissions filename)) 
                                       #t)
                                  #t)])
              (set! read-write? can-edit?)))
          
          (define/augment (can-insert? x y)
            (and read-write? (inner #t can-insert? x y)))
          (define/augment (can-delete? x y) 
            (and read-write? (inner #t can-delete? x y)))
          
          (define/augment (after-save-file success)
            (when success
              (check-lock))
            (inner (void) after-save-file success))
	   
          (define/augment (after-load-file sucessful?)
            (when sucessful?
              (check-lock))
            (inner (void) after-load-file sucessful?))
          (super-new)))

      
      (define ports<%>
        (interface ()
          delete/io
          get-insertion-point 
          set-insertion-point
          get-unread-start-point
          set-unread-start-point
          set-allow-edits
          get-allow-edits
          insert-between
          insert-before
          submit-to-port?
          on-submit
          send-eof-to-in-port
          send-eof-to-box-in-port
          reset-input-box
          clear-output-ports
          clear-input-port
          clear-box-input-port
          get-out-style-delta
          get-err-style-delta
          get-value-style-delta
          get-in-port
          get-in-box-port
          get-out-port
          get-err-port
          get-value-port
          after-io-insertion
          get-box-input-editor-snip%
          get-box-input-text%))
      
      (define-struct peeker (bytes skip-count pe resp-chan nack polling?) (make-inspector))
      (define-struct committer (kr commit-peeker-evt done-evt resp-chan resp-nack))
      
      (define msec-timeout 500)
      (define output-buffer-full 4096)
      
      (define-local-member-name 
        new-box-input
        box-input-not-used-anymore
        set-port-text)
      
      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      
      (define arrow-cursor (make-object cursor% 'arrow))
      
      (define eof-snip%
        (class image-snip%
          (init-field port-text)
          (define/override (get-extent dc x y w h descent space lspace rspace)
            (super get-extent dc x y w h descent space lspace rspace)
            (set-box/f! descent 7)) ;; depends on actual bitmap used ...
          
          (define/override (on-event dc x y editorx editory event)
            (when (send event button-up? 'left)
              (send port-text send-eof-to-box-in-port)))
          (define/override (adjust-cursor dc x y edx edy e)
            arrow-cursor)
          (super-make-object (icon:get-eof-bitmap))
          (inherit set-flags get-flags)
          (set-flags (list* 'handles-events (get-flags)))))
      
      (define ports-mixin
        (mixin (wide-snip<%>) (ports<%>)
          (inherit begin-edit-sequence
                   change-style
                   delete
                   end-edit-sequence
                   find-snip
                   insert
                   get-canvas
                   get-start-position
                   get-end-position
                   get-snip-position
                   get-style-list
                   is-locked?
                   last-position
                   lock
                   paragraph-start-position
                   position-paragraph
                   release-snip
                   set-caret-owner
                   split-snip
                   get-focus-snip
                   get-view-size
                   scroll-to-position
                   position-location)
          
          ;; private field
          (define eventspace (current-eventspace))
          
          ;; insertion-point : number
          ;; the place where the output ports insert data
          ;; only updated in `eventspace' (above)'s main thread
          (define insertion-point 0)
          
          ;; unread-start-points : number
          ;; from this position to the end of the buffer is the
          ;;      users editing that has not been committed to the
          ;;      port.
          ;; only updated in `eventspace' (above)'s main thread
          (define unread-start-point 0)
          
          ;; box-input : (union #f (is-a?/c editor-snip%))
          ;; the snip where the user's input is typed for the box input port
          (define box-input #f)
          (define eof-button (new eof-snip% (port-text this)))
          
          ;; allow-edits? : boolean
          ;; when this flag is set, only insert/delete after the
          ;; insertion-point are allowed.
          (define allow-edits? #f)
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  public interface
          ;;
          
          ;; insert-between : string/snp -> void
          ;; inserts something between the insertion point and the unread region
          (define/public-final (insert-between str/snp)
            (insert str/snp unread-start-point unread-start-point)
            (set! unread-start-point (+ unread-start-point 
                                        (amt-of-space str/snp))))
          
          ;; insert-before : string/snp -> void
          ;; inserts something before both the insertion point and the unread region
          (define/public-final (insert-before str/snp)
            (insert str/snp insertion-point insertion-point)
            (let ([amt (amt-of-space str/snp)])
              (set! insertion-point (+ insertion-point amt))
              (set! unread-start-point (+ unread-start-point amt))))
          
          (define/private (amt-of-space str/snp)
            (cond
              [(string? str/snp) (string-length str/snp)]
              [(is-a? str/snp snip%)
               (send str/snp get-count)]))
          
          (define/public-final (get-insertion-point) insertion-point)
          (define/public-final (set-insertion-point ip) (set! insertion-point ip))
          (define/public-final (get-unread-start-point)
            unread-start-point)
          (define/public-final (set-unread-start-point u) 
            (unless (<= u (last-position))
              (error 'set-unread-start-point "~e is too large, last-position is ~e"
                     unread-start-point 
                     (last-position)))
            (set! unread-start-point u))
          
          (define/public-final (set-allow-edits allow?) (set! allow-edits? allow?))
          (define/public-final (get-allow-edits) allow-edits?)
          
          (define/public-final (send-eof-to-in-port) 
            (channel-put read-chan (cons eof (position->line-col-pos unread-start-point))))
          (define/public-final (send-eof-to-box-in-port) 
            (channel-put box-read-chan (cons eof (position->line-col-pos unread-start-point))))
          (define/public-final (clear-input-port) (channel-put clear-input-chan (void)))
          (define/public-final (clear-box-input-port) (channel-put box-clear-input-chan (void)))
          (define/public-final (clear-output-ports) 
            (channel-put clear-output-chan (void))
            (init-output-ports))
          
          ;; delete/io: number number -> void
          (define/public-final (delete/io start end)
            (unless (<= start end insertion-point)
              (error 'delete/io "expected start (~a) <= end (~a) <= insertion-point (~a)"
                     start end insertion-point))
              
            (let ([dist (- end start)])
                (set! insertion-point (- insertion-point dist))
                (set! unread-start-point (- unread-start-point dist)))
              
            (let ([before-allowed? allow-edits?])
              (set! allow-edits? #t)
              (delete start end #f)
              (set! allow-edits? before-allowed?)))
                
          (define/public-final (get-in-port)
            (unless in-port (error 'get-in-port "not ready"))
            in-port)
          (define/public-final (get-in-box-port)
            (unless in-port (error 'get-in-box-port "not ready"))
            in-box-port)
          (define/public-final (get-out-port)
            (unless out-port (error 'get-out-port "not ready"))
            out-port)
          (define/public-final (get-err-port)
            (unless err-port (error 'get-err-port "not ready"))
            err-port)
          (define/public-final (get-value-port)
            (unless value-port (error 'get-value-port "not ready"))
            value-port)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  specialization interface
          ;;
          
          (define/pubment (submit-to-port? key) (inner #t submit-to-port? key))
          (define/pubment (on-submit) (inner (void) on-submit))
          (define/public (get-out-style-delta) 
            (let ([out-sd (make-object style-delta% 'change-nothing)])
              (send out-sd set-delta-foreground (make-object color% 150 0 150))
              out-sd))
          (define/public (get-err-style-delta)
            (let ([err-sd (make-object style-delta% 'change-italic)])
              (send err-sd set-delta-foreground (make-object color% 255 0 0))
              err-sd))
          (define/public (get-value-style-delta)
            (let ([value-sd (make-object style-delta% 'change-nothing)])
              (send value-sd set-delta-foreground (make-object color% 0 0 175))
              value-sd))

          (define/public (get-box-input-editor-snip%) editor-snip%)
          (define/public (get-box-input-text%) input-box%)
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  editor integration
          ;;
          
          (define/augment (can-insert? start len)
            (and (or allow-edits? 
                     (start . >= . unread-start-point))
                 (inner #t can-insert? start len)))
          
          (define/augment (can-delete? start len)
            (and (or allow-edits?
                     (start . >= . unread-start-point))
                 (inner #t can-delete? start len)))
          
          (define/override (on-local-char key)
            (let ([start (get-start-position)]
                  [end (get-end-position)]
                  [code (send key get-key-code)])
              (cond
                [(not (or (eq? code 'numpad-enter)
                          (equal? code #\return)
                          (equal? code #\newline)))
                 (super on-local-char key)]
                [(and (insertion-point . <= . start)
                      (= start end)
                      (submit-to-port? key))
                 (insert "\n")
                 (for-each/snips-chars
                  unread-start-point
                  (last-position)
                  (λ (s/c line-col-pos) 
                    (cond
                      [(is-a? s/c snip%)
                       (channel-put read-chan (cons s/c line-col-pos))]
                      [(char? s/c)
                       (for-each (λ (b) (channel-put read-chan (cons b line-col-pos)))
                                 (bytes->list (string->bytes/utf-8 (string s/c))))])))
                 (set! unread-start-point (last-position))
                 (set! insertion-point (last-position))
                 (on-submit)]
                [else
                 (super on-local-char key)])))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;; box input port management
          ;;
          
          (define/public-final (reset-input-box) 
            (when box-input
              (let ([l? (is-locked?)]
                    [old-allow-edits? allow-edits?])
                (lock #f)
                (set! allow-edits? #t)
                (send box-input release-from-owner)
                (send eof-button release-from-owner)
                (set! unread-start-point (- unread-start-point 2))
                (set! allow-edits? old-allow-edits?)
                (lock l?))
              (set! box-input #f)))
          
          (define/private (adjust-box-input-width)
            (when box-input
              (let ([w (box 0)]
                    [x (box 0)]
                    [bw (send (icon:get-eof-bitmap) get-width)])
                (get-view-size w #f)
                (let ([pos (- (last-position) 2)])
                  (position-location pos x #f #t
                                     (not (= pos (paragraph-start-position (position-paragraph pos))))))
                (let ([size (- (unbox w) (unbox x) bw 24)])
                  (when (positive? size)
                    (send box-input set-min-width size))))))
          
          (define/augment (on-display-size)
            (adjust-box-input-width)
            (inner (void) on-display-size))
          
          (define/private (on-box-peek)
            (unless box-input
              (let* ([ed (new (get-box-input-text%))]
                     [es (new (get-box-input-editor-snip%) 
                              (editor ed))]
                     [locked? (is-locked?)])
                (begin-edit-sequence)
                (send ed set-port-text this)
                (lock #f)
                #;(unless (= unread-start-point (paragraph-start-position (position-paragraph unread-start-point)))
                    (insert-between "\n"))
                (insert-between es)
                (insert-between eof-button)
                #;(send (get-canvas) add-wide-snip es)
                (set! box-input es)
                (adjust-box-input-width)
                (set-caret-owner es 'display)
                (lock locked?)
                (end-edit-sequence))))
          
          (define/public (new-box-input ed)
            (when (eq? ed (send box-input get-editor)) ;; just in case things get out of sync.
              (let ([locked? (is-locked?)])
                (begin-edit-sequence)
                (send box-input set-min-width 'none)
                (lock #f)
                
                (let ([old-insertion-point insertion-point])
                  (let loop ([snip (send (send box-input get-editor) find-first-snip)])
                    (when snip
                      (let ([next (send snip next)])
                        (send snip release-from-owner)
                        (do-insertion
                         (list (cons (cond
                                       [(is-a? snip string-snip%)
                                        (send snip get-text 0 (send snip get-count))]
                                       [else snip])
                                     (make-object style-delta%)))
                         #t)
                        (loop next))))
                  
                  ;; this is copied code ...
                  (for-each/snips-chars
                   old-insertion-point
                   insertion-point
                   (λ (s/c line-col-pos) 
                     (cond
                       [(is-a? s/c snip%)
                        (channel-put box-read-chan (cons s/c line-col-pos))]
                       [(char? s/c)
                        (for-each (λ (b) (channel-put box-read-chan (cons b line-col-pos)))
                                  (bytes->list (string->bytes/utf-8 (string s/c))))]))))
                
                (lock locked?)
                (adjust-box-input-width)
                (end-edit-sequence))))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;; output port syncronization code
          ;;
          
          ;; flush-chan : (channel (evt void))
          ;; signals that the buffer-thread should flush pending output
          ;; the evt inside is waited on to indicate the flush has occurred
          (define flush-chan (make-channel))
          
          ;; clear-output-chan : (channel void)
          (define clear-output-chan (make-channel))
          
          ;; write-chan : (channel (cons (union snip bytes) style))
          ;; send output to the editor
          (define write-chan (make-channel))
          
          ;; readers-chan : (channel (list (channel (union byte snip))
          ;;                               (channel ...)))
          (define readers-chan (make-channel))
          
          ;; queue-insertion : (listof (cons (union string snip) style)) evt -> void
          ;; txt is in the reverse order of the things to be inserted.
          ;; the evt is waited on when the text has actually been inserted
          ;; thread: any thread, except the eventspace main thread
          (define/private (queue-insertion txts signal)
            (parameterize ([current-eventspace eventspace])
              (queue-callback
               (λ ()
                 (do-insertion txts #f)
                 (sync signal)))))
          
          ;; do-insertion : (listof (cons (union string snip) style-delta)) boolean -> void
          ;; thread: eventspace main thread
          (define/private (do-insertion txts showing-input?)
            (let ([locked? (is-locked?)])
              (begin-edit-sequence)
              (lock #f)
              (set! allow-edits? #t)
              (let loop ([txts txts])
                (cond
                  [(null? txts) (void)]
                  [else 
                   (let* ([fst (car txts)]
                          [str/snp (car fst)]
                          [style (cdr fst)])
                     
                     (let ([inserted-count
                            (if (is-a? str/snp snip%)
                                (send str/snp get-count)
                                (string-length str/snp))]
                           [old-insertion-point insertion-point])
                       (set! insertion-point (+ insertion-point inserted-count))
                       (set! unread-start-point (+ unread-start-point inserted-count))
                       
                       (insert (if (is-a? str/snp snip%)
                                   (send str/snp copy)
                                   str/snp)
                               old-insertion-point
                               old-insertion-point
                               #t)
                       
                       ;; the idea here is that if you made a string snip, you
                       ;; could have made a string and gotten the style, so you
                       ;; must intend to have your own style.
                       (unless (is-a? str/snp string-snip%)
                         (change-style style old-insertion-point insertion-point))))
                   (loop (cdr txts))]))
              (set! allow-edits? #f)
              (lock locked?)
              (unless showing-input?
                (when box-input
                  (adjust-box-input-width)
                  (when (eq? box-input (get-focus-snip))
                    (scroll-to-position (last-position)))))
              (end-edit-sequence)
              (unless (null? txts)
                (after-io-insertion))))
          
          (define/public (after-io-insertion) (void))
          
          (define output-buffer-thread
            (let ([converter (bytes-open-converter "UTF-8-permissive" "UTF-8")])
              (thread
               (λ ()
                 (let loop (;; text-to-insert : (queue (cons (union snip bytes) style))
                            [text-to-insert (empty-queue)]
                            [last-flush (current-inexact-milliseconds)])
                   
                   (sync
                    (if (queue-empty? text-to-insert)
                        never-evt
                        (handle-evt
                         (alarm-evt (+ last-flush msec-timeout))
                         (λ (_)
                           (let-values ([(viable-bytes remaining-queue) (split-queue converter text-to-insert)])
                             (queue-insertion viable-bytes always-evt)
                             (loop remaining-queue (current-inexact-milliseconds))))))
                    (handle-evt
                     flush-chan
                     (λ (return-evt)
                       (let-values ([(viable-bytes remaining-queue) (split-queue converter text-to-insert)])
                         (queue-insertion viable-bytes return-evt)
                         (loop remaining-queue (current-inexact-milliseconds)))))
                    (handle-evt
                     clear-output-chan
                     (λ (_)
                       (loop (empty-queue) (current-inexact-milliseconds))))
                    (handle-evt
                     write-chan
                     (λ (pr)
                       (let ([new-text-to-insert (enqueue pr text-to-insert)])
                         (cond
                           [((queue-size text-to-insert) . < . output-buffer-full)
                            (loop new-text-to-insert last-flush)]
                           [else
                            (let ([chan (make-channel)])
                              (let-values ([(viable-bytes remaining-queue) 
                                            (split-queue converter new-text-to-insert)])
                                (queue-insertion viable-bytes (channel-put-evt chan (void)))
                                (channel-get chan)
                                (loop remaining-queue (current-inexact-milliseconds))))]))))))))))
          
          (field [in-port-args #f]
                 [out-port #f]
                 [err-port #f]
                 [value-port #f])
          
          (define/private (init-output-ports)
            
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;
            ;;  the following must be able to run
            ;;  in any thread (even concurrently)
            ;;
            (define (make-write-bytes-proc style)
              (λ (to-write start end block/buffer? enable-breaks?)
                (cond
		 [(= start end) (flush-proc)]
		 [(eq? (current-thread) (eventspace-handler-thread eventspace))
		  (error 'write-bytes-proc "cannot write to port on eventspace main thread")]
		 [else
                  (channel-put write-chan (cons (subbytes to-write start end) style))])
                (- end start)))
            
            (define (flush-proc)
              (cond
                [(eq? (current-thread) (eventspace-handler-thread eventspace))
                 (error 'flush-proc "cannot flush port on eventspace main thread")]
                [else
                 (sync
                  (nack-guard-evt
                   (λ (fail-channel)
                     (let* ([return-channel (make-channel)]
                            [return-evt
                             (choice-evt
                              fail-channel
                              (channel-put-evt return-channel (void)))])
                       (channel-put flush-chan return-evt)
                       return-channel))))]))
            
            (define (out-close-proc)
              (void))
            
            (define (make-write-special-proc style)
              (λ (special can-buffer? enable-breaks?)
                (cond
                  [(eq? (current-thread) (eventspace-handler-thread eventspace))
                   (error 'write-bytes-proc "cannot write to port on eventspace main thread")]
                  [else
                   (let ([str/snp (cond
                                    [(string? special) special]
                                    [(is-a? special snip%) special]
                                    [else (format "~s" special)])])
                     (channel-put 
                      write-chan 
                      (cons str/snp style)))])
                #t))
            
            (let* ([add-standard
                    (λ (sd)
                      (let* ([style-list (get-style-list)] 
                             [std (send style-list find-named-style "Standard")])
                        (if std
                            (send style-list find-or-create-style std sd)
                            (let ([basic (send style-list find-named-style "Basic")])
                              (send style-list find-or-create-style basic sd)))))]
                   [out-style (add-standard (get-out-style-delta))]
                   [err-style (add-standard (get-err-style-delta))]
                   [value-style (add-standard (get-value-style-delta))])
              (set! out-port (make-output-port #f
                                               always-evt
                                               (make-write-bytes-proc out-style)
                                               out-close-proc
                                               (make-write-special-proc out-style)))
              (set! err-port (make-output-port #f 
                                               always-evt
                                               (make-write-bytes-proc err-style)
                                               out-close-proc
                                               (make-write-special-proc err-style)))
              (set! value-port (make-output-port #f
                                                 always-evt
                                                 (make-write-bytes-proc value-style)
                                                 out-close-proc
                                                 (make-write-special-proc value-style)))
              (let ([install-handlers
                     (λ (port)
                       (set-interactive-print-handler port)
                       (set-interactive-write-handler port)
                       (set-interactive-display-handler port))])
                (install-handlers out-port)
                (install-handlers err-port)
                (install-handlers value-port))))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  helpers
          ;;

          ;; type line-col-pos = (list (union #f fixnum) (union #f fixnum) (union #f fixnum)))

          ;; position->line-col-pos : number -> (list number number number)
          (define/private (position->line-col-pos pos)
            (let* ([para (position-paragraph pos)]
                   [para-start (paragraph-start-position para)])
              (list (+ para 1)
                    (- pos para-start)
                    (+ pos 1))))
          
          ;; for-each/snips-chars : number number ((union char snip) line-col-pos -> void) -> void
          (define/private (for-each/snips-chars start end func)
            (split-snip start)
            (split-snip end)
            (let loop ([snip (find-snip start 'after-or-none)])
              (cond
                [(not snip) (void)]
                [(< (get-snip-position snip) end)
                 (let ([line-col-pos (position->line-col-pos (get-snip-position snip))])
                   (cond
                     [(is-a? snip string-snip%)
                      (let ([str (send snip get-text 0 (send snip get-count))])
                        (let loop ([i 0])
                          (when (< i (string-length str))
                            (func (string-ref str i)
                                  (list (car line-col-pos)
                                        (+ i (cadr line-col-pos))
                                        (+ i (caddr line-col-pos))))
                            (loop (+ i 1)))))
                      (loop (send snip next))]
                     [else
                      (func (send snip copy) line-col-pos)
                      (loop (send snip next))]))]
                [else (void)])))

                
          ;; split-queue : converter (queue (cons (union snip bytes) style) 
          ;;            -> (values (listof (queue (cons (union snip bytes) style)) queue)
          ;; this function must only be called on the output-buffer-thread
          ;; extracts the viable bytes (and other stuff) from the front of the queue
          ;; and returns them as strings (and other stuff).
          (define/private (split-queue converter q)
            (let ([lst (queue->list q)])
              (let loop ([lst lst]
                         [acc null])
                (if (null? lst)
                    (values (reverse acc)
                            (empty-queue))
                    (let-values ([(front rest) (peel lst)])
                      (cond
                        [(not front) (values (reverse acc)
                                             (empty-queue))]
                        [(bytes? (car front))
                         (let ([the-bytes (car front)]
                               [key (cdr front)])
                           (if (null? rest)
                               (let-values ([(converted-bytes src-read-k termination)
                                             (bytes-convert converter the-bytes)])
                                 (if (eq? termination 'aborts)
                                     (values (reverse (cons (cons (bytes->string/utf-8 converted-bytes) key) acc))
                                             (enqueue 
                                              (cons (subbytes the-bytes
                                                              (- (bytes-length the-bytes) src-read-k) 
                                                              (bytes-length the-bytes))
                                                    key)
                                              (empty-queue)))
                                     (values (reverse (cons (cons (bytes->string/utf-8 converted-bytes) key) acc))
                                             (empty-queue))))
                               (let-values ([(converted-bytes src-read-k termination)
                                             (bytes-convert converter the-bytes)]
                                            [(more-bytes more-termination) (bytes-convert-end converter)])
                                 (loop rest
                                       (cons (cons (bytes->string/utf-8 (bytes-append converted-bytes more-bytes))
                                                   key)
                                             acc)))))]
                        [else (loop rest
                                    (cons front acc))]))))))
          
          ;; peel : (cons (cons (union snip bytes) X) (listof (cons (union snip bytes) X))
          ;;     -> (values (cons (union snip bytes) X) (listof (cons (union snip bytes) X)
          ;; finds the first segment of bytes with the same style and combines them,
          ;; otherwise a lot like (define (peel x) (values (car x) (cdr x)))
          (define/private (peel lst)
            (let loop ([lst lst]
                       [acc #f]
                       [key #f])
              (cond
                [(null? lst) (values (cons acc key) null)]
                [else 
                 (let* ([fst (car lst)]
                        [fst-key (cdr fst)]
                        [fst-val (car fst)])
                   (cond
                     [(and (not key) (bytes? fst-val))
                      (loop (cdr lst)
                            fst-val
                            fst-key)]
                     [(and key (bytes? fst-val) (eq? key fst-key))
                      (loop (cdr lst)
                            (bytes-append acc fst-val)
                            key)]
                     [(not key)
                      (values fst (cdr lst))]
                     [else (if acc
                               (values (cons acc key) lst)
                               (values fst (cdr lst)))]))])))
          
          (super-new)
          (init-output-ports)
          (define-values (in-port read-chan clear-input-chan)
            (start-text-input-port this #f))
          (define-values (in-box-port box-read-chan box-clear-input-chan) 
            (start-text-input-port this (lambda () (on-box-peek))))))

      (define input-box<%>
        (interface ((class->interface text%))
          ))
      
      (define input-box-mixin
        (mixin ((class->interface text%)) (input-box<%>)
          (inherit erase lock)
          
          (define port-text #f)
          (define/public (set-port-text pt) (set! port-text pt))
          
          (define in-use? #t)
          (define/public (box-input-not-used-anymore)
            (lock #t)
            (set! in-use? #f))
          
          (define/override (on-default-char kevt)
            (super on-default-char kevt)
            (when in-use?
              (case (send kevt get-key-code)
                [(numpad-enter #\return)
                 (send port-text new-box-input this)]
                [else (void)])))
          
          (super-new)))
      
      (define (start-text-input-port source on-peek)

        ;; eventspace at the time this function was called. used for peek callbacks
        (define eventspace (current-eventspace))
        
        ;; read-chan : (channel (cons (union byte snip eof) line-col-pos))
        ;; send input from the editor
        (define read-chan (make-channel))
        
        ;; clear-input-chan : (channel void)
        (define clear-input-chan (make-channel))
        
        ;; progress-event-chan : (channel (cons (channel event) nack-evt)))
        (define progress-event-chan (make-channel))
        
        ;; peek-chan : (channel peeker)
        (define peek-chan (make-channel))
        
        ;; commit-chan : (channel committer)
        (define commit-chan (make-channel))
        
        ;; position-chan : (channel (cons (channel void) (channel line-col-pos)))
        (define position-chan (make-channel))
        
        (define input-buffer-thread
          (thread
           (λ ()
             
             ;; these vars are like arguments to the loop function
             ;; they are only set right before loop is called.
             ;; This is done to avoid passing the same arguments
             ;; over and over to loop.
             (define peeker-sema (make-semaphore 0))
             (define peeker-evt (semaphore-peek-evt peeker-sema))
             (define bytes-peeked 0)
             (define response-evts '())
             (define peekers '())     ;; waiting for a peek
             (define committers '())  ;; waiting for a commit
             (define positioners '()) ;; waiting for a position
             (define data (empty-queue)) ;; (queue (cons (union byte snip eof) line-col-pos))
             (define position #f)
             
             ;; loop : -> alpha
             ;; the main loop for this thread
             (define (loop)
               (let-values ([(not-ready-peekers new-peek-response-evts)
                             (separate peekers service-waiter)]
                            [(potential-commits new-commit-response-evts) 
                             (separate 
                              committers
                              (service-committer data peeker-evt))])
                 (when (and on-peek
                            (not (null? not-ready-peekers)))
                   (parameterize ([current-eventspace eventspace])
                     (queue-callback on-peek)))
                 (set! peekers not-ready-peekers)
                 (set! committers potential-commits)
                 (set! response-evts 
                       (append response-evts
                               new-peek-response-evts
                               new-commit-response-evts))
                 (sync
                  (handle-evt
                   position-chan
                   (λ (pr)
                     (let ([nack-chan (car pr)]
                           [resp-chan (cdr pr)])
                       (set! positioners (cons pr positioners))
                       (loop))))
                  (if position
                      (apply choice-evt (map service-positioner positioners))
                      never-evt)
                  (handle-evt
                   read-chan
                   (λ (ent)
                     (set! data (enqueue ent data))
                     (unless position
                       (set! position (cdr ent)))
                     (loop)))
                  (handle-evt
                   clear-input-chan
                   (λ (_)
                     (semaphore-post peeker-sema)
                     (set! peeker-sema (make-semaphore 0))
                     (set! peeker-evt (semaphore-peek-evt peeker-sema))
                     (set! data (empty-queue))
                     (set! position #f)
                     (loop)))
                  (handle-evt
                   progress-event-chan
                   (λ (return-pr)
                     (let ([return-chan (car return-pr)]
                           [return-nack (cdr return-pr)])
                       (set! response-evts
                             (cons (choice-evt
                                    return-nack
                                    (channel-put-evt return-chan peeker-evt))
                                   response-evts))
                       (loop))))
                  (handle-evt
                   peek-chan
                   (λ (peeker)
                     (set! peekers (cons peeker peekers))
                     (loop)))
                  (handle-evt
                   commit-chan
                   (λ (committer)
                     (set! committers (cons committer committers))
                     (loop)))
                  (apply 
                   choice-evt
                   (map
                    (λ (a-committer)
                      (match a-committer
                        [($ committer 
                            kr
                            commit-peeker-evt
                            done-evt
                            resp-chan
                            resp-nack)
                         (choice-evt
                          (handle-evt 
                           commit-peeker-evt
                           (λ (_)
                             ;; this committer will be thrown out in next iteration
                             (loop)))
                          (handle-evt
                           done-evt
                           (λ (v)
                             (let ([nth-pos (cdr (peek-n data (- kr 1)))])
                               (set! position
                                     (list (car nth-pos)
                                           (+ 1 (cadr nth-pos))
                                           (+ 1 (caddr nth-pos)))))
                             (set! data (dequeue-n data kr))
                             (semaphore-post peeker-sema)
                             (set! peeker-sema (make-semaphore 0))
                             (set! peeker-evt (semaphore-peek-evt peeker-sema))
                             (set! committers (remq a-committer committers))
                             (set! response-evts
                                   (cons 
                                    (choice-evt
                                     resp-nack
                                     (channel-put-evt resp-chan #t))
                                    response-evts))
                             (loop))))]))
                    committers))
                  (apply choice-evt 
                         (map (λ (resp-evt)
                                (handle-evt
                                 resp-evt
                                 (λ (_)
                                   (set! response-evts (remq resp-evt response-evts))
                                   (loop))))
                              response-evts)))))
             
             ;; service-positioner : (cons (channel void) (channel line-col-pos)) -> evt
             (define (service-positioner pr)
               (let ([nack-evt (car pr)]
                     [resp-evt (cdr pr)])
                 (handle-evt
                  (choice-evt nack-evt 
                              (channel-put-evt resp-evt position))
                  (let ([sent-position position])
                    (λ (_)
                      (set! positioners (remq pr positioners))
                      (loop))))))
             
             ;; service-committer : queue evt -> committer -> (union #f evt)
             ;; if the committer can be dumped, return an evt that
             ;; does the dumping. otherwise, return #f
             (define ((service-committer data peeker-evt) a-committer)
               (match a-committer
                 [($ committer
                     kr commit-peeker-evt
                     done-evt resp-chan resp-nack)
                  (let ([size (queue-size data)])
                    (cond
                      [(not (eq? peeker-evt commit-peeker-evt))
                       (choice-evt
                        resp-nack
                        (channel-put-evt resp-chan #f))]
                      [(< size kr)
                       (choice-evt
                        resp-nack
                        (channel-put-evt resp-chan 'commit-failure))]
                      [else  ;; commit succeeds
                       #f]))]))
             
             ;; service-waiter : peeker -> (union #f evt)
             ;; if the peeker can be serviced, build an event to service it
             ;; otherwise return #f
             (define (service-waiter a-peeker)
               (match a-peeker
                 [($ peeker bytes skip-count pe resp-chan nack-evt polling?)
                  (cond
                    [(and pe (not (eq? pe peeker-evt)))
                     (choice-evt (channel-put-evt resp-chan #f)
                                 nack-evt)]
                    [((queue-size data) . > . skip-count)
                     (let ([nth (car (peek-n data skip-count))])
                       (choice-evt
                        nack-evt
                        (cond
                          [(byte? nth)
                           (bytes-set! bytes 0 nth)
                           (channel-put-evt resp-chan 1)]
                          [(eof-object? nth)
                           (channel-put-evt resp-chan nth)]
                          [else
                           (channel-put-evt 
                            resp-chan
                            (λ (src line col pos)
                              (if (is-a? nth readable-snip<%>)
                                  (send nth read-special src line col pos)
                                  nth)))])))]
                    [polling? 
                     (choice-evt
                      nack-evt
                      (channel-put-evt resp-chan 0))]
                    [else
                     #f])]))
             
             ;; separate (listof X) (X -> (union #f Y)) -> (values (listof X) (listof Y))
             ;; separates `eles' into two lists -- those that `f' returns #f for
             ;; and then the results of calling `f' for those where `f' doesn't return #f
             (define (separate eles f)
               (let loop ([eles eles]
                          [transformed '()]
                          [left-alone '()])
                 (cond
                   [(null? eles) (values left-alone transformed)]
                   [else (let* ([ele (car eles)]
                                [maybe (f ele)])
                           (if maybe
                               (loop (cdr eles)
                                     (cons maybe transformed)
                                     left-alone)
                               (loop (cdr eles)
                                     transformed
                                     (cons ele left-alone))))])))
             
             ;;; start things going
             (loop))))
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;;  the following must be able to run
        ;;  in any thread (even concurrently)
        ;;
        (define (read-bytes-proc bstr)
          (let* ([progress-evt (progress-evt-proc)]
                 [v (peek-proc bstr 0 progress-evt)])
            (cond
              [(sync/timeout 0 progress-evt)
               0]
              [else 
               (wrap-evt 
                v 
                (λ (v) 
                  (if (and (number? v) (zero? v))
                      0
                      (if (commit-proc (if (number? v) v 1)
                                       progress-evt
                                       always-evt)
                          v
                          0))))])))
        
        (define (peek-proc bstr skip-count progress-evt)
          (poll-guard-evt
           (lambda (polling?)
             (if polling?
                 (let ([answer 
                        (sync
                         (nack-guard-evt
                          (λ (nack)
                            (let ([chan (make-channel)])
                              (channel-put peek-chan (make-peeker bstr skip-count progress-evt chan nack #t))
                              chan))))])
                   (wrap-evt always-evt (λ (_) answer)))
                 (nack-guard-evt
                  (λ (nack)
                    (let ([chan (make-channel)])
                      (channel-put peek-chan (make-peeker bstr skip-count progress-evt chan nack #f))
                      chan)))))))
        
        (define (progress-evt-proc)
          (sync
           (nack-guard-evt
            (λ (nack)
              (let ([chan (make-channel)])
                (channel-put progress-event-chan (cons chan nack))
                chan)))))
        
        (define (commit-proc kr progress-evt done-evt)
          (sync
           (nack-guard-evt
            (λ (nack)
              (let ([chan (make-channel)])
                (channel-put commit-chan (make-committer kr progress-evt done-evt chan nack))
                chan)))))
        
        (define (close-proc) (void))
        
        (define (position-proc)
          (let ([chan (make-channel)])
            (apply 
             values
             (sync
              (nack-guard-evt
               (λ (fail)
                 (channel-put position-chan (cons fail chan))
                 chan))))))
        (let ([p (make-input-port source
                                  read-bytes-proc
                                  peek-proc
                                  close-proc
                                  progress-evt-proc
                                  commit-proc
                                  position-proc)])
          (port-count-lines! p)
          (values p read-chan clear-input-chan)))
      

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; queues
      ;;
      (define-struct queue (front back count))
      (define (empty-queue) (make-queue '() '() 0))
      (define (enqueue e q) (make-queue 
                             (cons e (queue-front q))
                             (queue-back q)
                             (+ (queue-count q) 1)))
      (define (queue-first q)
        (flip-around q)
        (let ([back (queue-back q)])
          (if (null? back)
              (error 'queue-first "empty queue")
              (car back))))
      (define (queue-rest q)
        (flip-around q)
        (let ([back (queue-back q)])
          (if (null? back)
              (error 'queue-rest "empty queue")
              (make-queue (queue-front q)
                          (cdr back)
                          (- (queue-count q) 1)))))
      (define (flip-around q)
        (when (null? (queue-back q))
          (set-queue-back! q (reverse (queue-front q)))
          (set-queue-front! q '())))
      
      (define (queue-empty? q) (zero? (queue-count q)))
      (define (queue-size q) (queue-count q))
      
      ;; queue->list : (queue x) -> (listof x)
      ;; returns the elements in the order that successive deq's would have
      (define (queue->list q) 
        (let ([ans (append (queue-back q) (reverse (queue-front q)))])
          (set-queue-back! q ans)
          (set-queue-front! q '())
          ans))
      
      ;; dequeue-n : queue number -> queue
      (define (dequeue-n queue n)
        (let loop ([q queue]
                   [n n])
          (cond
            [(zero? n) q]
            [(queue-empty? q) (error 'dequeue-n "not enough!")]
            [else (loop (queue-rest q) (- n 1))])))
      
      ;; peek-n : queue number -> queue
      (define (peek-n queue init-n)
        (let loop ([q queue]
                   [n init-n])
          (cond
            [(zero? n) 
             (when (queue-empty? q)
               (error 'peek-n "not enough; asked for ~a but only ~a available" 
                      init-n 
                      (queue-size queue)))
             (queue-first q)]
            [else 
             (when (queue-empty? q)
               (error 'dequeue-n "not enough!"))
             (loop (queue-rest q) (- n 1))])))
      
      ;;
      ;;  end queue abstraction
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define basic% (basic-mixin (editor:basic-mixin text%)))
      (define hide-caret/selection% (hide-caret/selection-mixin basic%))
      (define nbsp->space% (nbsp->space-mixin basic%))
      (define delegate% (delegate-mixin basic%))
      (define wide-snip% (wide-snip-mixin basic%))
      (define standard-style-list% (editor:standard-style-list-mixin wide-snip%))
      (define input-box% (input-box-mixin standard-style-list%))
      (define -keymap% (editor:keymap-mixin standard-style-list%))
      (define return% (return-mixin -keymap%))
      (define autowrap% (editor:autowrap-mixin -keymap%))
      (define file% (file-mixin (editor:file-mixin autowrap%)))
      (define clever-file-format% (clever-file-format-mixin file%))
      (define backup-autosave% (editor:backup-autosave-mixin clever-file-format%))
      (define searching% (searching-mixin backup-autosave%))
      (define info% (info-mixin (editor:info-mixin searching%))))))

(module wxlitem racket/base
  (require racket/class
           racket/file
           (only-in racket/base remq)
           racket/snip/private/prefs
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "helper.rkt"
           "const.rkt"
           "wx.rkt"
           "check.rkt"
           "wxwindow.rkt"
           "wxitem.rkt"
           "wxpanel.rkt")

  (provide (protect-out wx-choice%
                        wx-list-box%
                        wx-radio-box%
                        wx-gauge%
                        wx-slider%))

  ;; ----------------------------------------

  (define (is-horiz? style parent)
    (cond
     [(memq 'vertical-label style) #f]
     [(memq 'horizontal-label style) #t]
     [else (eq? (send (send parent get-window) get-label-position) 'horizontal)]))

  (define (make-sub horiz? proxy this ha va)
    (if horiz?
        (begin
          (send this alignment ha va)
          this)
        (let ([p (make-object wx-vertical-pane% #f proxy this null #f)])
	  (send p skip-enter-leave-events #t)
          (send p skip-subwindow-events? #t)
          (send (send p area-parent) add-child p)
          (send p alignment ha va)
          p)))

  (define (make-label label proxy p font)
    (and label 
         (let ([l (make-object wx-message% #f proxy p label -1 -1 null font)])
	   (send l skip-enter-leave-events #t)
           (send l skip-subwindow-events? #t)
           l)))

  (define (filter-style style)
    (remq 'deleted style))

  (define-syntax-rule (bounce c (m . args) ...)
    (begin
      (define/public m (lambda args (send c m . args)))
      ...))

  ;; ----------------------------------------
  
  (define wx-label-panel%
    (class wx-control-horizontal-panel% 
      (init proxy parent label style font halign valign)
      (inherit area-parent skip-enter-leave-events set-event-positions-wrt)
      (define c #f)

      (define/override (enable on?) (if c (send c enable on?) (void)))
      (define/override (is-enabled?) (if c (send c is-enabled?) #t))
      (define/override (is-window-enabled?) (if c (send c is-window-enabled?) #t))

      (super-make-object #f proxy parent (if (memq 'deleted style) '(deleted) null) #f)
      (skip-enter-leave-events #t)
      (unless (memq 'deleted style)
        (send (area-parent) add-child this))
      (define horiz? (is-horiz? style parent))
      (define p (make-sub horiz? proxy this (if horiz? 'left halign) valign))

      (define l (make-label label proxy p font))
      (define/public (set-label s) (when l (send l set-label s)))
      (define/public (get-label) (and l (send l get-label)))

      (define/override (client-to-screen x y)
        (if c
            (send c client-to-screen x y)
            (super client-to-screen x y)))
      (define/override (screen-to-client x y)
        (if c
            (send c screen-to-client x y)
            (super screen-to-client x y)))

      (define/public (get-p) p)
      (define/public (set-c v sx? sy?) 
        (set! c v)
        (set-event-positions-wrt c)
        (when l (send l set-event-positions-wrt c))
        (when p (send p set-event-positions-wrt c))
        (send c stretchable-in-x sx?)
        (send c stretchable-in-y sy?)
        (send c skip-subwindow-events? #t))))

  ;; ----------------------------------------

  (define wx-internal-choice% 
    (class (make-window-glue% (make-simple-control% wx:choice% 0 0))
      (init mred proxy parent cb label x y w h choices style font)
      (override*
       [handles-key-code 
        (lambda (x alpha? meta?) 
          (or (memq x '(up down))
              (and alpha? (not meta?))))])
      (super-make-object mred proxy style parent cb label x y w h choices (cons 'deleted style) font)))

  (define wx-choice%
    (class wx-label-panel% 
      (init mred proxy parent cb label x y w h choices style font)
      (inherit stretchable-in-y stretchable-in-x get-p set-c)

      (super-make-object proxy parent label style font 'left 'center)

      (define c (make-object wx-internal-choice% mred proxy (get-p) cb label x y w h choices 
                             (filter-style style) font))
      (set-c c #t #f)

      (bounce 
       c
       (set-selection i)
       (get-selection)
       (number)
       (clear)
       (append lbl)
       (delete i))

      (stretchable-in-y #f)
      (stretchable-in-x #f)))

  ;; ----------------------------------------

  (define list-box-wheel-step #f)

  (define wx-internal-list-box%
    (make-window-glue% 
     (class (make-control% wx:list-box% 0 0 #t #t)
       (init parent cb label kind x y w h choices style font 
             label-font columns column-order)
       (inherit get-first-item
		set-first-visible-item
                number-of-visible-items)
       (private*
        [scroll (lambda (dir)
                  (unless list-box-wheel-step
                    (set! list-box-wheel-step (get-preference* '|GRacket:wheelStep| (lambda () 3)))
                    (unless (and (number? list-box-wheel-step)
                                 (exact? list-box-wheel-step)
                                 (integer? list-box-wheel-step)
                                 (<= 1 list-box-wheel-step 100))
                      (set! list-box-wheel-step 3)))
                  (let ([top (get-first-item)])
                    (set-first-visible-item
                     (max 0 (+ top (* (min list-box-wheel-step (number-of-visible-items)) dir))))))])
       (override*
        [handles-key-code (lambda (x alpha? meta?)
                            (case x
                              [(up down) #t]
                              [else (and alpha? (not meta?))]))]
        [pre-on-char (lambda (w e)
                       (or (super pre-on-char w e)
                           (and (not (eq? (system-type) 'macosx)) ; scrolling is built into NSListView 
                                (case (send e get-key-code)
                                  [(wheel-up) (scroll -1) #t]
                                  [(wheel-down) (scroll 1) #t]
                                  [else #f]))))])
       (super-make-object style parent cb label kind x y w h choices (cons 'deleted style) font 
                          label-font columns column-order))))

  (define wx-list-box%
    (class wx-label-panel% 
      (init mred proxy parent cb label kind x y w h choices style font label-font columns column-order)
      (inherit get-p set-c)

      (super-make-object proxy parent label style font 'left 'top)

      (define c (make-object wx-internal-list-box% mred proxy (get-p) cb label kind x y w h choices 
                             (filter-style style) font label-font columns column-order))
      (set-c c #t #t)

      (bounce
       c
       (get-label-font)
       (set-string i s col)
       (set-selection i)
       (get-selection)
       (get-selections)
       (visible-range)
       (get-first-item)
       (number-of-visible-items)
       (set-first-visible-item i)
       (number)
       (get-row n)
       (set-data i v)
       (get-data i)
       (selected? i)
       (delete i)
       (clear)
       (set choices . more)
       (reset)
       (get-column-order)
       (set-column-order l)
       (set-column-label i l)
       (set-column-size i w mn mx)
       (get-column-size i)
       (delete-column i)
       (append-column l))
      (define/public select 
        (case-lambda
         [(i) (send c select i)]
         [(i on?) (send c select i on?)]
         [(i on? extend?) (send c select i on? extend?)]))
      (define/public append
        (case-lambda
         [(s) (send c append s)]
         [(s v) (send c append s v)]))))

  ;; ----------------------------------------

  (define wx-internal-radio-box%
    (make-window-glue% 
     (class (make-simple-control% wx:radio-box% 0 0)
       (init parent cb label x y w h choices major style font)
       (inherit number orig-enable set-selection command)
       (override*
        [enable
         (case-lambda
	   [(on?) (super enable on?)]
	   [(which on?) (when (< -1 which (number))
			  (vector-set! enable-vector which (and on? #t))
			  (orig-enable which on?))])]
        [is-enabled?
         (case-lambda
	   [() (super is-enabled?)]
	   [(which) (and (< -1 which (number))
			 (vector-ref enable-vector which))])])

       (define is-vertical? (memq 'vertical style))
       (public*
        [vertical? (lambda () is-vertical?)]
        [char-to-button (lambda (i)
                          (as-exit
                           (lambda ()
                             (set-selection i)
                             (command (make-object wx:control-event% 'radio-box)))))])

       (super-make-object style parent cb label x y w h choices major (cons 'deleted style) font)

       (define enable-vector (make-vector (number) #t)))))

  (define wx-radio-box%
    (class wx-label-panel% 
      (init mred proxy parent cb label x y w h choices major style font)
      (inherit stretchable-in-y stretchable-in-x get-p set-c)

      (super-make-object proxy parent label style font 'left 'center)

      (define c (make-object wx-internal-radio-box% mred proxy (get-p) cb label x y w h choices 
                             major (filter-style style) font))
      (set-c c #t #t)

      (define enable-vector (make-vector (length choices) #t))

      (define/override enable
        (case-lambda
         [(on?) (super enable on?)]
         [(i on?) 
          (when (< -1 i (vector-length enable-vector))
            (vector-set! enable-vector i on?)
            (send c enable-button i on?))]))
      
      (define/override is-enabled?
        (case-lambda
         [() (super is-enabled?)]
         [(which) (and (< -1 which (vector-length enable-vector))
                       (vector-ref enable-vector which))]))

      (bounce 
       c
       (button-focus i)
       (set-selection i)
       (get-selection))
      (stretchable-in-y #f)
      (stretchable-in-x #f)))

  ;; ----------------------------------------

  (define wx-internal-gauge%
    (make-window-glue% 
     (class (make-control% wx:gauge% 0 0 #f #f)
       (init parent label range style font)
       (inherit get-client-size get-width get-height set-size 
		stretchable-in-x stretchable-in-y set-min-height set-min-width
		get-parent)
       (override* [gets-focus? (lambda () #f)])
       ;; # pixels per unit of value.
       (define pixels-per-value 1)
       (super-make-object style parent label range -1 -1 -1 -1 (cons 'deleted style) font)

       (let-values ([(client-width client-height) (get-two-int-values 
                                                   (lambda (a b) (get-client-size a b)))])
         (let ([delta-w (- (get-width) client-width)]
               [delta-h (- (get-height) client-height)]
               [vertical-labels? (eq? (send (send (get-parent) get-window) get-label-position) 'vertical)]
               [horizontal? (memq 'horizontal style)])
           (set-min-width (if horizontal?
                              (let ([cw (min const-max-gauge-length
                                             (* range pixels-per-value))])
                                (max (if vertical-labels?
                                         cw
                                         (+ cw delta-w))
                                     (get-width)))
                              ;; client-height is the default
                              ;; dimension in the minor direction.
                              (+ client-width delta-w)))
           (set-min-height (if horizontal?
                               (+ client-height delta-h)
                               (let ([ch (min const-max-gauge-length
                                              (* range pixels-per-value))])
                                 (max (if vertical-labels?
                                          (+ ch delta-h)
                                          ch)
                                      (get-height)))))))

       (if (memq 'horizontal style)
           (begin
             (stretchable-in-x #t)
             (stretchable-in-y #f))
           (begin
             (stretchable-in-x #f)
             (stretchable-in-y #t))))))

  (define wx-gauge%
    (class wx-label-panel% 
      (init mred proxy parent label range style font)
      (inherit stretchable-in-y stretchable-in-x get-p set-c)
      
      (super-make-object proxy parent label style font 'center 'center)
      
      (define c (make-object wx-internal-gauge% mred proxy (get-p) label range
                             (filter-style style) font))
      (set-c c 
             (memq 'horizontal style)
             (memq 'vertical style))

      (bounce 
       c
       (get-range)
       (set-range rng)
       (get-value)
       (set-value v))
      (let ([h? (and (memq 'horizontal style) #t)])
        (stretchable-in-x h?)
        (stretchable-in-y (not h?)))))

  ;; ----------------------------------------

  (define wx-internal-slider%
    (make-window-glue% 
     (class (make-control% wx:slider% 0 0 #f #f)
       (init parent func label value min-val max-val style font)
       (inherit set-min-width set-min-height stretchable-in-x stretchable-in-y
		get-client-size get-width get-height get-parent)
       ;; # pixels per possible setting.
       (define pixels-per-value 3)
       ;; 3 is good because with horizontal sliders under Xt, with 1 or 2
       ;; pixels per value, the thumb is too small to display the number,
       ;; which looks bad.
       
       (super-make-object style parent func label value min-val max-val -1 -1 -1 (cons 'deleted style) font)
	 
       (let-values ([(client-w client-h) (get-two-int-values (lambda (a b)
                                                               (get-client-size a b)))])
         (let* ([horizontal? (memq 'horizontal style)]
                [vertical-labels? (eq? (send (send (get-parent) get-window) get-label-position) 'vertical)]
                [range (+ (* pixels-per-value (add1 (- max-val min-val)))
                          (cond
                           [(and horizontal? (not vertical-labels?)) (- (get-width) client-w)]
                           [(and (not horizontal?) vertical-labels?) (- (get-height) client-h)]
                           [else 0]))])
           ((if horizontal? (lambda (v) (set-min-width v)) (lambda (v) (set-min-height v)))
            (max ((if horizontal? (lambda () (get-width)) (lambda () (get-height))))
                 (min const-max-gauge-length range)))
           (stretchable-in-x horizontal?)
           (stretchable-in-y (not horizontal?)))))))

  (define wx-slider%
    (class wx-label-panel% 
      (init mred proxy parent func label value min-val max-val style font)
      (inherit stretchable-in-y stretchable-in-x get-p set-c)

      (super-make-object proxy parent label style font 'center 'center)

      (define c (make-object wx-internal-slider% mred proxy (get-p) func label value min-val max-val
                             (filter-style style) font))
      
      (set-c c 
             (memq 'horizontal style)
             (memq 'vertical style))
      
      (bounce 
       c
       (get-value)
       (set-value v))
      (let ([h? (and (memq 'horizontal style) #t)])
        (stretchable-in-x h?)
        (stretchable-in-y (not h?)))))
  
)

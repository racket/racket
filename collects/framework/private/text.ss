#|

WARNING: printf is rebound in the body of the unit to always
         print to the original output port.

|#

#lang scheme/unit
(require string-constants
         mzlib/class
         mzlib/match
         scheme/path
         "sig.ss"
         "../gui-utils.ss"
         "../preferences.ss"
         mred/mred-sig
         mrlib/interactive-value-port
         mzlib/list
         mzlib/etc
         setup/dirs
         mzlib/string
         (prefix-in srfi1: srfi/1))
(require setup/xref
         scribble/xref
         scribble/struct
         scribble/manual-struct
         scribble/decode
         scribble/basic
         (prefix-in s/m: scribble/manual))

(import mred^
        [prefix icon: framework:icon^]
        [prefix editor: framework:editor^]
        [prefix keymap: framework:keymap^]
        [prefix color-model: framework:color-model^]
        [prefix frame: framework:frame^]
        [prefix scheme: framework:scheme^]
        [prefix number-snip: framework:number-snip^]
        [prefix finder: framework:finder^])
(export (rename framework:text^
                [-keymap% keymap%]))
(init-depend framework:editor^)

(define original-output-port (current-output-port))
(define (printf . args) 
  (apply fprintf original-output-port args)
  (void))

(define-struct range (start end b/w-bitmap color caret-space?) #:inspector #f)
(define-struct rectangle (left top right bottom b/w-bitmap color) #:inspector #f)

(define-values (register-port-name! lookup-port-name)
  ;; port-name->editor-ht: (hashof symbol (weakboxof editor:basic<%>))
  ;; Maintains a mapping from port names back to their respective editors.
  (let ([port-name->editor-ht (make-weak-hasheq)])
    
    ;; register-port-name-to-editor!: symbol editor<%> -> void
    ;; Registers the editor's port name.
    (define (register-port-name! a-port-name an-editor)
      (hash-set! port-name->editor-ht a-port-name (make-weak-box an-editor)))
    
    ;; lookup-port-name: symbol -> (or/c editor:basic<%> #f)
    ;; Given a port name, tries to get the editor with that name.
    (define (lookup-port-name a-port-name)
      (let ([a-weak-box (hash-ref port-name->editor-ht a-port-name #f)])
        (cond
          [(not a-weak-box)
           #f]
          [else
           (weak-box-value a-weak-box)])))
    
    (values register-port-name! lookup-port-name)))


;; wx: `default-wrapping?', add as the initial value for auto-wrap bitmap,
;; unless matthew makes it primitive

(define basic<%>
  (interface (editor:basic<%> (class->interface text%))
    highlight-range
    unhighlight-range
    get-highlighted-ranges
    get-styles-fixed
    get-fixed-style
    set-styles-fixed
    move/copy-to-edit
    initial-autowrap-bitmap
    get-port-name
    port-name-matches?))

(define basic-mixin
  (mixin (editor:basic<%> (class->interface text%)) (basic<%>)
    (inherit get-canvas get-canvases get-admin split-snip get-snip-position
             begin-edit-sequence end-edit-sequence
             set-autowrap-bitmap
             delete find-snip invalidate-bitmap-cache
             set-file-format get-file-format
             get-style-list is-modified? change-style set-modified
             position-location position-locations
             get-extent get-filename)
    
    (define port-name-identifier #f)
    (define/public (get-port-name)
      (let* ([b (box #f)]
             [n (get-filename b)])
        (cond
          [(or (unbox b) (not n))
           (unless port-name-identifier
             (set! port-name-identifier (gensym 'unsaved-editor))
             (register-port-name! port-name-identifier this))
           port-name-identifier]
          [else n])))
    (define/public (port-name-matches? id)
      (let ([filename (get-filename)])
        (or (and (path? id)
                 (path? filename)
                 (equal? (normal-case-path (normalize-path (get-filename)))
                         (normal-case-path (normalize-path id))))
            (and (symbol? port-name-identifier)
                 (symbol? id)
                 (equal? port-name-identifier id)))))
    
    (define highlight-pen #f)
    (define highlight-brush #f)
    (define highlight-tmp-color #f)

    (define range-rectangles null)
    (define ranges (make-hash))
    (define ranges-low 0) 
    (define ranges-high 0)
    (define ranges-list #f)
    
    (define/public-final (get-highlighted-ranges)
      (unless ranges-list
        (set! ranges-list (hash-map ranges (λ (x y) x))))
      ranges-list)
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
             [b3 (box 0)]
             [new-rectangles
              (λ (range rst)
                (let* ([start (range-start range)]
                       [end (range-end range)]
                       [b/w-bitmap (range-b/w-bitmap range)]
                       [color (range-color range)]
                       [caret-space? (range-caret-space? range)]
                       [start-eol? #f]
                       [end-eol? (if (= start end)
                                     start-eol?
                                     #t)])
                  (let-values ([(start-x top-start-y bottom-start-y)
                                (begin 
                                  (send this position-locations start b1 b2 #f b3 start-eol? #t)
                                  (values (if caret-space?
                                              (+ 1 (unbox b1))
                                              (unbox b1))
                                          (unbox b2)
                                          (unbox b3)))]
                               
                               
                               [(end-x top-end-y bottom-end-y)
                                (begin (send this position-locations end b1 b2 #f b3 end-eol? #t)
                                       (values (unbox b1) 
                                               (unbox b2)
                                               (unbox b3)))])
                    
                    (cond
                      [(= top-start-y top-end-y)
                       (cons 
                        (make-rectangle start-x
                                        top-start-y
                                        (if (= end-x start-x)
                                            (+ end-x 1)
                                            end-x)
                                        bottom-start-y
                                        b/w-bitmap 
                                        color)
                        rst)]
                      [else
                       (list*
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
                                        color)
                        rst)]))))]
             [old-rectangles range-rectangles])
        
        (set! range-rectangles 
              (foldl new-rectangles
                     null
                     (get-highlighted-ranges)))))
    
    (define delayed-highlights? #f)
    (define todo void)
    
    (define/augment (on-edit-sequence)
      (set! delayed-highlights? #t)
      (inner (void) on-edit-sequence))
    
    (define/augment (after-edit-sequence)
      (set! delayed-highlights? #f)
      (unless (eq? todo void)
        ;; don't redraw unless something changed
        (redraw-highlights todo)
        (set! todo void))
      (inner (void) after-edit-sequence))
    
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
        (unless (is-a? color color%)
          (error 'highlight-range "expected a color for the third argument, got ~s" color))
        
        (let* ([l (make-range start end bitmap color caret-space?)]
               [update-one
                (λ ()
                  (set! ranges-list #f)
                  (hash-set! ranges l (if (eq? priority 'high) (+ ranges-high 1) (- ranges-low 1)))
                  (if (eq? priority 'high) 
                      (set! ranges-high (+ ranges-high 1))
                      (set! ranges-low (- ranges-low 1))))])
          (cond
            [delayed-highlights?
             (set! todo
                   (let ([old-todo todo])
                     (λ ()
                       (old-todo)
                       (update-one))))]
            [else
             (redraw-highlights update-one)])
          (λ () (unhighlight-range start end color bitmap caret-space?)))))
    
    (define/private (redraw-highlights todo)
      (let ([old-rectangles range-rectangles])
        (todo)
        (cond
          [(> (hash-count ranges) 20)
           (invalidate-bitmap-cache)]
          [else
           (recompute-range-rectangles)
           (invalidate-rectangles (append old-rectangles range-rectangles))])))
    
    (define/public unhighlight-range
      (opt-lambda (start end color [bitmap #f] [caret-space? #f])
        (let ([new-todo
               (λ ()
                 (hash-remove! ranges (make-range start end bitmap color caret-space?))
                 (set! ranges-list #f))])
          (cond
            [delayed-highlights?
             (set! todo
                   (let ([old-todo todo])
                     (λ ()
                       (old-todo)
                       (new-todo))))]
            [else
             (redraw-highlights new-todo)]))))
    
    (define/private (matching-rectangle? r start end color bitmap caret-space?)
      (and (equal? start (range-start r))
           (equal? end (range-end r))
           (eq? bitmap (range-b/w-bitmap r))
           (equal? color (range-color r))
           (equal? caret-space? (range-caret-space? r))))
    
    (define/override (on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
      (super on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
      (when before (recompute-range-rectangles)) ;; assume this result cannot change between before & after
      (let-values ([(view-x view-y view-width view-height)
                    (let ([b1 (box 0)]
                          [b2 (box 0)]
                          [b3 (box 0)]
                          [b4 (box 0)])
                      (send (get-admin) get-view b1 b2 b3 b4)
                      (values (unbox b1)
                              (unbox b2)
                              (unbox b3)
                              (unbox b4)))])
        
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [last-color #f])
          (for-each
           (λ (rectangle)
             (let* ([b/w-bitmap (rectangle-b/w-bitmap rectangle)]
                    [color (let ([rc (rectangle-color rectangle)])
                             (cond
                               [(and last-color (eq? last-color rc))
                                rc]
                               [rc
                                (set! last-color #f)
                                (unless highlight-tmp-color
                                  (set! highlight-tmp-color (make-object color% 0 0 0)))
                                (send dc try-color rc highlight-tmp-color)
                                (if (<= (color-model:rgb-color-distance
                                         (send rc red)
                                         (send rc green)
                                         (send rc blue)
                                         (send highlight-tmp-color red)
                                         (send highlight-tmp-color green)
                                         (send highlight-tmp-color blue))
                                        18)
                                    (begin (set! last-color rc)
                                           rc)
                                    #f)]
                               [else 
                                (set! last-color #f)
                                rc]))]
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
               (let ([skip-it? #f])
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
                   [else (set! skip-it? #t)])
                 (unless skip-it? 
                   (send dc draw-rectangle (+ left dx) (+ top dy) width height)))))
           range-rectangles)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush))))
    
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
    
    (define/override (put-file directory default-name)
      (let* ([canvas (get-canvas)]
             [parent (and canvas (send canvas get-top-level-window))])
        (finder:put-file default-name
                         directory
                         #f
                         (string-constant select-file)
                         #f
                         ""
                         parent)))
    
    (super-new)
    (set-autowrap-bitmap (initial-autowrap-bitmap))))

(define foreground-color<%>
  (interface (basic<%> editor:standard-style-list<%>)
    ))

(define foreground-color-mixin
  (mixin (basic<%> editor:standard-style-list<%>) (foreground-color<%>)
    (inherit begin-edit-sequence end-edit-sequence change-style get-style-list)
    
    (define/override (default-style-name)
      (editor:get-default-color-style-name))
    
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
    (super-new)))

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

(define searching<%> 
  (interface (editor:keymap<%> basic<%>)
    set-searching-str))
(define searching-mixin
  (mixin (editor:keymap<%> basic<%>) (searching<%>)
    (define/override (get-keymaps)
      (cons (keymap:get-search) (super get-keymaps)))
    
    (define searching-str #f)
    (define case-sensitive? #f)
    (define search-hits 0)
    (define/public (get-search-hits) search-hits)
    
    (inherit invalidate-bitmap-cache)
    (define/public (set-searching-str s [cs? #t])
      (unless (and (equal? searching-str s)
                   (equal? case-sensitive? cs?))
        (set! searching-str s)
        (set! case-sensitive? cs?)
        (redo-search)))
    (define/augment (after-insert start len)
      (redo-search)
      (inner (void) after-insert start len))
    (define/augment (after-delete start len)
      (redo-search)
      (inner (void) after-delete start len))
    
    (inherit get-top-level-window)
    (define/override (on-focus on?)
      (let ([f (get-top-level-window)])
        (when (is-a? f frame:searchable<%>)
          (when on?
            (send f set-text-to-search this))))
      (super on-focus on?))
    
    (inherit highlight-range begin-edit-sequence end-edit-sequence find-string)
    
    (define clear-regions void)
    (define/private (redo-search)
      (begin-edit-sequence)
      (set! search-hits 0)
      (clear-regions)
      (cond
        [searching-str
         (let loop ([pos 0]
                    [n 0])
           (let ([next (do-search searching-str pos 'eof)])
             (when next
               (let-values ([(end counts) (find-end (+ next (string-length searching-str))
                                                    searching-str)])
                 (set! search-hits (+ search-hits counts))
                 (let ([old clear-regions]
                       [new (highlight-range next end (send the-color-database find-color "yellow"))])
                   (set! clear-regions (λ () (old) (new))))
                 (loop end (+ n 1))))))]
        [else
         (invalidate-bitmap-cache)])
      (end-edit-sequence))
    
    (define/private (find-end pos searching-str)
      (let loop ([pos pos]
                 [count 1])
        (cond
          [(do-search searching-str pos (+ pos (string-length searching-str)))
           =>
           (λ (pos)
             ;; if find-string returns #t here, then we know that we've found two of the search strings in a row, so just coalesce them
             (loop (+ pos (string-length searching-str))
                   (+ count 1)))]
          [else
           (values pos count)])))
    
    (define/private (do-search str start end) (find-string str 'forward start end #t case-sensitive?))
    
    (super-new)))

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
                  (hash-set! linked-snips snip new-snip)
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
                             (make-hasheq)
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
           (send delegate unhighlight-range 
                 (range-start range)
                 (range-end range)
                 (range-color range)
                 (range-b/w-bitmap range)
                 (range-caret-space? range)))
         (send delegate get-highlighted-ranges))
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
        (when delegate
          (send delegate highlight-range 
                start end color bitmap caret-space? priority))
        (super highlight-range start end color bitmap caret-space? priority)))
    
    (define/override unhighlight-range
      (opt-lambda (start end color [bitmap #f] [caret-space? #f])
        (when delegate
          (send delegate unhighlight-range start end color bitmap caret-space?))
        (super unhighlight-range start end color bitmap caret-space?)))
    
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
        (let ([delegate-copy (hash-ref linked-snips snip (λ () #f))])
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
    get-read-write?
    while-unlocked))

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
    
    (define/public (while-unlocked t)
      (let ([unlocked? 'unint])
        (dynamic-wind
         (λ () 
           (set! unlocked? read-write?)
           (set! read-write? #t))
         (λ () (t))
         (λ () (set! read-write? unlocked?)))))
    
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

(define-struct peeker (bytes skip-count pe resp-chan nack polling?) #:inspector (make-inspector))
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

(define out-style-name "text:ports out")
(define error-style-name "text:ports err")
(define value-style-name "text:ports value")
(let ([create-style-name
       (λ (name sd)
         (let* ([sl (editor:get-standard-style-list)])
           (send sl new-named-style 
                 name
                 (send sl find-or-create-style
                       (send sl find-named-style "Standard")
                       sd))))])
  (let ([out-sd (make-object style-delta% 'change-nothing)])
    (send out-sd set-delta-foreground (make-object color% 150 0 150))
    (create-style-name out-style-name out-sd))
  (let ([err-sd (make-object style-delta% 'change-italic)])
    (send err-sd set-delta-foreground (make-object color% 255 0 0))
    (create-style-name error-style-name err-sd))
  (let ([value-sd (make-object style-delta% 'change-nothing)])
    (send value-sd set-delta-foreground (make-object color% 0 0 175))
    (create-style-name value-style-name value-sd)))

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
             get-port-name
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
    (define/public (get-out-style-delta) out-style-name)
    (define/public (get-err-style-delta) error-style-name)
    (define/public (get-value-style-delta) value-style-name)
    
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
                (cond
                  [(string? sd)
                   (let ([style-list (get-style-list)])
                     (or (send style-list find-named-style sd)
                         (send style-list find-named-style "Standard")
                         (send style-list find-named-style "Basic")))]
                  [sd
                   (let* ([style-list (get-style-list)] 
                          [std (send style-list find-named-style "Standard")])
                     (if std
                         (send style-list find-or-create-style std sd)
                         (let ([basic (send style-list find-named-style "Basic")])
                           (send style-list find-or-create-style basic sd))))]))]
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
                 ;; don't want to set the port-print-handler here; 
                 ;; instead drscheme sets the global-port-print-handler
                 ;; to catch fractions and the like
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
                                                        src-read-k
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
      (start-text-input-port (get-port-name) #f))
    (define-values (in-box-port box-read-chan box-clear-input-chan) 
      (start-text-input-port (get-port-name) (lambda () (on-box-peek))))))

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
            (apply choice-evt (map service-positioner positioners))
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
                        (channel-put-evt resp-evt (or position 
                                                      
                                                      ;; a bogus position for when 
                                                      ;; nothing has happened yet.
                                                      (list 1 0 1))))
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
       (let ([evt 
              (nack-guard-evt
               (λ (nack)
                 (let ([chan (make-channel)])
                   (channel-put peek-chan (make-peeker bstr skip-count progress-evt chan nack polling?))
                   chan)))])
         (if polling? 
             (let ([v (sync evt)])
               (if (eq? v 0)
                   ;; Don't return 0, because that means something is
                   ;; probably ready. We want to indicate that nothing is
                   ;; ready.
                   never-evt
                   ;; Even on success, package it as an event, because
                   ;; `read-bytes-proc' expects an event
                   (wrap-evt always-evt (lambda (_) v))))
             evt)))))
  
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
(define-struct queue (front back count) #:mutable)
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

#| 
=== AUTOCOMPLETE ===

This module defines autocomplete-mixin, a mixin for editors that adds an
unintrusive autocompletion menu when a keystroke is pressed.

By default, the system works by reading the prefix whenever the autcomplete
keystroke is pressed, and then constructing a list of possible completions
by searching through the contents of the autocomplete-word-list parameter for all words
that share that prefix; when the user types another character or deletes a
character autocomplete-word-list is consulted again. This seems to be fast enough for
all but very large completion lists. However, the code has been designed
to allow more efficient implementations if that becomes necessary --- 
all autocomplete-word-list manipulation functions are isolated to the autocompletion-cursor<%>
interface, which implements two main methods, narrow and widen, to add or subtract
a character from the current prefix, respectively. A trie-based implementation,
for instance, could implement narrow and widen in constant-time at the cost of more
memory and more time to build the initial data structure.

===

autocomplete<%>

=new methods=

get-all-words : -> (listof string)
returns a list of all of the possible words that the completion should choose from

get-autocomplete-border-color : -> color string
returns the color for the border of the autocompletion menu

get-autocomplete-background-color : -> color string
returns the background color for the autocompletion menu

get-autocomplete-selected-color : -> color string
returns the selected color for the autocompletion menu

===

autocomplete-mixin: mixin (editor<%> -> editor<%>)

The autocomplete-text mixin produces a class that implements
editor<%> and provides the following extra public methods:

=overridden methods=

on-paint
overridden to draw the autocompletion menu as necessary.

on-char
overridden to intercept keypress events to control the completions
menu.

====

autocompletion-cursor<%>

An autocompletion-cursor<%> abstracts over a set of completions 
for a particular prefix. Typically an autocompletion-cursor<%>
implementation will be created with a particular initial prefix; 
from then on the autocomplete-text system will manipulate it
using the narrow and widen methods in response to user input.

The autocompletion-cursor<%> interface defines the following
methods:

get-completions : -> (listof string) 
Produces a list of all possible completions.

get-length      : -> int
Produces the number of possible completions.

empty?          : -> boolean
Determines if there are any completions in the given cursor.

narrow          : char -> autocompletion-cursor<%>
Yields a new cursor that represents the subset of
the completions held by this cursor that are also
completions of this cursor's prefix followed by the
given character.

widen           : -> autocompletion-cursor<%> | #f
Yields a new cursor that represents the completions
of this cursor's prefix with the last character
removed.

===
autocompletion-cursor%

The implementation of autcompletion-cursor<%> used
by the default get-completions method.

===

scrolling-cursor : mixin (autocompletion-cursor<%> -> scrolling-cursor<%>)

scrolling-cursor is a mixin that takes classes that implement
autocompletion-cursor<%> to classes that implement scrolling-cursor<%>
(not provided).

===
configuration parameters

These configuration parameters customize autocompletion behavior.

autocomplete-append-after : string parameter
designates text to insert after a completion. Default: ""

autocomplete-limit : positive int parameter
designates the maximum number of completions to show at a time. Default: 15

completion-mode-key : character parameter
designates the character that triggers autocompletion

|#

(define autocomplete<%>
  (interface ((class->interface text%))
    auto-complete
    get-autocomplete-border-color
    get-autocomplete-background-color
    get-autocomplete-selected-color
    completion-mode-key-event?
    get-all-words
    get-word-at))

;; ============================================================
;; auto-complete-text (mixin) implementation

(define selected-color (make-object color% 204 153 255))

(define autocomplete-mixin
  (mixin ((class->interface text%)) (autocomplete<%>)
    
    (inherit invalidate-bitmap-cache get-dc get-start-position get-end-position
             find-wordbreak get-text position-location insert dc-location-to-editor-location)
    
    ; get-autocomplete-border-color : -> string
    ; the color of text in the autocomplete menu
    (define/public (get-autocomplete-border-color) "black")
    
    ; get-background-color : -> string
    ; background color in the autocomplete menu
    (define/public (get-autocomplete-background-color) "lavender")
    
    ; get-autocomplete-selected-color : -> string
    ; selected option background color in the autocomplete menu
    (define/public (get-autocomplete-selected-color) selected-color)
    
    (define/public (completion-mode-key-event? key-event)
      (cond
        [(and (eq? (send key-event get-key-code) #\.)
              (send key-event get-control-down))
         (or (eq? (system-type) 'macosx)
             (not (preferences:get 'framework:menu-bindings)))]
        [else
         #f]))
    
    (define/public (get-all-words) (get-completions/manuals #f))
    
    (define completions-box #f) ; completions-box% or #f if no completions box is active right now
    (define word-start-pos #f)  ; start pos of that word, or #f if no autocompletion
    (define word-end-pos #f)    ; end pos of that word, or #f if none
    
    ; string -> scrolling-cursor<%>       given a prefix, returns the possible completions
    ; given a word, produces a cursor that describes
    ; all possible completions. The default implementation of autocompletion-cursor%
    ; returns all strings from the get-all-words method (below)
    ; that have the given string as a prefix; it performs a 
    ; linear-search at every narrow/widen.
    (define/private (get-completions word) 
      (new autocompletion-cursor% 
           [word word] 
           [all-words (get-all-words)]))
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (when (and completions-box (not before?))
        (send completions-box draw dc dx dy)))
    
    ;; (-> void)
    ;; Check for possible completions of the current word and give the user a menu for them.
    (define/public-final (auto-complete)
      (when (equal? (get-start-position) (get-end-position))
        (let* ([end-pos (get-end-position)]
               [word (get-word-at end-pos)]
               [completion-cursor (get-completions word)])
          (let ([start-pos (- end-pos (string-length word))])
            (set! word-start-pos start-pos)
            (set! word-end-pos end-pos)
            (show-options word start-pos end-pos completion-cursor)))))
    
    ;; Number -> String
    ;; The word that ends at the current positon of the editor
    (define/public (get-word-at current-pos)
      (let ([start-pos (box current-pos)]) 
        (find-wordbreak start-pos #f 'caret)
        (get-text (unbox start-pos) current-pos)))
    
    ;; String Number Number scrolling-cursor<%> -> void
    ;; Popup a menu of the given words at the location of the end-pos. Each menu item
    ;; should change the current word to the word in the list.
    (define/private (show-options word start-pos end-pos cursor)
      (let ([x (box 0)]
            [y (box 0)])
        (position-location start-pos x y #f)
        (set! completions-box (new completion-box%
                                   [completions (new scroll-manager% [cursor cursor])]
                                   [menu-x (unbox x)]
                                   [menu-y (+ (unbox y) 2)]
                                   [editor this]))
        (send completions-box redraw)))
    
    ;; on-char must handle inputs for two modes: normal text mode and in-the-middle-of-autocompleting mode
    ;; perhaps it would be better to handle this using the state machine pattern
    (define/override (on-char key-event)
      (cond
        [completions-box
         (let ([code (send key-event get-key-code)]
               [full? (not (send completions-box empty?))])
           (cond
             [(and full? (memq code '(up wheel-up)))
              (send completions-box prev-item)]
             [(and full? 
                   (or (memq code '(down wheel-down))
                       (completion-mode-key-event? key-event)))
              (send completions-box next-item)]
             [(and full? (eq? code 'prior)) (send completions-box scroll-display-up)]
             [(and full? (eq? code 'next))  (send completions-box scroll-display-down)]
             [(eq? code 'release)
              (void)]
             [(eq? code #\backspace)
              (widen-possible-completions)
              (super on-char key-event)]
             [(eq? code #\return)
              (when full?
                (insert-currently-selected-string))
              (destroy-completions-box)]
             [(and (char? code) (char-graphic? code))
              (super on-char key-event)
              (constrict-possible-completions code)]
             [else
              (destroy-completions-box)
              (super on-char key-event)]))]
        [(completion-mode-key-event? key-event)
         (auto-complete)]
        [else 
         (super on-char key-event)]))
    
    ;; on-event controls what happens with the mouse
    (define/override (on-event mouse-event)
      (cond
        [completions-box
         (let*-values ([(x) (send mouse-event get-x)]
                       [(y) (send mouse-event get-y)]
                       [(mouse-x mouse-y) (dc-location-to-editor-location x y)])
           (if (and (send completions-box point-inside-menu? mouse-x mouse-y)
                    (not (send completions-box empty?)))
               (cond
                 [(send mouse-event moving?)
                  (send completions-box handle-mouse-movement mouse-x mouse-y)
                  (super on-event mouse-event)]
                 [(send mouse-event button-down?)
                  (insert-currently-selected-string)
                  (destroy-completions-box)]
                 [else
                  (super on-event mouse-event)])
               (super on-event mouse-event)))]
        [else (super on-event mouse-event)]))
    
    (define/private (constrict-possible-completions char)
      (set! word-end-pos (add1 word-end-pos))
      (let-values ([(x0 y0 x1 y1) (send completions-box get-menu-coordinates)])
        (send completions-box narrow char)
        (let-values ([(_ __ x1p y1p) (send completions-box get-menu-coordinates)])
          (invalidate-bitmap-cache x0 y0 (max x1 x1p) (max y1 y1p)))))
    
    (define/private (widen-possible-completions)
      (let-values ([(x0 y0 x1 y1) (send completions-box get-menu-coordinates)])
        (let ([reasonable? (send completions-box widen)])
          (cond
            [reasonable?
             (let-values ([(_ __ x1p y1p) (send completions-box get-menu-coordinates)])
               (invalidate-bitmap-cache x0 y0 (max x1 x1p) (max y1 y1p)))]
            [else
             (set! completions-box #f)
             (invalidate-bitmap-cache x0 y0 x1 y1)]))))
    
    ;; destroy-completions-box : -> void
    ;; eliminates the active completions box
    (define/private (destroy-completions-box)
      (let-values ([(x0 y0 x1 y1) (send completions-box get-menu-coordinates)])
        (set! completions-box #f)
        (invalidate-bitmap-cache x0 y0 x1 y1)))
    
    ;; insert-currently-selected-string : -> void
    ;; inserts the string that is currently being autoselected
    (define/private (insert-currently-selected-string)
      (let ([css (send completions-box get-current-selection)])
        (insert (string-append css (autocomplete-append-after)) word-start-pos word-end-pos)))
    
    (super-new)))

;; ============================================================
;; autocompletion-cursor<%> implementations

(define autocompletion-cursor<%>
  (interface ()
    get-completions  ;      -> (listof string) 
    get-length       ;      -> int
    empty?           ;      -> boolean
    narrow           ; char -> autocompletion-cursor<%>
    widen))          ;      -> autocompletion-cursor<%> | #f

(define scrolling-cursor<%>
  (interface (autocompletion-cursor<%>)
    items-are-hidden?
    get-visible-completions
    get-visible-length
    scroll-down
    scroll-up))

(define autocompletion-cursor%
  (class* object% (autocompletion-cursor<%>)
    
    (init-field word all-words)
    
    (define/private (starts-with prefix)
      (let ([re (regexp (string-append "^" (regexp-quote prefix)))])
        (λ (w) (regexp-match re w))))
    
    (define all-completions (filter (starts-with word) all-words))
    (define all-completions-length (length all-completions))
    
    (define/public (narrow c)
      (new autocompletion-cursor%
           [word (string-append word (list->string (list c)))]
           [all-words all-words]))
    
    (define/public (widen)
      (let ([strlen (string-length word)])
        (cond
          [(< strlen 2) #f]
          [else 
           (new autocompletion-cursor%
                [word (substring word 0 (- (string-length word) 1))]
                [all-words all-words])])))
    
    (define/public (get-completions) all-completions)
    (define/public (get-length) all-completions-length)
    (define/public (empty?) (eq? (get-length) 0))
    
    (super-new)))

(define scroll-manager% 
  (class* object% ()
    (init-field cursor)
    
    (define all-completions #f)
    (define all-completions-length #f)
    (define visible-completions #f)
    (define visible-completions-length #f)
    (define hidden? #f)
    
    (define/private (initialize-state!)
      (cond
        [(<= (send cursor get-length) (autocomplete-limit))
         (set! hidden? #f)
         (set! all-completions (send cursor get-completions))
         (set! all-completions-length (send cursor get-length))
         (set! visible-completions all-completions)
         (set! visible-completions-length all-completions-length)]
        [else
         (set! hidden? #t)
         (set! all-completions (send cursor get-completions))
         (set! all-completions-length (send cursor get-length))
         (set! visible-completions (srfi1:take (send cursor get-completions) (autocomplete-limit)))
         (set! visible-completions-length (autocomplete-limit))]))
    
    (define/public (get-completions) all-completions)
    (define/public (get-length) all-completions-length)
    (define/public (empty?) (send cursor empty?))
    
    (define/public (get-visible-length) visible-completions-length)
    (define/public (get-visible-completions) visible-completions)
    
    (define/public (items-are-hidden?) hidden?)
    
    (define/public (scroll-down)
      (when hidden?
        (set! all-completions (append (srfi1:drop all-completions (autocomplete-limit)) visible-completions))
        (set! visible-completions (srfi1:take all-completions (autocomplete-limit)))))
    
    (define/public (scroll-up)
      (when hidden?
        (let ([n (- all-completions-length (autocomplete-limit))])
          (set! all-completions (append (srfi1:drop all-completions n) (srfi1:take all-completions n)))
          (set! visible-completions (srfi1:take all-completions (autocomplete-limit))))))
    
    (define/public (narrow char)
      (let ([new-cursor (send cursor narrow char)])
        (set! cursor new-cursor)
        (initialize-state!)))
    
    (define/public (widen)
      (let ([new-cursor (send cursor widen)])
        (cond
          [new-cursor
           (set! cursor new-cursor)
           (initialize-state!)
           #t]
          [else #f])))
    
    (initialize-state!)
    (super-new)))

;; ============================================================
;; completion-box<%> implementation

(define menu-padding-x 4)
(define menu-padding-y 0)

(define completion-box<%>
  (interface ()
    draw                   ; dc<%> int int -> void
    redraw                 ; -> void
    get-menu-coordinates   ; -> (values int int int int)
    next-item              ; -> void
    prev-item              ; -> void
    scroll-display-up      ; -> void
    scroll-display-down    ; -> void
    get-current-selection  ; -> string
    narrow                 ; char -> boolean
    widen                  ;      -> boolean
    empty?))               ; -> boolean


(define hidden-completions-text "⋮")
(define-struct geometry (menu-x
                         menu-y
                         menu-width
                         menu-height
                         mouse->menu-item-vector))

(define completion-box% 
  (class* object% (completion-box<%>)
    
    (init-field completions       ; scroll-manager%       the possible completions (all of which have base-word as a prefix)
                menu-x            ; int                   the menu's top-left x coordinate
                menu-y            ; int                   the menu's top-left y coordinate
                editor            ; editor<%>             the owner of this completion box
                )
    
    (define/public (empty?) (send completions empty?))
    
    (define/private (compute-geometry)
      
      (define vec #f)
      (define (initialize-mouse-offset-map! coord-map)
        (cond
          [(null? coord-map) (void)] ; is this possible?
          [else
           (let* ([last-index (cadr (car coord-map))]
                  [v (make-vector (add1 last-index))])
             (for-each
              (λ (elt)
                (let ([first (car elt)]
                      [last  (cadr elt)]
                      [val   (caddr elt)])
                  (let loop ([n first])
                    (when (<= n last)
                      (vector-set! v n val)
                      (loop (add1 n))))))
              coord-map)
             (set! vec v))]))
      
      (define-values (editor-width editor-height)
        (let* ([wb (box 0)]
               [hb (box 0)]
               [admin (send editor get-admin)])
          (if admin
              (begin
                (send admin get-view #f #f wb hb)
                (values (unbox wb)
                        (unbox hb)))
              (values 10 10))))
      
      (let* ([num-completions (send completions get-length)]
             [shown-completions (send completions get-visible-completions)])
        (define-values (w h)
          (let ([dc (send editor get-dc)])
            (cond
              [(zero? num-completions)
               (let-values ([(tw th _1 _2) (send dc get-text-extent (string-constant no-completions) 
                                                 (get-mt-font dc))])
                 (values (+ menu-padding-x tw menu-padding-x)
                         (+ menu-padding-y th menu-padding-y)))]
              [else
               (let loop ([pc shown-completions]
                          [w 0]
                          [h 0]
                          [coord-map '()]
                          [n 0])
                 (cond
                   [(null? pc)
                    (let-values ([(hidden?) (send completions items-are-hidden?)] 
                                 [(tw th _1 _2) (send dc get-text-extent hidden-completions-text)])
                      (let ([w (if hidden? (max tw w) w)]
                            [h (if hidden? (+ th h) h)])
                        (initialize-mouse-offset-map! coord-map)
                        (let ([offset-h menu-padding-y]
                              [offset-w (* menu-padding-x 2)])
                          (values (+ offset-w w)
                                  (+ offset-h h)))))]
                   [else 
                    (let ([c (car pc)])
                      (let-values ([(tw th _1 _2) (send dc get-text-extent c)])
                        (loop (cdr pc)
                              (max tw w)
                              (+ th h)
                              (cons (list (inexact->exact h) (inexact->exact (+ h th)) n) coord-map)
                              (add1 n))))]))])))
        
        (let ([final-x (cond
                         [(< (+ menu-x w) editor-width)
                          menu-x]
                         [(> editor-width w)
                          (- editor-width w)]
                         [else menu-x])]
              [final-y menu-y])
          
          (make-geometry final-x final-y w h vec))))
    
    ;; geometry records the menu's current width and height and a vector associating mouse location with
    ;; selected item
    (define geometry (compute-geometry))
    
    (define highlighted-menu-item 0) ; the currently-highlighted menu item
    
    ;; draw : dc<%> int int -> void
    ;; draws the menu to the given drawing context at offset dx, dy
    (define/public (draw dc dx dy)
      (let ([old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)])
        (send dc set-pen (send editor get-autocomplete-border-color) 1 'solid)
        (send dc set-brush (send editor get-autocomplete-background-color) 'solid)
        (let-values ([(mx my tw th) (get-menu-coordinates)])
          (send dc draw-rectangle (+ mx dx) (+ my dy) tw th)
          (cond
            [(send completions empty?)
             (let ([font (send dc get-font)])
               (send dc set-font (get-mt-font dc))
               (send dc draw-text (string-constant no-completions) (+ mx dx menu-padding-x) (+ menu-padding-y my dy))
               (send dc set-font font))]
            [else
             (let loop ([item-number 0] [y my] [pc (send completions get-visible-completions)])
               (cond
                 [(null? pc) 
                  (when (send completions items-are-hidden?)
                    (let-values ([(hw _1 _2 _3) (send dc get-text-extent hidden-completions-text)])
                      (send dc draw-text 
                            hidden-completions-text 
                            (+ mx dx (- (/ tw 2) (/ hw 2)))
                            (+ menu-padding-y y dy))))]
                 [else
                  (let ([c (car pc)])
                    (let-values ([(w h d a) (send dc get-text-extent c)])
                      (when (= item-number highlighted-menu-item)
                        (send dc set-pen "black" 1 'transparent)
                        (send dc set-brush (send editor get-autocomplete-selected-color) 'solid)
                        (send dc draw-rectangle (+ mx dx 1) (+ dy y menu-padding-y 1) (- tw 2) (- h 1)))
                      (send dc draw-text c (+ mx dx menu-padding-x) (+ menu-padding-y y dy))
                      (loop (add1 item-number) (+ y h) (cdr pc))))]))]))
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)))
    
    (define/private (get-mt-font dc)
      (let ([font (send dc get-font)])
        (send the-font-list find-or-create-font
              (send font get-point-size)
              (send font get-family)
              'italic
              (send font get-weight)
              (send font get-underlined)
              (send font get-smoothing))))
    
    ;; redraw : -> void
    ;; tells the parent to refresh enough of itself to redraw this menu
    (define/public (redraw)
      (let-values ([(x y w h) (get-menu-coordinates)])
        (send editor invalidate-bitmap-cache x y w h)))
    
    ;; get-menu-coordinates : -> (values int int int int)
    ;; get the menu's x, y, w, h coordinates with respect to its parent
    (define/public (get-menu-coordinates)
      (values (geometry-menu-x geometry)
              (geometry-menu-y geometry)
              (geometry-menu-width geometry)
              (geometry-menu-height geometry)))
    
    ;; next-item : -> void
    ;; tells the menu that the next item is selected
    (define/public (next-item)
      (cond
        [(and (= highlighted-menu-item (sub1 (autocomplete-limit)))
              (send completions items-are-hidden?))
         (set! highlighted-menu-item 0)
         (scroll-display-down)]
        [else
         (set! highlighted-menu-item (modulo (add1 highlighted-menu-item) (send completions get-visible-length)))
         (redraw)]))
    
    ;; prev-item : -> void
    ;; tells the menu that the previous item is selected
    (define/public (prev-item)
      (cond
        [(and (= highlighted-menu-item 0)
              (send completions items-are-hidden?))
         (set! highlighted-menu-item
               (sub1 (send completions get-visible-length)))
         (scroll-display-up)]
        [else
         (set! highlighted-menu-item (modulo (sub1 highlighted-menu-item) (send completions get-visible-length)))
         (redraw)]))
    
    ;; scroll-display-down : -> void
    ;; shows the next page possible completions
    (define/private (scroll-display do-it!)
      (let*-values ([(old-x1 old-y1 old-w old-h) (get-menu-coordinates)]
                    [(_) (do-it!)]
                    [(_) (set! geometry (compute-geometry))]
                    [(new-x1 new-y1 new-w new-h) (get-menu-coordinates)])
        (let ([old-x2 (+ old-x1 old-w)]
              [old-y2 (+ old-y1 old-h)]
              [new-x2 (+ new-x1 new-w)]
              [new-y2 (+ new-y1 new-h)])
          (let ([composite-x1 (min old-x1 new-x1)]
                [composite-y1 (min old-y1 new-y1)]
                [composite-x2 (max old-x2 new-x2)]
                [composite-y2 (max old-y2 new-y2)])
            (send editor invalidate-bitmap-cache 
                  composite-x1
                  composite-y1
                  (- composite-x2 composite-x1)
                  (- composite-y2 composite-y1))))))
    
    (define/public (scroll-display-down)
      (scroll-display (λ () (send completions scroll-down))))
    
    (define/public (scroll-display-up)
      (scroll-display (λ () (send completions scroll-up))))
    
    ;; point-inside-menu? : nat nat -> boolean
    ;; determines if the given x,y editor coordinate is inside
    ;; the drawn window or not
    (define/public (point-inside-menu? x y)
      (let*-values ([(mx my w h) (get-menu-coordinates)])
        (and (<= mx x (+ mx w))
             (<= my y (+ my h)))))
    
    ;; handle-mouse-movement : int int -> bool
    ;; takes an editor coordinate, returns whether it has intercept
    (define/public (handle-mouse-movement x y)
      (let*-values ([(mx my w h) (get-menu-coordinates)])
        (when (and (<= mx x (+ mx w))
                   (< (+ my menu-padding-y) y (+ my (vector-length (geometry-mouse->menu-item-vector geometry)))))
          (set! highlighted-menu-item (vector-ref (geometry-mouse->menu-item-vector geometry) (inexact->exact (- y my))))
          (redraw))))
    
    ;; get-current-selection : -> string
    ;; returns the selected string
    (define/public (get-current-selection)
      (list-ref (send completions get-visible-completions) highlighted-menu-item))
    
    ;; narrow : char -> boolean
    ;; narrows the given selection given a new character (faster than recomputing the whole thing)
    (define/public (narrow char)
      (send completions narrow char)
      (set! highlighted-menu-item 0)
      (set! geometry (compute-geometry))
      (not (send completions empty?)))
    
    ;; widen : -> boolean
    ;; attempts widens the selection by eliminating the last character from the word.
    ;; returns #f if that cannot be done (because there are no characters left); #t otherwise
    (define/public (widen)
      (let ([successfully-widened? (send completions widen)])
        (cond
          [successfully-widened?
           (set! highlighted-menu-item 0)
           (set! geometry (compute-geometry))
           #t]
          [else #f])))
    
    (super-new)))

;; ============================================================
;; configuration parameters

(define (make-guarded-parameter name description default okay?)
  (make-parameter
   default
   (λ (v)
     (cond
       [(okay? v) v]
       [else
        (raise (make-exn:fail:contract
                (string->immutable-string
                 (format "parameter ~a: expected ~a, given: ~e" name description v))
                (current-continuation-marks)))]))))

(define autocomplete-append-after
  (make-guarded-parameter 'append-after "string" "" string?))
(define autocomplete-limit
  (make-guarded-parameter 'limit "positive integer" 15 (λ (x) (and (integer? x) (> x 0)))))

;; ============================================================
;; read keywords from manuals

(define xref #f)

(define (get-completions/manuals manuals)
  (let* ([sym->mpi (λ (mp) (module-path-index-resolve (module-path-index-join mp #f)))]
         [manual-mpis (and manuals (map sym->mpi manuals))])
    
    (unless xref 
      (set! xref (load-collections-xref)))
    
    (let ([ht (make-hash)])
      (for-each
       (λ (entry)
         (let ([desc (entry-desc entry)])
           (when (exported-index-desc? desc)
             (let ([name (exported-index-desc-name desc)])
               (when name
                 (when (or (not manual-mpis)
                           (ormap (λ (from-lib) (memq from-lib manual-mpis))
                                  (map sym->mpi (exported-index-desc-from-libs desc))))
                   (hash-set! ht (symbol->string name) #t)))))))
       (xref-index xref))
      (sort (hash-map ht (λ (x y) x)) string<=?))))

;; ============================================================
;; auto complete example code

#;
(begin
  (define all-words (get-completions/manuals #f))
  
  (let* ([f (new frame% (label "Test") (height 400) (width 400))]
         [e (new (autocomplete-mixin text%))]
         [c (new editor-canvas% (editor e) (parent f))])
    (send c focus)
    (send e insert "\n\n     get")
    (send e set-position (send e last-position) (send e last-position))
    (send f show #t)))

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
(define info% (info-mixin (editor:info-mixin searching%)))

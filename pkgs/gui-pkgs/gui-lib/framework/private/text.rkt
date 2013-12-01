#lang racket/unit

(require string-constants
         racket/class
         racket/match
         racket/path
         racket/math
         "sig.rkt"
         "../gui-utils.rkt"
         "../preferences.rkt"
         "autocomplete.rkt"
         mred/mred-sig
         mrlib/interactive-value-port
         racket/list
         "logging-timer.rkt"
         "coroutine.rkt"
         data/queue)

(require scribble/xref
         scribble/manual-struct)

(import mred^
        [prefix icon: framework:icon^]
        [prefix editor: framework:editor^]
        [prefix keymap: framework:keymap^]
        [prefix color-model: framework:color-model^]
        [prefix frame: framework:frame^]
        [prefix racket: framework:racket^]
        [prefix number-snip: framework:number-snip^]
        [prefix finder: framework:finder^])
(export (rename framework:text^
                [-keymap% keymap%]))
(init-depend framework:editor^)

(define original-output-port (current-output-port))
(define (oprintf . args) (apply fprintf original-output-port args))

;; rectangles : (or/c #f (listof rectangle))
;;  #f => range information needs to be computed for this rectangle
(define-struct range ([start #:mutable] 
                      [end #:mutable]
                      caret-space?
                      style color 
                      adjust-on-insert/delete?
                      key
                      [rectangles #:mutable]) #:inspector #f)
(define-struct rectangle (left top right bottom style color) #:inspector #f)

(define (build-rectangle left top right bottom style color [info (λ () "")])
  (unless (or (symbol? right) (symbol? left))
    (when (right . < . left)
      (error 'build-rectangle "found right to the right of left: ~s; info ~a"
             (list left top right bottom style color)
             (info))))
  (unless (or (symbol? top) (symbol? bottom))
    (when (bottom . < . top)
      (error 'build-rectangle "found bottom above top: ~s; info ~a"
             (list left top right bottom style color)
             (info))))
  (make-rectangle left top right bottom style color))
  

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

(define basic<%>
  (interface (editor:basic<%> (class->interface text%))
    highlight-range
    unhighlight-range
    unhighlight-ranges
    unhighlight-ranges/key
    get-highlighted-ranges
    get-styles-fixed
    get-fixed-style
    set-styles-fixed
    move/copy-to-edit
    initial-autowrap-bitmap
    get-port-name
    port-name-matches?
    get-start-of-line))

(define highlight-range-mixin
  (mixin (editor:basic<%> (class->interface text%)) ()
  
    (inherit invalidate-bitmap-cache
             last-position 
             position-locations
             position-location
             position-line 
             line-start-position
             line-end-position
             get-style-list
             get-admin)

    (define highlight-tmp-color (make-object color% 0 0 0))

    (define ranges-deq (make-queue))
    
    (define/public-final (get-highlighted-ranges) 
      (for/list ([x (in-queue ranges-deq)]) x))
    
    (define/private (recompute-range-rectangles)
      (set! pending-ranges (queue->list ranges-deq))
      (unless recompute-callback-running?
        (set! recompute-callback-running? #t)
        (queue-callback (λ () (run-recompute-range-rectangles)) #f)))
    
    (define pending-ranges '())
    (define recompute-callback-running? #f)
    
    (define/private (run-recompute-range-rectangles)
      (when (get-admin)
        ;; when there is no admin, then the position-location information
        ;; is bogus, so we just give up trying to recompute this information
        
        (define done-time (+ (current-inexact-milliseconds) 20)) 
        (define did-something? #f)
        (let loop ([left #f]
                   [top #f]
                   [right #f]
                   [bottom #f])
          (cond
            [(and did-something? ((current-inexact-milliseconds) . >= . done-time))
             (final-invalidate left top right bottom)
             (queue-callback
              (λ () (run-recompute-range-rectangles))
              #f)]
            [(null? pending-ranges)
             (final-invalidate left top right bottom)
             (set! recompute-callback-running? #f)]
            [else
             (set! did-something? #t)
             (define a-range (car pending-ranges))
             (set! pending-ranges (cdr pending-ranges))
             (define old-rectangles (range-rectangles a-range))
             (cond
               [old-rectangles
                (define new-rectangles (compute-rectangles a-range))
                (cond
                  [(equal? new-rectangles old-rectangles)
                   (loop left top right bottom)]
                  [else
                   (define-values (new-left new-top new-right new-bottom)
                     (for/fold ([left left] [top top] [right right] [bottom bottom]) 
                       ([r (in-list new-rectangles)])
                       (join-rectangles left top right bottom r)))
                   (define-values (both-left both-top both-right both-bottom)
                     (for/fold ([left new-left] [top new-top] [right new-right] [bottom new-bottom]) 
                       ([r (in-list old-rectangles)])
                       (join-rectangles left top right bottom r)))
                   (set-range-rectangles! a-range new-rectangles)
                   (loop both-left both-top both-right both-bottom)])]
               [else 
                ;; when old-rectangles is #f, that means that this
                ;; range has been removed from the ranges-deq, so 
                ;; can just skip over it here.
                (loop left top right bottom)])]))))
    
    (define/private (join-rectangles left top right bottom r)
      (define this-left
        (if (number? (rectangle-left r))
            (adjust r (rectangle-left r) -)
            0.0))
      (define this-right
        (if (number? (rectangle-right r))
            (adjust r (rectangle-right r) +)
            'display-end))
      (define this-top (adjust r (rectangle-top r) -))
      (define this-bottom (adjust r (rectangle-bottom r) +))
      (if (and left top right bottom)
          (values (min this-left left)
                  (min this-top top)
                  (if (and (number? this-right) (number? right))
                      (max this-right right)
                      'display-end)
                  (max this-bottom bottom))
          (values this-left 
                  this-top
                  this-right
                  this-bottom)))
    
    (define/private (final-invalidate left top right bottom)
      (when left
        (let ([width (if (number? right) (- right left) 'display-end)]
              [height (if (number? bottom) (- bottom top) 'display-end)])
          (when (and (or (symbol? width) (> width 0))
                     (or (symbol? height) (> height 0)))
            (invalidate-bitmap-cache left top width height)))))
    
      (define/private (adjust r w f)
        (+ w (f (case (rectangle-style r)
                  [(dot hollow-ellipse) 8]
                  [else 0]))))
    
    (define b1 (box 0))
    (define b2 (box 0))
    (define b3 (box 0))
    (define/private (compute-rectangles range)
      (define start (range-start range))
      (define end (range-end range))
      (define caret-space? (range-caret-space? range))
      (define style (range-style range))
      (define color (range-color range))
      (define lp (last-position))
      (define-values (start-eol? end-eol?) (if (= start end) (values #f #f) (values #f #t)))
      (define-values (end-x top-end-y bottom-end-y)
        (begin (position-locations end b1 b2 #f b3 end-eol? #t)
               (values (unbox b1) 
                       (unbox b2)
                       (unbox b3))))
      (define-values (start-x top-start-y bottom-start-y)
        (begin 
          (position-locations start b1 b2 #f b3 start-eol? #t)
          (values (if (and caret-space? 
                           (not (= start end))
                           (<= (+ (unbox b1) 1) end-x))
                      (+ 1 (unbox b1))
                      (unbox b1))
                  (unbox b2)
                  (unbox b3))))
      (cond
        ;; the position-location values can be strange when
        ;; this condition is true, so we just bail out.
        [(or (> start lp) (> end lp)) '()]
        [(= top-start-y top-end-y)
         (list (build-rectangle start-x
                                top-start-y
                                (if (= end-x start-x)
                                    (+ end-x 1)
                                    end-x)
                                bottom-start-y
                                style
                                color
                                (λ () (format "start = ~s end = ~s filename = ~s content = ~s"
                                              start end 
                                              (send this get-filename)
                                              (send this get-text 0 100)))))]
        [(or (eq? style 'hollow-ellipse)
             (eq? style 'ellipse))
         (define end-line (position-line end end-eol?))
         (let loop ([l (min start-x end-x)]
                    [r (max start-x end-x)]
                    [line (position-line start start-eol?)])
           
           (cond
             [(> line end-line) 
              (list (build-rectangle l top-start-y
                                     r bottom-end-y
                                     style color))]
             [else
              (define line-start (line-start-position line))
              (define line-end (line-end-position line))
              (position-location line-start b1 #f #t)
              (position-location line-end b2 #f #t)
              (loop (min (unbox b1) (unbox b2) l)
                    (max (unbox b1) (unbox b2) r)
                    (+ line 1))]))]
        [else
         (list (build-rectangle start-x top-start-y
                                'right-edge bottom-start-y
                                style color)
               (build-rectangle 'left-edge bottom-start-y
                                'right-edge top-end-y
                                style color)
               (build-rectangle 'left-edge top-end-y
                                end-x bottom-end-y
                                style color))]))
    
    (define/augment (after-insert insert-start insert-len)
      (for ([r (in-queue ranges-deq)])
        (when (range-adjust-on-insert/delete? r)
          (define rstart (range-start r))
          (define rend (range-end r))
          (cond
            [(<= insert-start rstart) 
             (set-range-start! r (+ rstart insert-len))
             (set-range-end! r (+ rend insert-len))]
            [(<= insert-start rend)
             (set-range-end! r (+ rend insert-len))])))
      (inner (void) after-insert insert-start insert-len))
    (define/augment (after-delete delete-start delete-len)
      (define delete-end (+ delete-start delete-len))
      (for ([r (in-queue ranges-deq)])
        (when (range-adjust-on-insert/delete? r)
          (define rstart (range-start r))
          (define rend (range-end r))
          (cond
            [(<= delete-end rstart)
             (set-range-start! r (- rstart delete-len))
             (set-range-end! r (- rend delete-len))]
            [(<= delete-start rstart delete-end rend)
             (define new-len (- rend delete-end))
             (set-range-start! r delete-start)
             (set-range-end! r (+ delete-start new-len))]
            [(<= rstart delete-start delete-end rend)
             (define new-len (- rend delete-end))
             (set-range-start! r delete-start)
             (set-range-end! r (- rend delete-len))]
            [(<= rstart delete-start rend)
             (set-range-end! r delete-end)])))
      (inner (void) after-delete delete-start delete-len))
   
    (define/augment (on-reflow)
      (recompute-range-rectangles)
      (inner (void) on-reflow))
    
    (define/augment (after-load-file success?)
      (inner (void) after-load-file success?)
      (when success?
        (set! ranges-deq (make-queue))))
        
    (define/public (highlight-range start end in-color 
                                    [caret-space? #f]
                                    [priority 'low]
                                    [style 'rectangle] 
                                    #:adjust-on-insert/delete? [adjust-on-insert/delete? #f]
                                    #:key [key #f])
      (unless (let ([exact-pos-int?
                     (λ (x) (and (integer? x) (exact? x) (x . >= . 0)))])
                (and (exact-pos-int? start)
                     (exact-pos-int? end)))
        (error 'highlight-range
               "expected first two arguments to be non-negative exact integers, got: ~e ~e"
               start
               end))
      (unless (<= start end)
        (error 'highlight-range
               "expected start to be less than end, got ~e ~e" start end))
      (unless (or (eq? priority 'high) (eq? priority 'low))
        (error 'highlight-range
               "expected priority argument to be either 'high or 'low, got: ~e"
               priority))
      (unless (or (is-a? in-color color%)
                  (and (string? in-color)
                       (send the-color-database find-color in-color)))
        (error 'highlight-range
               "expected a color or a string in the-color-database for the third argument, got ~e" 
               in-color))
      (unless (memq style '(rectangle hollow-ellipse ellipse dot))
        (error 'highlight-range
               "expected one of 'rectangle, 'ellipse 'hollow-ellipse, or 'dot as the style, got ~e"
               style))
      (when (eq? style 'dot)
        (unless (= start end)
          (error 'highlight-range
                 "when the style is 'dot, the start and end regions must be the same")))
      
      (define color (if (is-a? in-color color%)
                        in-color
                        (send the-color-database find-color in-color)))
      (define l (make-range start end caret-space? style color adjust-on-insert/delete? key #f))
      (if (eq? priority 'high)
          (enqueue! ranges-deq l)
          (enqueue-front! ranges-deq l))
      (set-range-rectangles! l (compute-rectangles l))
      (invalidate-rectangles (range-rectangles l))
      (unless adjust-on-insert/delete?
        (λ () 
          (unhighlight-range start end color caret-space? style))))
        
    (define/public (unhighlight-range start end in-color [caret-space? #f] [style 'rectangle])
      (define color (if (is-a? in-color color%)
                        in-color
                        (send the-color-database find-color in-color)))
      (unhighlight-ranges
       (λ (r-start r-end r-color r-caret-space? r-style r-adjust-on-insert/delete? r-key)
         (and (equal? start r-start)
              (equal? end r-end)
              (equal? color r-color)
              (equal? caret-space? r-caret-space?)
              (equal? style r-style)))
       #t))
    
    (define/public (unhighlight-ranges/key key)
      (unhighlight-ranges
       (λ (r-start r-end r-color r-caret-space? r-style r-adjust-on-insert/delete? r-key)
         (equal? r-key key))))
    
    (define/public (unhighlight-ranges pred [just-one? #f])
      (define left #f)
      (define top #f)
      (define right #f)
      (define bottom #f)
      (define found-one? #f)
      (queue-filter! 
       ranges-deq
       (λ (a-range)
         (cond
           [(and just-one? found-one?) #t]
           [(pred (range-start a-range)
                  (range-end a-range)
                  (range-color a-range)
                  (range-caret-space? a-range)
                  (range-style a-range)
                  (range-adjust-on-insert/delete? a-range)
                  (range-key a-range))
            (set! found-one? #t)
            (for ([rect (in-list (range-rectangles a-range))])
              (set!-values (left top right bottom)
                (join-rectangles left top right bottom rect)))
            (set-range-rectangles! a-range #f)
            #f]
           [else
            #t])))
      (final-invalidate left top right bottom))
    
    (define/private (invalidate-rectangles rectangles)
      (let loop ([left #f]
                 [top #f]
                 [right #f]
                 [bottom #f]
                 [rectangles rectangles])
        (cond
          [(null? rectangles)
           (final-invalidate left top right bottom)]
          [else
           (define-values (new-left new-top new-right new-bottom)
             (join-rectangles left top right bottom (car rectangles)))
           (loop new-left new-top new-right new-bottom
                 (cdr rectangles))])))
    
    (define/override (on-paint before dc left-margin top-margin right-margin bottom-margin
                               dx dy draw-caret)
      (super on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
      (when before
        (define-values (view-x view-y view-width view-height)
          (let ([admin (get-admin)])
            (if admin
                (let ([b1 (box 0)]
                      [b2 (box 0)]
                      [b3 (box 0)]
                      [b4 (box 0)])
                  (send admin get-view b1 b2 b3 b4)
                  (values (unbox b1)
                          (unbox b2)
                          (unbox b3)
                          (unbox b4)))
                (values left-margin top-margin right-margin bottom-margin))))
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (define old-smoothing (send dc get-smoothing))
        (define last-color #f)
        (send dc set-smoothing 'aligned)
        (for ([range (in-queue ranges-deq)])
          (for ([rectangle (in-list (range-rectangles range))])
            (define left (if (number? (rectangle-left rectangle))
                             (rectangle-left rectangle)
                             view-x))
            (define top (rectangle-top rectangle))
            (define right (if (number? (rectangle-right rectangle))
                              (rectangle-right rectangle)
                              (+ view-x view-width)))
            (define bottom (rectangle-bottom rectangle))
            (when (and (or (<= left-margin left right-margin)
                           (<= left-margin right right-margin)
                           (<= left left-margin right-margin right))
                       (or (<= top-margin top bottom-margin)
                           (<= top-margin bottom bottom-margin)
                           (<= top top-margin bottom-margin bottom)))
              (define width (if (right . <= . left) 0 (- right left)))
              (define height (if (bottom . <= . top) 0 (- bottom top)))
              (define color (let ([rc (rectangle-color rectangle)])
                              (cond
                                [(not (= 1 (send rc alpha))) rc]
                                [(and last-color (eq? last-color rc))
                                 rc]
                                [rc
                                 (set! last-color #f)
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
                                 rc])))
              (when color
                (case (rectangle-style rectangle)
                  [(dot)
                   (let ([cx left]
                         [cy bottom])
                     (send dc set-pen "black" 1 'transparent)
                     (send dc set-brush color 'solid)
                     (send dc draw-ellipse (+ dx cx -3) (+ dy cy -3) 6 6))]
                  [(hollow-ellipse)
                   (send dc set-pen color 3 'solid)
                   (send dc set-brush "black" 'transparent)
                   (send dc draw-ellipse 
                         (+ dx left -4)
                         (+ dy top -4)
                         (+ width 8)
                         (+ height 8))]
                  [(rectangle)
                   (send dc set-pen color 1 'transparent)
                   (send dc set-brush color 'solid)
                   (send dc draw-rectangle (+ left dx) (+ top dy) width height)]
                  [(ellipse)
                   (send dc set-pen color 1 'transparent)
                   (send dc set-brush color 'solid)
                   (send dc draw-ellipse (+ left dx) (+ top dy) width height)])))))
            (send dc set-smoothing old-smoothing)
            (send dc set-pen old-pen)
            (send dc set-brush old-brush)))
    
    (super-new)))
  
(define other-basics-mixin
  (mixin (editor:basic<%> (class->interface text%)) ()
    (inherit get-canvas split-snip get-snip-position
             begin-edit-sequence end-edit-sequence
             set-autowrap-bitmap
             delete find-snip 
             get-style-list change-style
             position-line line-start-position
             get-filename)
    
    (define/public (get-fixed-style)
      (send (get-style-list) find-named-style "Standard"))

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
                 (or (equal? id filename) ;; "fast path" check
                     (equal? (normal-case-path (normalize-path (get-filename)))
                             (normal-case-path (normalize-path id)))))
            (and (symbol? port-name-identifier)
                 (symbol? id)
                 (equal? port-name-identifier id)))))
    
    
    (define styles-fixed? #f)
    (public get-styles-fixed set-styles-fixed)
    (define (get-styles-fixed) styles-fixed?)
    (define (set-styles-fixed b) (set! styles-fixed? b))
    
    (define edition 0)
    (define/public (get-edition-number) edition)
    
    (define/augment (on-insert start len)
      (begin-edit-sequence #t #f)
      (inner (void) on-insert start len))
    (define/augment (after-insert start len)
      (set! edition (+ edition 1))
      (when styles-fixed?
        (change-style (get-fixed-style) start (+ start len) #f))
      (inner (void) after-insert start len)
      (end-edit-sequence))
   (define/augment (after-delete start len)
     (set! edition (+ edition 1))
     (inner (void) after-delete start len))
    
    (define/public (move/copy-to-edit dest-edit start end dest-position
                                      #:try-to-move? [try-to-move? #t])
      (split-snip start)
      (split-snip end)
      (let loop ([snip (find-snip end 'before)])
        (cond
          [(or (not snip) (< (get-snip-position snip) start))
           (void)]
          [else
           (let ([prev (send snip previous)]
                 [released/copied 
                  (if try-to-move?
                      (if (send snip release-from-owner)
                          snip
                          (let* ([copy (send snip copy)]
                                 [snip-start (get-snip-position snip)]
                                 [snip-end (+ snip-start (send snip get-count))])
                            (delete snip-start snip-end)
                            snip))
                      (send snip copy))])
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
    
    (define/public (get-start-of-line pos)
      (line-start-position (position-line pos)))
    
    (super-new)
    (set-autowrap-bitmap (initial-autowrap-bitmap))))

(define (basic-mixin %)
  (class* (highlight-range-mixin (other-basics-mixin %)) (basic<%>)
    (super-new)))

(define line-spacing<%> (interface ()))

(define line-spacing-mixin
  (mixin (basic<%>) (line-spacing<%>)
    (super-new)
    (inherit set-line-spacing)
    ;; this is a field so that the weakly
    ;; held callback works out properly
    (define (pref-cb-func sym val)
      (set-line-spacing (if val 1 0)))
    (preferences:add-callback 'framework:line-spacing-add-gap?
                              pref-cb-func
                              #t)
    (set-line-spacing (if (preferences:get 'framework:line-spacing-add-gap?)
                          1
                          0))))

(define first-line<%>
  (interface ()
    highlight-first-line
    get-first-line-height
    first-line-currently-drawn-specially?
    is-special-first-line?))

(define dark-first-line-color (make-object color% 50 0 50))
(define dark-wob-first-line-color (make-object color% 255 200 255))

(define first-line-mixin
  (mixin ((class->interface text%)) (first-line<%>)
    (inherit get-text paragraph-end-position get-admin invalidate-bitmap-cache position-location
             scroll-to local-to-global get-dc get-padding)
    (define bx (box 0))
    (define by (box 0))
    (define bw (box 0))
    
    (define fancy-first-line? #f)
    
    (define first-line "")
    (define end-of-first-line 0)
    (define first-line-is-lang? #f)

    (define/public-final (highlight-first-line on?)
      (unless (equal? fancy-first-line? on?)
        (set! fancy-first-line? on?)
        (invalidate-bitmap-cache)
        (let ([canvas (send this get-canvas)])
          (when canvas
            (send canvas refresh)))))
    
    (define/public-final (get-first-line-height)
      (let-values ([(_1 h _2 _3) (send (get-dc) get-text-extent first-line (get-font))])
        h))
    
    (define/public-final (first-line-currently-drawn-specially?)
      (and (show-first-line?)
           (let ([admin (get-admin)])
             (and admin
                  (begin
                    (send admin get-view #f by #f #f #f)
                    (not (= (unbox by) 0)))))))
           
    (define/public (is-special-first-line? l) #f)
    
    (define/private (show-first-line?)
      (and fancy-first-line? first-line-is-lang?))
    
    (define/private (update-first-line)
      (set! end-of-first-line (paragraph-end-position 0))
      (set! first-line (get-text 0 end-of-first-line))
      (set! first-line-is-lang? (is-special-first-line? first-line)))
    
    (define/augment (after-insert start len)
      (when (<= start end-of-first-line)
        (update-first-line))
      (inner (void) after-insert start len))
    (define/augment (after-delete start len)
      (when (<= start end-of-first-line)
        (update-first-line))
      (inner (void) after-delete start len))
    
    (define/override (scroll-editor-to localx localy width height refresh? bias)
      (let ([admin (get-admin)])
        (cond
          [(not admin)
           #f]
          [(show-first-line?)
           (let ([h (get-first-line-height)])
             (set-box! by localy)
             (local-to-global #f by)
             (cond
               [(<= (unbox by) h)
                ;; the max is relevant when we're already scrolled to the top.
                (super scroll-editor-to localx (max 0 (- localy h)) width height refresh? bias)]
               [else
                (super scroll-editor-to localx localy width height refresh? bias)]))]
          [else
           (super scroll-editor-to localx localy width height refresh? bias)])))
    
    (define/override (on-event event)
      (cond
        [(or (send event moving?)
             (send event leaving?)
             (send event entering?))
         (super on-event event)]
        [else
         (let ([y (send event get-y)]
               [h (get-first-line-height)]
               [admin (get-admin)])
           (unless admin (send admin get-view #f by #f #f #f))
           (cond
             [(and admin
                   (< y h)
                   (not (= (unbox by) 0)))
              (send admin scroll-to (send event get-x) 0 0 0 #t)
              (super on-event event)]
             [else         
              (super on-event event)]))]))
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (when (show-first-line?) 
          (define admin (get-admin))
          (when admin
            (send admin get-view bx by bw #f #f)
            (unless (= (unbox by) 0)
              (define draw-first-line-number?
                (and (is-a? this line-numbers<%>)
                     (send this showing-line-numbers?)))
              (define first-line (get-text 0 (paragraph-end-position 0)))
              (define old-pen (send dc get-pen))
              (define old-brush (send dc get-brush))
              (define old-smoothing (send dc get-smoothing))
              (define old-α (send dc get-alpha))
              (define old-font (send dc get-font))
              (define old-text-foreground (send dc get-text-foreground))
              (define w-o-b? (preferences:get 'framework:white-on-black?))
              (send dc set-font (get-font))
              (send dc set-smoothing 'aligned)
              (define-values (tw th _1 _2) (send dc get-text-extent first-line))
              (define line-height (+ (unbox by) dy th 1))
              (define line-left (+ (unbox bx) dx))
              (define line-right (+ (unbox bx) dx (unbox bw)))
              
              (if w-o-b?
                  (send dc set-pen "white" 1 'solid)
                  (send dc set-pen "black" 1 'solid))
              (send dc draw-line line-left line-height line-right line-height)
              
              (when (eq? (send dc get-smoothing) 'aligned)
                (define start (if w-o-b? 6/10 3/10))
                (define end 0)
                (define steps 10)
                (send dc set-pen 
                      (if w-o-b? dark-wob-first-line-color dark-first-line-color) 
                      1
                      'solid)
                (let loop ([i steps])
                  (unless (zero? i)
                    (define alpha-value (+ start (* (- end start) (/ i steps))))
                    (send dc set-alpha alpha-value)
                    (send dc draw-line 
                          line-left
                          (+ line-height i)
                          line-right
                          (+ line-height i))
                    (loop (- i 1)))))
              
              (send dc set-alpha 1)
              (send dc set-pen "gray" 1 'transparent)
              (send dc set-brush (if w-o-b? "black" "white") 'solid)
              (send dc draw-rectangle (+ (unbox bx) dx) (+ (unbox by) dy) (unbox bw) th)
              (send dc set-text-foreground
                    (send the-color-database find-color
                          (if w-o-b? "white" "black")))
              (define x-start
                (cond
                  [draw-first-line-number?
                   (send this do-draw-single-line dc dx dy 0 (unbox by) #f #f)
                   (send dc set-pen (if w-o-b? "white" "black") 1 'solid)
                   (send this draw-separator dc (unbox by) (+ (unbox by) line-height) dx dy)
                   (define-values (padding-left _1 _2 _3) (get-padding))
                   padding-left]
                  [else 0]))
              (send dc draw-text first-line (+ x-start (+ (unbox bx) dx)) (+ (unbox by) dy))
              
              (send dc set-text-foreground old-text-foreground)
              (send dc set-font old-font)
              (send dc set-pen old-pen)
              (send dc set-brush old-brush)
              (send dc set-alpha old-α)
              (send dc set-smoothing old-smoothing)))))
      (super on-paint before? dc left top right bottom dx dy draw-caret))
    
    (inherit get-style-list)
    (define/private (get-font)
      (define style-list (get-style-list))
      (define std (or (send style-list find-named-style "Standard")
                      (send style-list basic-style)))
      (send std get-font))
    
    (super-new)))


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
      (begin-edit-sequence #t #f))
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
    (super-new)))

(define column-guide<%> (interface ((class->interface text%))))
(define column-guide-mixin-pen-size 2)
(define column-guide-mixin 
  (mixin ((class->interface text%)) (column-guide<%>)
    (inherit get-style-list invalidate-bitmap-cache get-dc
             begin-edit-sequence end-edit-sequence
             get-extent get-padding)
    (define char-width #f)
    (define pen #f)
    ;; these two functions are defined as private fields
    ;; because they are weakly held callbacks
    (define (bw-cb p v)
      (set! pen 
            (send the-pen-list find-or-create-pen
                  (if v
                      (make-object color% 225 225 51)
                      (make-object color% 204 204 51))
                  (* column-guide-mixin-pen-size 2)
                  'solid)))
    (define (cw-cb p v)
      (define new-cw (and (car v) (cadr v)))
      (unless (equal? new-cw char-width)
        (define (inv cw)
          (define x-pos (get-x-spot cw))
          (when x-pos
            (invalidate-bitmap-cache 
             (- x-pos (send pen get-width))
             0
             (+ x-pos (send pen get-width))
             'end)))
        (define old-char-w char-width)
        (set! char-width new-cw)
        (begin-edit-sequence #t #f)
        (inv old-char-w)
        (inv char-width)
        (end-edit-sequence)))
      
    (super-new)
    
    (preferences:add-callback 'framework:white-on-black? bw-cb #t)
    (bw-cb 'ignored-arg (preferences:get 'framework:white-on-black?))
    
    (preferences:add-callback 'framework:column-guide-width cw-cb #t)
    (cw-cb 'ignored-arg (preferences:get 'framework:column-guide-width))
    
    (define aw (box 0.0))
    (define ah (box 0.0))
    (define old-draw-the-line? #f)
    (define left-padding 0)
    
    (define/augment (on-change)
      (inner (void) on-change)
      (define old-aw (unbox aw))
      (define old-ah (unbox ah))
      (get-extent aw ah)
      (define new-draw-the-line? (draw-the-line?))
      (define-values (left top right bottom) (get-padding))
      (unless (and (= old-aw (unbox aw)) 
                   (= old-ah (unbox ah))
                   (= left left-padding)
                   (equal? new-draw-the-line? old-draw-the-line?))
        (set! old-draw-the-line? new-draw-the-line?)
        (set! left-padding left)
        (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end)))
    
    ;; pre: aw initialized to current value
    (define/private (draw-the-line?)
      (define x-pos (get-x-spot char-width))
      (and x-pos
           (< x-pos (- (unbox aw) 3))))
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (when char-width
        (when before?
          (define x-pos (get-x-spot char-width))
          (when x-pos
            (define old-pen (send dc get-pen))
            (send dc set-pen pen)
            (when (draw-the-line?)
              (send dc draw-line
                    (+ dx x-pos)
                    (+ dy top column-guide-mixin-pen-size)
                    (+ dx x-pos)
                    (+ dy (min (unbox ah) bottom) (- column-guide-mixin-pen-size))))
            (send dc set-pen old-pen)))))

    (define/private (get-x-spot char-width)
      (let/ec return
        (unless char-width (return #f))
        (define dc (get-dc))
        (unless dc (return #f))
        (define style (or (send (get-style-list) find-named-style "Standard")
                          (send (get-style-list) find-named-style "Basic")))
        (unless style (return #f))
        (define fnt (send style get-font))
        (define-values (xw _1 _2 _3) (send dc get-text-extent "x" fnt))
        (+ left-padding (* xw char-width))))))

(define normalize-paste<%> (interface ((class->interface text%))
                             ask-normalize?
                             string-normalize))
(define normalize-paste-mixin
  (mixin (basic<%>) (normalize-paste<%>)
    (inherit begin-edit-sequence end-edit-sequence
             delete insert split-snip find-snip
             get-snip-position get-top-level-window find-string)
    
    ;; pasting-info : (or/c #f (listof (list number number)))
    ;; when #f, we are not in a paste
    ;; when a list, we are in a paste and the 
    ;;   list contains the regions that have
    ;;   been changed by the paste
    (define paste-info #f)

    (define/public (ask-normalize?)
      (cond
        [(preferences:get 'framework:ask-about-paste-normalization)
         (let-values ([(mbr checked?)
                       (message+check-box/custom
                        (string-constant drscheme)
                        (string-constant normalize-string-info)
                        (string-constant dont-ask-again)
                        (string-constant normalize)
                        (string-constant leave-alone)
                        #f
                        (get-top-level-window)
                        (cons (if (preferences:get 'framework:do-paste-normalization)
                                  'default=1
                                  'default=2)
                              '(caution))
                        2)])
           (let ([normalize? (not (equal? 2 mbr))])
             (preferences:set 'framework:ask-about-paste-normalization (not checked?))
             (preferences:set 'framework:do-paste-normalization normalize?)
             normalize?))]
        [else
         (preferences:get 'framework:do-paste-normalization)]))
    (define/public (string-normalize s) 
      (regexp-replace* #rx"\u200b" (string-normalize-nfkc s) ""))
    
    (define/override (do-paste start time)
      (dynamic-wind
       (λ () (set! paste-info '()))
       (λ () (super do-paste start time)
         (let ([local-paste-info paste-info])
           (set! paste-info #f)
           (deal-with-paste local-paste-info)))
       ;; use the dynamic wind to be sure that the paste-info is set back to #f
       ;; in the case that the middle thunk raises an exception
       (λ () (set! paste-info #f))))
    
    (define/augment (after-insert start len)
      (when paste-info
        (set! paste-info (cons (list start len) paste-info)))
      (inner (void) after-insert start len))

    (define/private (deal-with-paste local-paste-info)
      (let/ec abort
        (define ask? #t)
        (for ([insertion (in-list local-paste-info)])
          (define start (list-ref insertion 0))
          (define len (list-ref insertion 1))
          (split-snip start)
          (split-snip (+ start len))
          (let loop ([snip (find-snip start 'after-or-none)])
            (when snip
              (let ([pos (get-snip-position snip)])
                (when (< pos (+ start len))
                  (when (is-a? snip string-snip%)
                    (let* ([old (send snip get-text 0 (send snip get-count))]
                           [new (string-normalize old)])
                      (unless (equal? new old)
                        (when ask?
                          (set! ask? #f)
                          (unless (ask-normalize?) (abort)))
                        (let ([snip-pos (get-snip-position snip)])
                          (delete snip-pos (+ snip-pos (string-length old)))
                          (insert new snip-pos snip-pos #f)))))
                  (loop (send snip next)))))))))
    
    (super-new)))

(define searching<%> 
  (interface (editor:keymap<%> basic<%>)
    set-replace-start
    get-replace-search-hit
    set-searching-state
    set-search-anchor
    get-search-bubbles
    get-search-hit-count))

(define normal-search-color (send the-color-database find-color "plum"))
(define dark-search-color (send the-color-database find-color "mediumorchid"))
(define light-search-color 
  (let ([f (λ (x) (+ x (floor (* (- 255 x) 2/3))))])
    (make-object color% 
      (f (send normal-search-color red))
      (f (send normal-search-color green))
      (f (send normal-search-color blue)))))
(define white-on-black-yellow-bubble-color (make-object color% 50 50 5))

(define searching-mixin
  (mixin (editor:basic<%> editor:keymap<%> basic<%>) (searching<%>)
    (inherit invalidate-bitmap-cache
             get-start-position get-end-position
             unhighlight-ranges/key unhighlight-range highlight-range
             run-after-edit-sequence begin-edit-sequence end-edit-sequence
             find-string get-admin position-line
             in-edit-sequence? get-pos/text-dc-location
             get-canvas get-top-level-window)
    
    (define has-focus? #f)
    (define clear-yellow void)
    (define searching-str #f)
    (define case-sensitive? #f)
    (define search-hit-count 0)
    (define before-caret-search-hit-count 0)
    (define search-coroutine #f)
    
    (define update-replace-bubble-callback-running? #f)
    (define search-position-callback-running? #f)
      
    (define anchor-pos #f)
    
    ;; replace-mode? : boolean?
    ;; #t if the replace portion of the GUI is visible
    ;; (and thus we have light/dark bubbles)
    (define replace-mode? #f)
    
    ;; to-replace-highlight : (or/c #f (cons/c number number))
    ;; the location where the next replacement will happen, or #f
    ;; if there isn't one (in case the insertion point is past
    ;; the last search hit, or replace-mode? is #f)
    ;; invariant: to-replace-highlight is not mapped in search-bubble-table
    ;;            (even though it is a legtimate hit)
    (define to-replace-highlight #f)
    
    ;; search-bubble-table : hash-table[(cons number number) -o> #t]
    (define search-bubble-table (make-hash))
    
    ;; get-replace-search-hit : -> (or/c number #f)
    ;; returns the nearest search hit after `replace-start'
    (define/public (get-replace-search-hit) 
      (and searching-str
           to-replace-highlight
           (car to-replace-highlight)))

    ;; NEW METHOD: used for test suites
    (define/public (search-updates-pending?) 
      (or update-replace-bubble-callback-running?
          search-position-callback-running?
          search-coroutine))
    
    (define/public (set-replace-start n) (void))
    
    (define/public (get-anchor-pos) anchor-pos)

    (define/public (set-search-anchor position)
      (begin-edit-sequence #t #f)
      (when anchor-pos (unhighlight-anchor))
      (cond
        [(and position
              (preferences:get 'framework:anchored-search))
         (set! anchor-pos position)
         (highlight-anchor)]
        [else
         (set! anchor-pos #f)])
      (end-edit-sequence))
    
    (define/public (get-search-hit-count) (values before-caret-search-hit-count search-hit-count))
    
    (define/public (set-searching-state s in-cs? in-r? [notify-frame? #f])
      (define r? (and in-r? #t))
      (define cs? (and in-cs? #t))
      (unless (and (equal? searching-str s)
                   (equal? case-sensitive? cs?)
                   (equal? r? replace-mode?))
        (set! searching-str s)
        (set! case-sensitive? cs?)
        (set! replace-mode? r?)
        (redo-search notify-frame?)))
    
    (define/override (get-keymaps)
      (editor:add-after-user-keymap (keymap:get-search) (super get-keymaps)))
    
    (define/augment (after-insert start len)
      (when searching-str
        (redo-search #t))
      (inner (void) after-insert start len))
    (define/augment (after-delete start len)
      (when searching-str
        (redo-search #t))
      (inner (void) after-delete start len))
    
    (define/override (on-focus on?)
      (let ([f (get-top-level-window)])
        (when (is-a? f frame:searchable<%>)
          (set! has-focus? on?)
          (cond
            [on?
             ;; this triggers a call to update-yellow
             (send f set-text-to-search this)]
            [else
             (update-yellow)])))
      (super on-focus on?))
    
    (define/augment (after-set-position)
      (update-yellow)
      (maybe-queue-update-replace-bubble)
      (maybe-queue-search-position-update)
      (inner (void) after-set-position))
    
    (define/private (maybe-queue-update-replace-bubble)
      (unless update-replace-bubble-callback-running?
        (set! update-replace-bubble-callback-running? #t)
        (queue-callback
         (λ () 
           (set! update-replace-bubble-callback-running? #f)
           (unless search-coroutine
             ;; the search co-routine will update
             ;; the replace bubble to its proper color
             ;; before it finishes so we can just let
             ;; do this job
             
             
             (define (replace-highlight->normal-hit)
               (when to-replace-highlight
                 (let ([old-to-replace-highlight to-replace-highlight])
                   (unhighlight-replace)
                   (highlight-hit old-to-replace-highlight))))
             
             (cond
               [(or (not searching-str)
                    (not replace-mode?))
                (when to-replace-highlight
                  (unhighlight-replace))]
               [else
                (define next (do-search (get-start-position) 'eof))
                (begin-edit-sequence #t #f)
                (cond
                  [next
                   (unless (and to-replace-highlight
                                (= (car to-replace-highlight) next)
                                (= (cdr to-replace-highlight) 
                                   (+ next (string-length searching-str))))
                     (replace-highlight->normal-hit)
                     (define pr (cons next (+ next (string-length searching-str))))
                     (unhighlight-hit pr)
                     (highlight-replace pr))]
                  [else
                   (replace-highlight->normal-hit)])
                (end-edit-sequence)])))
         #f)))
    
    ;; maybe-queue-editor-position-update : -> void
    ;; updates the editor-position in the frame,
    ;; but delays it until the next low-priority event occurs.
    (define/private (maybe-queue-search-position-update)
      (unless search-position-callback-running?
        (set! search-position-callback-running? #t)
        (queue-callback
         (λ ()
           (when searching-str
             (define count 0)
             (define start-pos (get-start-position))
             (hash-for-each
              search-bubble-table
              (λ (k v)
                (when (<= (car k) start-pos)
                  (set! count (+ count 1)))))
             (update-before-caret-search-hit-count count))
           (set! search-position-callback-running? #f))
         #f)))
    
    (define/private (update-before-caret-search-hit-count c)
      (unless (equal? before-caret-search-hit-count c)
        (set! before-caret-search-hit-count c)
        (let ([tlw (get-top-level-window)])
          (when (is-a? tlw frame:searchable<%>)
            (send tlw search-hits-changed)))))
    
    (define/private (update-yellow)
      (cond
        [has-focus?
         (unless (eq? clear-yellow void)
           (clear-yellow)
           (set! clear-yellow void))]
        [searching-str
         (let ([start (get-start-position)]
               [end (get-end-position)])
           (cond
             [(= start end)
              (clear-yellow)
              (set! clear-yellow void)]
             [else
              (begin-edit-sequence #t #f)
              (clear-yellow)
              (set! clear-yellow void)
              (when (and searching-str (= (string-length searching-str) (- end start)))
                (when (do-search start end)
                  (set! clear-yellow (highlight-range
                                      start end
                                      (if (preferences:get 'framework:white-on-black?)
                                          white-on-black-yellow-bubble-color
                                          "khaki")
                                      #f 'low 'ellipse))))
              (end-edit-sequence)]))]
        [else
         (clear-yellow)
         (set! clear-yellow void)]))

    (define/public (get-search-bubbles)
      (sort 
       (append
        (if to-replace-highlight
            (list (list to-replace-highlight 'dark-search-color))
            (list))
        (hash-map search-bubble-table
                  (λ (x true) 
                    (list x (if replace-mode? 'light-search-color 'normal-search-color)))))
       string<?
       #:key (λ (x) (format "~s" (car x)))))
    
    
    (define/private (redo-search notify-frame?)
      (define old-search-coroutine search-coroutine)
      (set! search-coroutine (create-search-coroutine notify-frame?))
      (unless old-search-coroutine 
        ;; when old-search-coroutine is not #f, then
        ;; we know that there is already a callback
        ;; pending; the set! above just change what 
        ;; it will be doing.
        (queue-callback (λ () (run-search)) #f)))
    
    (define/private (run-search)
      (define done? (coroutine-run search-coroutine (void)))
      (cond
        [done?
         (set! search-coroutine #f)]
        [else
         (queue-callback
          (λ () (run-search))
          #f)]))
    
    (define/private (create-search-coroutine notify-frame?)
      (coroutine
       pause
       first-val
       (define start-time (current-inexact-milliseconds))
       (define did-something? #f)
       (define (maybe-pause)
         (cond
           [(not did-something?)
            (set! did-something? #t)]
           [((+ start-time 30) . < . (current-inexact-milliseconds))
            (define was-in-edit-sequence? (in-edit-sequence?))
            (when was-in-edit-sequence?
              (end-edit-sequence))
            (pause)
            (when was-in-edit-sequence?
              (begin-edit-sequence #t #f))
            (set! did-something? #f)
            (set! start-time (current-inexact-milliseconds))
            #t]
           [else #f]))
         
       (cond
        [searching-str
         (define new-search-bubbles '())
         (define new-replace-bubble #f)
         (define first-hit (do-search 0 'eof))
         (define-values (this-search-hit-count this-before-caret-search-hit-count)
           (cond
             [first-hit
              (define sp (get-start-position))
              (let loop ([bubble-start first-hit]
                         [search-hit-count 0]
                         [before-caret-search-hit-count 1])
                (maybe-pause)
                (define bubble-end (+ bubble-start (string-length searching-str)))
                (define bubble (cons bubble-start bubble-end))
                (define this-bubble
                  (cond
                    [(and replace-mode?
                          (not new-replace-bubble)
                          (<= sp bubble-start))
                     (set! new-replace-bubble bubble)
                     'the-replace-bubble]
                    [else
                     bubble]))
                (set! new-search-bubbles (cons this-bubble new-search-bubbles))
                      
                (define next (do-search bubble-end 'eof))
                (define next-before-caret-search-hit-count
                  (if (and next (< next sp))
                      (+ 1 before-caret-search-hit-count)
                      before-caret-search-hit-count))
                (cond
                  [next
                   ;; start a new one if there is another hit
                   (loop next 
                         (+ search-hit-count 1)
                         next-before-caret-search-hit-count)]
                  [else
                   (values (+ search-hit-count 1) 
                           before-caret-search-hit-count)]))]
             [else (values 0 0)]))
         
         (set! search-hit-count this-search-hit-count)
         (set! before-caret-search-hit-count this-before-caret-search-hit-count)
         
         (maybe-pause)
         
         (begin-edit-sequence #t #f)
         (clear-all-regions)
         
         (maybe-pause)
         
         (for ([search-bubble (in-list (reverse new-search-bubbles))])
           (cond
             [(eq? search-bubble 'the-replace-bubble)
              (highlight-replace new-replace-bubble)]
             [else
              (highlight-hit search-bubble)])
           (maybe-pause))
         
         (update-yellow) 
         (end-edit-sequence)]
        [else
         (begin-edit-sequence #t #f)
         (clear-all-regions)
         (set! search-hit-count 0)
         (set! before-caret-search-hit-count 0)
         (update-yellow) 
         (end-edit-sequence)])
       (when notify-frame?
         (define canvas (get-canvas))
         (when canvas
           (let loop ([w canvas])
             (cond
               [(is-a? w frame:searchable<%>)
                (send w search-hits-changed)]
               [(is-a? w area<%>)
                (loop (send w get-parent))]))))))
    
    (define/private (clear-all-regions) 
      (when to-replace-highlight 
        (unhighlight-replace))
      (unhighlight-ranges/key 'plt:framework:search-bubbles)
      (set! search-bubble-table (make-hash)))
    
    (define/private (do-search start end)
      (find-string searching-str 'forward start end #t case-sensitive?))
    
    ;; INVARIANT: when a search bubble is highlighted,
    ;; the search-bubble-table has it mapped to #t
    ;; the two methods below contribute to this, but
    ;; so does the 'clear-all-regions' method above
    
    
    ;; this method may be called with bogus inputs (ie a pair that has no highlight)
    ;; but only when there is a pending "erase all highlights and recompute everything" callback
    (define/private (unhighlight-hit pair)
      (hash-remove! search-bubble-table pair)
      (unhighlight-range (car pair) (cdr pair) 
                         (if replace-mode? light-search-color normal-search-color)
                         #f
                         'hollow-ellipse))
    (define/private (highlight-hit pair)
      (hash-set! search-bubble-table pair #t)
      (highlight-range (car pair) (cdr pair) 
                       (if replace-mode? light-search-color normal-search-color)
                       #f
                       'low
                       'hollow-ellipse
                       #:key 'plt:framework:search-bubbles
                       #:adjust-on-insert/delete? #t))
    
    ;; INVARIANT: the "next to replace" highlight is always
    ;; saved in 'to-replace-highlight'
    (define/private (unhighlight-replace)
      (unhighlight-range (car to-replace-highlight)
                         (cdr to-replace-highlight)
                         dark-search-color
                         #f
                         'hollow-ellipse)
      (set! to-replace-highlight #f))
    
    (define/private (highlight-replace new-to-replace)
      (set! to-replace-highlight new-to-replace)
      (highlight-range (car to-replace-highlight)
                       (cdr to-replace-highlight)
                       dark-search-color
                       #f
                       'high
                       'hollow-ellipse))
    
    (define/private (unhighlight-anchor)
      (unhighlight-range anchor-pos anchor-pos "red" #f 'dot)
      (unhighlight-range anchor-pos anchor-pos "red"))
    
    (define/private (highlight-anchor)
      (highlight-range anchor-pos anchor-pos "red" #f 'low 'dot)
      (highlight-range anchor-pos anchor-pos "red"))
    
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
    (define/override (partial-offset dc x y len)
      len)
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
    
    (define cache-function void)
    (define cache-str (make-string 1 #\space))
    (define container-str (make-string 1 #\space))
    
    (inherit get-text!)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (let ([len (get-count)])
        (unless (= len (string-length container-str))
          (set! container-str (make-string len #\space))
          (set! cache-function void))
        (get-text! container-str 0 len 0)
        (unless (string=? container-str cache-str)
          (set! cache-function (for-each/sections container-str))
          (set! cache-str (make-string len #\space))
          (get-text! cache-str 0 len 0)))
      (when (<= top y bottom)
        (cache-function dc x y)))
    
    (apply super-make-object args)))

;; for-each/sections : string -> dc number number -> void
(define (for-each/sections str)
  (let ([str-len (string-length str)])
    (cond
      [(zero? str-len)
       void]
      [else
       (let loop ([i 1]
                  [len 1]
                  [start 0]
                  [blank? (char-whitespace? (string-ref str 0))])
         (cond
           [(= i str-len)
            (if blank?
                void
                (λ (dc x y)
                  (send dc draw-line (+ x start) y (+ x start (- len 1)) y)))]
           [else
            (let ([white? (char-whitespace? (string-ref str i))])
              (cond
                [(eq? white? blank?)
                 (loop (+ i 1) (+ len 1) start blank?)]
                [else
                 (let ([res (loop (+ i 1) 1 i (not blank?))])
                   (if blank?
                       res
                       (λ (dc x y)
                         (res dc x y)
                         (send dc draw-line (+ x start) y (+ x start (- len 1)) y))))]))]))])))


#;
(let ()
  ;; test cases for for-each/section
  (define (run-fe/s str)
    (let ([calls '()])
      ((for-each/sections str)
       (new (class object%
              (define/public (draw-line x1 y1 x2 y2)
                (set! calls (cons (list x1 x2) calls)))
              (super-new)))
       0
       0)
      calls))
  
  (printf "framework/private/text.rkt: ~s\n" 
          (list
           (equal? (run-fe/s "") '())
           (equal? (run-fe/s "a") '((0 0)))
           (equal? (run-fe/s " ") '())
           (equal? (run-fe/s "ab") '((0 1)))
           (equal? (run-fe/s "ab c") '((0 1) (3 3)))
           (equal? (run-fe/s "a bc") '((0 0) (2 3)))
           (equal? (run-fe/s "a b c d") '((0 0) (2 2) (4 4) (6 6)))
           (equal? (run-fe/s "a b c d    ") '((0 0) (2 2) (4 4) (6 6)))
           (equal? (run-fe/s "abc def ghi") '((0 2) (4 6) (8 10)))
           (equal? (run-fe/s "abc   def   ghi") '((0 2) (6 8) (12 14))))))

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
    (inherit split-snip find-snip 
             get-snip-position
             find-first-snip 
             get-style-list set-tabs)
    
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
      (when delegate
        ;; the delegate may be in a bad state because we've killed the pending todo
        ;; items; to clear out the bad state, end any edit sequences, and unhighlight
        ;; any highlighted ranges. The rest of the state is reset if the editor
        ;; is ever installed as a delegate again (by refresh-delegate)
        (let loop ()
          (when (send delegate in-edit-sequence?)
            (send delegate end-edit-sequence)
            (loop)))
        (for ([range (in-list (send delegate get-highlighted-ranges))])
          (send delegate unhighlight-range
                (range-start range)
                (range-end range)
                (range-color range)
                (range-caret-space? range)
                (range-style range))))
      
      (set! delegate _d)
      (set! linked-snips (if _d
                             (make-hasheq)
                             #f))
      (refresh-delegate))
    
    (define/private (refresh-delegate)
      (when delegate 
        (refresh-delegate/do-work)))
    
    (define/private (refresh-delegate/do-work)
      (send delegate begin-edit-sequence)
      (send delegate lock #f)
      (when (is-a? this racket:text<%>)
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
               (range-caret-space? range)
               (range-style range)))
       (send delegate get-highlighted-ranges))
      (for-each
       (λ (range)
         (send delegate highlight-range 
               (range-start range)
               (range-end range)
               (range-color range)
               (range-caret-space? range)
               'high
               (range-style range)))
       (reverse (get-highlighted-ranges)))
      (send delegate lock #t)
      (send delegate end-edit-sequence))
    
    (define/override (highlight-range start end color 
                                      [caret-space? #f] 
                                      [priority 'low]
                                      [style 'rectangle] 
                                      #:adjust-on-insert/delete? [adjust-on-insert/delete? #f]
                                      #:key [key #f])
      (when delegate 
        (send delegate highlight-range start end color caret-space? priority style
              #:adjust-on-insert/delete? adjust-on-insert/delete?
              #:key key))
      (super highlight-range start end color caret-space? priority style 
             #:adjust-on-insert/delete? adjust-on-insert/delete?
             #:key key))
    
    ;; only need to override this unhighlight-ranges, since 
    ;; all the other unhighlighting variants call this one
    (define/override (unhighlight-ranges pred [just-one? #f])
      (when delegate 
        (send delegate unhighlight-ranges pred just-one?))
      (super unhighlight-ranges pred just-one?))
    
    (inherit get-canvases get-active-canvas has-focus?)
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret?)
      (super on-paint before? dc left top right bottom dx dy draw-caret?)
      (when delegate 
        (unless before?
          (let ([active-canvas (get-active-canvas)])
            (when active-canvas
              (send (send active-canvas get-top-level-window) delegate-moved))))))
    
    (define no-delegate-edit-sequence-depth 0)
     
    (define/augment (on-edit-sequence)
      (cond
        [delegate 
         (send delegate begin-edit-sequence)]
        [else
         (set! no-delegate-edit-sequence-depth
               (+ no-delegate-edit-sequence-depth 1))])
      (inner (void) on-edit-sequence))

    (define/augment (after-edit-sequence)
      (cond
        [(and delegate 
              (= 0 no-delegate-edit-sequence-depth))
         (send delegate end-edit-sequence)]
        [else
         (set! no-delegate-edit-sequence-depth
               (- no-delegate-edit-sequence-depth 1))])
      (inner (void) after-edit-sequence))
    
    (define/override (resized snip redraw-now?)
      (super resized snip redraw-now?)
      (when (and delegate
                 (not (is-a? snip string-snip%)))
        (when linked-snips
          (let ([delegate-copy (hash-ref linked-snips snip (λ () #f))])
            (when delegate-copy
              (send delegate resized delegate-copy redraw-now?))))))
    
    (define/augment (after-insert start len)
      (when delegate 
        (send delegate begin-edit-sequence)
        (send delegate lock #f)
        (split-snip start)
        (split-snip (+ start len))
        (let loop ([snip (find-snip (+ start len) 'before-or-none)])
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
               [style (send snip get-style)])
          (send delegate change-style style start (+ start len)))
        (send delegate lock #f)
        (send delegate end-edit-sequence))
      (inner (void) after-change-style start len))
    
    (define/augment (after-load-file success?)
      (when success?
        (refresh-delegate))
      (inner (void) after-load-file success?))
    (super-new)))

(define info<%> (interface (basic<%>)))

(define info-mixin
  (mixin (editor:keymap<%> basic<%>) (info<%>)
    (inherit get-start-position get-end-position get-canvas
             run-after-edit-sequence)
    (define/private (enqueue-for-frame call-method tag)
      (run-after-edit-sequence
       (let ([from-enqueue-for-frame
              (λ ()
                (call-with-frame call-method))])
         from-enqueue-for-frame)
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
    (define/override use-file-text-mode
      (case-lambda
        [() (super use-file-text-mode)]
        [(x) (super use-file-text-mode x)
             (enqueue-for-frame
              (λ (x) (send x use-file-text-mode-changed))
              'framework:file-text-mode-changed)]))
    
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
      
    ;; all-string-snips : -> boolean
    ;; returns #t when it is safe to save this file in regular (non-WXME) mode.
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
    
    (super-new)))

(define unix-line-endings-regexp #rx"(^$)|((^|[^\r])\n)")
(unless (and (regexp-match? unix-line-endings-regexp "")
             (regexp-match? unix-line-endings-regexp "\n")
             (regexp-match? unix-line-endings-regexp "a\n")
             (not (regexp-match? unix-line-endings-regexp "\r\n"))
             (regexp-match? unix-line-endings-regexp "x\ny\r\nz\n")
             (regexp-match? unix-line-endings-regexp "\n\r\n")
             (not (regexp-match? unix-line-endings-regexp "a\r\nb\r\nc\r\n"))
             (regexp-match? unix-line-endings-regexp "a\r\nb\r\nc\n")
             (regexp-match? unix-line-endings-regexp "a\nb\r\nc\r\n"))
  (error 'framework/private/text.rkt "unix-line-endings-regexp test failure"))

(define crlf-line-endings<%> (interface ((class->interface text%))))

(define crlf-line-endings-mixin
  (mixin ((class->interface text%)) (crlf-line-endings<%>)
    (inherit get-filename use-file-text-mode)
    (define/augment (after-load-file success?)
      (when success?
        (cond
          [(preferences:get 'framework:always-use-platform-specific-linefeed-convention)
           (use-file-text-mode #t)]
          [else
           (define unix-endings?
             (with-handlers ((exn:fail:filesystem? (λ (x) #t)))
               (call-with-input-file (get-filename)
                 (λ (port)
                   (regexp-match? unix-line-endings-regexp port)))))
           (use-file-text-mode
            (and (eq? (system-type) 'windows) 
                 (not unix-endings?)))]))
      (inner (void) after-load-file success?))

    (super-new)
    
    ;; for empty files we want to use LF mode so
    ;; set it this way until a file is loaded in the editor
    (when (eq? (system-type) 'windows)
      (unless (preferences:get 'framework:always-use-platform-specific-linefeed-convention)
        (use-file-text-mode #f)))))

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
      (define filename (get-filename))
      (define can-edit?
        (if (and filename
                 (file-exists? filename))
            (and (member 'write 
                         (with-handlers ([exn:fail:filesystem? (λ (x) '())])
                           (file-or-directory-permissions filename)))
                 #t)
            #t))
      (set! read-write? can-edit?))
    
    (define/public (while-unlocked t)
      (define unlocked? 'unint)
      (dynamic-wind
       (λ () 
         (set! unlocked? read-write?)
         (set! read-write? #t))
       (λ () (t))
       (λ () (set! read-write? unlocked?))))
    
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

;; this value (4096) is also mentioned in the test suite (collects/tests/framework/test.rkt)
;; so if you change it, be sure to change things over there too
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

;; data : any
;; to-insert-chan : (or/c #f channel)
;;   if to-insert-chan is a channel, this means
;;   the eventspace handler thread is the one that
;;   is initiating the communication, so instead of
;;   queueing a callback to do the update of the editor,
;;   just send the work back directly and it will be done
;;   syncronously there. If it is #f, then we queue a callback
;;   to do the work
(define-struct data/chan (data to-insert-chan))

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
             position-location
             get-styles-fixed
             set-styles-fixed)
    
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
      (when box-input (new-box-input (send box-input get-editor)))
      (channel-put read-chan (cons eof (position->line-col-pos unread-start-point))))
    (define/public-final (send-eof-to-box-in-port) 
      (when box-input (new-box-input (send box-input get-editor)))
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
    
    (inherit set-position)
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
           (insert "\n" (last-position) (last-position))
           (do-submission)]
          [else
           (super on-local-char key)])))
    
    (define/public-final (do-submission)
      (set-position (last-position) (last-position))
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
      (on-submit))
    
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
          #;(unless (= unread-start-point (paragraph-start-position 
                                           (position-paragraph unread-start-point)))
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
    ;; output port synchronization code
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
      (let ([locked? (is-locked?)]
            [sf? (get-styles-fixed)])
        (begin-edit-sequence)
        (lock #f)
        (set-styles-fixed #f)
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
                             (let ([s (send str/snp copy)])
                               (if (is-a? s snip%)
                                   s
                                   (new snip%)))
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
        (set-styles-fixed sf?)
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
                      [text-to-insert (empty-at-queue)]
                      [last-flush (current-inexact-milliseconds)])
             
             (sync
              (if (at-queue-empty? text-to-insert)
                  never-evt
                  (handle-evt
                   (alarm-evt (+ last-flush msec-timeout))
                   (λ (_)
                     (define-values (viable-bytes remaining-queue)
                       (split-queue converter text-to-insert))
                     ;; we always queue the work here since the 
                     ;; always event means no one waits for the callback
                     (queue-insertion viable-bytes always-evt)
                     (loop remaining-queue (current-inexact-milliseconds)))))
              (handle-evt
               flush-chan
               (λ (return-evt/to-insert-chan)
                 (define-values (viable-bytes remaining-queue) (split-queue converter text-to-insert))
                 (if (channel? return-evt/to-insert-chan)
                     (channel-put return-evt/to-insert-chan viable-bytes) 
                     (queue-insertion viable-bytes return-evt/to-insert-chan))
                 (loop remaining-queue (current-inexact-milliseconds))))
              (handle-evt
               clear-output-chan
               (λ (_)
                 (loop (empty-at-queue) (current-inexact-milliseconds))))
              (handle-evt
               write-chan
               (λ (pr-pr)
                 (define return-chan (car pr-pr))
                 (define pr (cdr pr-pr))
                 (let ([new-text-to-insert (at-enqueue pr text-to-insert)])
                   (cond
                     [((at-queue-size text-to-insert) . < . output-buffer-full)
                      (when return-chan
                        (channel-put return-chan '()))
                      (loop new-text-to-insert last-flush)]
                     [else
                      (let ([chan (make-channel)])
                        (let-values ([(viable-bytes remaining-queue) 
                                      (split-queue converter new-text-to-insert)])
                          (if return-chan
                              (channel-put return-chan viable-bytes)
                              (queue-insertion viable-bytes (channel-put-evt chan (void))))
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
            [else
             (define pair (cons (subbytes to-write start end) style))
             (cond
               [(eq? (current-thread) (eventspace-handler-thread eventspace))
                (define return-channel (make-channel))
                (thread (λ () (channel-put write-chan (cons return-channel pair))))
                (do-insertion (channel-get return-channel) #f)]
               [else
                (channel-put write-chan (cons #f pair))])])
          (- end start)))
      
      (define (flush-proc)
        (cond
          [(eq? (current-thread) (eventspace-handler-thread eventspace))
           (define to-insert-channel (make-channel))
           (thread (λ () (channel-put flush-chan to-insert-channel)))
           (do-insertion (channel-get to-insert-channel) #f)]
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
          (define str/snp (cond
                            [(string? special) special]
                            [(is-a? special snip%) special]
                            [else (format "~s" special)]))
          (define to-send (cons str/snp style))
          (cond
            [(eq? (current-thread) (eventspace-handler-thread eventspace))
             (define return-chan (make-channel))
             (thread (λ () (channel-put write-chan (cons return-chan to-send))))
             (do-insertion (channel-get return-chan) #f)]
            [else
             (channel-put write-chan (cons #f to-send))])
          #t))
      
      (let* ([add-standard
              (λ (sd)
                (cond
                  [(string? sd)
                   (let ([style-list (get-style-list)])
                     (or (send style-list find-named-style sd)
                         (send style-list find-named-style "Standard")
                         (send style-list basic-style)))]
                  [sd
                   (let* ([style-list (get-style-list)] 
                          [std (send style-list find-named-style "Standard")])
                     (if std
                         (send style-list find-or-create-style std sd)
                         (let ([basic (send style-list basic-style)])
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
                 ;; instead drracket sets the global-port-print-handler
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
      (define lst (at-queue->list q))
      (let loop ([lst lst] [acc null])
        (cond
          [(null? lst)
           (values (reverse acc)
                   (empty-at-queue))]
          [else
           (define-values (front rest) (peel lst))
           (cond
             [(not front) (values (reverse acc)
                                  (empty-at-queue))]
             [(bytes? (car front))
              (define the-bytes (car front))
              (define key (cdr front))
              (cond
                [(null? rest)
                 (define-values (converted-bytes src-read-k termination)
                   (bytes-convert converter the-bytes))
                 (cond
                   [(eq? termination 'aborts)
                    (values (reverse (cons (cons (bytes->string/utf-8 converted-bytes) key) acc))
                            (at-enqueue 
                             (cons (subbytes the-bytes
                                             src-read-k
                                             (bytes-length the-bytes))
                                   key)
                             (empty-at-queue)))]
                   [else
                    (values (reverse (cons (cons (bytes->string/utf-8 converted-bytes) key) acc))
                            (empty-at-queue))])]
                [else
                 (define-values (converted-bytes src-read-k termination)
                   (bytes-convert converter the-bytes))
                 (define-values (more-bytes more-termination) (bytes-convert-end converter))
                 (loop rest
                       (cons (cons (bytes->string/utf-8 (bytes-append converted-bytes more-bytes))
                                   key)
                             acc))])]
             [else (loop rest
                         (cons front acc))])])))
    
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
       (define data (empty-at-queue)) ;; (queue (cons (union byte snip eof) line-col-pos))
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
               (set! data (at-enqueue ent data))
               (unless position
                 (set! position (cdr ent)))
               (loop)))
            (handle-evt
             clear-input-chan
             (λ (_)
               (semaphore-post peeker-sema)
               (set! peeker-sema (make-semaphore 0))
               (set! peeker-evt (semaphore-peek-evt peeker-sema))
               (set! data (empty-at-queue))
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
                  [(struct committer 
                           (kr
                            commit-peeker-evt
                            done-evt
                            resp-chan
                            resp-nack))
                   (choice-evt
                    (handle-evt 
                     commit-peeker-evt
                     (λ (_)
                       ;; this committer will be thrown out in next iteration
                       (loop)))
                    (handle-evt
                     done-evt
                     (λ (v)
                       (let ([nth-pos (cdr (at-peek-n data (- kr 1)))])
                         (set! position
                               (list (car nth-pos)
                                     (+ 1 (cadr nth-pos))
                                     (+ 1 (caddr nth-pos)))))
                       (set! data (at-dequeue-n data kr))
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
           [(struct committer
                    (kr commit-peeker-evt
                        done-evt resp-chan resp-nack))
            (let ([size (at-queue-size data)])
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
           [(struct peeker (bytes skip-count pe resp-chan nack-evt polling?))
            (cond
              [(and pe (not (eq? pe peeker-evt)))
               (choice-evt (channel-put-evt resp-chan #f)
                           nack-evt)]
              [((at-queue-size data) . > . skip-count)
               (let ([nth (car (at-peek-n data skip-count))])
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
       (define evt 
         (nack-guard-evt
          (λ (nack)
            (define chan (make-channel))
            (channel-put peek-chan (make-peeker bstr skip-count progress-evt chan nack polling?))
            chan)))
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
           evt))))
  
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
(define-struct at-queue (front back count) #:mutable)
(define (empty-at-queue) (make-at-queue '() '() 0))
(define (at-enqueue e q) (make-at-queue 
                          (cons e (at-queue-front q))
                          (at-queue-back q)
                          (+ (at-queue-count q) 1)))
(define (at-queue-first q)
  (at-flip-around q)
  (let ([back (at-queue-back q)])
    (if (null? back)
        (error 'at-queue-first "empty queue")
        (car back))))
(define (at-queue-rest q)
  (at-flip-around q)
  (let ([back (at-queue-back q)])
    (if (null? back)
        (error 'queue-rest "empty queue")
        (make-at-queue (at-queue-front q)
                       (cdr back)
                       (- (at-queue-count q) 1)))))
(define (at-flip-around q)
  (when (null? (at-queue-back q))
    (set-at-queue-back! q (reverse (at-queue-front q)))
    (set-at-queue-front! q '())))

(define (at-queue-empty? q) (zero? (at-queue-count q)))
(define (at-queue-size q) (at-queue-count q))

;; queue->list : (queue x) -> (listof x)
;; returns the elements in the order that successive deq's would have
(define (at-queue->list q) 
  (let ([ans (append (at-queue-back q) (reverse (at-queue-front q)))])
    (set-at-queue-back! q ans)
    (set-at-queue-front! q '())
    ans))

;; dequeue-n : queue number -> queue
(define (at-dequeue-n queue n)
  (let loop ([q queue]
             [n n])
    (cond
      [(zero? n) q]
      [(at-queue-empty? q) (error 'dequeue-n "not enough!")]
      [else (loop (at-queue-rest q) (- n 1))])))

;; peek-n : queue number -> queue
(define (at-peek-n queue init-n)
  (let loop ([q queue]
             [n init-n])
    (cond
      [(zero? n) 
       (when (at-queue-empty? q)
         (error 'peek-n "not enough; asked for ~a but only ~a available" 
                init-n 
                (at-queue-size queue)))
       (at-queue-first q)]
      [else 
       (when (at-queue-empty? q)
         (error 'dequeue-n "not enough!"))
       (loop (at-queue-rest q) (- n 1))])))

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
    ;; The word that ends at the current position of the editor
    (define/public (get-word-at current-pos)
      (let ([start-pos (box current-pos)]) 
        (find-wordbreak start-pos #f 'caret)
        (get-text (unbox start-pos) current-pos)))
    
    ;; String Number Number scrolling-cursor<%> -> void
    ;; Popup a menu of the given words at the location of the end-pos. Each menu item
    ;; should change the current word to the word in the list.
    (define/private (show-options word start-pos end-pos cursor)
      (let ([x (box 0)]
            [yb (box 0)]
            [yt (box 0)])
        (position-location start-pos x yb #f)
        (position-location start-pos #f yt #t)
        (set! completions-box (new completion-box%
                                   [completions (new scroll-manager% [cursor cursor])]
                                   [line-x (unbox x)]
                                   [line-y-above (unbox yt)]
                                   [line-y-below (unbox yb)]
                                   [editor this]))
        (send completions-box redraw)))
    
    (define/augment (after-set-position)
      (when completions-box
        (destroy-completions-box)
        (auto-complete))
      (inner (void) after-set-position))
    
    ;; on-char must handle inputs for two modes: normal text mode and in-the-middle-of-autocompleting
    ;; mode perhaps it would be better to handle this using the state machine pattern
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

(define scrolling-cursor<%>
  (interface (autocompletion-cursor<%>)
    items-are-hidden?
    get-visible-completions
    get-visible-length
    scroll-down
    scroll-up))

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
         (set! visible-completions (take (send cursor get-completions) (autocomplete-limit)))
         (set! visible-completions-length (autocomplete-limit))]))
    
    (define/public (get-completions) all-completions)
    (define/public (get-length) all-completions-length)
    (define/public (empty?) (send cursor empty?))
    
    (define/public (get-visible-length) visible-completions-length)
    (define/public (get-visible-completions) visible-completions)
    
    (define/public (items-are-hidden?) hidden?)
    
    (define/public (scroll-down)
      (when hidden?
        (set! all-completions (append (drop all-completions (autocomplete-limit))
                                      visible-completions))
        (set! visible-completions (take all-completions (autocomplete-limit)))))
    
    (define/public (scroll-up)
      (when hidden?
        (let ([n (- all-completions-length (autocomplete-limit))])
          (set! all-completions (append (drop all-completions n) (take all-completions n)))
          (set! visible-completions (take all-completions (autocomplete-limit))))))
    
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
    
    (init-field completions       ; scroll-manager%      
                ;                   the possible completions (all of which have base-word as a prefix)
                line-x            ; int                  
                ;                   the x coordinate of the line where the menu goes
                line-y-above      ; int                   
                ;                   the y coordinate of the top of the line where the menu goes
                line-y-below      ; int                  
                ;                   the y coordinate of the bottom of the line where the menu goes
                editor            ; editor<%>             
                ;                   the owner of this completion box
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
                                                 (get-mt-font))])
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
                                 [(tw th _1 _2) (send dc get-text-extent
                                                      hidden-completions-text
                                                      (get-reg-font))])
                      (let ([w (if hidden? (max tw w) w)]
                            [h (if hidden? (+ th h) h)])
                        (initialize-mouse-offset-map! coord-map)
                        (let ([offset-h menu-padding-y]
                              [offset-w (* menu-padding-x 2)])
                          (values (+ offset-w w)
                                  (+ offset-h h)))))]
                   [else 
                    (let ([c (car pc)])
                      (let-values ([(tw th _1 _2) (send dc get-text-extent c (get-reg-font))])
                        (loop (cdr pc)
                              (max tw w)
                              (+ th h)
                              (cons (list (inexact->exact h) (inexact->exact (+ h th)) n) coord-map)
                              (add1 n))))]))])))
        
        (let ([final-x (cond
                         [(< (+ line-x w) editor-width)
                          line-x]
                         [(> editor-width w)
                          (- editor-width w)]
                         [else line-x])]
              [final-y (cond
                         [(< (+ line-y-below 2 h) editor-height)
                          (+ line-y-below 2)]
                         [(> (- line-y-above h) 0)
                          (- line-y-above h)]
                         [else
                          (+ line-y-below 2)])])
          
          (make-geometry final-x final-y w h vec))))
    
    ;; geometry records the menu's current width and height and
    ;; a vector associating mouse location with a selected item
    (define geometry (compute-geometry))
    
    (define highlighted-menu-item 0) ; the currently-highlighted menu item
    
    ;; draw : dc<%> int int -> void
    ;; draws the menu to the given drawing context at offset dx, dy
    (define/public (draw dc dx dy)
      (let ([old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)]
            [font (send dc get-font)])
        (define-values (mx my tw th) (get-menu-coordinates))
        (send dc set-pen (send editor get-autocomplete-border-color) 1 'solid)
        (send dc set-brush (send editor get-autocomplete-background-color) 'solid)
        (send dc draw-rectangle (+ mx dx) (+ my dy) tw th)
        
        (cond
          [(send completions empty?)
           (let ([font (send dc get-font)])
             (send dc set-font (get-mt-font))
             (send dc draw-text 
                   (string-constant no-completions) 
                   (+ mx dx menu-padding-x)
                   (+ menu-padding-y my dy))
             (send dc set-font font))]
          [else
           (send dc set-font (get-reg-font))
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
                    (loop (add1 item-number) (+ y h) (cdr pc))))]))])
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-font font)))
    
    (define/private (get-mt-font)
      (send the-font-list find-or-create-font
            (editor:get-current-preferred-font-size)
            'default
            'italic
            'normal))
    
    (define/private (get-reg-font)
      (send the-font-list find-or-create-font
            (editor:get-current-preferred-font-size)
            'default
            'normal
            'normal))
    
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
         (set! highlighted-menu-item (modulo (add1 highlighted-menu-item)
                                             (send completions get-visible-length)))
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
         (set! highlighted-menu-item (modulo (sub1 highlighted-menu-item)
                                             (send completions get-visible-length)))
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
      (define-values (mx my w h) (get-menu-coordinates))
      (define index (floor (inexact->exact (- y my))))
      (when (and (<= mx x (+ mx w))
                 (< menu-padding-y
                    index
                    (vector-length (geometry-mouse->menu-item-vector geometry))))
        (set! highlighted-menu-item (vector-ref (geometry-mouse->menu-item-vector geometry)
                                                index))
        (redraw)))
    
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
      (let ([load-collections-xref
             ;; Make the dependency on `setup/xref' indirect, so that a
             ;; GUI does not depend on having documentation installed:
             (with-handlers ([exn:missing-module? (lambda (exn)
                                                         (lambda ()
                                                           (load-xref null)))])
               (dynamic-require 'setup/xref 'load-collections-xref))])
        (set! xref (load-collections-xref))))
    
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
      (sort (hash-map ht (λ (x y) x)) string<?))))

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

;; ============================================================
;; line number text%

(define line-numbers<%>
  (interface ()
             show-line-numbers!
             showing-line-numbers?
             set-line-numbers-color))

(define-local-member-name do-draw-single-line draw-separator)

;; draws line numbers on the left hand side of a text% object
(define line-numbers-mixin
  (mixin ((class->interface text%) editor:standard-style-list<%>) (line-numbers<%>)
    (inherit begin-edit-sequence
             end-edit-sequence
             in-edit-sequence?
             get-visible-line-range
             get-visible-position-range
             last-line
             line-location
             line-paragraph
             line-start-position
             line-end-position
             get-view-size
             set-padding
             get-padding
             get-start-position
             get-end-position
             position-paragraph
             position-line
             position-location
             paragraph-start-position
             invalidate-bitmap-cache
             get-dc)

    (init-field [line-numbers-color #f])
    (init-field [show-line-numbers? #t])
    ;; whether the numbers are aligned on the left or right
    ;; only two values should be 'left or 'right
    (init-field [alignment 'right])

    (define need-to-setup-padding? #f)
    
    (define/private (number-space)
      (number->string (max (* 10 (add1 (last-line))) 100)))
    ;; add an extra 0 so it looks nice
    (define/private (number-space+1) (string-append (number-space) "0"))

    (define/private (setup-padding)
      (cond
        [(showing-line-numbers?)
         (send padding-dc set-font (get-style-font))
         (define-values (padding-left padding-top padding-right padding-bottom) (get-padding))
         (define new-padding (text-width padding-dc (number-space+1)))
         (set-padding new-padding 0 0 0)
         (unless (= padding-left new-padding)
           (invalidate-bitmap-cache))]
        [else 
         (set-padding 0 0 0 0)]))
    
    ;; call this method with #t or #f to turn on/off line numbers
    (define/public (show-line-numbers! what)
      (set! show-line-numbers? what)
      (setup-padding))

    (define/public (showing-line-numbers?)
      show-line-numbers?)

    (define/public (set-line-numbers-color color)
      (set! line-numbers-color color))

    (define notify-registered-in-list #f)

    (define style-change-notify
      (lambda (style) (unless style (setup-padding))))

    (define/private (get-style)
      (let* ([style-list (editor:get-standard-style-list)]
             [std (or (send style-list
                            find-named-style
                            (editor:get-default-color-style-name))
                      (send style-list find-named-style "Standard")
                      (send style-list basic-style))])
        ;; If the style changes, we should re-check the width of
        ;; drawn line numbers:
        (unless (eq? notify-registered-in-list style-list)
          ;; `notify-on-change' holds the given function weakly:
          (send style-list notify-on-change style-change-notify)
          ;; Avoid registering multiple notifications:
          (set! notify-registered-in-list style-list))
        std))

    (define/private (get-style-foreground)
      (send (get-style) get-foreground))

    (define/private (get-style-font)
      (send (get-style) get-font))

    (define/private (save-dc-state dc)
      (saved-dc-state (send dc get-smoothing)
                      (send dc get-pen)
                      (send dc get-brush)
                      (send dc get-font)
                      (send dc get-text-foreground)))

    (define/private (restore-dc-state dc dc-state)
      (send dc set-smoothing (saved-dc-state-smoothing dc-state))
      (send dc set-pen (saved-dc-state-pen dc-state))
      (send dc set-brush (saved-dc-state-brush dc-state))
      (send dc set-font (saved-dc-state-font dc-state))
      (send dc set-text-foreground (saved-dc-state-foreground-color dc-state)))

    (define/private (get-foreground)
      (if line-numbers-color
        (make-object color% line-numbers-color)
        (get-style-foreground)))
        
    ;; set the dc stuff to values we want
    (define/private (setup-dc dc)
      (send dc set-smoothing 'aligned)
      (send dc set-font (get-style-font))
      (send dc set-text-foreground (get-foreground)))

    (define/private (lighter-color color)
      (define (integer number)
        (inexact->exact (round number)))
      ;; hue 0-360
      ;; saturation 0-1
      ;; lightness 0-1
      ;; returns rgb as float values with ranges 0-1
      (define (hsl->rgb hue saturation lightness)
        (define (helper x a b)
          (define x* (cond
                       [(< x 0) (+ x 1)]
                       [(> x 1) (- x 1)]
                       [else x]))
          (cond
            [(< (* x 6) 1) (+ b (* 6 (- a b) x))]
            [(< (* x 6) 3) a]
            [(< (* x 6) 4) (+ b (* (- a b) (- 4 (* 6 x))))]
            [else b]))

        (define h (/ hue 360))
        (define a (if (< lightness 0.5)
                    (+ lightness (* lightness saturation))
                    (- (+ lightness saturation) (* lightness saturation))))
        (define b (- (* lightness 2) a))
        (define red (helper (+ h (/ 1.0 3)) a b))
        (define green (helper h a b))
        (define blue (helper (- h (/ 1.0 3)) a b))
        (values red green blue))
        
      ;; red 0-255
      ;; green 0-255
      ;; blue 0-255
      (define (rgb->hsl red green blue)
        (define-values (a b c d)
                       (if (> red green)
                         (if (> red blue)
                           (if (> green blue)
                             (values red (- green blue) blue 0)
                             (values red (- green blue) green 0))
                           (values blue (- red green) green 4))
                         (if (> red blue)
                           (values green (- blue red) blue 2)
                           (if (> green blue)
                             (values green (- blue red) red 2)
                             (values blue (- red green) red 4)))))
        (define hue (if (= a c) 0
                      (let ([x (* 60 (+ d (/ b (- a c))))])
                        (if (< x 0) (+ x 360) x))))
        (define saturation (cond
                             [(= a c) 0]
                             [(< (+ a c) 1) (/ (- a c) (+ a c))]
                             [else (/ (- a c) (- 2 a c))]))
        (define lightness (/ (+ a c) 2))
        (values hue saturation lightness))
      (define-values (hue saturation lightness)
                     (rgb->hsl (send color red)
                               (send color green)
                               (send color blue)))
      (define-values (red green blue)
                     (hsl->rgb hue saturation (+ 0.5 lightness)))
      (make-object color% (min 255 (integer (* 255 red)))
                          (min 255 (integer (* 255 green)))
                          (min 255 (integer (* 255 blue)))))

    ;; adjust space so that we are always at the left-most position where
    ;; drawing looks right
    (define/private (left-space dc dx)
      (define left (box 0))
      (define top (box 0))
      (define width (box 0))
      (define height (box 0))
      (send (send this get-admin) get-view left top width height)
      (+ (unbox left) dx))

    (define/augment (after-insert start length)
      (inner (void) after-insert start length)
      ; in case the max line number changed:
      (if (in-edit-sequence?)
          (set! need-to-setup-padding? #t)
          (setup-padding)))

    (define/augment (after-delete start length)
      (inner (void) after-delete start length)
      ; in case the max line number changed:
      (if (in-edit-sequence?)
          (set! need-to-setup-padding? #t)
          (setup-padding)))

    (define/augment (after-edit-sequence)
      (when need-to-setup-padding?
        (set! need-to-setup-padding? #f)
        (setup-padding))
      (inner (void) after-edit-sequence))
    
    (define/private (draw-numbers dc left top right bottom dx dy start-line end-line)
      (unless (left . > . (line-x-coordinate dc dx))
        (define last-paragraph #f)
        (define insertion-para
          (let ([sp (get-start-position)])
            (if (= sp (get-end-position))
                (position-paragraph sp)
                #f)))
        (for ([line (in-range start-line end-line)])
          (define y (line-location line))
          (define yb (line-location line #f))
          (define this-paragraph (line-paragraph line))
          (when (and (y . <= . bottom) (yb . >= . top))
            (do-draw-single-line dc dx dy line y last-paragraph 
                                 (and insertion-para
                                      (= insertion-para this-paragraph))))
          (set! last-paragraph this-paragraph))))
    
    (define/public (do-draw-single-line dc dx dy line y last-paragraph is-insertion-line?)
      (define single-space (text-width dc "0"))
      (define-values (single-w single-h _1 _2) (send dc get-text-extent "0"))
      (define view (number->string (add1 (line-paragraph line))))
      (define ls (left-space dc dx))
      (define right-space (text-width dc (number-space)))
      (define final-x
        (+ ls
           (case alignment
             [(left) 0]
             [(right) (- right-space (text-width dc view) single-space)]
             [else 0])))
      (define final-y (+ dy y))
      (cond
        [is-insertion-line?
         (send dc set-pen "black" 1 'transparent)
         (send dc set-brush 
               (if (get-highlight-text-color)
                   (get-highlight-background-color)
                   (if (preferences:get 'framework:white-on-black?)
                       "lime"
                       "forestgreen"))               
               'solid)
         
         (send dc draw-rectangle ls final-y (- right-space single-w) single-h)
         (send dc draw-arc 
               (- (+ ls (- right-space single-w)) single-w) final-y
               (* 2 single-w) single-h
               (* pi 3/2) (* pi 1/2))
         
         (define text-fg (send dc get-text-foreground))
         (send dc set-text-foreground (if (get-highlight-text-color)
                                          (send dc get-text-foreground)
                                          (if (preferences:get 'framework:white-on-black?)
                                              "black"
                                              "white")))
         (send dc draw-text view final-x final-y)
         (send dc set-text-foreground text-fg)]
        [(and last-paragraph (= last-paragraph (line-paragraph line)))
         (send dc set-text-foreground (lighter-color (send dc get-text-foreground)))
         (send dc draw-text view final-x final-y)
         (send dc set-text-foreground (get-foreground))]
        [else
         (send dc draw-text view final-x final-y)]))

    ;; draw the line between the line numbers and the actual text
    (define/public (draw-separator dc top bottom dx dy)
      (define line-x (line-x-coordinate dc dx))
      (define line-y1 (+ dy top))
      (define line-y2 (+ dy bottom))
      (send dc set-pen (get-foreground) 1 'solid)
      (send dc draw-line line-x line-y1
                         line-x line-y2))
    
    (define/private (line-x-coordinate dc dx)
      (define x (text-width dc (number-space)))
      (+ (left-space dc dx) x))

    ;; `line-numbers-space' will get mutated in the `on-paint' method
    ;; (define line-numbers-space 0)

    (define/private (draw-line-numbers dc left top right bottom dx dy)
      (define saved-dc (save-dc-state dc))
      (setup-dc dc)
      (define start-line (box 0))
      (define end-line (box 0))
      (get-visible-line-range start-line end-line #f)

      (draw-numbers dc left top right bottom dx dy (unbox start-line) (add1 (unbox end-line)))
      (draw-separator dc top bottom dx dy)
      (restore-dc-state dc saved-dc))

    (define/private (text-width dc stuff)
      (define-values (font-width font-height baseline space)
                     (send dc get-text-extent stuff))
      font-width)

    (define/private (text-height dc stuff)
      (define-values (font-width height baseline space)
                     (send dc get-text-extent stuff))
      height)

    (define old-clipping #f)
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when show-line-numbers?
        (cond
          [before?
           (define left-most (left-space dc dx))
           (set! old-clipping (send dc get-clipping-region))
           (define saved-dc (save-dc-state dc))
           (setup-dc dc)
           (define clipped (make-object region% dc))
           (define copy (make-object region% dc))
           (if old-clipping
               (send copy union old-clipping)
               (let ([all (make-object region% dc)])
                 (send all set-rectangle
                       (+ dx left) (+ dy top)
                       (- right left) (- bottom top))
                 (send copy union all)))
           (send clipped set-rectangle
                 0 (+ dy top)
                 (text-width dc (number-space+1))
                 (- bottom top))
           (restore-dc-state dc saved-dc)
           (send copy subtract clipped)
           (send dc set-clipping-region copy)]
          [else
           (send dc set-clipping-region old-clipping)
           (draw-line-numbers dc left top right bottom dx dy)]))
      (super on-paint before? dc left top right bottom dx dy draw-caret))

    (define old-position #f)
    (define/augment (after-set-position)
      (cond
        [(and old-position
              (= (get-start-position)
                 (get-end-position))
              (= (position-line old-position)
                 (position-line (get-start-position))))
         ;; when the line stays the same, don't invalidate anything
         (set! old-position (get-start-position))]
        [else
         (define old-position-before old-position)
         (set! old-position (and (= (get-start-position)
                                    (get-end-position))
                                 (get-start-position)))
         (define single-edit-sequence?
           (and old-position-before 
                old-position
                (<= (abs (- (position-paragraph old-position-before)
                            (position-paragraph old-position)))
                    1)))
         (when single-edit-sequence? (begin-edit-sequence #f #f))
         (when old-position-before (invalidate-at-position old-position-before))
         (when old-position (invalidate-at-position old-position))
         (when single-edit-sequence? (end-edit-sequence))])
      (inner (void) after-set-position))
    
    (define/private (invalidate-at-position pos)
      (when (showing-line-numbers?)
        (define dc (get-dc))
        (when dc
          (begin-edit-sequence #f #f)
          (define bx (box 0))
          (define by (box 0))
          (define tw (text-width dc (number-space+1)))
          (define th (text-height dc "0"))
          (define start-para (position-paragraph pos))
          (define start-line (position-line (paragraph-start-position start-para)))
          (let loop ([line start-line])
            (define para (position-paragraph (line-start-position line)))
            (when (= start-para para)
              (position-location (line-start-position line) bx by)
              (invalidate-bitmap-cache (- (unbox bx) tw)
                                       (unbox by) 
                                       tw
                                       th)
              (unless (= line (last-line))
                (loop (+ line 1)))))
          (end-edit-sequence))))
    
    (super-new)
    (setup-padding)))

(define-struct saved-dc-state (smoothing pen brush font foreground-color))
(define padding-dc (new bitmap-dc% [bitmap (make-screen-bitmap 1 1)]))

(define basic% (basic-mixin (editor:basic-mixin text%)))
(define line-spacing% (line-spacing-mixin basic%))
(define hide-caret/selection% (hide-caret/selection-mixin line-spacing%))
(define nbsp->space% (nbsp->space-mixin line-spacing%))
(define normalize-paste% (normalize-paste-mixin line-spacing%))
(define delegate% (delegate-mixin line-spacing%))
(define wide-snip% (wide-snip-mixin line-spacing%))
(define standard-style-list% (editor:standard-style-list-mixin wide-snip%))
(define input-box% (input-box-mixin standard-style-list%))
(define -keymap% (editor:keymap-mixin standard-style-list%))
(define return% (return-mixin -keymap%))
(define autowrap% (editor:autowrap-mixin -keymap%))
(define file% (file-mixin (editor:file-mixin autowrap%)))
(define clever-file-format% (crlf-line-endings-mixin (clever-file-format-mixin file%)))
(define backup-autosave% (editor:backup-autosave-mixin clever-file-format%))
(define searching% (searching-mixin backup-autosave%))
(define info% (info-mixin (editor:info-mixin searching%)))

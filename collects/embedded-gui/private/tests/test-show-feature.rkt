#| This tests to make sure show works. It's not a correct test because
   it's been abandoned and was reduced to wont-shrink.rkt This is still
   a good file for sandboxing show stuff though.
|#

(require
 mzlib/class
 mred
 mzlib/etc
 mzlib/list
 mzlib/match
 (prefix a: "../alignment.rkt")
   
 "../snip-lib.rkt"
 "../interface.rkt"
 "../alignment-helpers.rkt"
 mrlib/click-forwarding-editor
 "../on-show-pasteboard.rkt"
 "../really-resized-pasteboard.rkt"
 "../interface.rkt"
 "../snip-lib.rkt"
 "../locked-pasteboard.rkt"
 "../verthoriz-alignment.rkt"
 "../suppress-modify-editor.rkt")
  
(require mike-lib/print-debug)
  
(define aligned-pasteboard%
  (class (click-forwarding-editor-mixin
          (on-show-pasteboard-mixin
           (suppress-modify-editor-mixin
            (locked-pasteboard-mixin
             (really-resized-pasteboard-mixin pasteboard%)))))
    
    (inherit begin-edit-sequence end-edit-sequence
             get-max-view-size refresh-delayed?)
    (init align)
    (field
     [alignment (new (case align
                       [(horizontal) horizontal-alignment%]
                       [(vertical) vertical-alignment%]))]
     [lock-alignment? false]
     [needs-alignment? false])
    
    (define/public (get-alignment) alignment)
    
    #|
     snip : snip% object
     before : snip% object or #f
     x : real number
     y : real number
    |#
    (rename [super-after-insert after-insert])
    (define/override (after-insert snip before x y)
      (super-after-insert snip before x y)
      (realign))
    
    #|
      snip : snip% object
    |#
    (rename [super-after-delete after-delete])
    (define/override (after-delete snip)
      (super-after-delete snip)
      (realign))
    
    #|
      snip : snip% object
    |#
    (rename [super-really-resized really-resized])
    (define/override (really-resized snip)
      (super-really-resized snip)
      (realign))
    
    (rename [super-on-show on-show])
    (define/override (on-show)
      (realign)
      (super-on-show))
    
    (define/public (lock-alignment lock?)
      (set! lock-alignment? lock?)
      (when (and needs-alignment? (not lock-alignment?))
        (realign))
      (if lock?
          (begin-edit-sequence)
          (end-edit-sequence)))
    
    (define/public (realign)
      (if lock-alignment?
          (set! needs-alignment? true)
          (fluid-let ([lock-alignment? true])
            (send alignment set-min-sizes)
            (let ([width (send alignment get-min-width)]
                  [height (send alignment get-min-height)])
              (unless (or (zero? width) (zero? height))
                (send alignment align 0 0 width height)
                (set! needs-alignment? false))))))
    
    (super-new)
    (send alignment set-pasteboard this)))
  
(define (vert/horiz-alignment type)
  (class* object% (alignment<%>)
    
    (init-field
     [parent false]
     [show? true])
    
    (field
     [pasteboard false]
     [children empty]
     [min-width 0]
     [min-height 0])
      
    ;; STATUS: This function (through lock-alignment false) invokes a call
    ;; to realign of the pasteboard even when this alignement has show? = false
    ;; so the call is not needed.
    (define/public (add child)
      (set! children (append children (list child)))
      (send pasteboard lock-alignment true)
      (cond
        [(is-a? child snip%)
         (when (get-show?)
           (send pasteboard insert child false))]
        [(is-a? child alignment<%>)
         (send child set-pasteboard pasteboard)])
      (send pasteboard lock-alignment false))
    
    (define/public (get-min-width)
      (if (get-show?) min-width 0))
    (define/public (get-min-height)
      (if (get-show?) min-height 0))
    (define/public (set-pasteboard pb) (set! pasteboard pb))
    (define/public (stretchable-width?) true)
    (define/public (stretchable-height?) true)
    
    #;(boolean? . -> . void?)
    ;; Shows or hides the alignment
    (define/public (show bool)
      (set! show? bool)
      (when (parent-show?)
        (send pasteboard lock-alignment true)
        (show/hide-snips show?)
        (send pasteboard lock-alignment false)))
    
    #;(boolean? . -> . void?)
    ;; Inserts or deletes all the snips in the tree.
    (define/public (show/hide-snips bool)
      (when (boolean=? show? bool)
        (for-each (show/hide-child bool) children)))
    
    (define ((show/hide-child show?) child)
      (if (is-a? child alignment<%>)
          (send child show/hide-snips show?)
          (if show?
              (send pasteboard insert child)
              (send pasteboard release-snip child))))
    
    (define/public (get-show?)
      (and show? (parent-show?)))
    
    (define (parent-show?)
      (if (and parent (is-a? parent alignment<%>))
          (send parent get-show?)
          true))
    
    (define/public (align x-offset y-offset width height)
      
      (define move/resize
        (match-lambda*
          [(child ($ a:rect
                     ($ a:dim x w stretchable-width?)
                     ($ a:dim y h stretchable-height?)))
           (let ([global-x (+ x x-offset)]
                 [global-y (+ y y-offset)])
             (cond
               [(is-a? child snip%)
                (send pasteboard move-to child global-x global-y)
                (when (or stretchable-width? stretchable-height?)
                      (send child stretch w h))]
               [(is-a? child alignment<%>)
                (send child align global-x global-y w h)]))]))
      
      (when (and (get-show?) (not (empty? children)))
        (for-each move/resize
                  children
                  (a:align type width height
                           (map build-rect children)))))
    
    (define/public (set-min-sizes)
      (when show?
        (for-each
         (lambda (child)
           (when (is-a? child alignment<%>)
             (send child set-min-sizes)))
         children)
        (let-values ([(x-accum y-accum)
                      (if (symbol=? type 'vertical)
                          (values vacuous-max +)
                          (values + vacuous-max))])
          (set! min-width
                (apply x-accum
                       (map child-width
                            children)))
          (set! min-height
                (apply y-accum
                       (map child-height
                            children))))))
    
    (super-new)
    ;; NOTE: Try to figure out how it's getting a nonalignment<%> parent
    (when (and parent (is-a? parent alignment<%>))
      (send parent add this))))

(define vertical-alignment% (vert/horiz-alignment 'vertical))
(define horizontal-alignment% (vert/horiz-alignment 'horizontal))

;; build-rect ((is-a?/c snip%) . -> . rect?)
;; makes a new default rect out of a snip
(define (build-rect item)
  (cond
    [(is-a? item snip%)
     (a:make-rect
      (a:make-dim 0 (snip-min-width item) (stretchable-width? item))
      (a:make-dim 0 (snip-min-height item) (stretchable-height? item)))]
    [(is-a? item alignment<%>)
     (a:make-rect
      (a:make-dim 0 (send item get-min-width) (send item stretchable-width?))
      (a:make-dim 0 (send item get-min-height) (send item stretchable-height?)))]))

;;;;;;;;;;
;; main

(define f (new frame% (label "f") (width 400) (height 400)))
(define e (new text%))
(define c (new editor-canvas% (parent f) (editor e)))
(define ap (new aligned-pasteboard% (align 'horizontal)))
(define es (new editor-snip% (editor ap)))
(define a1 (send ap get-alignment))

(define a2 (new vertical-alignment% (parent a1)))
(define a3 (new vertical-alignment% (parent a1)))
(define a4 (new vertical-alignment% (parent a3)))
(define a5 (new vertical-alignment% (parent a3) (show? #f)))

(send a2 add (make-object string-snip% "a2"))
(send a4 add (make-object string-snip% "a4"))
(send a5 add (make-object string-snip% "a5"))

;; this next line blows up but should not
#;(send a5 show #t)
  
(send f show #t)
(send e insert es)

(let ([a (send a1 get-min-height)])
  (send a5 show #t)
  (let ([b (send a1 get-min-height)])
    (send a5 show #f)
    (let ([c (send a1 get-min-height)])
      (values (equal? a c)
              (equal? a (/ b 2))))))

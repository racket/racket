#lang racket/base
#|

Check Syntax separates two classes of identifiers,
those bound in this file and those bound by require,
and uses identifier-binding and identifier-transformer-binding
to distinguish them. 

Variables come from 'origin, 'disappeared-use, and 'disappeared-binding 
syntax properties, as well as from variable references and binding (letrec-values,
let-values, define-values) in the fully expanded text.

Variables inside #%top (not inside a module) are treated specially. 
If the namespace has a binding for them, they are colored bound color.
If the namespace does not, they are colored the unbound color.

|#


(require string-constants
         racket/unit
         racket/contract
         racket/class
         racket/list
         racket/promise
         racket/pretty
         drracket/tool
         syntax/toplevel
         syntax/boundmap
         mrlib/switchable-button
         (prefix-in drracket:arrow: drracket/arrow)
         (prefix-in fw: framework/framework)
         mred
         framework
         setup/xref
         scribble/xref
         scribble/manual-struct
         net/url
         net/uri-codec
         browser/external
         (for-syntax racket/base)
         "syncheck-drracket-button.rkt")
(provide tool@)

(define o (current-output-port))

(define status-init (string-constant cs-status-init))
(define status-coloring-program (string-constant cs-status-coloring-program))
(define status-eval-compile-time (string-constant cs-status-eval-compile-time))
(define status-expanding-expression (string-constant cs-status-expanding-expression))
(define status-loading-docs-index (string-constant cs-status-loading-docs-index))

(define jump-to-next-bound-occurrence (string-constant cs-jump-to-next-bound-occurrence))
(define jump-to-binding (string-constant cs-jump-to-binding))
(define jump-to-definition (string-constant cs-jump-to-definition))

(define cs-my-obligation-color "my obligations")
(define cs-their-obligation-color "my assumptions")
(define cs-unk-obligation-color "unknown obligations")
(define cs-mode-menu-show-contract "Show Contract Obligations")
(define cs-mode-menu-show-syntax "Show Syntactic Categories")

(define-local-member-name
  syncheck:init-arrows
  syncheck:clear-arrows
  syncheck:add-menu
  syncheck:add-arrow
  syncheck:add-tail-arrow
  syncheck:add-mouse-over-status
  syncheck:add-jump-to-definition
  syncheck:sort-bindings-table
  syncheck:jump-to-next-bound-occurrence
  syncheck:jump-to-binding-occurrence
  syncheck:jump-to-definition
  
  syncheck:clear-highlighting
  syncheck:apply-style/remember
  ;syncheck:error-report-visible? ;; test suite uses this one.
  ;syncheck:get-bindings-table    ;; test suite uses this one.
  syncheck:clear-error-message
  
  hide-error-report
  get-error-report-text
  get-error-report-visible?
  
  turn-off-error-report
  turn-on-error-report
  
  update-button-visibility/settings
  
  set-syncheck-mode
  get-syncheck-mode
  update-menu-item-label)

(define tool@ 
  (unit 
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    ;; use this to communicate the frame being
    ;; syntax checked w/out having to add new
    ;; parameters to all of the functions
    (define currently-processing-definitions-text (make-parameter #f))
    
    (define (phase1) 
      (drracket:module-language-tools:add-opt-out-toolbar-button
       (λ (frame parent)
         (new switchable-button%
              (label (string-constant check-syntax))
              (bitmap syncheck-bitmap)
              (parent parent)
              (callback (λ (button) (send frame syncheck:button-callback)))))
       'drracket:syncheck)
      (drracket:unit:add-to-program-editor-mixin clearing-text-mixin))
    (define (phase2) (void))
    
    (define (printf . args) (apply fprintf o args))
    
    
    (define xref (if (getenv "PLTDRXREFDELAY")
                     (begin
                       (printf "PLTDRXREFDELAY: using plain delay\n")
                       (delay (begin
                                (printf "PLTDRXREFDELAY: loading xref\n")
                                (begin0
                                  (load-collections-xref)
                                  (printf "PLTDRXREFDELAY: loaded xref\n")))))
                     (delay/idle (load-collections-xref))))
    (define (get-xref) (force xref))
    
    
    ;;;  ;;; ;;; ;;;;; 
    ;   ;  ;   ;    ;   
    ;   ;  ;   ;    ;   
    ;      ;   ;    ;   
    ;  ;;  ;   ;    ;   
    ;   ;  ;   ;    ;   
    ;   ;  ;; ;;    ;   
    ;;;    ;;;   ;;;;; 
    
    
    ;; used for quicker debugging of the preference panel
    '(define test-preference-panel
       (λ (name f)
         (let ([frame (make-object frame% name)])
           (f frame)
           (send frame show #t))))
    
    (define-struct graphic (pos* locs->thunks draw-fn click-fn))
    
    (define-struct arrow (start-x start-y end-x end-y) #:mutable)
    (define-struct (var-arrow arrow)
      (start-text start-pos-left start-pos-right
                  end-text end-pos-left end-pos-right
                  actual? level)) ;; level is one of 'lexical, 'top-level, 'import
    (define-struct (tail-arrow arrow) (from-text from-pos to-text to-pos))
    
    ;; color : string
    ;; text: text:basic<%>
    ;; start, fin: number
    ;; used to represent regions to highlight when passing the mouse over the syncheck window
    (define-struct colored-region (color text start fin))
    
    ;; id : symbol  --  the nominal-source-id from identifier-binding
    ;; filename : path
    (define-struct def-link (id filename) #:inspector (make-inspector))
    
    (define tacked-var-brush (send the-brush-list find-or-create-brush "BLUE" 'solid))
    (define var-pen (send the-pen-list find-or-create-pen "BLUE" 1 'solid))
    
    (define templ-color (send the-color-database find-color "purple"))
    (define templ-pen (send the-pen-list find-or-create-pen templ-color 1 'solid))
    (define tacked-templ-brush (send the-brush-list find-or-create-brush templ-color 'solid))
    
    (define tail-pen (send the-pen-list find-or-create-pen "orchid" 1 'solid))
    (define tacked-tail-brush (send the-brush-list find-or-create-brush "orchid" 'solid))
    (define untacked-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
    
    (define syncheck-text<%>
      (interface ()
        syncheck:init-arrows
        syncheck:clear-arrows
        syncheck:add-menu
        syncheck:add-arrow
        syncheck:add-tail-arrow
        syncheck:add-mouse-over-status
        syncheck:add-jump-to-definition
        syncheck:sort-bindings-table
        syncheck:get-bindings-table
        syncheck:jump-to-next-bound-occurrence
        syncheck:jump-to-binding-occurrence
        syncheck:jump-to-definition))
    
    ;; clearing-text-mixin : (mixin text%)
    ;; overrides methods that make sure the arrows go away appropriately.
    ;; adds a begin/end-edit-sequence to the insertion and deletion
    ;;  to ensure that the on-change method isn't called until after
    ;;  the arrows are cleared.
    (define clearing-text-mixin
      (mixin ((class->interface text%)) ()
        
        (inherit begin-edit-sequence end-edit-sequence)
        (define/augment (on-delete start len)
          (begin-edit-sequence)
          (inner (void) on-delete start len))
        (define/augment (after-delete start len)
          (inner (void) after-delete start len)
          (clean-up)
          (end-edit-sequence))
        
        (define/augment (on-insert start len)
          (begin-edit-sequence)
          (inner (void) on-insert start len))
        (define/augment (after-insert start len)
          (inner (void) after-insert start len)
          (clean-up)
          (end-edit-sequence))
        
        (define/private (clean-up)
          (let ([st (find-syncheck-text this)])
            (when (and st
                       (is-a? st drracket:unit:definitions-text<%>))
              (let ([tab (send st get-tab)])
                (send tab syncheck:clear-error-message)
                (send tab syncheck:clear-highlighting)))))
        
        (super-new)))
    
    (define make-syncheck-text%
      (λ (super%)
        (let* ([cursor-arrow (make-object cursor% 'arrow)])
          (class* super% (syncheck-text<%>)
            (inherit set-cursor get-admin invalidate-bitmap-cache set-position
                     get-pos/text position-location
                     get-canvas last-position dc-location-to-editor-location
                     find-position begin-edit-sequence end-edit-sequence
                     highlight-range unhighlight-range
                     paragraph-end-position first-line-currently-drawn-specially?)
            
            
            
            ;; arrow-vectors : 
            ;; (union 
            ;;  #f
            ;;  (hash-table
            ;;    (text%
            ;;     . -o> .
            ;;    (vector (listof (union (cons (union #f sym) (menu -> void))
            ;;                           def-link
            ;;                           tail-link
            ;;                           arrow
            ;;                           string))))))
            (define arrow-vectors #f)
            
            ;; cleanup-texts : (or/c #f (listof text))
            (define cleanup-texts #f)
            (define style-mapping #f)
            
            ;; bindings-table : hash-table[(list text number number) -o> (listof (list text number number))]
            ;; this is a private field
            (define bindings-table (make-hash))
            
            ;; add-to-bindings-table : text number number text number number -> boolean
            ;; results indicates if the binding was added to the table. It is added, unless
            ;;  1) it is already there, or
            ;;  2) it is a link to itself
            (define/private (add-to-bindings-table start-text start-left start-right
                                                   end-text end-left end-right)
              (cond
                [(and (object=? start-text end-text)
                      (= start-left end-left)
                      (= start-right end-right))
                 #f]
                [else
                 (let* ([key (list start-text start-left start-right)]
                        [priors (hash-ref bindings-table key (λ () '()))]
                        [new (list end-text end-left end-right)])
                   (cond
                     [(member new priors)
                      #f]
                     [else
                      (hash-set! bindings-table key (cons new priors))
                      #t]))]))
            
            ;; for use in the automatic test suite
            (define/public (syncheck:get-bindings-table) bindings-table)
            
            (define/public (syncheck:sort-bindings-table)
              
              ;; compare-bindings : (list text number number) (list text number number) -> boolean
              (define (compare-bindings l1 l2)
                (let ([start-text (list-ref l1 0)]
                      [start-left (list-ref l1 1)]
                      [end-text (list-ref l2 0)]
                      [end-left (list-ref l2 1)])
                  (let-values ([(sx sy) (find-dc-location start-text start-left)]
                               [(ex ey) (find-dc-location end-text end-left)])
                    (cond
                      [(= sy ey) (< sx ex)]
                      [else (< sy ey)]))))
              
              ;; find-dc-location : text number -> (values number number)
              (define (find-dc-location text pos)
                (let ([bx (box 0)]
                      [by (box 0)])
                  (send text position-location pos bx by)
                  (send text editor-location-to-dc-location (unbox bx) (unbox by))))
              
              (hash-for-each
               bindings-table
               (λ (k v)
                 (hash-set! bindings-table k (sort v compare-bindings)))))
            
            (define tacked-hash-table (make-hasheq))
            (define cursor-location #f)
            (define cursor-text #f)
            (define cursor-eles #f)
            
            ;; find-char-box : text number number -> (values number number number number)
            ;; returns the bounding box (left, top, right, bottom) for the text range.
            ;; only works right if the text is on a single line.
            (define/private (find-char-box text left-pos right-pos)
              (let ([xlb (box 0)]
                    [ylb (box 0)]
                    [xrb (box 0)]
                    [yrb (box 0)])
                (send text position-location left-pos xlb ylb #t)
                (send text position-location right-pos xrb yrb #f)
                (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                              [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                              [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                              [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                  (values 
                   xl
                   yl
                   xr 
                   yr))))
            
            (define/private (update-arrow-poss arrow)
              (cond
                [(var-arrow? arrow) (update-var-arrow-poss arrow)]
                [(tail-arrow? arrow) (update-tail-arrow-poss arrow)]))
            
            (define/private (update-var-arrow-poss arrow)
              (let-values ([(start-x start-y) (find-poss 
                                               (var-arrow-start-text arrow)
                                               (var-arrow-start-pos-left arrow)
                                               (var-arrow-start-pos-right arrow))]
                           [(end-x end-y) (find-poss 
                                           (var-arrow-end-text arrow)
                                           (var-arrow-end-pos-left arrow)
                                           (var-arrow-end-pos-right arrow))])
                (set-arrow-start-x! arrow start-x)
                (set-arrow-start-y! arrow start-y)
                (set-arrow-end-x! arrow end-x)
                (set-arrow-end-y! arrow end-y)))
            
            (define/private (update-tail-arrow-poss arrow)
              ;; If the item is an embedded editor snip, redirect
              ;; the arrow to point at the left edge rather than the
              ;; midpoint.
              (define (find-poss/embedded text pos)
                (let* ([snip (send text find-snip pos 'after)])
                  (cond
                    [(and snip 
                          (is-a? snip editor-snip%)
                          (= pos (send text get-snip-position snip)))
                     (find-poss text pos pos)]
                    [else
                     (find-poss text pos (+ pos 1))])))
              (let-values ([(start-x start-y) (find-poss/embedded 
                                               (tail-arrow-from-text arrow)
                                               (tail-arrow-from-pos arrow))]
                           [(end-x end-y) (find-poss/embedded
                                           (tail-arrow-to-text arrow)
                                           (tail-arrow-to-pos arrow))])
                (set-arrow-start-x! arrow start-x)
                (set-arrow-start-y! arrow start-y)
                (set-arrow-end-x! arrow end-x)
                (set-arrow-end-y! arrow end-y)))
                       
            (define/private (find-poss text left-pos right-pos)
              (let ([xlb (box 0)]
                    [ylb (box 0)]
                    [xrb (box 0)]
                    [yrb (box 0)])
                (send text position-location left-pos xlb ylb #t)
                (send text position-location right-pos xrb yrb #f)
                (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                              [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                              [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                              [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                  (values (/ (+ xl xr) 2)
                          (/ (+ yl yr) 2)))))
            
            ;; syncheck:init-arrows : -> void
            (define/public (syncheck:init-arrows)
              (set! tacked-hash-table (make-hasheq))
              (set! arrow-vectors (make-hasheq))
              (set! bindings-table (make-hash))
              (set! cleanup-texts '())
              (set! style-mapping (make-hash))
              (let ([f (get-top-level-window)])
                (when f
                  (send f open-status-line 'drracket:check-syntax:mouse-over))))
            
            ;; syncheck:clear-arrows : -> void
            (define/public (syncheck:clear-arrows)
              (when (or arrow-vectors cursor-location cursor-text)
                (let ([any-tacked? #f])
                  (when tacked-hash-table
                    (let/ec k
                      (hash-for-each
                       tacked-hash-table
                       (λ (key val)
                         (set! any-tacked? #t)
                         (k (void))))))
                  (set! tacked-hash-table #f)
                  (set! arrow-vectors #f)
                  (set! cursor-location #f)
                  (set! cursor-text #f)
                  (set! cursor-eles #f)
                  (when cleanup-texts
                    (for-each (λ (text) (send text thaw-colorer))
                              cleanup-texts))
                  (set! cleanup-texts #f)
                  (set! style-mapping #f)
                  (when any-tacked?
                    (invalidate-bitmap-cache))
                  (update-docs-background #f)
                  (let ([f (get-top-level-window)])
                    (when f
                      (send f close-status-line 'drracket:check-syntax:mouse-over))))))
            
            ;; syncheck:apply-style/remember : (is-a?/c text%) number number style% symbol -> void
            (define/public (syncheck:apply-style/remember txt start finish style mode)
              (when (eq? mode syncheck-mode)
                (add-to-cleanup/apply-style txt start finish style))
              (when cleanup-texts
                (hash-set! style-mapping mode (cons (list txt start finish style)
                                                    (hash-ref style-mapping mode '())))))
            
            ;; add-to-cleanup/apply-style : (is-a?/c text%) number number style% symbol -> boolean
            (define/private (add-to-cleanup/apply-style txt start finish style)
              (cond
                [cleanup-texts
                 (unless (memq txt cleanup-texts)
                   (send txt freeze-colorer)
                   (set! cleanup-texts (cons txt cleanup-texts)))
                 (send txt change-style style start finish #f)
                 #t]
                [else #f]))
            
            (define/public (syncheck:add-menu text start-pos end-pos key make-menu)
              (when arrow-vectors
                (when (and (<= 0 start-pos end-pos (last-position)))
                  (add-to-range/key text start-pos end-pos make-menu key #t))))
            
            (define/public (syncheck:add-background-color text color start fin key)
              (when arrow-vectors
                (when (is-a? text text:basic<%>)
                  (add-to-range/key text start fin (make-colored-region color text start fin) key #f))))
            
            ;; syncheck:add-arrow : symbol text number number text number number boolean -> void
            ;; pre: start-editor, end-editor are embedded in `this' (or are `this')
            (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                               end-text end-pos-left end-pos-right
                                               actual? level)
              (when arrow-vectors
                (let* ([arrow (make-var-arrow #f #f #f #f
                                              start-text start-pos-left start-pos-right
                                              end-text end-pos-left end-pos-right
                                              actual? level)])
                  (when (add-to-bindings-table
                         start-text start-pos-left start-pos-right
                         end-text end-pos-left end-pos-right)
                    (add-to-range/key start-text start-pos-left start-pos-right arrow #f #f)
                    (add-to-range/key end-text end-pos-left end-pos-right arrow #f #f)))))
            
            ;; syncheck:add-tail-arrow : text number text number -> void
            (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos)
              (when arrow-vectors
                (let ([tail-arrow (make-tail-arrow #f #f #f #f to-text to-pos from-text from-pos)])
                  (add-to-range/key from-text from-pos (+ from-pos 1) tail-arrow #f #f)
                  (add-to-range/key to-text to-pos (+ to-pos 1) tail-arrow #f #f))))
            
            ;; syncheck:add-jump-to-definition : text start end id filename -> void
            (define/public (syncheck:add-jump-to-definition text start end id filename)
              (when arrow-vectors
                (add-to-range/key text start end (make-def-link id filename) #f #f)))
            
            ;; syncheck:add-mouse-over-status : text pos-left pos-right string -> void
            (define/public (syncheck:add-mouse-over-status text pos-left pos-right str)
              (when arrow-vectors
                (add-to-range/key text pos-left pos-right str #f #f)))
            
            ;; add-to-range/key : text number number any any boolean -> void
            ;; adds `key' to the range `start' - `end' in the editor
            ;; If use-key? is #t, it adds `to-add' with the key, and does not
            ;; replace a value with that key already there.
            ;; If use-key? is #f, it adds `to-add' without a key.
            ;; pre: arrow-vectors is not #f
            (define/private (add-to-range/key text start end to-add key use-key?)
              (let ([arrow-vector (hash-ref
                                   arrow-vectors
                                   text 
                                   (λ ()
                                     (let ([new-vec 
                                            (make-vector
                                             (add1 (send text last-position))
                                             null)])
                                       (hash-set! 
                                        arrow-vectors 
                                        text
                                        new-vec)
                                       new-vec)))])
                (let loop ([p start])
                  (when (and (<= p end)
                             (< p (vector-length arrow-vector))) 
                    ;; the last test in the above and is because some syntax objects
                    ;; appear to be from the original source, but can have bogus information.
                    
                    (let ([r (vector-ref arrow-vector p)])
                      (cond
                        [use-key?
                         (unless (ormap (λ (x) 
                                          (and (pair? x) 
                                               (car x)
                                               (eq? (car x) key)))
                                        r)
                           (vector-set! arrow-vector p (cons (cons key to-add) r)))]
                        [else
                         (vector-set! arrow-vector p (cons to-add r))]))
                    (loop (add1 p))))))
            
            (inherit get-top-level-window)
            
            (define/augment (on-change)
              (inner (void) on-change)
              (when arrow-vectors
                (flush-arrow-coordinates-cache)
                (let ([any-tacked? #f])
                  (when tacked-hash-table
                    (let/ec k
                      (hash-for-each
                       tacked-hash-table
                       (λ (key val)
                         (set! any-tacked? #t)
                         (k (void))))))
                  (when any-tacked?
                    (invalidate-bitmap-cache)))))
            
            ;; flush-arrow-coordinates-cache : -> void
            ;; pre-condition: arrow-vector is not #f.
            (define/private (flush-arrow-coordinates-cache)
              (hash-for-each
               arrow-vectors
               (λ (text arrow-vector)
                 (let loop ([n (vector-length arrow-vector)])
                   (unless (zero? n)
                     (let ([eles (vector-ref arrow-vector (- n 1))])
                       (for-each (λ (ele)
                                   (cond
                                     [(arrow? ele)
                                      (set-arrow-start-x! ele #f)
                                      (set-arrow-start-y! ele #f)
                                      (set-arrow-end-x! ele #f)
                                      (set-arrow-end-y! ele #f)]))
                                 eles))
                     (loop (- n 1)))))))
            
            (define/override (on-paint before dc left top right bottom dx dy draw-caret)
              (when (and arrow-vectors (not before))
                (let ([draw-arrow2
                       (λ (arrow)
                         (unless (arrow-start-x arrow)
                           (update-arrow-poss arrow))
                         (let ([start-x (arrow-start-x arrow)]
                               [start-y (arrow-start-y arrow)]
                               [end-x   (arrow-end-x arrow)]
                               [end-y   (arrow-end-y arrow)])
                           (unless (and (= start-x end-x)
                                        (= start-y end-y))
                             (drracket:arrow:draw-arrow dc start-x start-y end-x end-y dx dy)
                             (when (and (var-arrow? arrow) (not (var-arrow-actual? arrow)))
                               (let-values ([(fw fh _d _v) (send dc get-text-extent "x")])
                                 (send dc draw-text "?"
                                       (+ end-x dx fw)
                                       (+ end-y dy (- fh))))))))]
                      [old-brush (send dc get-brush)]
                      [old-pen   (send dc get-pen)]
                      [old-font  (send dc get-font)]
                      [old-text-foreground (send dc get-text-foreground)]
                      [old-text-mode (send dc get-text-mode)])
                  (send dc set-font
                        (send the-font-list find-or-create-font
                              (send old-font get-point-size)
                              'default
                              'normal
                              'bold))
                  (send dc set-text-foreground templ-color)
                  (hash-for-each tacked-hash-table
                                 (λ (arrow v) 
                                    (when v 
                                      (cond
                                       [(var-arrow? arrow)
                                        (if (var-arrow-actual? arrow)
                                            (begin (send dc set-pen var-pen)
                                                   (send dc set-brush tacked-var-brush))
                                            (begin (send dc set-pen templ-pen)
                                                   (send dc set-brush tacked-templ-brush)))]
                                       [(tail-arrow? arrow)
                                        (send dc set-pen tail-pen)
                                        (send dc set-brush tacked-tail-brush)])
                                      (draw-arrow2 arrow))))
                  (when (and cursor-location
                             cursor-text)
                    (let* ([arrow-vector (hash-ref arrow-vectors cursor-text (λ () #f))])
                      (when arrow-vector
                        (let ([eles (vector-ref arrow-vector cursor-location)])
                          (for-each (λ (ele) 
                                      (cond
                                        [(var-arrow? ele)
                                         (if (var-arrow-actual? ele)
                                             (begin (send dc set-pen var-pen)
                                                    (send dc set-brush untacked-brush))
                                             (begin (send dc set-pen templ-pen)
                                                    (send dc set-brush untacked-brush)))
                                         (draw-arrow2 ele)]
                                        [(tail-arrow? ele)
                                         (send dc set-pen tail-pen)
                                         (send dc set-brush untacked-brush)
                                         (for-each-tail-arrows draw-arrow2 ele)]))
                                    eles)))))
                  (send dc set-brush old-brush)
                  (send dc set-pen old-pen)
                  (send dc set-font old-font)
                  (send dc set-text-foreground old-text-foreground)
                  (send dc set-text-mode old-text-mode)))
              
              ;; do the drawing before calling super so that the arrows don't
              ;; cross the "#lang ..." line, if it is present.
              (super on-paint before dc left top right bottom dx dy draw-caret))
            
            ;; for-each-tail-arrows : (tail-arrow -> void) tail-arrow -> void
            (define/private (for-each-tail-arrows f tail-arrow)
              ;; call-f-ht ensures that `f' is only called once per arrow
              (define call-f-ht (make-hasheq))
              
              (define (for-each-tail-arrows/to/from tail-arrow-pos tail-arrow-text
                                                    tail-arrow-other-pos tail-arrow-other-text)
                
                ;; traversal-ht ensures that we don't loop in the arrow traversal.
                (let ([traversal-ht (make-hasheq)])
                  (let loop ([tail-arrow tail-arrow])
                    (unless (hash-ref traversal-ht tail-arrow (λ () #f))
                      (hash-set! traversal-ht tail-arrow #t)
                      (unless (hash-ref call-f-ht tail-arrow (λ () #f))
                        (hash-set! call-f-ht tail-arrow #t)
                        (f tail-arrow))
                      (let* ([next-pos (tail-arrow-pos tail-arrow)]
                             [next-text (tail-arrow-text tail-arrow)]
                             [arrow-vector (hash-ref arrow-vectors next-text (λ () #f))])
                        (when arrow-vector
                          (let ([eles (vector-ref arrow-vector next-pos)])
                            (for-each (λ (ele) 
                                        (cond
                                          [(tail-arrow? ele)
                                           (let ([other-pos (tail-arrow-other-pos ele)]
                                                 [other-text (tail-arrow-other-text ele)])
                                             (when (and (= other-pos next-pos)
                                                        (eq? other-text next-text))
                                               (loop ele)))]))
                                      eles))))))))
              
              (for-each-tail-arrows/to/from tail-arrow-to-pos tail-arrow-to-text
                                            tail-arrow-from-pos tail-arrow-from-text)
              (for-each-tail-arrows/to/from tail-arrow-from-pos tail-arrow-from-text
                                            tail-arrow-to-pos tail-arrow-to-text))
            
            (define/override (on-event event)
              (if arrow-vectors
                  (cond
                    [(send event leaving?)
                     (update-docs-background #f)
                     (when (and cursor-location cursor-text)
                       (set! cursor-location #f)
                       (set! cursor-text #f)
                       (set! cursor-eles #f)
                       (let ([f (get-top-level-window)])
                         (when f
                           (send f update-status-line 'drracket:check-syntax:mouse-over #f)))
                       (invalidate-bitmap-cache))
                     (super on-event event)]
                    [(or (send event moving?)
                         (send event entering?))
                     (let-values ([(pos text) (get-pos/text event)])
                       (cond
                         [(and pos (is-a? text text%))
                          (unless (and (equal? pos cursor-location)
                                       (eq? cursor-text text))
                            (set! cursor-location pos)
                            (set! cursor-text text)
                            
                            (let* ([arrow-vector (hash-ref arrow-vectors cursor-text (λ () #f))]
                                   [eles (and arrow-vector (vector-ref arrow-vector cursor-location))])
                              
                              (unless (equal? cursor-eles eles)
                                (set! cursor-eles eles)
                                (update-docs-background eles)
                                (when eles
                                  (update-status-line eles)
                                  (for-each (λ (ele)
                                              (cond
                                                [(arrow? ele)
                                                 (update-arrow-poss ele)]))
                                            eles)
                                  (invalidate-bitmap-cache)))))]
                         [else
                          (update-docs-background #f)
                          (let ([f (get-top-level-window)])
                            (when f
                              (send f update-status-line 'drracket:check-syntax:mouse-over #f)))
                          (when (or cursor-location cursor-text)
                            (set! cursor-location #f)
                            (set! cursor-text #f)
                            (set! cursor-eles #f)
                            (invalidate-bitmap-cache))]))
                     (super on-event event)]
                    [(send event button-down? 'right)
                     (let-values ([(pos text) (get-pos/text event)])
                       (if (and pos (is-a? text text%))
                           (let ([arrow-vector (hash-ref arrow-vectors text (λ () #f))])
                             (when arrow-vector
                               (let ([vec-ents (vector-ref arrow-vector pos)]
                                     [start-selection (send text get-start-position)]
                                     [end-selection (send text get-end-position)])
                                 (cond
                                   [(and (null? vec-ents) (= start-selection end-selection))
                                    (super on-event event)]
                                   [else
                                    (let* ([menu (make-object popup-menu% #f)]
                                           [arrows (filter arrow? vec-ents)]
                                           [def-links (filter def-link? vec-ents)]
                                           [var-arrows (filter var-arrow? arrows)]
                                           [add-menus (map cdr (filter pair? vec-ents))])
                                      (unless (null? arrows)
                                        (make-object menu-item%
                                          (string-constant cs-tack/untack-arrow)
                                          menu
                                          (λ (item evt) (tack/untack-callback arrows))))
                                      (unless (null? def-links)
                                        (let ([def-link (car def-links)])
                                          (make-object menu-item%
                                            jump-to-definition
                                            menu
                                            (λ (item evt)
                                              (jump-to-definition-callback def-link)))))
                                      (unless (null? var-arrows)
                                        (make-object menu-item%
                                          jump-to-next-bound-occurrence
                                          menu
                                          (λ (item evt) (jump-to-next-callback pos text arrows)))
                                        (make-object menu-item%
                                          jump-to-binding
                                          menu
                                          (λ (item evt) (jump-to-binding-callback arrows))))
                                      (unless (= start-selection end-selection)
                                        (let ([arrows-menu
                                               (make-object menu%
                                                            "Arrows crossing selection"
                                                            menu)]
                                              [callback
                                               (lambda (accept)
                                                 (tack-crossing-arrows-callback
                                                  arrow-vector
                                                  start-selection
                                                  end-selection
                                                  text
                                                  accept))])
                                          (make-object menu-item%
                                                       "Tack arrows"
                                                       arrows-menu
                                                       (lambda (item evt)
                                                         (callback
                                                          '(lexical top-level imported))))
                                          (make-object menu-item%
                                                       "Tack non-import arrows"
                                                       arrows-menu
                                                       (lambda (item evt)
                                                         (callback
                                                          '(lexical top-level))))
                                          (make-object menu-item%
                                                       "Untack arrows"
                                                       arrows-menu
                                                       (lambda (item evt)
                                                         (untack-crossing-arrows
                                                          arrow-vector
                                                          start-selection
                                                          end-selection)))))
                                      (for-each (λ (f) (f menu)) add-menus)
                                      (send (get-canvas) popup-menu menu
                                            (+ 1 (inexact->exact (floor (send event get-x))))
                                            (+ 1 (inexact->exact (floor (send event get-y))))))]))))
                           (super on-event event)))]
                    [else (super on-event event)])
                  (super on-event event)))
            
            (define/private (update-status-line eles)
              (let ([has-txt? #f])
                (for-each (λ (ele)
                            (cond
                              [(string? ele)
                               (set! has-txt? #t)
                               (let ([f (get-top-level-window)])
                                 (when f
                                   (send f update-status-line 
                                         'drracket:check-syntax:mouse-over 
                                         ele)))]))
                          eles)
                (unless has-txt?
                  (let ([f (get-top-level-window)])
                    (when f
                      (send f update-status-line 'drracket:check-syntax:mouse-over #f))))))
            
            (define current-colored-region #f)
            ;; update-docs-background : (or/c false/c (listof any)) -> void
            (define/private (update-docs-background eles)
              (let ([new-region (and eles (ormap (λ (x) (and (colored-region? x) x)) eles))])
                (unless (eq? current-colored-region new-region)
                  (when current-colored-region
                    (send (colored-region-text current-colored-region) unhighlight-range 
                          (colored-region-start current-colored-region)
                          (colored-region-fin current-colored-region)
                          (send the-color-database find-color (colored-region-color current-colored-region))))
                  (when new-region
                    (send (colored-region-text new-region) highlight-range
                          (colored-region-start new-region)
                          (colored-region-fin new-region)
                          (send the-color-database find-color (colored-region-color new-region))))
                  (set! current-colored-region new-region))))
                
            ;; tack/untack-callback : (listof arrow) -> void
            ;; callback for the tack/untack menu item
            (define/private (tack/untack-callback arrows)
              (let ([arrow-tacked?
                     (λ (arrow)
                       (hash-ref
                        tacked-hash-table
                        arrow
                        (λ () #f)))]
                    [untack-arrows? #f])
                (for-each 
                 (λ (arrow)
                   (cond
                     [(var-arrow? arrow)
                      (set! untack-arrows? (or untack-arrows? (arrow-tacked? arrow)))]
                     [(tail-arrow? arrow)
                      (for-each-tail-arrows
                       (λ (arrow) (set! untack-arrows? (or untack-arrows? (arrow-tacked? arrow))))
                       arrow)]))
                 arrows)
                (for-each 
                 (λ (arrow)
                   (cond
                     [(var-arrow? arrow)
                      (hash-set! tacked-hash-table arrow (not untack-arrows?))]
                     [(tail-arrow? arrow)
                      (for-each-tail-arrows
                       (λ (arrow) 
                         (hash-set! tacked-hash-table arrow (not untack-arrows?)))
                       arrow)]))
                 arrows))
              (invalidate-bitmap-cache))
            
            (define/private (tack-crossing-arrows-callback arrow-vector start end text kinds)
              (define (xor a b)
                (or (and a (not b)) (and (not a) b)))
              (define (within t p)
                (and (eq? t text)
                     (<= start p end)))
              (for ([position (in-range start end)])
                (define things (vector-ref arrow-vector position))
                (for ([va things] #:when (var-arrow? va))
                  (define va-start (var-arrow-start-pos-left va))
                  (define va-start-text (var-arrow-start-text va))
                  (define va-end (var-arrow-end-pos-left va))
                  (define va-end-text (var-arrow-end-text va))
                  (when (xor (within va-start-text va-start)
                             (within va-end-text va-end))
                    (when (memq (var-arrow-level va) kinds)
                      (hash-set! tacked-hash-table va #t)))))
              (invalidate-bitmap-cache))

            (define/private (untack-crossing-arrows arrow-vector start end)
              (for ([position (in-range start end)])
                (for ([va (vector-ref arrow-vector position)] #:when (var-arrow? va))
                  (hash-set! tacked-hash-table va #f))))

            ;; syncheck:jump-to-binding-occurrence : text -> void
            ;; jumps to the next occurrence, based on the insertion point
            (define/public (syncheck:jump-to-next-bound-occurrence text)
              (jump-to-binding/bound-helper 
               text 
               (λ (pos text vec-ents)
                 (jump-to-next-callback pos text vec-ents))))
            
            ;; syncheck:jump-to-binding-occurrence : text -> void
            (define/public (syncheck:jump-to-binding-occurrence text)
              (jump-to-binding/bound-helper 
               text 
               (λ (pos text vec-ents)
                 (jump-to-binding-callback vec-ents))))
            
            (define/private (jump-to-binding/bound-helper text do-jump)
              (let ([pos (send text get-start-position)])
                (when arrow-vectors
                  (let ([arrow-vector (hash-ref arrow-vectors text (λ () #f))])
                    (when arrow-vector
                      (let ([vec-ents (filter var-arrow? (vector-ref arrow-vector pos))])
                        (unless (null? vec-ents)
                          (do-jump pos text vec-ents))))))))
            
            ;; jump-to-next-callback : (listof arrow) -> void
            ;; callback for the jump popup menu item
            (define/private (jump-to-next-callback pos txt input-arrows)
              (unless (null? input-arrows)
                (let* ([arrow-key (car input-arrows)]
                       [orig-arrows (hash-ref bindings-table
                                              (list (var-arrow-start-text arrow-key)
                                                    (var-arrow-start-pos-left arrow-key)
                                                    (var-arrow-start-pos-right arrow-key))
                                              (λ () '()))])
                  (cond
                    [(null? orig-arrows) (void)]
                    [(null? (cdr orig-arrows)) (jump-to (car orig-arrows))]
                    [else
                     (let loop ([arrows orig-arrows])
                       (cond
                         [(null? arrows) (jump-to (car orig-arrows))]
                         [else (let ([arrow (car arrows)])
                                 (cond
                                   [(and (object=? txt (list-ref arrow 0))
                                         (<= (list-ref arrow 1) pos (list-ref arrow 2)))
                                    (jump-to (if (null? (cdr arrows))
                                                 (car orig-arrows)
                                                 (cadr arrows)))]
                                   [else (loop (cdr arrows))]))]))]))))
            
            ;; jump-to : (list text number number) -> void
            (define/private (jump-to to-arrow)
              (let ([end-text (list-ref to-arrow 0)]
                    [end-pos-left (list-ref to-arrow 1)]
                    [end-pos-right (list-ref to-arrow 2)])
                (send end-text set-position end-pos-left end-pos-right)
                (send end-text set-caret-owner #f 'global)))
            
            ;; jump-to-binding-callback : (listof arrow) -> void
            ;; callback for the jump popup menu item
            (define/private (jump-to-binding-callback arrows)
              (unless (null? arrows)
                (let* ([arrow (car arrows)]
                       [start-text (var-arrow-start-text arrow)]
                       [start-pos-left (var-arrow-start-pos-left arrow)]
                       [start-pos-right (var-arrow-start-pos-right arrow)])
                  (send start-text set-position start-pos-left start-pos-right)
                  (send start-text set-caret-owner #f 'global))))
            
            ;; syncheck:jump-to-definition : text -> void
            (define/public (syncheck:jump-to-definition text)
              (let ([pos (send text get-start-position)])
                (when arrow-vectors
                  (let ([arrow-vector (hash-ref arrow-vectors text (λ () #f))])
                    (when arrow-vector
                      (let ([vec-ents (filter def-link? (vector-ref arrow-vector pos))])
                        (unless (null? vec-ents)
                          (jump-to-definition-callback (car vec-ents)))))))))
            
            (define/private (jump-to-definition-callback def-link)
              (let* ([filename (def-link-filename def-link)]
                     [id-from-def (def-link-id def-link)]
                     [frame (fw:handler:edit-file filename)])
                (when (is-a? frame syncheck-frame<%>)
                  (send frame syncheck:button-callback id-from-def))))
            
            (define/augment (after-set-next-settings settings)
              (let ([frame (get-top-level-window)])
                (when frame
                  (send frame update-button-visibility/settings settings)))
              (inner (void) after-set-next-settings settings))
            
            (define syncheck-mode 'default-mode)
            (define/public (set-syncheck-mode m) 
              (let ([old-mode syncheck-mode])
                (set! syncheck-mode m)
                (when style-mapping
                  (unless (eq? old-mode syncheck-mode)
                    (apply-syncheck-mode)))))
            (define/public (get-syncheck-mode) (if style-mapping
                                                   syncheck-mode
                                                   #f))
            
            (define/private (apply-syncheck-mode)
              (let ([edit-sequences '()])
                (for ((l (in-list (reverse (hash-ref style-mapping syncheck-mode)))))
                  (let-values ([(txt start finish style) (apply values l)])
                    (unless (memq txt edit-sequences)
                      (send txt begin-edit-sequence #f)
                      
                      ;; this little dance resets the 
                      ;; colors to their natural values
                      (begin
                        (cond
                          [(send txt is-frozen?)
                           (send txt thaw-colorer)]
                          [else
                           (send txt freeze-colorer)
                           (send txt thaw-colorer)])
                        (send txt freeze-colorer))
                      
                      (set! edit-sequences (cons txt edit-sequences)))
                    (add-to-cleanup/apply-style txt start finish style)))
                (for ((txt (in-list edit-sequences)))
                  (send txt end-edit-sequence))))
              
            (super-new)))))
    
    (define syncheck-frame<%>
      (interface ()
        syncheck:button-callback
        syncheck:error-report-visible?))
    
    (define tab-mixin
      
      (mixin (drracket:unit:tab<%>) ()
        (inherit is-current-tab? get-defs get-frame)
        
        (define report-error-text (new (fw:text:ports-mixin fw:scheme:text%)))
        (define error-report-visible? #f)
        (send report-error-text auto-wrap #t)
        (send report-error-text set-autowrap-bitmap #f)
        (send report-error-text lock #t)
        
        (define/public (get-error-report-text) report-error-text)
        (define/public (get-error-report-visible?) error-report-visible?)
        (define/public (turn-on-error-report) (set! error-report-visible? #t))
        (define/public (turn-off-error-report) (set! error-report-visible? #f))
        (define/augment (clear-annotations)
          (inner (void) clear-annotations)
          (syncheck:clear-error-message)
          (syncheck:clear-highlighting))
        
        (define/public (syncheck:clear-error-message)
          (set! error-report-visible? #f)
          (send report-error-text clear-output-ports)
          (send report-error-text lock #f)
          (send report-error-text delete/io 0 (send report-error-text last-position))
          (send report-error-text lock #t)
          (when (is-current-tab?)
            (send (get-frame) hide-error-report)
            (send (get-frame) update-menu-item-label this)))
        
        (define/public (syncheck:clear-highlighting)
          (let* ([definitions (get-defs)]
                 [locked? (send definitions is-locked?)])
            (send definitions begin-edit-sequence #f)
            (send definitions lock #f)
            (send definitions syncheck:clear-arrows)
            (send definitions lock locked?)
            (send definitions end-edit-sequence)))
        
        (define/augment (can-close?)
          (and (send report-error-text can-close?)
               (inner #t can-close?)))
        
        (define/augment (on-close)
          (send report-error-text on-close)
          (send (get-defs) syncheck:clear-arrows)
          (inner (void) on-close))
        
        (super-new)))
    
    (define unit-frame-mixin
      (mixin (drracket:unit:frame<%>) (syncheck-frame<%>)
        
        (inherit get-button-panel 
                 get-definitions-canvas 
                 get-definitions-text
                 get-interactions-text
                 get-current-tab)
        
        (define/augment (on-tab-change old-tab new-tab)
          (inner (void) on-tab-change old-tab new-tab)
          (if (send new-tab get-error-report-visible?)
              (show-error-report)
              (hide-error-report))
          (send report-error-canvas set-editor (send new-tab get-error-report-text))
          (update-menu-item-label new-tab)
          (update-button-visibility/tab new-tab))
        
        (define/private (update-button-visibility/tab tab)
          (update-button-visibility/settings (send (send tab get-defs) get-next-settings)))
        (define/public (update-button-visibility/settings settings)
          (let* ([lang (drracket:language-configuration:language-settings-language settings)]
                 [visible? (and (not (is-a? lang drracket:module-language:module-language<%>))
                                (send lang capability-value 'drscheme:check-syntax-button))])
            (send check-syntax-button-parent-panel change-children
                  (λ (l)
                    (if visible?
                        (list check-syntax-button)
                        '())))))
        
        (define/augment (enable-evaluation)
          (send check-syntax-button enable #t)
          (send mode-menu-item enable #t)
          (inner (void) enable-evaluation))
        
        (define/augment (disable-evaluation)
          (send mode-menu-item enable #f)
          (send check-syntax-button enable #f)
          (inner (void) disable-evaluation))
        
        (define report-error-parent-panel 'uninitialized-report-error-parent-panel)
        (define report-error-panel 'uninitialized-report-error-panel)
        (define report-error-canvas 'uninitialized-report-error-editor-canvas)
        (define/override (get-definitions/interactions-panel-parent)
          (set! report-error-parent-panel
                (make-object vertical-panel%
                  (super get-definitions/interactions-panel-parent)))
          (set! report-error-panel (instantiate horizontal-panel% ()
                                     (parent report-error-parent-panel)
                                     (stretchable-height #f)
                                     (alignment '(center center))
                                     (style '(border))))
          (send report-error-parent-panel change-children (λ (l) null))
          (let ([message-panel (instantiate vertical-panel% ()
                                 (parent report-error-panel)
                                 (stretchable-width #f)
                                 (stretchable-height #f)
                                 (alignment '(left center)))])
            (make-object message% (string-constant check-syntax) message-panel)
            (make-object message% (string-constant cs-error-message) message-panel))
          (set! report-error-canvas (new editor-canvas% 
                                         (parent report-error-panel)
                                         (editor (send (get-current-tab) get-error-report-text))
                                         (line-count 3)
                                         (style '(no-hscroll))))
          (instantiate button% () 
            (label (string-constant hide))
            (parent report-error-panel)
            (callback (λ (x y) (hide-error-report)))
            (stretchable-height #t))
          (make-object vertical-panel% report-error-parent-panel))
        
        (define/public-final (syncheck:error-report-visible?)
          (and (is-a? report-error-parent-panel area-container<%>)
               (member report-error-panel (send report-error-parent-panel get-children))))
        
        (define/public (hide-error-report) 
          (when (syncheck:error-report-visible?)
            (send (get-current-tab) turn-off-error-report)
            (send report-error-parent-panel change-children
                  (λ (l) (remq report-error-panel l)))))
        
        (define/private (show-error-report)
          (unless (syncheck:error-report-visible?)
            (send report-error-parent-panel change-children
                  (λ (l) (cons report-error-panel l)))))
        
        (define rest-panel 'uninitialized-root)
        (define super-root 'uninitialized-super-root)
        (define/override (make-root-area-container % parent)
          (let* ([s-root (super make-root-area-container
                                vertical-panel%
                                parent)]
                 [r-root (make-object % s-root)])
            (set! super-root s-root)
            (set! rest-panel r-root)
            r-root))
        
        (define mode-menu-item #f)
        
        (define/override (add-show-menu-items show-menu)
          (super add-show-menu-items show-menu)
          (set! mode-menu-item
                (new menu-item% 
                     [parent show-menu]
                     [label ""]
                     [callback
                      (λ (a b)
                        (let ([defs (send (get-current-tab) get-defs)])
                          (case (send defs get-syncheck-mode)
                            [(#f) (syncheck:button-callback #f 'contract-mode)]
                            [(default-mode)
                             (send defs set-syncheck-mode 'contract-mode)
                             (update-menu-item-label (get-current-tab))]
                            [else 
                             (send defs set-syncheck-mode 'default-mode)
                             (update-menu-item-label (get-current-tab))])))])))
        
        (define/public (update-menu-item-label tab)
          (when mode-menu-item
            (let ([mode (send (send (get-current-tab) get-defs) get-syncheck-mode)])
              (case mode
                [(#f) 
                 (send mode-menu-item set-label cs-mode-menu-show-contract)]
                [(default-mode)
                 (send mode-menu-item set-label cs-mode-menu-show-contract)]
                [(contract-mode)
                 (send mode-menu-item set-label cs-mode-menu-show-syntax)]))))
                
        (inherit open-status-line close-status-line update-status-line ensure-rep-hidden)
        ;; syncheck:button-callback : (case-> (-> void) ((union #f syntax) -> void)
        ;; this is the only function that has any code running on the user's thread
        (define/public syncheck:button-callback
          (case-lambda
            [() (syncheck:button-callback #f)]
            [(jump-to-id) (syncheck:button-callback jump-to-id 'default-mode)]
            [(jump-to-id mode)
             (when (send check-syntax-button is-enabled?)
               (open-status-line 'drracket:check-syntax)
               (update-status-line 'drracket:check-syntax status-init)
               (ensure-rep-hidden)
               (let-values ([(expanded-expression expansion-completed) (make-traversal)])
                 (let* ([definitions-text (get-definitions-text)]
                        [interactions-text (get-interactions-text)]
                        [drs-eventspace (current-eventspace)]
                        [the-tab (get-current-tab)])
                   (let-values ([(old-break-thread old-custodian) (send the-tab get-breakables)])
                     (let* ([user-namespace #f]
                            [user-directory #f]
                            [user-custodian #f]
                            [normal-termination? #f]
                            
                            [show-error-report/tab
                             (λ () ; =drs=
                               (send the-tab turn-on-error-report)
                               (send (send the-tab get-error-report-text) scroll-to-position 0)
                               (when (eq? (get-current-tab) the-tab)
                                 (show-error-report)))]
                            [cleanup
                             (λ () ; =drs=
                               (send the-tab set-breakables old-break-thread old-custodian)
                               (send the-tab enable-evaluation)
                               (send definitions-text end-edit-sequence)
                               (close-status-line 'drracket:check-syntax)
                               
                               ;; do this with some lag ... not great, but should be okay.
                               (thread
                                (λ ()
                                  (flush-output (send (send the-tab get-error-report-text) get-err-port))
                                  (queue-callback
                                   (λ ()
                                     (unless (= 0 (send (send the-tab get-error-report-text) last-position))
                                       (show-error-report/tab)))))))]
                            [kill-termination
                             (λ ()
                               (unless normal-termination?
                                 (parameterize ([current-eventspace drs-eventspace])
                                   (queue-callback
                                    (λ ()
                                      (send the-tab syncheck:clear-highlighting)
                                      (cleanup)
                                      (custodian-shutdown-all user-custodian))))))]
                            [error-display-semaphore (make-semaphore 0)]
                            [uncaught-exception-raised
                             (λ () ;; =user=
                               (set! normal-termination? #t)
                               (parameterize ([current-eventspace drs-eventspace])
                                 (queue-callback
                                  (λ () ;;  =drs=
                                    (yield error-display-semaphore) ;; let error display go first
                                    (send the-tab syncheck:clear-highlighting)
                                    (cleanup)
                                    (custodian-shutdown-all user-custodian)))))]
                            [error-port (send (send the-tab get-error-report-text) get-err-port)]
                            [init-proc
                             (λ () ; =user=
                               (send the-tab set-breakables (current-thread) (current-custodian))
                               (set-directory definitions-text)
                               (current-error-port error-port)
                               (error-display-handler 
                                (λ (msg exn) ;; =user=
                                  (parameterize ([current-eventspace drs-eventspace])
                                    (queue-callback
                                     (λ () ;; =drs=
                                       
                                       ;; a call like this one also happens in 
                                       ;; drracket:debug:error-display-handler/stacktrace
                                       ;; but that call won't happen here, because
                                       ;; the rep is not in the current-rep parameter
                                       (send interactions-text highlight-errors/exn exn)
                                       
                                       (show-error-report/tab))))
                                  
                                  (drracket:debug:error-display-handler/stacktrace 
                                   msg 
                                   exn 
                                   '()
                                   #:definitions-text definitions-text)
                                  
                                  (semaphore-post error-display-semaphore)))
                               
                               (error-print-source-location #f) ; need to build code to render error first
                               (uncaught-exception-handler
                                (let ([oh (uncaught-exception-handler)])
                                  (λ (exn)
                                    (uncaught-exception-raised)
                                    (oh exn))))
                               (update-status-line 'drracket:check-syntax status-expanding-expression)
                               (set! user-custodian (current-custodian))
                               (set! user-directory (current-directory)) ;; set by set-directory above
                               (set! user-namespace (current-namespace)))])
                       (send the-tab disable-evaluation) ;; this locks the editor, so must be outside.
                       (send definitions-text begin-edit-sequence #f)
                       (with-lock/edit-sequence
                        definitions-text
                        (λ ()
                          (send the-tab clear-annotations)
                          (send the-tab reset-offer-kill)
                          (send (send the-tab get-defs) syncheck:init-arrows)
                          
                          (drracket:eval:expand-program
                           (drracket:language:make-text/pos definitions-text 0 (send definitions-text last-position))
                           (send definitions-text get-next-settings)
                           #t
                           init-proc
                           kill-termination
                           (λ (sexp loop) ; =user=
                             (cond
                               [(eof-object? sexp)
                                (set! normal-termination? #t)
                                (parameterize ([current-eventspace drs-eventspace])
                                  (queue-callback
                                   (λ () ; =drs=
                                     (with-lock/edit-sequence
                                      definitions-text
                                      (λ ()
                                        (parameterize ([currently-processing-definitions-text definitions-text])
                                          (expansion-completed user-namespace user-directory)
                                          (send (send (get-current-tab) get-defs) set-syncheck-mode mode)
                                          (update-menu-item-label (get-current-tab))
                                          (send definitions-text syncheck:sort-bindings-table))))
                                     (cleanup)
                                     (custodian-shutdown-all user-custodian))))]
                               [else
                                (update-status-line 'drracket:check-syntax status-eval-compile-time)
                                (eval-compile-time-part-of-top-level sexp)
                                (parameterize ([current-eventspace drs-eventspace])
                                  (queue-callback
                                   (λ () ; =drs=
                                     (with-lock/edit-sequence
                                      definitions-text
                                      (λ ()
                                        (open-status-line 'drracket:check-syntax)
                                        (update-status-line 'drracket:check-syntax status-coloring-program)
                                        (parameterize ([currently-processing-definitions-text definitions-text])
                                          (expanded-expression user-namespace user-directory sexp jump-to-id))
                                        (close-status-line 'drracket:check-syntax))))))
                                (update-status-line 'drracket:check-syntax status-expanding-expression)
                                (loop)]))))))))))]))
        
        ;; set-directory : text -> void
        ;; sets the current-directory and current-load-relative-directory
        ;; based on the file saved in the definitions-text
        (define/private (set-directory definitions-text)
          (let* ([tmp-b (box #f)]
                 [fn (send definitions-text get-filename tmp-b)])
            (unless (unbox tmp-b)
              (when fn
                (let-values ([(base name dir?) (split-path fn)])
                  (current-directory base)
                  (current-load-relative-directory base))))))
        
        ;; with-lock/edit-sequence : text (-> void) -> void
        ;; sets and restores some state of the definitions text
        ;; so that edits to the definitions text work out.
        (define/private (with-lock/edit-sequence definitions-text thnk)
          (let* ([locked? (send definitions-text is-locked?)])
            (send definitions-text begin-edit-sequence)
            (send definitions-text lock #f)
            (thnk)
            (send definitions-text end-edit-sequence)
            (send definitions-text lock locked?)))
                
        (super-new)
        
        (define check-syntax-button-parent-panel 
          (new horizontal-panel%
               [parent (get-button-panel)]
               [stretchable-width #f]
               [stretchable-height #f]))
        (define check-syntax-button
          (new switchable-button%
               (label (string-constant check-syntax))
               (bitmap syncheck-bitmap)
               (parent check-syntax-button-parent-panel)
               (callback (λ (button) (syncheck:button-callback)))))
        (inherit register-toolbar-button)
        (register-toolbar-button check-syntax-button)
        (define/public (syncheck:get-button) check-syntax-button)
        (send (get-button-panel) change-children
              (λ (l)
                (cons check-syntax-button-parent-panel
                      (remove check-syntax-button-parent-panel l))))
        (update-button-visibility/tab (get-current-tab))))
    
    (define report-error-style (make-object style-delta% 'change-style 'italic))
    (send report-error-style set-delta-foreground "red")
    
    (define (add-check-syntax-key-bindings keymap)
      (send keymap add-function
            "check syntax"
            (λ (obj evt)
              (when (is-a? obj editor<%>)
                (let ([canvas (send obj get-canvas)])
                  (when canvas
                    (let ([frame (send canvas get-top-level-window)])
                      (when (is-a? frame syncheck-frame<%>)
                        (send frame syncheck:button-callback))))))))
      
      (let ([jump-callback
             (λ (send-msg)
               (λ (obj evt)
                 (when (is-a? obj text%)
                   (let ([canvas (send obj get-canvas)])
                     (when canvas
                       (let ([frame (send canvas get-top-level-window)])
                         (when (is-a? frame syncheck-frame<%>)
                           (let ([defs (send frame get-definitions-text)])
                             (when (is-a? defs syncheck-text<%>)
                               (send-msg defs obj))))))))))])
        (send keymap add-function
              "jump to binding occurrence"
              (jump-callback (λ (defs obj) (send defs syncheck:jump-to-binding-occurrence obj))))
        (send keymap add-function
              "jump to next bound occurrence"
              (jump-callback (λ (defs obj) (send defs syncheck:jump-to-next-bound-occurrence obj))))
        (send keymap add-function
              "jump to definition (in other file)"
              (jump-callback (λ (defs obj) (send defs syncheck:jump-to-definition obj)))))
      
      (send keymap map-function "f6" "check syntax")
      (send keymap map-function "c:c;c:c" "check syntax")
      (send keymap map-function "c:x;b" "jump to binding occurrence")
      (send keymap map-function "c:x;n" "jump to next bound occurrence")
      (send keymap map-function "c:x;d" "jump to definition (in other file)"))
    
    (define lexically-bound-variable-style-pref 'drracket:check-syntax:lexically-bound)
    (define imported-variable-style-pref 'drracket:check-syntax:imported)
    (define set!d-variable-style-pref 'drracket:check-syntax:set!d)
    
    (define lexically-bound-variable-style-name (symbol->string lexically-bound-variable-style-pref))
    (define imported-variable-style-name (symbol->string imported-variable-style-pref))
    (define set!d-variable-style-name (symbol->string set!d-variable-style-pref))

    (define my-obligation-style-pref 'drracket:check-syntax:my-obligation-style-pref)
    (define their-obligation-style-pref 'drracket:check-syntax:their-obligation-style-pref)
    (define unk-obligation-style-pref 'drracket:check-syntax:unk-obligation-style-pref)
    (define my-obligation-style-name (symbol->string my-obligation-style-pref))
    (define their-obligation-style-name (symbol->string their-obligation-style-pref))
    (define unk-obligation-style-name (symbol->string unk-obligation-style-pref))
    
    (define error-style-name (fw:scheme:short-sym->style-name 'error))
    ;(define constant-style-name (fw:scheme:short-sym->style-name 'constant))
    
    (define (syncheck-add-to-preferences-panel parent)
      (fw:color-prefs:build-color-selection-panel parent
                                                  lexically-bound-variable-style-pref
                                                  lexically-bound-variable-style-name
                                                  (string-constant cs-lexical-variable))
      (fw:color-prefs:build-color-selection-panel parent
                                                  imported-variable-style-pref
                                                  imported-variable-style-name
                                                  (string-constant cs-imported-variable))
      (fw:color-prefs:build-color-selection-panel parent
                                                  set!d-variable-style-pref
                                                  set!d-variable-style-name
                                                  (string-constant cs-set!d-variable))
      (fw:color-prefs:build-color-selection-panel parent
                                                  my-obligation-style-pref
                                                  my-obligation-style-name
                                                  cs-my-obligation-color)
      (fw:color-prefs:build-color-selection-panel parent
                                                  their-obligation-style-pref
                                                  their-obligation-style-name
                                                  cs-their-obligation-color)
      (fw:color-prefs:build-color-selection-panel parent
                                                  unk-obligation-style-pref
                                                  unk-obligation-style-name
                                                  cs-unk-obligation-color))
    
    (fw:color-prefs:register-color-preference lexically-bound-variable-style-pref
                                              lexically-bound-variable-style-name
                                              (make-object color% 81 112 203)
                                              (make-object color% 50 163 255))
    (fw:color-prefs:register-color-preference set!d-variable-style-pref
                                              set!d-variable-style-name
                                              (send the-color-database find-color "firebrick")
                                              (send the-color-database find-color "pink"))
    (fw:color-prefs:register-color-preference imported-variable-style-pref
                                              imported-variable-style-name
                                              (make-object color% 68 0 203)
                                              (make-object color% 166 0 255))
    (fw:color-prefs:register-color-preference my-obligation-style-pref
                                              my-obligation-style-name
                                              (send the-color-database find-color "firebrick")
                                              (send the-color-database find-color "pink"))
    (fw:color-prefs:register-color-preference their-obligation-style-pref
                                              their-obligation-style-name
                                              (make-object color% 0 116 0)
                                              (send the-color-database find-color "limegreen"))
    (fw:color-prefs:register-color-preference unk-obligation-style-pref
                                              unk-obligation-style-name
                                              (make-object color% 139 142 28)
                                              (send the-color-database find-color "khaki"))
    
    
    
    
    
    ;                                                                                                             
    ;                                                                                                             
    ;                                                                                                             
    ;                                                                                                   ;         
    ;                                                                                                   ;         
    ;                         ;                       ;                                                 ;         
    ;    ;;;  ;     ; ; ;;   ;;;;  ;;;   ;     ;     ;;;;  ; ;  ;;;   ;     ;  ;;;   ; ;   ;;;   ;;;    ;    ;;;  
    ;   ;     ;     ; ;;  ;   ;   ;   ;   ;   ;       ;    ;;  ;   ;   ;   ;  ;   ;  ;;   ;     ;   ;   ;   ;     
    ;   ;;     ;   ;  ;   ;   ;       ;    ; ;        ;    ;       ;   ;   ; ;    ;  ;    ;;        ;   ;   ;;    
    ;    ;;    ;   ;  ;   ;   ;    ;;;;     ;         ;    ;    ;;;;    ; ;  ;;;;;;  ;     ;;    ;;;;   ;    ;;   
    ;      ;    ; ;   ;   ;   ;   ;   ;    ; ;        ;    ;   ;   ;    ; ;  ;       ;       ;  ;   ;   ;      ;  
    ;      ;    ; ;   ;   ;   ;   ;   ;   ;   ;       ;    ;   ;   ;     ;    ;      ;       ;  ;   ;   ;      ;  
    ;   ;;;      ;    ;   ;    ;;  ;;;;; ;     ;       ;;  ;    ;;;;;    ;     ;;;;  ;    ;;;    ;;;;;  ;   ;;;   
    ;            ;                                                                                                
    ;            ;                                                                                                
    ;           ;                                                                                                 
    
    
    
    ;; make-traversal : -> (values (namespace syntax (union #f syntax) -> void)
    ;;                             (namespace string[directory] -> void))
    ;; returns a pair of functions that close over some state that
    ;; represents the top-level of a single program. The first value
    ;; is called once for each top-level expression and the second
    ;; value is called once, after all expansion is complete.
    (define (make-traversal)
      (let* ([tl-low-binders (make-id-set)]
             [tl-high-binders (make-id-set)]
             [tl-low-varrefs (make-id-set)]
             [tl-high-varrefs (make-id-set)]
             [tl-low-varsets (make-id-set)]
             [tl-high-varsets (make-id-set)]
             [tl-low-tops (make-id-set)]
             [tl-high-tops (make-id-set)]
             [tl-templrefs (make-id-set)]
             [tl-requires (make-hash)]
             [tl-require-for-syntaxes (make-hash)]
             [tl-require-for-templates (make-hash)]
             [tl-require-for-labels (make-hash)]
             [expanded-expression
              (λ (user-namespace user-directory sexp jump-to-id)
                (parameterize ([current-load-relative-directory user-directory])
                  (let ([is-module? (syntax-case sexp (module)
                                      [(module . rest) #t]
                                      [else #f])])
                    (cond
                      [is-module?
                       (let ([low-binders (make-id-set)]
                             [high-binders (make-id-set)]
                             [varrefs (make-id-set)]
                             [high-varrefs (make-id-set)]
                             [varsets (make-id-set)]
                             [high-varsets (make-id-set)]
                             [low-tops (make-id-set)]
                             [high-tops (make-id-set)]
                             [templrefs (make-id-set)]
                             [requires (make-hash)]
                             [require-for-syntaxes (make-hash)]
                             [require-for-templates (make-hash)]
                             [require-for-labels (make-hash)])
                          (annotate-basic sexp
                                          user-namespace user-directory jump-to-id
                                          low-binders high-binders
                                          varrefs high-varrefs
                                          varsets high-varsets
                                          low-tops high-tops
                                          templrefs
                                          requires require-for-syntaxes require-for-templates require-for-labels) 
                         (annotate-variables user-namespace
                                             user-directory
                                             low-binders
                                             high-binders
                                             varrefs
                                             high-varrefs
                                             varsets
                                             high-varsets
                                             low-tops
                                             high-tops
                                             templrefs
                                             requires
                                             require-for-syntaxes
                                             require-for-templates
                                             require-for-labels)
                         (annotate-contracts sexp))]
                      [else
                       (annotate-basic sexp
                                       user-namespace user-directory jump-to-id
                                       tl-low-binders tl-high-binders
                                       tl-low-varrefs tl-high-varrefs
                                       tl-low-varsets tl-high-varsets
                                       tl-low-tops tl-high-tops
                                       tl-templrefs
                                       tl-requires
                                       tl-require-for-syntaxes
                                       tl-require-for-templates
                                       tl-require-for-labels)]))))]
             [expansion-completed
              (λ (user-namespace user-directory)
                (parameterize ([current-load-relative-directory user-directory])
                  (annotate-variables user-namespace
                                      user-directory
                                      tl-low-binders
                                      tl-high-binders
                                      tl-low-varrefs
                                      tl-high-varrefs
                                      tl-low-varsets
                                      tl-high-varsets
                                      tl-low-tops
                                      tl-high-tops
                                      tl-templrefs
                                      tl-requires
                                      tl-require-for-syntaxes
                                      tl-require-for-templates
                                      tl-require-for-labels)))])
        (values expanded-expression expansion-completed)))
    
    
    ;; type req/tag = (make-req/tag syntax sexp boolean)
    (define-struct req/tag (req-stx req-sexp used?))
    
    ;; annotate-basic : syntax 
    ;;                  namespace
    ;;                  string[directory]
    ;;                  syntax[id]
    ;;                  id-set (8 of them)
    ;;                  hash-table[require-spec -> syntax] (three of them)
    ;;               -> void
    (define (annotate-basic sexp 
                            user-namespace user-directory jump-to-id
                            low-binders high-binders 
                            low-varrefs high-varrefs 
                            low-varsets high-varsets
                            low-tops high-tops
                            templrefs
                            requires require-for-syntaxes require-for-templates require-for-labels)
      
      (let ([tail-ht (make-hasheq)]
            [maybe-jump
             (λ (vars)
               (when jump-to-id
                 (for-each (λ (id)
                             (let ([binding (identifier-binding id 0)])
                               (when (pair? binding)
                                 (let ([nominal-source-id (list-ref binding 3)])
                                   (when (eq? nominal-source-id jump-to-id)
                                     (jump-to id))))))
                           (syntax->list vars))))])

        (let level-loop ([sexp sexp]
                         [high-level? #f])
          
          (let* ([loop (λ (sexp) (level-loop sexp high-level?))]
                 [varrefs (if high-level? high-varrefs low-varrefs)]
                 [varsets (if high-level? high-varsets low-varsets)]
                 [binders (if high-level? high-binders low-binders)]
                 [tops (if high-level? high-tops low-tops)]
                 [collect-general-info
                  (λ (stx)
                    (add-origins stx varrefs)
                    (add-disappeared-bindings stx binders varrefs)
                    (add-disappeared-uses stx varrefs))])
            (collect-general-info sexp)
            (syntax-case* sexp (#%plain-lambda case-lambda if begin begin0 let-values letrec-values set!
                                               quote quote-syntax with-continuation-mark 
                                               #%plain-app #%top #%plain-module-begin
                                               define-values define-syntaxes define-values-for-syntax module
                                               #%require #%provide #%expression)
              (if high-level? free-transformer-identifier=? free-identifier=?)
              [(#%plain-lambda args bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                 (add-binders (syntax args) binders)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              [(case-lambda [argss bodiess ...]...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each (λ (bodies/stx) (annotate-tail-position/last sexp 
                                                                        (syntax->list bodies/stx)
                                                                        tail-ht))
                           (syntax->list (syntax ((bodiess ...) ...))))
                 (for-each
                  (λ (args bodies)
                    (add-binders args binders)
                    (for-each loop (syntax->list bodies)))
                  (syntax->list (syntax (argss ...)))
                  (syntax->list (syntax ((bodiess ...) ...)))))]
              [(if test then else)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position sexp (syntax then) tail-ht)
                 (annotate-tail-position sexp (syntax else) tail-ht)
                 (loop (syntax test))
                 (loop (syntax else))
                 (loop (syntax then)))]
              [(begin bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              ;; treat a single body expression specially, since this has
              ;; different tail behavior.
              [(begin0 body)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position sexp (syntax body) tail-ht)
                 (loop (syntax body)))]
              
              [(begin0 bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              [(let-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x) (add-binders x binders))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...))))))]
              [(letrec-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x) (add-binders x binders))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...))))))]
              [(set! var e)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 
                 ;; tops are used here because a binding free use of a set!'d variable
                 ;; is treated just the same as (#%top . x).
                 (when (syntax-original? (syntax var))
                   (add-id varsets (syntax var))
                   (if (identifier-binding (syntax var) 0)
                       (add-id varrefs (syntax var))
                       (add-id tops (syntax var))))
                 
                 (loop (syntax e)))]
              [(quote datum)
               ;(color-internal-structure (syntax datum) constant-style-name 'default-mode)
               (annotate-raw-keyword sexp varrefs)]
              [(quote-syntax datum)
               ;(color-internal-structure (syntax datum) constant-style-name 'default-mode)
               (annotate-raw-keyword sexp varrefs)
               (let loop ([stx #'datum])
                 (cond [(identifier? stx)
                        (when (syntax-original? stx)
                          (add-id templrefs stx))]
                       [(syntax? stx)
                        (loop (syntax-e stx))]
                       [(pair? stx)
                        (loop (car stx))
                        (loop (cdr stx))]
                       [(vector? stx)
                        (for-each loop (vector->list stx))]
                       [(box? stx)
                        (loop (unbox stx))]
                       [else (void)]))]
              [(with-continuation-mark a b c)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position sexp (syntax c) tail-ht)
                 (loop (syntax a))
                 (loop (syntax b))
                 (loop (syntax c)))]
              [(#%plain-app pieces ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each loop (syntax->list (syntax (pieces ...)))))]
              [(#%top . var)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (when (syntax-original? (syntax var))
                   (add-id tops (syntax var))))]
              [(define-values vars b)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax vars) binders)
                 (maybe-jump (syntax vars))
                 (loop (syntax b)))]
              [(define-syntaxes names exp)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax names) binders)
                 (maybe-jump (syntax names))
                 (level-loop (syntax exp) #t))]
              [(define-values-for-syntax names exp)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax names) high-binders)
                 (maybe-jump (syntax names))
                 (level-loop (syntax exp) #t))]
              [(module m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 ((annotate-require-open user-namespace user-directory) (syntax lang))
                 
                 (hash-cons! requires (syntax->datum (syntax lang)) (syntax lang))
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              ; top level or module top level only:
              [(#%require require-specs ...)
               (let ([at-phase
                      (lambda (stx requires)
                        (syntax-case stx ()
                          [(_ require-specs ...)
                           (with-syntax ([((require-specs ...) ...)
                                          (map (lambda (spec)
                                                 (syntax-case spec (just-meta)
                                                   [(just-meta m spec ...)
                                                    #'(spec ...)]
                                                   [else (list spec)]))
                                               (syntax->list #'(require-specs ...)))])
                             (let ([new-specs (map trim-require-prefix
                                                   (syntax->list (syntax (require-specs ... ...))))])
                               (annotate-raw-keyword sexp varrefs)
                               (for-each (annotate-require-open user-namespace
                                                                user-directory)
                                         new-specs)
                               (for-each (add-require-spec requires)
                                         new-specs
                                         (syntax->list (syntax (require-specs ... ...))))))]))])
                 (for-each (lambda (spec)
                             (let loop ([spec spec])
                               (syntax-case* spec (for-syntax for-template for-label for-meta just-meta) 
                                             (lambda (a b)
                                               (eq? (syntax-e a) (syntax-e b)))
                                 [(just-meta phase specs ...)
                                  (for-each loop (syntax->list #'(specs ...)))]
                                 [(for-syntax specs ...)
                                  (at-phase spec require-for-syntaxes)]
                                 [(for-meta 1 specs ...)
                                  (at-phase #'(for-syntax specs ...) require-for-syntaxes)]
                                 [(for-template specs ...)
                                  (at-phase spec require-for-templates)]
                                 [(for-meta -1 specs ...)
                                  (at-phase #'(for-template specs ...) require-for-templates)]
                                 [(for-label specs ...)
                                  (at-phase spec require-for-labels)]
                                 [(for-meta #f specs ...)
                                  (at-phase #'(for-label specs ...) require-for-labels)]
                                 [(for-meta 0 specs ...)
                                  (at-phase #'(for-run specs ...) requires)]
                                 [(for-meta . _) (void)]
                                 [else
                                  (at-phase (list #f spec) requires)])))
                           (syntax->list #'(require-specs ...))))]
              
              ; module top level only:
              [(#%provide provide-specs ...)
               (let ([provided-varss (map extract-provided-vars
                                          (syntax->list (syntax (provide-specs ...))))])
                 (annotate-raw-keyword sexp varrefs)
                 (for-each (λ (provided-vars)
                             (for-each
                              (λ (provided-var)
                                (when (syntax-original? provided-var)
                                  (add-id varrefs provided-var)))
                              provided-vars))
                           provided-varss))]
              
              [(#%expression arg)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (loop #'arg))]
              [id
               (identifier? (syntax id))
               (when (syntax-original? sexp)
                 (add-id varrefs sexp))]
              [_
               (begin
                 #;
                 (printf "unknown stx: ~e datum: ~e source: ~e\n"
                         sexp
                         (and (syntax? sexp)
                              (syntax->datum sexp))
                         (and (syntax? sexp)
                              (syntax-source sexp)))
                 (void))])))
        (add-tail-ht-links tail-ht)))
    
    (define (hash-cons! ht k v)
      (hash-set! ht k (cons v (hash-ref ht k '()))))
    
    ;; add-disappeared-bindings : syntax id-set -> void
    (define (add-disappeared-bindings stx binders disappaeared-uses)
      (let ([prop (syntax-property stx 'disappeared-binding)])
        (when prop
          (let loop ([prop prop])
            (cond
              [(pair? prop)
               (loop (car prop))
               (loop (cdr prop))]
              [(identifier? prop)
               (add-origins prop disappaeared-uses)
               (add-id binders prop)])))))
    
    ;; add-disappeared-uses : syntax id-set -> void
    (define (add-disappeared-uses stx id-set)
      (let ([prop (syntax-property stx 'disappeared-use)])
        (when prop
          (let loop ([prop prop])
            (cond
              [(pair? prop)
               (loop (car prop))
               (loop (cdr prop))]
              [(identifier? prop)
               (add-id id-set prop)])))))
    
    ;; add-require-spec : hash-table[sexp[require-spec] -o> (listof syntax)]
    ;;                 -> sexp[require-spec]
    ;;                    syntax
    ;;                 -> void
    (define (add-require-spec require-ht)
      (λ (raw-spec syntax)
        (when (syntax-original? syntax)
          (let ([key (syntax->datum raw-spec)])
            (hash-set! require-ht
                       key
                       (cons syntax
                             (hash-ref require-ht
                                       key
                                       (λ () '()))))))))
    
    ;; annotate-variables : namespace directory string id-set[four of them] (listof syntax) (listof syntax) -> void
    ;; colors in and draws arrows for variables, according to their classifications
    ;; in the various id-sets
    (define (annotate-variables user-namespace
                                user-directory
                                low-binders
                                high-binders
                                low-varrefs
                                high-varrefs
                                low-varsets
                                high-varsets
                                low-tops
                                high-tops
                                templrefs
                                requires
                                require-for-syntaxes
                                require-for-templates
                                require-for-labels)
      
      (let ([rename-ht
             ;; hash-table[(list source number number) -> (listof syntax)]
             (make-hash)]
            [unused-requires (make-hash)]
            [unused-require-for-syntaxes (make-hash)]
            [unused-require-for-templates (make-hash)]
            [unused-require-for-labels (make-hash)]
            [requires/phases (make-hash)]
            [unused/phases (make-hash)]
            ;; there is no define-for-template form, thus no for-template binders
            [template-binders (make-id-set)]
            [label-binders (make-id-set)]
            [id-sets (list low-binders high-binders low-varrefs high-varrefs low-tops high-tops)])
        
        (hash-set! requires/phases 0 requires)
        (hash-set! requires/phases 1 require-for-syntaxes)
        (hash-set! requires/phases -1 require-for-templates)
        (hash-set! requires/phases #f require-for-labels)

        (hash-set! unused/phases 0 unused-requires)
        (hash-set! unused/phases 1 unused-require-for-syntaxes)
        (hash-set! unused/phases -1 unused-require-for-templates)
        (hash-set! unused/phases #f unused-require-for-labels)
        
        (hash-for-each requires
                       (λ (k v) (hash-set! unused-requires k #t)))
        (hash-for-each require-for-syntaxes
                       (λ (k v) (hash-set! unused-require-for-syntaxes k #t)))
        (hash-for-each require-for-templates
                       (lambda (k v) (hash-set! unused-require-for-templates k #t)))
        (hash-for-each require-for-labels
                       (lambda (k v) (hash-set! unused-require-for-labels k #t)))
        
        (let ([handle-var-bind
               (λ (var varsets)
                 (when (syntax-original? var)
                   (color-variable var 0 varsets)
                   (document-variable var 0)
                   (record-renamable-var rename-ht var)))])
          (for-each (λ (vars) 
                      (for-each (λ (var) (handle-var-bind var high-varsets)) 
                                vars))
                    (get-idss high-binders))
          (for-each (λ (vars) 
                      (for-each (λ (var) (handle-var-bind var low-varsets)) 
                                vars))
                    (get-idss low-binders)))
        
        
        (let ([handle-var-ref
               (λ (var index binders varsets)
                 (color-variable var index varsets)
                 (when (syntax-original? var)
                   (document-variable var index))
                 (connect-identifier var
                                     rename-ht
                                     binders
                                     unused/phases
                                     requires/phases
                                     index
                                     user-namespace 
                                     user-directory
                                     #t))])
          (for-each (λ (vars) (for-each 
                               (λ (var) (handle-var-ref var 0 low-binders low-varsets))
                               vars))
                    (get-idss low-varrefs))
          
          (for-each (λ (vars) (for-each 
                               (λ (var) (handle-var-ref var 1 high-binders high-varsets))
                               vars))
                    (get-idss high-varrefs)))
        
        (for-each (lambda (vars) (for-each
                                  (lambda (var)
                                    ;; no color variable
                                    (connect-identifier var
                                                        rename-ht
                                                        low-binders
                                                        unused/phases
                                                        requires/phases
                                                        0
                                                        user-namespace
                                                        user-directory
                                                        #f)
                                    (connect-identifier var
                                                        rename-ht
                                                        high-binders
                                                        unused/phases
                                                        requires/phases
                                                        1
                                                        user-namespace
                                                        user-directory
                                                        #f)
                                    (connect-identifier var
                                                        rename-ht
                                                        template-binders ;; dummy; always empty
                                                        unused/phases
                                                        requires/phases
                                                        -1
                                                        user-namespace
                                                        user-directory
                                                        #f)
                                    (connect-identifier var
                                                        rename-ht
                                                        label-binders ;; dummy; always empty
                                                        unused/phases
                                                        requires/phases
                                                        #f
                                                        user-namespace
                                                        user-directory
                                                        #f))
                                  vars))
                  (get-idss templrefs))
        
        (for-each 
         (λ (vars) 
           (for-each
            (λ (var) 
              (color/connect-top rename-ht user-namespace user-directory low-binders var))
            vars))
         (get-idss low-tops))
        
        (for-each 
         (λ (vars) 
           (for-each
            (λ (var) 
              (color/connect-top rename-ht user-namespace user-directory high-binders var))
            vars))
         (get-idss high-tops))
        
        (color-unused require-for-labels unused-require-for-labels)
        (color-unused require-for-templates unused-require-for-templates)
        (color-unused require-for-syntaxes unused-require-for-syntaxes)
        (color-unused requires unused-requires)
        (hash-for-each rename-ht (lambda (k stxs) (make-rename-menu stxs id-sets)))))
    
    (define (annotate-contracts stx)
      (define start-map (make-hash))
      (define arrow-map (make-hash))
      (define domain-map (make-hash))
      (define range-map (make-hash))
      (define (add-to-map stx prop map)
        (let loop ([val (syntax-property stx prop)])
          (cond
            [(symbol? val)
             (hash-set! map val (cons stx (hash-ref map val '())))]
            [(pair? val)
             (loop (car val))
             (loop (cdr val))])))
      
      (let loop ([stx stx])
        (add-to-map stx 'racket/contract:contract-on-boundary start-map)
        (add-to-map stx 'racket/contract:domain-of domain-map)
        (add-to-map stx 'racket/contract:rng-of range-map)
        (add-to-map stx 'racket/contract:function-contract arrow-map)
        (syntax-case stx ()
          [(a . b) (loop #'a) (loop #'b)]
          [else (void)])))
    
#|
    (define (annotate-contracts stx)
      (let loop ([stx stx])
        (let sloop ([prop (syntax-property stx 'provide/contract-original-contract)])
          (cond
            [(vector? prop)
             (color-obligations (vector-ref prop 1))]
            [(pair? prop) (sloop (car prop)) 
                          (sloop (cdr prop))]))
        (syntax-case stx ()
          [(a . b) (loop #'a) (loop #'b)]
          [else (void)])))
    
    (define (color-obligations stx)
      (let loop ([stx stx]
                 [polarity #t])
      (syntax-case stx (->)
        [(-> a ... rng)
         (begin
           (base-color (car (syntax-e stx)) polarity)
           (for-each (λ (x) (loop x (not polarity))) (syntax->list #'(a ...)))
           (syntax-case #'rng (values any)
             [(values b ...)
              (for-each (λ (x) (loop x polarity)) (syntax->list #'(b ...)))]
             [any
              (void)]
             [rng
              (loop #'rng polarity)]))]
        [id
         (and (identifier? #'id)
              (known-predicate? #'id))
         (base-color stx polarity)]
        [else
         (color stx unk-obligation-style-name 'contract-mode)])))
|#
    ;; returns #t if the result is known to be a predicate that shoudl correspond to a
    ;; complete obligation for the contract. If it is some unknown variable, this variable
    ;; may refer to some other contract with nested obligations, so we have to return #f here.
    ;; approximate this by just asking 'did this identifier come from the core?' (which is known
    ;; to not bind any contracts (I hope))
    (define (known-predicate? id)
      (let ([ib (identifier-binding id)])
        (and (list? ib)
             (let ([src (list-ref ib 0)])
               (let-values ([(base rel) (module-path-index-split src)])
                 (member base '('#%kernel racket racket/base scheme scheme/base)))))))
    
    (define (base-color stx polarity)
      (color stx 
             (if polarity my-obligation-style-name their-obligation-style-name) 
             'contract-mode))
        
    ;; record-renamable-var : rename-ht syntax -> void
    (define (record-renamable-var rename-ht stx)
      (let ([key (list (syntax-source stx) (syntax-position stx) (syntax-span stx))])
        (hash-set! rename-ht
                   key
                   (cons stx (hash-ref rename-ht key '())))))
    
    ;; color-unused : hash-table[sexp -o> syntax] hash-table[sexp -o> #f] -> void
    (define (color-unused requires unused)
      (hash-for-each
       unused
       (λ (k v)
         (for-each (λ (stx) (color stx error-style-name 'default-mode))
                   (hash-ref requires k)))))
    
    ;; connect-identifier : syntax
    ;;                      id-set 
    ;;                      (union #f hash-table)
    ;;                      (union #f hash-table)
    ;;                      integer or 'lexical or #f
    ;;                      (listof id-set)
    ;;                      namespace
    ;;                      directory
    ;;                      boolean
    ;;                   -> void
    ;; adds arrows and rename menus for binders/bindings
    (define (connect-identifier var rename-ht all-binders
                                unused/phases requires/phases
                                phase-level user-namespace user-directory actual?)
      (connect-identifier/arrow var all-binders 
                                unused/phases requires/phases
                                phase-level user-namespace user-directory actual?)
      (when (and actual? (get-ids all-binders var))
        (record-renamable-var rename-ht var)))
    
    ;; id-level : integer-or-#f-or-'lexical identifier -> symbol
    (define (id-level phase-level id)
      (define (self-module? mpi)
        (let-values ([(a b) (module-path-index-split mpi)])
          (and (not a) (not b))))
      (let ([binding (identifier-binding id phase-level)])
        (cond [(list? binding)
               (if (self-module? (car binding))
                   'top-level
                   'imported)]
              [(eq? binding 'lexical) 'lexical]
              [else 'top-level])))
    
    ;; connect-identifier/arrow : syntax
    ;;                            id-set 
    ;;                            (union #f hash-table)
    ;;                            (union #f hash-table)
    ;;                            (union identifier-binding identifier-transformer-binding)
    ;;                            boolean
    ;;                         -> void
    ;; adds the arrows that correspond to binders/bindings
    (define (connect-identifier/arrow var all-binders unused/phases requires/phases phase-level user-namespace user-directory actual?)
      (let ([binders (get-ids all-binders var)])
        (when binders
          (for-each (λ (x)
                      (when (syntax-original? x)
                        (connect-syntaxes x var actual? (id-level phase-level x))))
                    binders))
        
        (when (and unused/phases requires/phases)
          (let ([req-path/pr (get-module-req-path (identifier-binding var phase-level)
                                                  phase-level)]
                [source-req-path/pr (get-module-req-path (identifier-binding var phase-level)
                                                          phase-level
                                                          #:nominal? #f)])
            (when (and req-path/pr source-req-path/pr)
              (let* ([req-path (list-ref req-path/pr 0)]
                     [id (list-ref req-path/pr 1)]
                     [source-req-path (list-ref source-req-path/pr 3)]
                     [source-id (list-ref source-req-path/pr 1)]
                     [req-phase-level (list-ref req-path/pr 2)]
                     [unused (hash-ref unused/phases req-phase-level)]
                     [requires (hash-ref requires/phases req-phase-level)]
                     [req-stxes (hash-ref requires req-path (λ () #f))])
                (when req-stxes
                  (hash-remove! unused req-path)
                  (for-each (λ (req-stx) 
                              (when (id/require-match? (syntax->datum var) 
                                                       id 
                                                       (syntax->datum req-stx))
                                (when id
                                  (let ([filename (get-require-filename source-req-path user-namespace user-directory)])
                                    (when filename
                                      (add-jump-to-definition
                                       var
                                       source-id
                                       filename))))
                                (add-mouse-over var
                                                (fw:gui-utils:format-literal-label 
                                                 (string-constant cs-mouse-over-import)
                                                 (syntax-e var)
                                                 req-path))
                                (connect-syntaxes req-stx var actual?
                                                  (id-level phase-level var))))
                            req-stxes))))))))
    
    (define (id/require-match? var id req-stx)
      (cond
        [(and (pair? req-stx)
              (eq? (list-ref req-stx 0) 'prefix))
         (let ([prefix (list-ref req-stx 1)])
           (equal? (format "~a~a" prefix id)
                   (symbol->string var)))]
        [(and (pair? req-stx)
              (eq? (list-ref req-stx 0) 'prefix-all-except))
         (let ([prefix (list-ref req-stx 1)])
           (and (not (memq id (cdddr req-stx)))
                (equal? (format "~a~a" prefix id)
                        (symbol->string var))))]
        [(and (pair? req-stx)
              (eq? (list-ref req-stx 0) 'rename))
         (eq? (list-ref req-stx 2)
              var)]
        [else (eq? var id)]))
    
    
    ;; get-module-req-path : binding number [#:nominal? boolean] -> (union #f (list require-sexp sym ?? module-path))
    ;; argument is the result of identifier-binding or identifier-transformer-binding
    (define (get-module-req-path binding phase-level #:nominal? [nominal-source-path? #t])
      (and (pair? binding)
           (or (not (number? phase-level))
               (= phase-level
                  (+ (list-ref binding 5)
                     (list-ref binding 6))))
           (let ([mod-path (if nominal-source-path? (list-ref binding 2) (list-ref binding 0))])
             (cond
               [(module-path-index? mod-path)
                (let-values ([(base offset) (module-path-index-split mod-path)])
                  (list base
                        (if nominal-source-path? (list-ref binding 3) (list-ref binding 1))
                        (list-ref binding 5)
                        mod-path))]
               [(symbol? mod-path)
                (list mod-path 
                      (if nominal-source-path? (list-ref binding 3) (list-ref binding 1))
                      (list-ref binding 5)
                      mod-path)]
               [else #f]))))
    
    ;; color/connect-top : namespace directory id-set syntax -> void
    (define (color/connect-top rename-ht user-namespace user-directory binders var)
      (let ([top-bound?
             (or (get-ids binders var)
                 (parameterize ([current-namespace user-namespace])
                   (let/ec k
                     (namespace-variable-value (syntax-e var) #t (λ () (k #f)))
                     #t)))])
        (if top-bound?
            (color var lexically-bound-variable-style-name 'default-mode)
            (color var error-style-name 'default-mode))
        (connect-identifier var rename-ht binders #f #f 0 user-namespace user-directory #t)))
    
    ;; color-variable : syntax phase-level module-identifier-mapping -> void
    (define (color-variable var phase-level varsets)
      (let* ([b (identifier-binding var phase-level)]
             [lexical? 
              (or (not b)
                  (eq? b 'lexical)
                  (and (pair? b)
                       (let ([path (caddr b)])
                         (and (module-path-index? path)
                              (let-values ([(a b) (module-path-index-split path)])
                                (and (not a)
                                     (not b)))))))])
        (cond
          [(get-ids varsets var)
           (color var set!d-variable-style-name 'default-mode)]
          [lexical? (color var lexically-bound-variable-style-name 'default-mode)]
          [(pair? b) (color var imported-variable-style-name 'default-mode)])))
    
    ;; add-var : hash-table -> syntax -> void
    ;; adds the variable to the hash table.
    (define (add-var ht)
      (λ (var)
        (let* ([key (syntax-e var)]
               [prev (hash-ref ht key (λ () null))])
          (hash-set! ht key (cons var prev)))))
    
    ;; connect-syntaxes : syntax[original] syntax[original] boolean symbol -> void
    ;; adds an arrow from `from' to `to', unless they have the same source loc. 
    (define (connect-syntaxes from to actual? level)
      (let ([from-source (find-source-editor from)] 
            [to-source (find-source-editor to)]
            [defs-text (get-defs-text)])
        (when (and from-source to-source defs-text)
          (let ([pos-from (syntax-position from)]
                [span-from (syntax-span from)]
                [pos-to (syntax-position to)]
                [span-to (syntax-span to)])
            (when (and pos-from span-from pos-to span-to)
              (let* ([from-pos-left (- (syntax-position from) 1)]
                     [from-pos-right (+ from-pos-left (syntax-span from))]
                     [to-pos-left (- (syntax-position to) 1)]
                     [to-pos-right (+ to-pos-left (syntax-span to))])
                (unless (= from-pos-left to-pos-left)
                  (send defs-text syncheck:add-arrow
                        from-source from-pos-left from-pos-right
                        to-source to-pos-left to-pos-right
                        actual? level))))))))
    
    ;; add-mouse-over : syntax[original] string -> void
    ;; registers the range in the editor so that a mouse over
    ;; this area shows up in the status line.
    (define (add-mouse-over stx str)
      (let* ([source (find-source-editor stx)]
             [defs-text (get-defs-text)])
        (when (and defs-text 
                   source
                   (syntax-position stx)
                   (syntax-span stx))
          (let* ([pos-left (- (syntax-position stx) 1)]
                 [pos-right (+ pos-left (syntax-span stx))])
            (send defs-text syncheck:add-mouse-over-status
                  source pos-left pos-right str)))))
    
    ;; add-jump-to-definition : syntax symbol path -> void
    ;; registers the range in the editor so that the
    ;; popup menu in this area allows the programmer to jump
    ;; to the definition of the id.
    (define (add-jump-to-definition stx id filename)
      (let ([source (find-source-editor stx)]
            [defs-text (get-defs-text)])
        (when (and source 
                   defs-text
                   (syntax-position stx)
                   (syntax-span stx))
          (let* ([pos-left (- (syntax-position stx) 1)]
                 [pos-right (+ pos-left (syntax-span stx))])
            (send defs-text syncheck:add-jump-to-definition
                  source
                  pos-left
                  pos-right
                  id
                  filename)))))
    
    ;; find-syncheck-text : text% -> (union #f (is-a?/c syncheck-text<%>))
    (define (find-syncheck-text text)
      (let loop ([text text])
        (cond
          [(is-a? text syncheck-text<%>) text]
          [else 
           (let ([admin (send text get-admin)])
             (and (is-a? admin editor-snip-editor-admin<%>)
                  (let* ([enclosing-editor-snip (send admin get-snip)]
                         [editor-snip-admin (send enclosing-editor-snip get-admin)]
                         [enclosing-editor (send editor-snip-admin get-editor)])
                    (loop enclosing-editor))))])))
    
    ;; annotate-tail-position/last : (listof syntax) -> void
    (define (annotate-tail-position/last orig-stx stxs tail-ht)
      (unless (null? stxs)
        (annotate-tail-position orig-stx (car (last-pair stxs)) tail-ht)))
    
    ;; annotate-tail-position : syntax -> void
    ;; colors the parens (if any) around the argument
    ;; to indicate this is a tail call.
    (define (annotate-tail-position orig-stx tail-stx tail-ht)
      (hash-set!
       tail-ht 
       orig-stx 
       (cons
        tail-stx
        (hash-ref 
         tail-ht
         orig-stx
         (λ () null)))))
    
    ;; annotate-require-open : namespace string -> (stx -> void)
    ;; relies on current-module-name-resolver, which in turn depends on
    ;; current-directory and current-namespace
    (define (annotate-require-open user-namespace user-directory)
      (λ (require-spec)
        (when (syntax-original? require-spec)
          (let ([source (find-source-editor require-spec)])
            (when (and (is-a? source text%)
                       (syntax-position require-spec)
                       (syntax-span require-spec))
              (let ([defs-text (get-defs-text)])
                (when defs-text
                  (let* ([start (- (syntax-position require-spec) 1)]
                         [end (+ start (syntax-span require-spec))]
                         [file (get-require-filename (syntax->datum require-spec)
                                                     user-namespace
                                                     user-directory)])
                    (when file
                      (send defs-text syncheck:add-menu
                            source
                            start end 
                            #f
                            (make-require-open-menu file)))))))))))
    
    ;; get-require-filename : sexp-or-module-path-index namespace string[directory] -> filename or #f
    ;; finds the filename corresponding to the require in stx
    (define (get-require-filename datum user-namespace user-directory)
      (parameterize ([current-namespace user-namespace]
                     [current-directory user-directory]
                     [current-load-relative-directory user-directory])
        (let* ([rkt-path/mod-path
                (with-handlers ([exn:fail? (λ (x) #f)])
                  (cond
                    [(module-path-index? datum)
                     (resolved-module-path-name 
                      (module-path-index-resolve datum))]
                    [else
                     (resolved-module-path-name 
                      ((current-module-name-resolver) datum #f #f))]))]
               [rkt-path/f (and (path? rkt-path/mod-path) rkt-path/mod-path)])
          (let/ec k
            (unless (path? rkt-path/f) (k rkt-path/f))
            (when (file-exists? rkt-path/f) (k rkt-path/f))
            (let* ([bts (path->bytes rkt-path/f)]
                   [len (bytes-length bts)])
              (unless (and (len . >= . 4) 
                           (bytes=? #".rkt" (subbytes bts (- len 4))))
                (k rkt-path/f))
              (let ([ss-path (bytes->path (bytes-append (subbytes bts 0 (- len 4)) #".ss"))])
                (unless (file-exists? ss-path)
                  (k rkt-path/f))
                ss-path))))))
    
    ;; make-require-open-menu : path -> menu -> void
    (define (make-require-open-menu file)
      (λ (menu)
        (let-values ([(base name dir?) (split-path file)])
          (instantiate menu-item% ()
            (label (fw:gui-utils:format-literal-label (string-constant cs-open-file) (path->string name)))
            (parent menu)
            (callback (λ (x y) (fw:handler:edit-file file))))
          (void))))
    
    ;; possible-suffixes : (listof string)
    ;; these are the suffixes that are checked for the reverse 
    ;; module-path mapping.
    (define possible-suffixes '(".rkt" ".ss" ".scm" ""))
    
    ;; module-name-sym->filename : symbol -> (union #f string)
    (define (module-name-sym->filename sym)
      (let ([str (symbol->string sym)])
        (and ((string-length str) . > . 1)
             (char=? (string-ref str 0) #\,)
             (let ([fn (substring str 1 (string-length str))])
               (ormap (λ (x)
                        (let ([test (string->path (string-append fn x))])
                          (and (file-exists? test)
                               test)))
                      possible-suffixes)))))
    
    ;; add-origins : sexp id-set -> void
    (define (add-origins sexp id-set)
      (let ([origin (syntax-property sexp 'origin)])
        (when origin
          (let loop ([ct origin])
            (cond
              [(pair? ct) 
               (loop (car ct))
               (loop (cdr ct))]
              [(syntax? ct) 
               (when (syntax-original? ct)
                 (add-id id-set ct))]
              [else (void)])))))
    
    ;; FIXME: handle for-template and for-label
    ;; extract-provided-vars : syntax -> (listof syntax[identifier])
    (define (extract-provided-vars stx)
      (syntax-case* stx (rename struct all-from all-from-except all-defined-except) symbolic-compare?
        [identifier
         (identifier? (syntax identifier))
         (list (syntax identifier))]
        
        [(rename local-identifier export-identifier) 
         (list (syntax local-identifier))]
        
        ;; why do I even see this?!?
        [(struct struct-identifier (field-identifier ...))
         null]
        
        [(all-from module-name) null] 
        [(all-from-except module-name identifier ...)
         null]
        [(all-defined-except identifier ...)
         (syntax->list #'(identifier ...))]
        [_ 
         null]))
    
    
    ;; trim-require-prefix : syntax -> syntax
    (define (trim-require-prefix require-spec)
      (syntax-case* require-spec (only prefix all-except prefix-all-except rename just-meta) symbolic-compare?
        [(only module-name identifer ...)
         (syntax module-name)]
        [(prefix identifier module-name) 
         (syntax module-name)]
        [(all-except module-name identifer ...)
         (syntax module-name)]
        [(prefix-all-except module-name identifer ...)
         (syntax module-name)]
        [(rename module-name local-identifer exported-identifer)
         (syntax module-name)]
        [_ require-spec]))
    
    (define (symbolic-compare? x y) (eq? (syntax-e x) (syntax-e y)))
    
    ;; add-binders : syntax id-set -> void
    ;; transforms an argument list into a bunch of symbols/symbols
    ;; and puts them into the id-set
    ;; effect: colors the identifiers
    (define (add-binders stx id-set)
      (let loop ([stx stx])
        (let ([e (if (syntax? stx) (syntax-e stx) stx)])
          (cond
            [(cons? e)
             (let ([fst (car e)]
                   [rst (cdr e)])
               (if (syntax? fst)
                   (begin
                     (when (syntax-original? fst)
                       (add-id id-set fst))
                     (loop rst))
                   (loop rst)))]
            [(null? e) (void)]
            [else 
             (when (syntax-original? stx)
               (add-id id-set stx))]))))
    
    ;; annotate-raw-keyword : syntax id-map -> void
    ;; annotates keywords when they were never expanded. eg.
    ;; if someone just types `(λ (x) x)' it has no 'origin
    ;; field, but there still are keywords.
    (define (annotate-raw-keyword stx id-map)
      (let ([lst (syntax-e stx)])
        (when (pair? lst)
          (let ([f-stx (car lst)])
            (when (and (syntax-original? f-stx)
                       (identifier? f-stx))
              (add-id id-map f-stx))))))
    
    ;; color-internal-structure : syntax str -> void
    (define (color-internal-structure stx style-name mode)
      (let ([ht (make-hasheq)]) 
        ;; ht : stx -o> true
        ;; indicates if we've seen this syntax object before
        
        (let loop ([stx stx]
                   [datum (syntax->datum stx)])
          (unless (hash-ref ht datum (λ () #f))
            (hash-set! ht datum #t)
            (cond
              [(pair? stx) 
               (loop (car stx) (car datum))
               (loop (cdr stx) (cdr datum))]
              [(syntax? stx)
               (when (syntax-original? stx)
                 (color stx style-name mode))
               (let ([stx-e (syntax-e stx)]) 
                 (cond
                   [(cons? stx-e)
                    (loop (car stx-e) (car datum))
                    (loop (cdr stx-e) (cdr datum))]
                   [(null? stx-e)
                    (void)]
                   [(vector? stx-e)
                    (for-each loop
                              (vector->list stx-e)
                              (vector->list datum))]
                   [(box? stx-e)
                    (loop (unbox stx-e) (unbox datum))]
                   [else (void)]))])))))
    
    ;; jump-to : syntax -> void
    (define (jump-to stx)
      (let ([src (find-source-editor stx)]
            [pos (syntax-position stx)]
            [span (syntax-span stx)])
        (when (and (is-a? src text%)
                   pos
                   span)
          (send src set-position (- pos 1) (+ pos span -1)))))
    
    ;; color : syntax[original] str -> void
    ;; colors the syntax with style-name's style
    (define (color stx style-name mode)
      (let ([source (find-source-editor stx)])
        (when (and (is-a? source text%)
                   (syntax-position stx)
                   (syntax-span stx))
          (let ([pos (- (syntax-position stx) 1)]
                [span (syntax-span stx)])
            (color-range source pos (+ pos span) style-name mode)))))
    
    ;; color-range : text start finish style-name 
    ;; colors a range in the text based on `style-name'
    (define (color-range source start finish style-name mode)
      (let ([style (send (send source get-style-list)
                         find-named-style
                         style-name)])
        (apply-style/remember source start finish style mode)))
    
    ;; hash-table[syntax -o> (listof syntax)] -> void
    (define (add-tail-ht-links tail-ht)
      (begin
        (collapse-tail-links tail-ht)
        (hash-for-each
         tail-ht
         (λ (stx-from stx-tos)
           (for-each (λ (stx-to) (add-tail-ht-link stx-from stx-to))
                     stx-tos)))))
    
    ;; hash-table[syntax -o> (listof syntax)] -> void
    ;; take something like a transitive closure, except
    ;; only when there are non-original links in between
    
    (define (collapse-tail-links tail-ht)
      (let loop ()
        (let ([found-one? #f])
          (hash-for-each
           tail-ht
           (λ (stx-from stx-tos)
             (for-each
              (λ (stx-to)
                (let ([stx-to-tos (hash-ref tail-ht stx-to '())])
                  (for-each
                   (λ (stx-to-to)
                     (unless (and (add-tail-link? stx-from stx-to) 
                                  (add-tail-link? stx-to stx-to-to))
                       (unless (memq stx-to-to (hash-ref tail-ht stx-from '()))
                         (set! found-one? #t)
                         (hash-cons! tail-ht stx-from stx-to-to))))
                   stx-to-tos)))
              stx-tos)))
          
          ;; this takes O(n^3) in general, so we just do
          ;; one iteration. This doesn't work for case 
          ;; expressions but it seems to for most others.
          ;; turning this on makes this function go from about
          ;; 55 msec to about 2400 msec on my laptop, 
          ;; (a 43x slowdown) when checking the syntax of this file.
        
          #;
          (when found-one?
            (loop)))))
    
    ;; add-tail-ht-link : syntax syntax -> void
    (define (add-tail-ht-link from-stx to-stx)
      (let* ([to-src (find-source-editor to-stx)]
             [from-src (find-source-editor from-stx)]
             [defs-text (get-defs-text)])
        (when (and to-src from-src defs-text)
	  (let ([from-pos (syntax-position from-stx)]
		[to-pos (syntax-position to-stx)])
	    (when (and from-pos to-pos)
	      (send defs-text syncheck:add-tail-arrow
		    from-src (- from-pos 1)
		    to-src (- to-pos 1)))))))
    
    ;; add-tail-link? : syntax syntax -> boolean
    (define (add-tail-link? from-stx to-stx)
      (let* ([to-src (find-source-editor to-stx)]
             [from-src (find-source-editor from-stx)]
             [defs-text (get-defs-text)])
        (and to-src from-src defs-text
             (let ([from-pos (syntax-position from-stx)]
                   [to-pos (syntax-position to-stx)])
               (and from-pos to-pos)))))
    
    ;; apply-style/remember : (is-a?/c editor<%>) number number style% symbol -> void
    (define (apply-style/remember ed start finish style mode)
      (let ([outermost (find-outermost-editor ed)])
        (and (is-a? outermost drracket:unit:definitions-text<%>)
             (send outermost syncheck:apply-style/remember ed start finish style mode))))
    
    (define (find-outermost-editor ed)
      (let loop ([ed ed])
        (let ([admin (send ed get-admin)])
          (if (is-a? admin editor-snip-editor-admin<%>)
              (let* ([enclosing-snip (send admin get-snip)]
                     [enclosing-snip-admin (send enclosing-snip get-admin)])
                (loop (send enclosing-snip-admin get-editor)))
              ed))))

    ;; find-source-editor : stx -> editor or false
    (define (find-source-editor stx)
      (let ([defs-text (get-defs-text)])
        (and defs-text 
             (find-source-editor/defs stx defs-text))))
    
    ;; find-source-editor : stx text -> editor or false
    (define (find-source-editor/defs stx defs-text)
      (cond
        [(not (syntax-source stx)) #f]
        [(and (symbol? (syntax-source stx))
              (text:lookup-port-name (syntax-source stx)))
         => values]
        [else
         (let txt-loop ([text defs-text])
           (cond
             [(and (is-a? text fw:text:basic<%>)
                   (send text port-name-matches? (syntax-source stx)))
              text]
             [else
              (let snip-loop ([snip (send text find-first-snip)])
                (cond
                  [(not snip)
                   #f]
                  [(and (is-a? snip editor-snip%)
                        (send snip get-editor))
                   (or (txt-loop (send snip get-editor))
                       (snip-loop (send snip next)))]
                  [else 
                   (snip-loop (send snip next))]))]))]))
    ;; get-defs-text : -> text or false
    (define (get-defs-text)
      (currently-processing-definitions-text))
 
    
;                                                                                             
;                                                                                             
;       ;                                                                                     
;       ;                                                                   ;                 
;       ;                                             ;             ;                         
;    ;; ;   ;;;    ;;;;  ;   ; ; ;; ;;  ;;;   ; ;;   ;;;;;  ;;;;   ;;;;;  ;;;     ;;;   ; ;;  
;   ;  ;;  ;   ;  ;      ;   ; ;; ;; ; ;   ;  ;;  ;   ;         ;   ;       ;    ;   ;  ;;  ; 
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;   ;  ;   ;   ;         ;   ;       ;    ;   ;  ;   ; 
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;;;;;  ;   ;   ;      ;;;;   ;       ;    ;   ;  ;   ; 
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;      ;   ;   ;     ;   ;   ;       ;    ;   ;  ;   ; 
;   ;  ;;  ;   ;  ;      ;  ;; ;  ;  ; ;      ;   ;   ;     ;  ;;   ;       ;    ;   ;  ;   ; 
;    ;; ;   ;;;    ;;;;   ;; ; ;  ;  ;  ;;;;  ;   ;    ;;;   ;;  ;   ;;;    ;     ;;;   ;   ; 
;                                                                                             
;                                                                                             
;                                                                                             

    
    ;; document-variable : stx[identifier,original] phase-level -> void
    (define (document-variable stx phase-level)
      (let ([defs-text (currently-processing-definitions-text)])
        (when defs-text
          (let ([binding-info (identifier-binding stx phase-level)])
            (when (and (pair? binding-info)
                       (syntax-position stx)
                       (syntax-span stx))
              (let* ([start (- (syntax-position stx) 1)]
                     [fin (+ start (syntax-span stx))]
                     [source-editor (find-source-editor stx)]
                     [xref (get-xref)])
                (when (and xref source-editor)
                  (let ([definition-tag (xref-binding->definition-tag xref binding-info #f)])
                    (when definition-tag
                      (let-values ([(path tag) (xref-tag->path+anchor xref definition-tag)])
                        (when path
                          (let ([index-entry (xref-tag->index-entry xref definition-tag)])
                            (when index-entry
                              (send defs-text syncheck:add-background-color
                                    source-editor "navajowhite" start fin (syntax-e stx))
                              (send defs-text syncheck:add-menu
                                    source-editor
                                    start 
                                    fin 
                                    (syntax-e stx)
                                    (λ (menu)
                                      (instantiate menu-item% ()
                                        (parent menu)
                                        (label (build-docs-label (entry-desc index-entry)))
                                        (callback
                                         (λ (x y)
                                           (let* ([url (path->url path)]
                                                  [url2 (if tag
                                                            (make-url (url-scheme url)
                                                                      (url-user url)
                                                                      (url-host url)
                                                                      (url-port url)
                                                                      (url-path-absolute? url)
                                                                      (url-path url)
                                                                      (url-query url)
                                                                      tag)
                                                            url)])
                                             (send-url (url->string url2)))))))))))))))))))))
    
    (define (build-docs-label desc)
      (let ([libs (exported-index-desc-from-libs desc)])
        (cond
          [(null? libs)
           (fw:gui-utils:format-literal-label
            (string-constant cs-view-docs)
            (exported-index-desc-name desc))]
          [else
           (fw:gui-utils:format-literal-label
            (string-constant cs-view-docs-from)
            (format 
             (string-constant cs-view-docs)
             (exported-index-desc-name desc))
            (apply string-append 
                   (add-between 
                    (map (λ (x) (format "~s" x)) libs) 
                    ", ")))])))
    
    
    
    ;                                                          
    ;                                                          
    ;                                                          
    ;                                        ;                 
    ;                                        ;                 
    ;                                                          
    ;   ; ;;    ;;;   ;;;;    ;;;;  ;;;;;    ;    ;;;;    ;;;; 
    ;   ;;  ;  ;   ;  ;   ;  ;   ;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;   ;  ;;;;;  ;   ;  ;   ;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;      ;      ;   ;  ;   ;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;      ;   ;  ;   ;  ;  ;;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;       ;;;   ;   ;   ;; ;  ; ; ;    ;    ;   ;   ;;;; 
    ;                                                        ; 
    ;                                                    ;   ; 
    ;                                                     ;;;  
    
    
    ;; make-rename-menu : (cons stx[original,id] (listof stx[original,id])) (listof id-set) -> void
    (define (make-rename-menu stxs id-sets)
      (let ([defs-text (currently-processing-definitions-text)])
        (when defs-text
          (let* ([source (syntax-source (car stxs))] ;; all stxs in the list must have the same source
                 [source-editor (find-source-editor (car stxs))])
            (when (is-a? source-editor text%)
              (let* ([start (- (syntax-position (car stxs)) 1)]
                     [fin (+ start (syntax-span (car stxs)))])
                (send defs-text syncheck:add-menu
                      source-editor
                      start 
                      fin 
                      (syntax-e (car stxs))
                      (λ (menu)
                        (let ([name-to-offer (format "~a" (syntax->datum (car stxs)))])
                          (instantiate menu-item% ()
                            (parent menu)
                            (label (fw:gui-utils:format-literal-label (string-constant cs-rename-var) name-to-offer))
                            (callback
                             (λ (x y)
                               (let ([frame-parent (find-menu-parent menu)])
                                 (rename-callback name-to-offer
                                                  defs-text
                                                  stxs
                                                  id-sets
                                                  frame-parent))))))))))))))
    
    ;; find-parent : menu-item-container<%> -> (union #f (is-a?/c top-level-window<%>)
    (define (find-menu-parent menu)
      (let loop ([menu menu])
        (cond
          [(is-a? menu menu-bar%) (send menu get-frame)]
          [(is-a? menu popup-menu%)
           (let ([target (send menu get-popup-target)])
             (cond
               [(is-a? target editor<%>) 
                (let ([canvas (send target get-canvas)])
                  (and canvas
                       (send canvas get-top-level-window)))]
               [(is-a? target window<%>) 
                (send target get-top-level-window)]
               [else #f]))]
          [(is-a? menu menu-item<%>) (loop (send menu get-parent))]
          [else #f])))
    
    ;; rename-callback : string 
    ;;                   (and/c syncheck-text<%> definitions-text<%>)
    ;;                   (listof syntax[original])
    ;;                   (listof id-set) 
    ;;                   (union #f (is-a?/c top-level-window<%>)) 
    ;;                -> void
    ;; callback for the rename popup menu item
    (define (rename-callback name-to-offer defs-text stxs id-sets parent)
      (let ([new-str
             (fw:keymap:call/text-keymap-initializer
              (λ ()
                (get-text-from-user
                 (string-constant cs-rename-id)
                 (fw:gui-utils:format-literal-label (string-constant cs-rename-var-to) name-to-offer)
                 parent
                 name-to-offer)))])
        (when new-str
          (let* ([new-sym (format "~s" (string->symbol new-str))]
                 [to-be-renamed 
                  (remove-duplicates
                   (sort 
                    (apply 
                     append
                     (map (λ (id-set) 
                            (apply
                             append
                             (map (λ (stx) (or (get-ids id-set stx) '())) stxs)))
                          id-sets))
                    (λ (x y) 
                      ((syntax-position x) . >= . (syntax-position y)))))]
                 [do-renaming?
                  (or (not (name-duplication? to-be-renamed id-sets new-sym))
                      (equal?
                       (message-box/custom
                        (string-constant check-syntax)
                        (fw:gui-utils:format-literal-label (string-constant cs-name-duplication-error) 
                                                           new-sym)
                        (string-constant cs-rename-anyway)
                        (string-constant cancel)
                        #f
                        parent
                        '(stop default=2))
                       1))])
            (when do-renaming?
              (unless (null? to-be-renamed)
                (let ([txts (list defs-text)])
                  (send defs-text begin-edit-sequence)
                  (for-each (λ (stx) 
                              (let ([source-editor (find-source-editor/defs stx defs-text)])
                                (when (is-a? source-editor text%)
                                  (unless (memq source-editor txts)
                                    (send source-editor begin-edit-sequence)
                                    (set! txts (cons source-editor txts)))
                                  (let* ([start (- (syntax-position stx) 1)]
                                         [end (+ start (syntax-span stx))])
                                    (send source-editor delete start end #f)
                                    (send source-editor insert new-sym start start #f)))))
                            to-be-renamed)
                  (send defs-text invalidate-bitmap-cache)
                  (for-each
                   (λ (txt) (send txt end-edit-sequence))
                   txts))))))))
    
    ;; name-duplication? : (listof syntax) (listof id-set) symbol -> boolean
    ;; returns #t if the name chosen would be the same as another name in this scope.
    (define (name-duplication? to-be-renamed id-sets new-str)
      (let ([new-ids (map (λ (id) (datum->syntax id (string->symbol new-str)))
                          to-be-renamed)])
        (ormap (λ (id-set)
                 (ormap (λ (new-id) (get-ids id-set new-id)) 
                        new-ids))
               id-sets)))
    
    ;; remove-duplicates : (listof syntax[original]) -> (listof syntax[original])
    ;; removes duplicates, based on the source locations of the identifiers
    (define (remove-duplicates ids)
      (cond
        [(null? ids) null]
        [else (let loop ([fst (car ids)]
                         [rst (cdr ids)])
                (cond
                  [(null? rst) (list fst)]
                  [else (if (and (eq? (syntax-source fst)
                                      (syntax-source (car rst)))
                                 (= (syntax-position fst)
                                    (syntax-position (car rst))))
                            (loop fst (cdr rst))
                            (cons fst (loop (car rst) (cdr rst))))]))]))
    
    
    ;                                            
    ;                                            
    ;                                            
    ;   ;       ;                                
    ;           ;                                
    ;           ;                     ;          
    ;   ;    ;; ;        ;;;    ;;;  ;;;;   ;;;  
    ;   ;   ;  ;;       ;      ;   ;  ;    ;     
    ;   ;  ;    ;       ;;    ;    ;  ;    ;;    
    ;   ;  ;    ;        ;;   ;;;;;;  ;     ;;   
    ;   ;  ;    ;          ;  ;       ;       ;  
    ;   ;   ;  ;;          ;   ;      ;       ;  
    ;   ;    ;; ;       ;;;     ;;;;   ;;  ;;;   
    ;                                            
    ;                                            
    ;                                            
    
    ;; make-id-set : -> id-set
    (define (make-id-set) (make-module-identifier-mapping))
    
    ;; add-id : id-set identifier -> void
    (define (add-id mapping id)
      (let* ([old (module-identifier-mapping-get mapping id (λ () '()))]
             [new (cons id old)])
        (module-identifier-mapping-put! mapping id new)))
    
    ;; get-idss : id-set -> (listof (listof identifier))
    (define (get-idss mapping)
      (module-identifier-mapping-map mapping (λ (x y) y)))
    
    ;; get-ids : id-set identifier -> (union (listof identifier) #f)
    (define (get-ids mapping var)
      (module-identifier-mapping-get mapping var (λ () #f)))
    
    ;; for-each-ids : id-set ((listof identifier) -> void) -> void
    (define (for-each-ids mapping f)
      (module-identifier-mapping-for-each mapping (λ (x y) (f y))))
    
    
    ;                                                 
    ;                                                 
    ;                                                 
    ;  ;    ;    ; ;                                  
    ;  ;    ;    ;                                    
    ;  ;   ; ;   ;                                    
    ;   ;  ; ;  ;  ;   ; ;   ;;;       ;   ;   ; ;;   
    ;   ;  ; ;  ;  ;   ;;   ;   ;      ;   ;   ;;  ;  
    ;   ; ;   ; ;  ;   ;   ;    ;      ;   ;   ;    ; 
    ;   ; ;   ; ;  ;   ;   ;;;;;;      ;   ;   ;    ; 
    ;   ; ;   ; ;  ;   ;   ;           ;   ;   ;    ; 
    ;    ;     ;   ;   ;    ;          ;  ;;   ;;  ;  
    ;    ;     ;   ;   ;     ;;;;       ;; ;   ; ;;   
    ;                                          ;      
    ;                                          ;      
    ;                                          ;      
    
    
    (add-check-syntax-key-bindings (drracket:rep:get-drs-bindings-keymap))
    (fw:color-prefs:add-to-preferences-panel (string-constant check-syntax)
                                             syncheck-add-to-preferences-panel)
    (drracket:language:register-capability 'drscheme:check-syntax-button (flat-contract boolean?) #t)
    (drracket:get/extend:extend-definitions-text make-syncheck-text%)
    (drracket:get/extend:extend-unit-frame unit-frame-mixin #f)
    (drracket:get/extend:extend-tab tab-mixin)))

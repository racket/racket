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
         racket/dict
         data/interval-map
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
         "../../syncheck-drracket-button.rkt"
         "intf.rkt"
         "colors.rkt"
         "traversals.rkt")
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

(define cs-check-syntax-mode (string-constant cs-check-syntax-mode))
(define cs-mode-menu-show-my-obligations (string-constant cs-mode-menu-show-my-obligations))
(define cs-mode-menu-show-client-obligations (string-constant cs-mode-menu-show-client-obligations))
(define cs-mode-menu-show-syntax (string-constant cs-mode-menu-show-syntax))

(preferences:set-default 'drracket:syncheck-mode 'default-mode
                         (λ (x) (memq x '(default-mode 
                                           my-obligations-mode 
                                           client-obligations-mode))))
(define tool@ 
  (unit 
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
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
        
    ;; clearing-text-mixin : (mixin text%)
    ;; overrides methods that make sure the arrows go away appropriately.
    ;; adds a begin/end-edit-sequence to the insertion and deletion
    ;;  to ensure that the on-change method isn't called until after
    ;;  the arrows are cleared.
    (define (clearing-text-mixin super%)
      (define-local-member-name set-do-cleanup)
      
      (define cs-clearing<%>
        (interface ()
          set-do-cleanup))
      
      ;; the commented out definition of extra
      ;; is because of PR 11279. When it is fixed, use it
      ;; instead of this one.
      (define (extra super%)
        (class super%
          (inherit set-do-cleanup)
          (define/augment (begin-metadata-changes)
            (set-do-cleanup #f)
            (inner (void) begin-metadata-changes))
          (define/augment (end-metadata-changes)
            (set-do-cleanup #t)
            (inner (void) end-metadata-changes))
          (super-new)))
      
      #|
      (define extra
        (mixin (cs-clearing<%> drracket:unit:definitions-text<%>) ()
          (inherit set-do-cleanup)
          (define/augment (begin-metadata-changes)
            (set-do-cleanup #f)
            (inner (void) begin-metadata-changes))
          (define/augment (end-metadata-changes)
            (set-do-cleanup #t)
            (inner (void) end-metadata-changes))
          (super-new)))
      |#

      (define basic
        (mixin ((class->interface text%)) (cs-clearing<%>)
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
          
          (define do-cleanup #t)
          (define/public (set-do-cleanup s)
            (set! do-cleanup s))
          
          (define/private (clean-up)
            (when do-cleanup
              (let ([st (find-syncheck-text this)])
                (when (and st
                           (is-a? st drracket:unit:definitions-text<%>))
                  (let ([tab (send st get-tab)])
                    (send tab syncheck:clear-error-message)
                    (send tab syncheck:clear-highlighting))))))
          
          (super-new)))
      
      (cond
        [(implementation? super% drracket:unit:definitions-text<%>)
         (extra (basic super%))]
        [else
         (basic super%)]))
    
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

            ;; arrow-records : (U #f hash[text% => arrow-record])
            ;; arrow-record = interval-map[(listof arrow-entry)]
            ;; arrow-entry is one of
            ;;   - (cons (U #f sym) (menu -> void))
            ;;   - def-link
            ;;   - tail-link
            ;;   - arrow
            ;;   - string
            (define (get-arrow-record table text)
              (hash-ref! table text (lambda () (make-interval-map))))

            (define arrow-records #f)
            
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
              (set! arrow-records (make-hasheq))
              (set! bindings-table (make-hash))
              (set! cleanup-texts '())
              (set! style-mapping (make-hash))
              (let ([f (get-top-level-window)])
                (when f
                  (send f open-status-line 'drracket:check-syntax:mouse-over))))
            
            (define/public (syncheck:arrows-visible?)
              (or arrow-records cursor-location cursor-text))
            
            ;; syncheck:clear-arrows : -> void
            (define/public (syncheck:clear-arrows)
              (when (or arrow-records cursor-location cursor-text)
                (let ([any-tacked? #f])
                  (when tacked-hash-table
                    (let/ec k
                      (hash-for-each
                       tacked-hash-table
                       (λ (key val)
                         (set! any-tacked? #t)
                         (k (void))))))
                  (set! tacked-hash-table #f)
                  (set! arrow-records #f)
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
              (when arrow-records
                (when (and (<= 0 start-pos end-pos (last-position)))
                  (add-to-range/key text start-pos end-pos make-menu key #t))))
            
            (define/public (syncheck:add-background-color text color start fin key)
              (when arrow-records
                (when (is-a? text text:basic<%>)
                  (add-to-range/key text start fin (make-colored-region color text start fin) key #f))))
            
            ;; syncheck:add-arrow : symbol text number number text number number boolean -> void
            ;; pre: start-editor, end-editor are embedded in `this' (or are `this')
            (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                               end-text end-pos-left end-pos-right
                                               actual? level)
              (when arrow-records
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
              (when arrow-records
                (let ([tail-arrow (make-tail-arrow #f #f #f #f to-text to-pos from-text from-pos)])
                  (add-to-range/key from-text from-pos (+ from-pos 1) tail-arrow #f #f)
                  (add-to-range/key to-text to-pos (+ to-pos 1) tail-arrow #f #f))))
            
            ;; syncheck:add-jump-to-definition : text start end id filename -> void
            (define/public (syncheck:add-jump-to-definition text start end id filename)
              (when arrow-records
                (add-to-range/key text start end (make-def-link id filename) #f #f)))
            
            ;; syncheck:add-mouse-over-status : text pos-left pos-right string -> void
            (define/public (syncheck:add-mouse-over-status text pos-left pos-right str)
              (when arrow-records
                (add-to-range/key text pos-left pos-right str #f #f)))
            
            ;; add-to-range/key : text number number any any boolean -> void
            ;; adds `key' to the range `start' - `end' in the editor
            ;; If use-key? is #t, it adds `to-add' with the key, and does not
            ;; replace a value with that key already there.
            ;; If use-key? is #f, it adds `to-add' without a key.
            ;; pre: arrow-records is not #f
            (define/private (add-to-range/key text start end to-add key use-key?)
              (let ([arrow-record (get-arrow-record arrow-records text)])
                ;; Dropped the check (< _ (vector-length arrow-vector))
                ;; which had the following comment:
                ;;    the last test in the above and is because some syntax objects
                ;;    appear to be from the original source, but can have bogus information.

                ;; Use (add1 end) below, because interval-maps use half-open intervals
                ;; ie, [start, end] = [start, end+1)
                (cond [use-key?
                       (interval-map-update*! arrow-record
                                              start (add1 end)
                                              (lambda (old)
                                                (if (for/or ([x (in-list old)])
                                                      (and (pair? x) (car x) (eq? (car x) key)))
                                                    old
                                                    (cons to-add old)))
                                              null)]
                      [else
                       (interval-map-cons*! arrow-record
                                            start (add1 end) 
                                            to-add null)])))
            
            (inherit get-top-level-window)
            
            (define/augment (on-change)
              (inner (void) on-change)
              (when arrow-records
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
            ;; pre-condition: arrow-records is not #f.
            (define/private (flush-arrow-coordinates-cache)
              (for ([(text arrow-record) (in-hash arrow-records)])
                (for ([(start+end eles) (in-dict arrow-record)])
                  (for ([ele (in-list eles)])
                    (when (arrow? ele)
                      (set-arrow-start-x! ele #f)
                      (set-arrow-start-y! ele #f)
                      (set-arrow-end-x! ele #f)
                      (set-arrow-end-y! ele #f))))))
            
            (define/override (on-paint before dc left top right bottom dx dy draw-caret)
              (when (and arrow-records (not before))
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
                    (let* ([arrow-record (hash-ref arrow-records cursor-text #f)])
                      (when arrow-record
                        (for ([ele (in-list (interval-map-ref arrow-record cursor-location null))])
                          (cond [(var-arrow? ele)
                                 (if (var-arrow-actual? ele)
                                     (begin (send dc set-pen var-pen)
                                            (send dc set-brush untacked-brush))
                                     (begin (send dc set-pen templ-pen)
                                            (send dc set-brush untacked-brush)))
                                 (draw-arrow2 ele)]
                                [(tail-arrow? ele)
                                 (send dc set-pen tail-pen)
                                 (send dc set-brush untacked-brush)
                                 (for-each-tail-arrows draw-arrow2 ele)])))))
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
                             [arrow-record (hash-ref arrow-records next-text #f)])
                        (when arrow-record
                          (for ([ele (in-list (interval-map-ref arrow-record next-pos null))])
                            (cond
                             [(tail-arrow? ele)
                              (let ([other-pos (tail-arrow-other-pos ele)]
                                    [other-text (tail-arrow-other-text ele)])
                                (when (and (= other-pos next-pos)
                                           (eq? other-text next-text))
                                  (loop ele)))]))))))))
              
              (for-each-tail-arrows/to/from tail-arrow-to-pos tail-arrow-to-text
                                            tail-arrow-from-pos tail-arrow-from-text)
              (for-each-tail-arrows/to/from tail-arrow-from-pos tail-arrow-from-text
                                            tail-arrow-to-pos tail-arrow-to-text))
            
            (define/override (on-event event)
              (if arrow-records
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
                            
                            (let* ([arrow-record (hash-ref arrow-records cursor-text #f)]
                                   [eles (and arrow-record (interval-map-ref arrow-record cursor-location null))])
                              
                              (unless (equal? cursor-eles eles)
                                (set! cursor-eles eles)
                                (update-docs-background eles)
                                (when eles
                                  (update-status-line eles)
                                  (for ([ele (in-list eles)])
                                    (cond [(arrow? ele)
                                           (update-arrow-poss ele)]))
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
                           (let ([arrow-record (hash-ref arrow-records text #f)])
                             (when arrow-record
                               (let ([vec-ents (interval-map-ref arrow-record pos null)]
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
                                                  arrow-record
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
                                                          arrow-record
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
            
            (define/private (tack-crossing-arrows-callback arrow-record start end text kinds)
              (define (xor a b)
                (or (and a (not b)) (and (not a) b)))
              (define (within t p)
                (and (eq? t text)
                     (<= start p end)))
              ;; FIXME: Add to interval-map: iteration over distinct ranges w/i given range
              (for ([position (in-range start end)])
                (define things (interval-map-ref arrow-record position null))
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

            (define/private (untack-crossing-arrows arrow-record start end)
              ;; FIXME: same comment as in 'tack-crossing...'
              (for ([position (in-range start end)])
                (for ([va (interval-map-ref arrow-record position null)]
                      #:when (var-arrow? va))
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
                (when arrow-records
                  (let ([arrow-record (hash-ref arrow-records text #f)])
                    (when arrow-record
                      (let ([vec-ents (filter var-arrow? (interval-map-ref arrow-record pos null))])
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
                (when arrow-records
                  (let ([arrow-record (hash-ref arrow-records text #f)])
                    (when arrow-record
                      (let ([vec-ents (filter def-link? (interval-map-ref arrow-record pos null))])
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
                
                ;; we need to reset the colors in every editor we can find, not just those that have new colors
                (for ([(k v) (in-hash style-mapping)])
                  (for ((l (in-list v)))
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
                        
                        (set! edit-sequences (cons txt edit-sequences))))))
                
                (for ((l (in-list (reverse (hash-ref style-mapping syncheck-mode '())))))
                  (let-values ([(txt start finish style) (apply values l)])
                    (add-to-cleanup/apply-style txt start finish style)))
                
                (for ((txt (in-list edit-sequences)))
                  (send txt end-edit-sequence))))
                          
            (super-new)))))
    
    (define syncheck-frame<%>
      (interface ()
        syncheck:button-callback
        syncheck:error-report-visible?
        syncheck:get-error-report-contents))
    
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
          (when error-report-visible?
            (set! error-report-visible? #f)
            (send report-error-text clear-output-ports)
            (send report-error-text lock #f)
            (send report-error-text delete/io 0 (send report-error-text last-position))
            (send report-error-text lock #t)
            (when (is-current-tab?)
              (send (get-frame) hide-error-report)
              (send (get-frame) update-menu-status this))))
        
        (define/public (syncheck:clear-highlighting)
          (let ([definitions (get-defs)])
            (when (send definitions syncheck:arrows-visible?)
              (let ([locked? (send definitions is-locked?)])
                (send definitions begin-edit-sequence #f)
                (send definitions lock #f)
                (send definitions syncheck:clear-arrows)
                (send definitions lock locked?)
                (send definitions end-edit-sequence)))))
        
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
          (update-menu-status new-tab)
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
          (send mode-menu-item1 enable #t)
          (send mode-menu-item2 enable #t)
          (inner (void) enable-evaluation))
        
        (define/augment (disable-evaluation)
          (send mode-menu-item1 enable #f)
          (send mode-menu-item2 enable #f)
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
        
        (define/public-final (syncheck:get-error-report-contents)
          (and (syncheck:error-report-visible?)
               (send (send report-error-canvas get-editor) get-text)))
        
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
        
        (define mode-menu-item1 #f)
        (define mode-menu-item2 #f)
        (define mode-menu-item3 #f)
        
        (define/override (add-show-menu-items show-menu)
          (define (start-checking mode)
            (let* ([tab (get-current-tab)]
                   [defs (send tab get-defs)])
              (preferences:set 'drracket:syncheck-mode mode)
              (cond
                [(send defs get-syncheck-mode)
                 (send defs set-syncheck-mode mode)
                 (update-menu-status tab)]
                [else
                 (syncheck:button-callback #f mode)])))
          
          (super add-show-menu-items show-menu)
          (let ([p (new menu%
                        [parent show-menu]
                        [label cs-check-syntax-mode])])
            (set! mode-menu-item1
                  (new checkable-menu-item% 
                       [parent p]
                       [label cs-mode-menu-show-syntax]
                       [callback (λ (a b) (start-checking 'default-mode))]))
            (set! mode-menu-item2
                  (new checkable-menu-item% 
                       [parent p]
                       [label cs-mode-menu-show-my-obligations]
                       [callback (λ (a b) (start-checking 'my-obligations-mode))]))
            (set! mode-menu-item3
                  (new checkable-menu-item%
                       [parent p]
                       [label cs-mode-menu-show-client-obligations]
                       [callback (λ (a b) (start-checking 'client-obligations-mode))]))))
        
        (define/public (update-menu-status tab)
          (when mode-menu-item1
            (let ([mode (send (send (get-current-tab) get-defs) get-syncheck-mode)])
              (send mode-menu-item1 check (eq? mode 'default-mode))
              (send mode-menu-item2 check (eq? mode 'my-obligations-mode))
              (send mode-menu-item3 check (eq? mode 'client-obligations-mode)))))
                
        (inherit open-status-line close-status-line update-status-line ensure-rep-hidden)
        ;; syncheck:button-callback : (case-> (-> void) ((union #f syntax) -> void)
        ;; this is the only function that has any code running on the user's thread
        (define/public syncheck:button-callback
          (case-lambda
            [() (syncheck:button-callback #f)]
            [(jump-to-id) (syncheck:button-callback jump-to-id  (preferences:get 'drracket:syncheck-mode))]
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
                           #:gui-modules? #f
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
                                          (update-menu-status (get-current-tab))
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

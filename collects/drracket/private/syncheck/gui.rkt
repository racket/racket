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
         racket/match
         racket/contract
         racket/class
         racket/list
         racket/promise
         racket/dict
         racket/set
         racket/runtime-path
         racket/place
         data/interval-map
         drracket/tool
         syntax/toplevel
         mrlib/switchable-button
         (prefix-in drracket:arrow: drracket/arrow)
         (prefix-in fw: framework/framework)
         mred
         framework
         net/url
         browser/external
         (for-syntax racket/base)
         (only-in ffi/unsafe register-finalizer)
         "../../syncheck-drracket-button.rkt"
         "../../private/eval-helpers.rkt"
         "intf.rkt"
         "local-member-names.rkt"
         "colors.rkt"
         "traversals.rkt"
         "annotate.rkt"
         "../tooltip.rkt"
         "blueboxes-gui.rkt"
         framework/private/logging-timer)
(provide tool@)

(define orig-output-port (current-output-port))
(define (oprintf . args) (apply fprintf orig-output-port args))

(define status-init (string-constant cs-status-init))
(define status-coloring-program (string-constant cs-status-coloring-program))
(define status-eval-compile-time (string-constant cs-status-eval-compile-time))
(define status-expanding-expression (string-constant cs-status-expanding-expression))

(define jump-to-next-bound-occurrence (string-constant cs-jump-to-next-bound-occurrence))
(define jump-to-binding (string-constant cs-jump-to-binding))
(define jump-to-definition (string-constant cs-jump-to-definition))

(define cs-check-syntax-mode (string-constant cs-check-syntax-mode))
(define cs-mode-menu-show-my-obligations (string-constant cs-mode-menu-show-my-obligations))
(define cs-mode-menu-show-client-obligations (string-constant cs-mode-menu-show-client-obligations))
(define cs-mode-menu-show-syntax (string-constant cs-mode-menu-show-syntax))

(define cs-syncheck-running "Check Syntax Running")

;; This delay should be long enough that the arrows won't be drawn if drawing
;; the editor hitches while scrolling:
(define syncheck-scroll-arrow-cooldown 500)

;; This delay should be longer than the time it takes for a quick mouse motion
;; to pass vertically through an identifier
;; It should also be longer than the polling delay for mouse events (which should
;; be < 50ms)
(define syncheck-arrow-delay 100)

(let ([number-between-zero-and-one?
       (λ (x) (and (number? x) (<= 0 x 1)))])
  (preferences:set-default 
   'drracket:check-syntax-error-report-window-percentage 
   1/10
   number-between-zero-and-one?))
(preferences:set-default 'drracket:syncheck:show-arrows? #t boolean?)

(define (syncheck-add-to-online-expansion-prefs-panel vp)
  (preferences:add-check vp
                         'drracket:syncheck:show-arrows?
                         (string-constant show-arrows-on-mouseover)))

(define (syncheck-add-to-preferences-panel parent)
  (color-prefs:build-color-selection-panel parent
                                           lexically-bound-variable-style-pref
                                           lexically-bound-variable-style-name
                                           (string-constant cs-lexical-variable))
  (color-prefs:build-color-selection-panel parent
                                           imported-variable-style-pref
                                           imported-variable-style-name
                                           (string-constant cs-imported-variable))
  (color-prefs:build-color-selection-panel parent
                                           set!d-variable-style-pref
                                           set!d-variable-style-name
                                           (string-constant cs-set!d-variable))
  (color-prefs:build-color-selection-panel parent
                                           unused-require-style-pref
                                           unused-require-style-name
                                           (string-constant cs-unused-require))
  (color-prefs:build-color-selection-panel parent
                                           free-variable-style-pref
                                           free-variable-style-name
                                           (string-constant cs-free-variable))
  
  (color-prefs:build-color-selection-panel parent
                                           my-obligation-style-pref
                                           my-obligation-style-name
                                           cs-my-obligation-color)
  (color-prefs:build-color-selection-panel parent
                                           their-obligation-style-pref
                                           their-obligation-style-name
                                           cs-their-obligation-color)
  (color-prefs:build-color-selection-panel parent
                                           unk-obligation-style-pref
                                           unk-obligation-style-name
                                           cs-unk-obligation-color)
  (color-prefs:build-color-selection-panel parent
                                           both-obligation-style-pref
                                           both-obligation-style-name
                                           cs-both-obligation-color))

(color-prefs:register-color-preference lexically-bound-variable-style-pref
                                       lexically-bound-variable-style-name
                                       (make-object color% 81 112 203)
                                       (make-object color% 50 163 255))
(color-prefs:register-color-preference set!d-variable-style-pref
                                       set!d-variable-style-name
                                       (send the-color-database find-color "firebrick")
                                       (send the-color-database find-color "pink"))
(color-prefs:register-color-preference unused-require-style-pref
                                       unused-require-style-name
                                       (send the-color-database find-color "red")
                                       (send the-color-database find-color "pink"))
(color-prefs:register-color-preference free-variable-style-pref
                                       free-variable-style-name
                                       (send the-color-database find-color "red")
                                       (send the-color-database find-color "pink"))

(color-prefs:register-color-preference imported-variable-style-pref
                                       imported-variable-style-name
                                       (make-object color% 68 0 203)
                                       (make-object color% 166 0 255))
(color-prefs:register-color-preference my-obligation-style-pref
                                       my-obligation-style-name
                                       (send the-color-database find-color "firebrick")
                                       (send the-color-database find-color "pink"))
(color-prefs:register-color-preference their-obligation-style-pref
                                       their-obligation-style-name
                                       (make-object color% 0 116 0)
                                       (send the-color-database find-color "limegreen"))
(color-prefs:register-color-preference unk-obligation-style-pref
                                       unk-obligation-style-name
                                       (send the-color-database find-color "black")
                                       (send the-color-database find-color "white"))
(color-prefs:register-color-preference both-obligation-style-pref
                                       both-obligation-style-name
                                       (make-object color% 139 142 28)
                                       (send the-color-database find-color "khaki"))

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
              (alternate-bitmap syncheck-small-bitmap)
              (parent parent)
              (callback (λ (button) (send frame syncheck:button-callback)))))
       'drracket:syncheck
       #:number 50)
      (drracket:unit:add-to-program-editor-mixin clearing-text-mixin))
    (define (phase2) (void))
    
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
    
    (define-struct arrow () #:mutable #:transparent)
    (define-struct (var-arrow arrow)
      (start-text start-pos-left start-pos-right
                  end-text end-pos-left end-pos-right
                  actual? level) ;; level is one of 'lexical, 'top-level, 'import
      #:transparent)
    (define-struct (tail-arrow arrow) (from-text from-pos to-text to-pos) #:transparent)
    
    (define-struct tooltip-info (text pos-left pos-right msg) #:transparent)
    
    ;; color : string
    ;; text: text:basic<%>
    ;; start, fin: number
    ;; used to represent regions to highlight when passing the mouse over the syncheck window
    (define-struct colored-region (color text start fin))
    
    ;; id : symbol  --  the nominal-source-id from identifier-binding
    ;; filename : path
    (define-struct def-link (id filename) #:inspector (make-inspector))
    
    (define (get-tacked-var-brush white-on-black?)
      (if white-on-black?
          (send the-brush-list find-or-create-brush "LightSteelBlue" 'solid)
          (send the-brush-list find-or-create-brush "BLUE" 'solid)))
    (define (get-var-pen white-on-black?)
      (if white-on-black?
          (send the-pen-list find-or-create-pen "LightSteelBlue" 1 'solid)
          (send the-pen-list find-or-create-pen "BLUE" 1 'solid)))
    
    (define templ-color (send the-color-database find-color "purple"))
    (define (get-templ-pen white-on-black?)
      (if white-on-black?
          (send the-pen-list find-or-create-pen "orchid" 1 'solid)
          (send the-pen-list find-or-create-pen templ-color 1 'solid)))
    (define (get-tacked-templ-brush white-on-black?) 
      (if white-on-black?
          (send the-brush-list find-or-create-brush "orchid" 'solid)
          (send the-brush-list find-or-create-brush templ-color 'solid)))
    
    (define (get-tail-pen white-on-black?) (send the-pen-list find-or-create-pen "orchid" 1 'solid))
    (define (get-tacked-tail-brush white-on-black?) (send the-brush-list find-or-create-brush "orchid" 'solid))
    (define (get-untacked-brush white-on-black?) (send the-brush-list find-or-create-brush "WHITE" 'solid))
        
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
                    (send (send tab get-frame) set-syncheck-running-mode #f)
                    (send tab syncheck:clear-error-message)
                    (send tab syncheck:clear-highlighting))))))
          
          (super-new)))
      
      (cond
        [(implementation? super% drracket:unit:definitions-text<%>)
         (extra (basic super%))]
        [else
         (basic super%)]))
                          
    (struct tooltip-spec (strings x y w h) #:transparent)
            
    (define make-syncheck-text%
      (λ (super%)
        (let* ([cursor-arrow (make-object cursor% 'arrow)])
          (class* (docs-text-mixin super%) (syncheck-text<%>)
            (inherit set-cursor get-admin invalidate-bitmap-cache set-position
                     get-pos/text-dc-location position-location
                     get-canvas last-position dc-location-to-editor-location
                     find-position begin-edit-sequence end-edit-sequence
                     highlight-range unhighlight-range
                     paragraph-end-position first-line-currently-drawn-specially?
                     line-end-position position-line
                     syncheck:add-docs-range)
            
            ;; arrow-records : (U #f hash[text% => arrow-record])
            ;; arrow-record = interval-map[(listof arrow-entry)]
            ;; arrow-entry is one of
            ;;   - (cons (U #f sym) (menu -> void))
            ;;   - def-link
            ;;   - tail-link
            ;;   - arrow
            ;;   - string
            (define/private (get-arrow-record table text)
              (hash-ref! table text (lambda () (make-interval-map))))

            (define arrow-records #f)
            
            ;; cleanup-texts : (or/c #f (listof text))
            (define cleanup-texts #f)
            
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
            
            ;; for use in the automatic test suite (both)
            (define/public (syncheck:get-bindings-table [tooltips? #f])
              (cond
                [tooltips?
                 (define unsorted
                   (apply 
                    append
                    (for/list ([(k interval-map) (in-hash arrow-records)])
                      (apply
                       append
                       (dict-map
                        interval-map
                        (λ (key x)
                          (for/list ([x (in-list x)]
                                     #:when (tooltip-info? x))
                            (list (tooltip-info-pos-left x)
                                  (tooltip-info-pos-right x)
                                  (tooltip-info-msg x)))))))))
                 (define (compare l1 l2)
                   (cond
                     [(equal? (list-ref l1 0) (list-ref l2 0))
                      (cond
                        [(equal? (list-ref l1 2) (list-ref l2 2))
                         (string<=? (list-ref l1 2) (list-ref l2 2))]
                        [else
                         (< (list-ref l1 1) (list-ref l2 1))])]
                     [else
                      (< (list-ref l1 0) (list-ref l2 0))]))
                 (sort unsorted compare)]
                [else
                 bindings-table]))
            
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
                (send text position-location pos xlb xrb)
                (send text editor-location-to-dc-location (unbox xlb) (unbox xrb)))
              
              (hash-for-each
               bindings-table
               (λ (k v)
                 (hash-set! bindings-table k (sort v compare-bindings)))))
            
            (define tacked-hash-table (make-hasheq))
            
            ;; find-char-box : text number number -> (values number number number number)
            ;; returns the bounding box (left, top, right, bottom) for the text range.
            ;; only works right if the text is on a single line.
            (define/private (find-char-box text left-pos right-pos)
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
                 yr)))
            
            (define/private (get-arrow-poss arrow)
              (cond
                [(var-arrow? arrow) (get-var-arrow-poss arrow)]
                [(tail-arrow? arrow) (get-tail-arrow-poss arrow)]))
            
            (define/private (get-var-arrow-poss arrow)
              (let-values ([(start-x start-y) (find-poss 
                                               (var-arrow-start-text arrow)
                                               (var-arrow-start-pos-left arrow)
                                               (var-arrow-start-pos-right arrow))]
                           [(end-x end-y) (find-poss 
                                           (var-arrow-end-text arrow)
                                           (var-arrow-end-pos-left arrow)
                                           (var-arrow-end-pos-right arrow))])
                (values start-x start-y end-x end-y)))
            
            (define/private (get-tail-arrow-poss arrow)
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
                (values start-x start-y end-x end-y)))

            (define xlb (box 0))
            (define ylb (box 0))
            (define xrb (box 0))
            (define yrb (box 0))

            (define/private (find-poss text left-pos right-pos)
              (send text position-location left-pos xlb ylb #t)
              (send text position-location right-pos xrb yrb #f)
              (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                            [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                            [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                            [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                (values (/ (+ xl xr) 2)
                        (/ (+ yl yr) 2))))
            
            ;; syncheck:init-arrows : -> void
            (define/public (syncheck:init-arrows)
              (set! tacked-hash-table (make-hasheq))
              (set! arrow-records (make-hasheq))
              (set! bindings-table (make-hash))
              (set! cleanup-texts '()))
            
            (define/public (syncheck:arrows-visible?)
              (or arrow-records cursor-pos cursor-text cursor-eles cursor-tooltip))
            
            ;; syncheck:clear-arrows : -> void
            (define/public (syncheck:clear-arrows)
              (when (syncheck:arrows-visible?)
                (set! tacked-hash-table #f)
                (set! arrow-records #f)
                (when (update-latent-arrows #f #f)
                  (update-drawn-arrows))
                (syncheck:clear-coloring)
                (invalidate-bitmap-cache)))
            
            (define/public (syncheck:clear-coloring)
              (when cleanup-texts
                (for-each (λ (text) (send text thaw-colorer))
                          cleanup-texts))
              (set! cleanup-texts #f))
            
            ;; syncheck:apply-style/remember : (is-a?/c text%) number number style% symbol -> void
            (define/public (syncheck:apply-style/remember txt start finish style)
              (add-to-cleanup/apply-style txt start finish style))
            
            (define/public (syncheck:color-range source start finish style-name)
              (when (is-a? source text%)
                (define (apply-style/remember ed start finish style)
                  (let ([outermost (find-outermost-editor ed)])
                    (and (is-a? outermost syncheck-text<%>)
                         (send outermost syncheck:apply-style/remember ed start finish style))))
                
                (define (find-outermost-editor ed)
                  (let loop ([ed ed])
                    (let ([admin (send ed get-admin)])
                      (if (is-a? admin editor-snip-editor-admin<%>)
                          (let* ([enclosing-snip (send admin get-snip)]
                                 [enclosing-snip-admin (send enclosing-snip get-admin)])
                            (loop (send enclosing-snip-admin get-editor)))
                          ed))))
                
                (let ([style (send (send source get-style-list)
                                   find-named-style
                                   style-name)])
                  (apply-style/remember source start finish style))))

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
            
            (define/public (syncheck:add-require-open-menu text start-pos end-pos file)
              (define ((make-require-open-menu file) menu)
                (define-values (base name dir?) (split-path file))
                (new menu-item%
                     (label (fw:gui-utils:format-literal-label (string-constant cs-open-file) (path->string name)))
                     (parent menu)
                     (callback (λ (x y) (fw:handler:edit-file file))))
                (void))
              (syncheck:add-menu text start-pos end-pos #f (make-require-open-menu file)))
            
            (define/public (syncheck:add-docs-menu text start-pos end-pos id the-label path definition-tag tag)
              (define (visit-docs-url)
                (define url (path->url path))
                (define url2 (if tag
                                 (make-url (url-scheme url)
                                           (url-user url)
                                           (url-host url)
                                           (url-port url)
                                           (url-path-absolute? url)
                                           (url-path url)
                                           (url-query url)
                                           tag)
                                 url))
                (send-url (url->string url2)))
              (syncheck:add-docs-range start-pos end-pos definition-tag visit-docs-url)
              (syncheck:add-menu 
               text start-pos end-pos id
               (λ (menu)
                 (instantiate menu-item% ()
                   (parent menu)
                   (label (gui-utils:format-literal-label "~a" the-label))
                   (callback
                    (λ (x y)
                      (visit-docs-url)))))))
            
            (define/public (syncheck:add-rename-menu id-as-sym to-be-renamed/poss name-dup?)
              (define (make-menu menu)
                (let ([name-to-offer (format "~a" id-as-sym)])
                  (new menu-item%
                       [parent menu]
                       [label (fw:gui-utils:format-literal-label (string-constant cs-rename-var) name-to-offer)]
                       [callback
                        (λ (x y)
                          (let ([frame-parent (find-menu-parent menu)])
                            (rename-callback name-to-offer
                                             frame-parent)))])))
              
              ;; rename-callback : string 
              ;;                   (and/c syncheck-text<%> definitions-text<%>)
              ;;                   (list source number number)
              ;;                   (listof id-set) 
              ;;                   (union #f (is-a?/c top-level-window<%>)) 
              ;;                -> void
              ;; callback for the rename popup menu item
              (define (rename-callback name-to-offer parent)
                (let ([new-str
                       (fw:keymap:call/text-keymap-initializer
                        (λ ()
                          (get-text-from-user
                           (string-constant cs-rename-id)
                           (fw:gui-utils:format-literal-label (string-constant cs-rename-var-to) name-to-offer)
                           parent
                           name-to-offer
                           #:dialog-mixin frame:focus-table-mixin)))])
                  (when new-str
                    (define new-sym (format "~s" (string->symbol new-str)))
                    (define dup-name? (name-dup? new-sym))
                    
                    (define do-renaming?
                      (or (not dup-name?)
                          (equal?
                           (message-box/custom
                            (string-constant check-syntax)
                            (fw:gui-utils:format-literal-label (string-constant cs-name-duplication-error) 
                                                               new-sym)
                            (string-constant cs-rename-anyway)
                            (string-constant cancel)
                            #f
                            parent
                            '(stop default=2)
                            #:dialog-mixin frame:focus-table-mixin)
                           1)))
                    
                    (when do-renaming?
                      (unless (null? to-be-renamed/poss)
                        (let ([txts (list this)])
                          (define positions-to-rename 
                            (remove-duplicates
                             (sort to-be-renamed/poss
                                   >
                                   #:key cadr)))
                          (begin-edit-sequence)
                          (for ([info (in-list positions-to-rename)])
                            (define source-editor (list-ref info 0))
                            (define start (list-ref info 1))
                            (define end (list-ref info 2))
                            (when (is-a? source-editor text%)
                              (unless (memq source-editor txts)
                                (send source-editor begin-edit-sequence)
                                (set! txts (cons source-editor txts)))
                              (send source-editor delete start end #f)
                              (send source-editor insert new-sym start start #f)))
                          (invalidate-bitmap-cache)
                          (for ([txt (in-list txts)])
                            (send txt end-edit-sequence))))))))
              
              
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
              
              (for ([loc (in-list to-be-renamed/poss)])
                (define source (list-ref loc 0))
                (define start (list-ref loc 1))
                (define fin (list-ref loc 2))
                (syncheck:add-menu source start fin id-as-sym make-menu)))
              
            (define/private (syncheck:add-menu text start-pos end-pos key make-menu)
              (when arrow-records
                (when (<= 0 start-pos end-pos (last-position))
                  (add-to-range/key text start-pos end-pos make-menu key (and key #t)))))
            
            (define/public (syncheck:add-background-color text start fin raw-color)
              (when arrow-records
                (when (is-a? text text:basic<%>)
                  ;; we adjust the colors over here based on the white-on-black
                  ;; preference so we don't have to have the preference set up
                  ;; in the other place when running check syntax in online mode.
                  (define color 
                    (if (preferences:get 'framework:white-on-black?)
                        (cond
                          [(equal? raw-color "palegreen") "darkgreen"]
                          [else raw-color])
                        raw-color))
                  (add-to-range/key text start fin (make-colored-region color text start fin) #f #f))))
            
            ;; syncheck:add-arrow : symbol text number number text number number boolean -> void
            ;; pre: start-editor, end-editor are embedded in `this' (or are `this')
            (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                               end-text end-pos-left end-pos-right
                                               actual? level)
              (when (and arrow-records
                         (preferences:get 'drracket:syncheck:show-arrows?))
                (when (add-to-bindings-table
                       start-text start-pos-left start-pos-right
                       end-text end-pos-left end-pos-right)
                  (let ([arrow (make-var-arrow start-text start-pos-left start-pos-right
                                               end-text end-pos-left end-pos-right
                                               actual? level)])
                    (add-to-range/key start-text start-pos-left start-pos-right arrow #f #f)
                    (add-to-range/key end-text end-pos-left end-pos-right arrow #f #f)))))
            
            ;; syncheck:add-tail-arrow : text number text number -> void
            (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos)
              (when (and arrow-records
                         (preferences:get 'drracket:syncheck:show-arrows?))
                (let ([tail-arrow (make-tail-arrow to-text to-pos from-text from-pos)])
                  (add-to-range/key from-text from-pos (+ from-pos 1) tail-arrow #f #f)
                  (add-to-range/key to-text to-pos (+ to-pos 1) tail-arrow #f #f))))
            
            ;; syncheck:add-jump-to-definition : text start end id filename -> void
            (define/public (syncheck:add-jump-to-definition text start end id filename)
              (when arrow-records
                (add-to-range/key text start end (make-def-link id filename) #f #f)))
            
            ;; syncheck:add-mouse-over-status : text pos-left pos-right string -> void
            (define/public (syncheck:add-mouse-over-status text pos-left pos-right str)
              (when arrow-records
                (add-to-range/key text pos-left pos-right 
                                  (make-tooltip-info text pos-left pos-right str)
                                  #f #f)))
            
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
                                                    (cons (cons key to-add) old)))
                                              null)]
                      [else
                       (interval-map-cons*! arrow-record
                                            start (add1 end) 
                                            to-add null)])))
            
            (inherit get-top-level-window)
            
            (define/augment (on-change)
              (inner (void) on-change)
              (when arrow-records
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
            
            (define view-corner-hash (make-weak-hasheq))
            
            (define/private (get-last-view-corner admin)
              (hash-ref view-corner-hash admin (λ () (cons #f #f))))
            
            (define/private (set-last-view-corner! admin corner)
              (hash-set! view-corner-hash admin corner))
            
            (define/private (get-view-corner admin)
              (define new-x (box #f))
              (define new-y (box #f))
              (send admin get-view new-x new-y #f #f)
              (cons (unbox new-x) (unbox new-y)))
            
            (define/private (update-view-corner admin)
              (define old-corner (get-last-view-corner admin))
              (define new-corner (get-view-corner admin))
              (define scrolled? (not (equal? old-corner new-corner)))
              (set-last-view-corner! admin new-corner)
              scrolled?)
            
            (define/override (on-paint before dc left top right bottom dx dy draw-caret)
              (when (and arrow-records (not before))
                (define admin (get-admin))
                ;; update the known editor location for the upper-left corner
                (define scrolled? (update-view-corner admin))
                ;; when painting on the canvas the mouse is over...
                (when (eq? mouse-admin admin)
                  (define update-tooltip-frame?
                    (cond
                      ;; turn off arrows immediately if scrolling
                      [scrolled? (set! cursor-tooltip #f)
                                 (set! cursor-pos #f)
                                 (set! cursor-text #f)
                                 (set! cursor-eles #f)
                                 (update-docs-background #f)
                                 (start-arrow-draw-cooldown syncheck-scroll-arrow-cooldown)
                                 #t]
                      ;; try to update the tooltips if they're wrong
                      [(eq? cursor-tooltip 'out-of-sync)
                       (set! cursor-tooltip (get-tooltip cursor-eles))
                       (not (eq? cursor-tooltip 'out-of-sync))]
                      [else #f]))
                  (when update-tooltip-frame?
                    (update-tooltip-frame))
                  ;; update on a timer if the arrows changed
                  (when (update-latent-arrows mouse-x mouse-y)
                    (start-arrow-draw-timer syncheck-arrow-delay)))
                (let ([draw-arrow2
                       (λ (arrow)
                         (define-values (start-x start-y end-x end-y)
                           (get-arrow-poss arrow))
                         (unless (and (= start-x end-x)
                                      (= start-y end-y))
                           (drracket:arrow:draw-arrow dc start-x start-y end-x end-y dx dy
                                                      #:pen-width 2)
                           (when (and (var-arrow? arrow) (not (var-arrow-actual? arrow)))
                             (let-values ([(fw fh _d _v) (send dc get-text-extent "x")])
                               (send dc draw-text "?"
                                     (+ end-x dx fw)
                                     (+ end-y dy (- fh)))))))]
                      [old-brush (send dc get-brush)]
                      [old-pen   (send dc get-pen)]
                      [old-font  (send dc get-font)]
                      [old-text-foreground (send dc get-text-foreground)]
                      [old-text-mode (send dc get-text-mode)]
                      [old-alpha (send dc get-alpha)]
                      [white-on-black? (preferences:get 'framework:white-on-black?)]) 
                  (send dc set-font
                        (send the-font-list find-or-create-font
                              (send old-font get-point-size)
                              'default
                              'normal
                              'bold))
                  (send dc set-text-foreground templ-color)
                  (send dc set-alpha 0.5)
                  (hash-for-each tacked-hash-table
                                 (λ (arrow v) 
                                    (when v 
                                      (cond
                                       [(var-arrow? arrow)
                                        (if (var-arrow-actual? arrow)
                                            (begin (send dc set-pen (get-var-pen white-on-black?))
                                                   (send dc set-brush (get-tacked-var-brush white-on-black?)))
                                            (begin (send dc set-pen (get-templ-pen white-on-black?))
                                                   (send dc set-brush (get-tacked-templ-brush white-on-black?))))]
                                       [(tail-arrow? arrow)
                                        (send dc set-pen (get-tail-pen white-on-black?))
                                        (send dc set-brush (get-tacked-tail-brush white-on-black?))])
                                      (draw-arrow2 arrow))))
                  (when (and cursor-pos
                             cursor-text)
                    (define arrow-record (hash-ref arrow-records cursor-text #f))
                    (define tail-arrows '())
                    (when arrow-record
                      (for ([ele (in-list (interval-map-ref arrow-record cursor-pos null))])
                        (cond [(var-arrow? ele)
                               (if (var-arrow-actual? ele)
                                   (begin (send dc set-pen (get-var-pen white-on-black?))
                                          (send dc set-brush (get-untacked-brush white-on-black?)))
                                   (begin (send dc set-pen (get-templ-pen white-on-black?))
                                          (send dc set-brush (get-untacked-brush white-on-black?))))
                               (draw-arrow2 ele)]
                              [(tail-arrow? ele)
                               (set! tail-arrows (cons ele tail-arrows))])))
                    
                    (send dc set-pen (get-tail-pen white-on-black?))
                    (send dc set-brush (get-untacked-brush white-on-black?))
                    (for-each-tail-arrows draw-arrow2 tail-arrows))
                  (send dc set-brush old-brush)
                  (send dc set-pen old-pen)
                  (send dc set-font old-font)
                  (send dc set-text-foreground old-text-foreground)
                  (send dc set-text-mode old-text-mode)
                  (send dc set-alpha old-alpha)))
              
              ;; do the drawing before calling super so that the arrows don't
              ;; cross the "#lang ..." line, if it is present.
              (super on-paint before dc left top right bottom dx dy draw-caret))
            
            ;; for-each-tail-arrows : (tail-arrow -> void) tail-arrow -> void
            (define/private (for-each-tail-arrows f tail-arrows)
              ;; call-f-ht ensures that `f' is only called once per arrow
              (define call-f-ht (make-hash))
              
              (for ([tail-arrow (in-list tail-arrows)])
                (define (for-each-tail-arrows/to/from tail-arrow-pos tail-arrow-text
                                                      tail-arrow-other-pos tail-arrow-other-text)
                  
                  ;; traversal-ht ensures that we don't loop in the arrow traversal.
                  (let ([traversal-ht (make-hasheq)])
                    (let loop ([tail-arrow tail-arrow])
                      (unless (hash-ref traversal-ht tail-arrow #f)
                        (hash-set! traversal-ht tail-arrow #t)
                        (unless (hash-ref call-f-ht tail-arrow #f)
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
                                              tail-arrow-to-pos tail-arrow-to-text)))
            
            ;; after a short delay, current-* are set to latent-*, and arrows are drawn
            (define latent-pos #f)
            (define latent-text #f)
            (define latent-eles #f)
            (define latent-tooltip #f)
            
            (define cursor-pos #f)
            (define cursor-text #f)
            (define cursor-eles #f)
            (define cursor-tooltip #f)
            
            ;; this gives errors if constructed immediately
            (define arrow-draw-timer #f)
            ;; Starts or restarts a one-shot arrow draw timer
            (define/private (start-arrow-draw-timer delay-ms)
              (unless arrow-draw-timer
                (set! arrow-draw-timer (make-object logging-timer% (λ () (maybe-update-drawn-arrows)))))
              (send arrow-draw-timer start delay-ms #t))
            
            ;; this will be set to a time in the future if arrows shouldn't be drawn until then
            (define arrow-draw-cooldown-time (current-milliseconds))
            ;; Starts an arrow draw cooldown
            (define/private (start-arrow-draw-cooldown delay-ms)
              (set! arrow-draw-cooldown-time (+ (current-milliseconds) delay-ms)))
            
            ;; The arrow-draw-timer proc
            (define/private (maybe-update-drawn-arrows)
              (cond
                [(arrow-draw-cooldown-time . > . (current-milliseconds))
                 ;; keep restarting the timer until we pass cooldown-time
                 ;; (should happen < 5 times for a scroll, which is cheaper than mouse move)
                 (start-arrow-draw-timer 100)]
                [else
                 (update-drawn-arrows)]))
            
            (define tooltips-enabled? #f)
            (define/public (enable-tooltips x?)
              (set! tooltips-enabled? x?)
              (when (update-latent-arrows mouse-x mouse-y)
                (start-arrow-draw-timer syncheck-arrow-delay)))
            
            ;; Given a mouse position, updates latent-* variables and tooltips
            (define/private (update-latent-arrows x y)
              (define-values (pos text eles tooltip)
                (cond
                  ;; need to check this first so syncheck:clear-arrows will work
                  [(not arrow-records)
                   (values #f #f #f #f)]
                  [(and x y)
                   (define-values (pos text) (get-pos/text-dc-location x y))
                   (define arrow-record (and text pos (hash-ref arrow-records text #f)))
                   (define eles (and arrow-record (interval-map-ref arrow-record pos null)))
                   (define tooltip (cond [(not tooltips-enabled?) #f]
                                         [(and (equal? latent-eles eles) latent-tooltip)
                                          latent-tooltip]
                                         [else (get-tooltip eles)]))
                   (values pos text eles tooltip)]
                  [else
                   (values #f #f #f #f)]))
              (define text-changed? (not (eq? latent-text text)))
              (define eles-changed? (not (equal? latent-eles eles)))
              (define tooltip-changed? (not (equal? latent-tooltip tooltip)))
              
              (set! latent-pos pos)
              (set! latent-text text)
              (set! latent-eles eles)
              (set! latent-tooltip tooltip)
              
              (or text-changed? eles-changed? tooltip-changed?))
            
            (define/private (update-drawn-arrows)
              (set! cursor-pos latent-pos)
              (set! cursor-text latent-text)
              (set! cursor-eles latent-eles)
              (set! cursor-tooltip latent-tooltip)
              
              (update-tooltip-frame)
              (update-docs-background cursor-eles)
              
              (invalidate-bitmap-cache))
            
            (define mouse-admin #f)  ; editor admin for the last mouse move
            (define mouse-x #f)      ; last known mouse position
            (define mouse-y #f)
            (define/override (on-event event)
              (define-values (x y)
                (cond [(send event leaving?) (values #f #f)]
                      [else (values (send event get-x) (send event get-y))]))
              
              (set! mouse-admin (get-admin))
              (set! mouse-x x)
              (set! mouse-y y)
              
              ;; mouse motion cancels arrow draw cooldown
              (when (eq? 'motion (send event get-event-type))
                (set! arrow-draw-cooldown-time (current-milliseconds)))
              
              ;; if the arrows changed, start the draw timer
              (when (update-latent-arrows x y)
                (start-arrow-draw-timer syncheck-arrow-delay))
              
              (super on-event event))
            
            (define/public (syncheck:update-drawn-arrows)
              ;; This will ensure on-paint is called, once for each canvas that
              ;; is displaying the editor. In the on-paint call for the canvas
              ;; that the mouse is over, arrows will be updated, arrow-draw-timer
              ;; will be set, etc.
              ;; If this were done more directly, the tooltip would show up in
              ;; the wrong canvas half the time - when the current admin isn't
              ;; the admin for the canvas the mouse is over.
              (invalidate-bitmap-cache 0 0 'display-end 'display-end))
            
            (define/public (syncheck:build-popup-menu menu pos text)
              (when arrow-records
                (define arrow-record (hash-ref arrow-records text #f))
                (when arrow-record
                  (define added-sep? #f)
                  (define (add-sep) 
                    (unless added-sep? 
                      (set! added-sep? #t)
                      (new separator-menu-item% [parent menu])))
                  (define vec-ents (interval-map-ref arrow-record pos null))
                  (define start-selection (send text get-start-position))
                  (define end-selection (send text get-end-position))
                  (define arrows (filter arrow? vec-ents))
                  (define def-links (filter def-link? vec-ents))
                  (define var-arrows (filter var-arrow? arrows))
                  (define add-menus (append (map cdr (filter pair? vec-ents))
                                            (filter procedure? vec-ents)))
                  (unless (null? arrows)
                    (add-sep)
                    (make-object menu-item%
                      (string-constant cs-tack/untack-arrow)
                      menu
                      (λ (item evt) (tack/untack-callback arrows))))
                  (unless (null? def-links)
                    (add-sep)
                    (let ([def-link (car def-links)])
                      (make-object menu-item%
                        jump-to-definition
                        menu
                        (λ (item evt)
                          (jump-to-definition-callback def-link)))))
                  (unless (null? var-arrows)
                    (add-sep)
                    (make-object menu-item%
                      jump-to-next-bound-occurrence
                      menu
                      (λ (item evt) (jump-to-next-callback pos text arrows)))
                    (make-object menu-item%
                      jump-to-binding
                      menu
                      (λ (item evt) (jump-to-binding-callback arrows))))
                  (unless (= start-selection end-selection)
                    (add-sep)
                    (define arrows-menu
                      (make-object menu%
                        "Arrows crossing selection"
                        menu))
                    (define (callback accept)
                      (tack-crossing-arrows-callback
                       arrow-record
                       start-selection
                       end-selection
                       text
                       accept))
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
                         end-selection))))
                  (for-each (λ (f) (f menu)) add-menus))))
            
            (define tooltip-frame #f)
            (define/private (update-tooltip-frame)
              (unless tooltip-frame (set! tooltip-frame (new tooltip-frame%)))
              (match cursor-tooltip
                [(tooltip-spec strings x y w h)
                 ;; hiding keeps it from flashing the new tooltip in the old location
                 (send tooltip-frame show #f)
                 (send tooltip-frame set-tooltip strings)
                 (send tooltip-frame show-over x y w h)]
                ;; #f or 'out-of-sync
                [_ (send tooltip-frame show #f)]))
            
            ;; Sometimes when this is called, the calls to 'tooltip-info->ltrb'
            ;; fail and we get no information back. When that happens, we return 
            ;; 'out-of-sync and try again in on-paint (which happens every time
            ;; the caret blinks).
            (define/private (get-tooltip eles)
              (define tooltip-infos (if eles (filter tooltip-info? eles) null))
              (let loop ([tooltip-infos tooltip-infos]
                         [l #f] [t #f] [r #f] [b #f]
                         [strings (set)])
                (cond
                  [(null? tooltip-infos)
                   (cond
                     [(and l t r b)
                      (define-values (dx dy) (get-display-left-top-inset))
                      (tooltip-spec (sort (set->list strings) string<=?)
                                    (- l dx) (- t dy) (- r l) (- b t))]
                     [else #f])]
                  [else
                   (define-values (tl tt tr tb) (tooltip-info->ltrb (car tooltip-infos)))
                   (cond
                     [(and tl tt tr tb)
                      (define (min/f x y) (cond [(and x y) (min x y)] [x x] [y y] [else #f]))
                      (define (max/f x y) (cond [(and x y) (max x y)] [x x] [y y] [else #f]))
                      (loop (cdr tooltip-infos)
                            (min/f tl l) (min/f tt t) (max/f tr r) (max/f tb b)
                            (set-add strings (tooltip-info-msg (car tooltip-infos))))]
                     [else
                      ;(printf "~a: out of sync~n" (current-milliseconds))
                      'out-of-sync])])))
            
            ;; Given an editor, returns the canvas that the mouse is currently over,
            ;; as opposed to the one with keyboard focus (which get-canvas usually returns)
            (define/private (find-mouse-canvas ed)
              (define current-admin (send ed get-admin))
              (let/ec return
                (for ([canvas  (in-list (send ed get-canvases))])
                  (define admin (send canvas call-as-primary-owner
                                      (λ () (send ed get-admin))))
                  (when (eq? admin current-admin)
                    (return canvas)))
                (send ed get-canvas)))
            
            (define/private (tooltip-info->ltrb tooltip)
              (define left-pos (tooltip-info-pos-left tooltip))
              (define right-pos (tooltip-info-pos-right tooltip))
              (define text (tooltip-info-text tooltip))
              
              (define eol-pos (line-end-position (position-line right-pos)))
              
              (send text position-location eol-pos xlb ylb #t #t)
              (define-values (x-off y-off) (send text editor-location-to-dc-location (+ (unbox xlb) 4) (unbox ylb)))
              (define window
                (let loop ([ed text])
                  (cond
                    [(find-mouse-canvas ed) => values]
                    [else
                     (define admin (send ed get-admin))
                     (if (is-a? admin editor-snip-editor-admin<%>)
                         (loop (send (send admin get-snip) get-editor))
                         #f)])))
              (cond
                [(and window (< -10000 x-off 10000) (< -10000 y-off 10000))
                 (define (c n) (inexact->exact (round n)))
                 (define-values (gx gy) (send window client->screen (c x-off) (c y-off)))
                 (values gx gy gx gy)]
                [else
                 (values #f #f #f #f)]))
            
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
                       (list arrow))]))
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
                       (list arrow))]))
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

            (define/public (syncheck:find-source-object stx)
              (cond
                [(not (syntax-source stx)) #f]
                [(and (symbol? (syntax-source stx))
                      (text:lookup-port-name (syntax-source stx)))
                 => values]
                [else
                 (let txt-loop ([text this])
                   (cond
                     [(and (is-a? text text:basic<%>)
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
            
            (super-new)))))
    
    (keymap:add-to-right-button-menu
     (let ([old (keymap:add-to-right-button-menu)])
       (λ (menu editor event)
         (old menu editor event)
         (when (is-a? editor syncheck-text<%>)
           (define-values (pos text) (send editor get-pos/text event))
           (when (and pos (is-a? text text%))
             (send editor syncheck:build-popup-menu menu pos text))))))
            
    (define syncheck-frame<%>
      (interface ()
        syncheck:button-callback
        syncheck:error-report-visible?
        syncheck:get-error-report-contents))
    
    (define tab-mixin
      
      (mixin (drracket:unit:tab<%>) ()
        (inherit is-current-tab? get-defs get-frame)
        
        (define report-error-text-has-something? #f)
        (define report-error-text (new (fw:text:ports-mixin fw:racket:text%)))
        (define error-report-visible? #f)
        (send report-error-text auto-wrap #t)
        (send report-error-text set-autowrap-bitmap #f)
        (send report-error-text lock #t)
        
        (define/public (get-error-report-text) report-error-text)
        (define/public (get-error-report-visible?) error-report-visible?)
        (define/public (turn-on-error-report) (set! error-report-visible? #t))
        (define/public (turn-off-error-report) 
          (when error-report-visible?
            (send report-error-text clear-output-ports)
            (send report-error-text lock #f)
            (send report-error-text delete/io 0 (send report-error-text last-position))
            (send report-error-text lock #t)
            (set! error-report-visible? #f)))
        (define/augment (clear-annotations)
          (inner (void) clear-annotations)
          (syncheck:clear-error-message)
          ;; we only clear out the highlighting that check syntax 
          ;; may have introduced here; we don't reset the arrows 
          ;; (or other mouse-over stuff)
          ;; this code is also run by syncheck:clear-arrows, which
          ;; used to be called here (indirectly by syncheck:clear-highlighting)
          (send (get-defs) syncheck:clear-coloring))

        (define/public (syncheck:clear-error-message)
          (define old-error-report-visible? error-report-visible?)
          (turn-off-error-report)
          (when old-error-report-visible?
            (when (is-current-tab?)
              (send (get-frame) hide-error-report))))
        
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
          (send (send old-tab get-defs) enable-tooltips #f)
          (send (send new-tab get-defs) enable-tooltips #t)
          (send (send old-tab get-defs) syncheck:update-drawn-arrows)
          (send (send new-tab get-defs) syncheck:update-drawn-arrows)
          (if (send new-tab get-error-report-visible?)
              (show-error-report)
              (hide-error-report))
          (send report-error-canvas set-editor (send new-tab get-error-report-text))
          (update-button-visibility/tab new-tab))
        
        (define/override (on-activate active?)
          (define defs (send (get-current-tab) get-defs))
          (send defs enable-tooltips active?)
          (send defs syncheck:update-drawn-arrows)
          (super on-activate active?))
        
        (define/private (update-button-visibility/tab tab)
          (update-button-visibility/settings (send (send tab get-defs) get-next-settings)))
        (inherit sort-toolbar-buttons-panel)
        (define/public (update-button-visibility/settings settings)
          (let* ([lang (drracket:language-configuration:language-settings-language settings)]
                 [visible? (and (not (is-a? lang drracket:module-language:module-language<%>))
                                (send lang capability-value 'drscheme:check-syntax-button))])
            (send (get-button-panel) change-children
                  (λ (l)
                    (if visible?
                        (cons check-syntax-button (remq check-syntax-button l))
                        (remq check-syntax-button l))))
            (sort-toolbar-buttons-panel)))
        
        ;; set-syncheck-running-mode : (or/c (box boolean?) 'button #f) -> boolean
        ;; records how a particular check syntax is being played out in the editor right now.
        ;; - #f means nothing is currently running.
        ;; - 'button means someone clicked the check syntax button (or the menu item or keyboard shortcut...)
        ;; - the boxed boolean means that a trace is being replayed from the other place.
        ;;   if the box is set to #f, then the trace replay will be stopped.
        ;; if #f is returned, then the mode change is not allowed; this only happens when
        ;;    a box is passed in
        (define/public (set-syncheck-running-mode mode)
          (cond
            [(not mode)
             (when (box? current-syncheck-running-mode)
               (set-box! current-syncheck-running-mode #f))
             (set! current-syncheck-running-mode #f)
             #t]
            [(box? mode)
             (cond
               [(eq? current-syncheck-running-mode 'button)
                #f]
               [(eq? mode current-syncheck-running-mode)
                ;; this shouldn't happen, I think
                #t]
               [else
                (when (box? current-syncheck-running-mode)
                  (set-box! current-syncheck-running-mode #f))
                (set! current-syncheck-running-mode mode)
                #t])]
            [(eq? 'button mode)
             (when (box? current-syncheck-running-mode)
               (set-box! current-syncheck-running-mode #f))
             (set! current-syncheck-running-mode mode)
             #t]
            [else
             (error 'set-syncheck-running-mode "unknown new mode ~s\n" mode)]))
        
        (define current-syncheck-running-mode #f)
    
        (define/public (replay-compile-comp-trace defs-text val)
          (define bx (box #t))
          (when (set-syncheck-running-mode bx)
            
            ;; reset any previous check syntax information
            (let ([tab (send defs-text get-tab)])
              (send tab syncheck:clear-error-message)
              (send tab syncheck:clear-highlighting)
              (send defs-text syncheck:reset-docs-im))
            
            (send (send defs-text get-tab) add-bkg-running-color 'syncheck "orchid" cs-syncheck-running)
            (send defs-text syncheck:init-arrows)
            (let loop ([val val]
                       [start-time (current-inexact-milliseconds)]
                       [i 0])
              (cond
                [(null? val)
                 (send defs-text syncheck:update-blue-boxes)
                 (send defs-text syncheck:update-drawn-arrows)
                 (send (send defs-text get-tab) remove-bkg-running-color 'syncheck)
                 (set-syncheck-running-mode #f)]
                [(and (i . > . 0)  ;; check i just in case things are really strange
                      (20 . <= . (- (current-inexact-milliseconds) start-time)))
                 (queue-callback
                  (λ ()
                    (when (unbox bx)
                      (log-timeline "continuing replay-compile-comp-trace"
                                    (loop val (current-inexact-milliseconds) 0))))
                  #f)]
                [else
                 (process-trace-element defs-text (car val))
                 (loop (cdr val) start-time (+ i 1))]))))
        
        (define/private (process-trace-element defs-text x)
          ;; using 'defs-text' all the time is wrong in the case of embedded editors,
          ;; but they already don't work and we've arranged for them to not appear here ....
          (match x
            [`#(syncheck:add-arrow ,start-pos-left ,start-pos-right
                                   ,end-pos-left ,end-pos-right
                                   ,actual? ,level)
             (send defs-text syncheck:add-arrow
                   defs-text start-pos-left start-pos-right
                   defs-text end-pos-left end-pos-right 
                   actual? level)]
            [`#(syncheck:add-tail-arrow ,from-pos ,to-pos)
             (send defs-text syncheck:add-tail-arrow defs-text from-pos defs-text to-pos)]
            [`#(syncheck:add-mouse-over-status ,pos-left ,pos-right ,str)
             (send defs-text syncheck:add-mouse-over-status defs-text pos-left pos-right str)]
            [`#(syncheck:add-background-color ,color ,start ,fin)
             (send defs-text syncheck:add-background-color defs-text color start fin)]
            [`#(syncheck:add-jump-to-definition ,start ,end ,id ,filename)
             (send defs-text syncheck:add-jump-to-definition defs-text start end id filename)]
            [`#(syncheck:add-require-open-menu ,start-pos ,end-pos ,file)
             (send defs-text syncheck:add-require-open-menu defs-text start-pos end-pos file)]
            [`#(syncheck:add-docs-menu,start-pos ,end-pos ,key ,the-label ,path ,definition-tag ,tag)
             (send defs-text syncheck:add-docs-menu defs-text start-pos end-pos key the-label path definition-tag tag)]
            [`#(syncheck:add-rename-menu ,id-as-sym ,to-be-renamed/poss ,name-dup-pc ,name-dup-id)
             (define other-side-dead? #f)
             (define (name-dup? name) 
               (cond
                 [other-side-dead? 
                  ;; just give up here ...
                  #f]
                 [else
                  (place-channel-put name-dup-pc (list name-dup-id name))
                  (define res (sync/timeout .5 (handle-evt name-dup-pc list)))
                  (cond
                    [(list? res) (car res)]
                    [else
                     (printf "other side died\n")
                     (set! other-side-dead? #t)
                     #f])]))
             (define to-be-renamed/poss/fixed
               (for/list ([lst (in-list to-be-renamed/poss)])
                 (list defs-text (list-ref lst 0) (list-ref lst 1))))
             (send defs-text syncheck:add-rename-menu id-as-sym to-be-renamed/poss/fixed 
                   name-dup?)]))
        
        (define/augment (enable-evaluation)
          (send check-syntax-button enable #t)
          (inner (void) enable-evaluation))
        
        (define/augment (disable-evaluation)
          (send check-syntax-button enable #f)
          (inner (void) disable-evaluation))
        
        (define report-error-parent-panel 'uninitialized-report-error-parent-panel)
        (define report-error-panel 'uninitialized-report-error-panel)
        (define report-error-canvas 'uninitialized-report-error-editor-canvas)
        (define/override (get-definitions/interactions-panel-parent)
          (set! report-error-parent-panel
                (new (class panel:vertical-dragable%
                       (inherit get-percentages)
                       (define record-prefs? #f)
                       (define/public (stop-recording-prefs) (set! record-prefs? #f))
                       (define/public (start-recording-prefs) (set! record-prefs? #t))
                       (define/augment (after-percentage-change)
                         (define ps (get-percentages))
                         (when (and record-prefs? (= 2 (length ps)))
                           (preferences:set 'drracket:check-syntax-error-report-window-percentage (list-ref ps 0)))
                         (inner (void) after-percentage-change))
                       (super-new))
                     [parent (super get-definitions/interactions-panel-parent)]))
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
          (define res (make-object vertical-panel% report-error-parent-panel))
          res)
        
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
            (send report-error-parent-panel stop-recording-prefs)
            (send report-error-parent-panel change-children
                  (λ (l) (cons report-error-panel l)))
            (let ([p (preferences:get 'drracket:check-syntax-error-report-window-percentage)])
              (send report-error-parent-panel set-percentages 
                    (list p (- 1 p))))
            (send report-error-parent-panel start-recording-prefs)))
        
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
                
        (inherit open-status-line close-status-line update-status-line ensure-rep-hidden)
        ;; syncheck:button-callback : (case-> (-> void) ((union #f syntax) -> void)
        ;; this is the only function that has any code running on the user's thread
        (define/public (syncheck:button-callback [jump-to-id #f])
          (when (send check-syntax-button is-enabled?)
            (open-status-line 'drracket:check-syntax:status)
            (update-status-line 'drracket:check-syntax:status status-init)
            (ensure-rep-hidden)
            (define definitions-text (get-definitions-text))
            (define interactions-text (get-interactions-text))
            (define drs-eventspace (current-eventspace))
            (define the-tab (get-current-tab))
            (define-values (old-break-thread old-custodian) (send the-tab get-breakables))
            
            ;; set by the init-proc
            (define expanded-expression void)
            (define expansion-completed void)
            (define user-custodian #f)
            
            (define normal-termination? #f)
            
            (define show-error-report/tab
              (λ () ; =drs=
                (send the-tab turn-on-error-report)
                (send (send the-tab get-error-report-text) scroll-to-position 0)
                (when (eq? (get-current-tab) the-tab)
                  (show-error-report))))
            (define cleanup
              (λ () ; =drs=
                (send the-tab set-breakables old-break-thread old-custodian)
                (send the-tab enable-evaluation)
                (set-syncheck-running-mode #f) 
                (close-status-line 'drracket:check-syntax:status)
                
                ;; do this with some lag ... not great, but should be okay.
                (let ([err-port (send (send the-tab get-error-report-text) get-err-port)])
                  (thread
                   (λ ()
                     (flush-output err-port)
                     (queue-callback
                      (λ ()
                        (unless (= 0 (send (send the-tab get-error-report-text) last-position))
                          (show-error-report/tab)))))))))
            (define kill-termination
              (λ ()
                (unless normal-termination?
                  (parameterize ([current-eventspace drs-eventspace])
                    (queue-callback
                     (λ ()
                       (send the-tab syncheck:clear-highlighting)
                       (cleanup)
                       (custodian-shutdown-all user-custodian)))))))
            (define error-display-semaphore (make-semaphore 0))
            (define uncaught-exception-raised
              (λ () ;; =user=
                (set! normal-termination? #t)
                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (λ () ;;  =drs=
                     (yield error-display-semaphore) ;; let error display go first
                     (send the-tab syncheck:clear-highlighting)
                     (cleanup)
                     (custodian-shutdown-all user-custodian))))))
            (define error-port (send (send the-tab get-error-report-text) get-err-port))
            (define output-port (send (send the-tab get-error-report-text) get-out-port))
            (define init-proc
              (λ () ; =user=
                (send the-tab set-breakables (current-thread) (current-custodian))
                (set-directory definitions-text)
                (current-load-relative-directory #f)
                (current-error-port error-port)
                (current-output-port output-port)
                (error-display-handler 
                 (λ (msg exn) ;; =user=
                   (parameterize ([current-eventspace drs-eventspace])
                     (queue-callback
                      (λ () ;; =drs=
                        
                        ;; this has to come first or else the positioning
                        ;; computations in the highlight-errors/exn method
                        ;; will be wrong by the size of the error report box
                        (show-error-report/tab)
                        
                        ;; a call like this one also happens in 
                        ;; drracket:debug:error-display-handler/stacktrace
                        ;; but that call won't happen here, because
                        ;; the rep is not in the current-rep parameter
                        (send interactions-text highlight-errors/exn exn))))
                   
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
                (update-status-line 'drracket:check-syntax:status status-expanding-expression)
                (set!-values (expanded-expression expansion-completed) 
                             (make-traversal (current-namespace)
                                             (current-directory))) ;; set by set-directory above
                (set! user-custodian (current-custodian))))
            
            (set-syncheck-running-mode 'button)
            (send the-tab disable-evaluation) ;; this locks the editor, so must be outside.
            (define definitions-text-copy 
              (new (class text:basic%
                     ;; overriding get-port-name like this ensures
                     ;; that the resulting syntax objects are connected
                     ;; to the actual definitions-text, not this copy
                     (define/override (get-port-name)
                       (send definitions-text get-port-name))
                     (super-new))))
            (define settings (send definitions-text get-next-settings))
            (define module-language?
              (is-a? (drracket:language-configuration:language-settings-language settings)
                     drracket:module-language:module-language<%>))
            (send definitions-text-copy set-style-list (send definitions-text get-style-list)) ;; speeds up the copy
            (send definitions-text copy-self-to definitions-text-copy)
            (with-lock/edit-sequence
             definitions-text-copy
             (λ ()
               (send the-tab clear-annotations)
               (send the-tab reset-offer-kill)
               (send the-tab syncheck:clear-highlighting)
               (send (send the-tab get-defs) syncheck:init-arrows)
               (drracket:eval:expand-program
                #:gui-modules? #f
                (drracket:language:make-text/pos definitions-text-copy 0 (send definitions-text-copy last-position))
                settings
                (not module-language?)
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
                             (parameterize ([current-annotations definitions-text])
                               (expansion-completed))
                             (send definitions-text syncheck:sort-bindings-table)))
                          (cleanup)
                          (custodian-shutdown-all user-custodian))))]
                    [else
                     (open-status-line 'drracket:check-syntax:status)
                     (unless module-language?
                       (update-status-line 'drracket:check-syntax:status status-eval-compile-time)
                       (eval-compile-time-part-of-top-level sexp))
                     (parameterize ([current-eventspace drs-eventspace])
                       (queue-callback
                        (λ () ; =drs=
                          (with-lock/edit-sequence
                           definitions-text
                           (λ ()
                             (open-status-line 'drracket:check-syntax:status)
                             (update-status-line 'drracket:check-syntax:status status-coloring-program)
                             (parameterize ([current-annotations definitions-text])
                               (expanded-expression sexp (if jump-to-id (make-visit-id jump-to-id) void)))
                             (close-status-line 'drracket:check-syntax:status))))))
                     (update-status-line 'drracket:check-syntax:status status-expanding-expression)
                     (close-status-line 'drracket:check-syntax:status)
                     (loop)])))))))

        (define (make-visit-id jump-to-id)
          (λ (vars)
            (when jump-to-id
              (for ([id (in-list (syntax->list vars))])
                (let ([binding (identifier-binding id 0)])
                  (when (pair? binding)
                    (let ([nominal-source-id (list-ref binding 3)])
                      (when (eq? nominal-source-id jump-to-id)
                        (let ([stx id])
                          (let ([src (find-source-editor stx)]
                                [pos (syntax-position stx)]
                                [span (syntax-span stx)])
                            (when (and (is-a? src text%)
                                       pos
                                       span)
                              (send src begin-edit-sequence)
                              
                              ;; try to scroll so stx's location is
                              ;; near the top of the visible region
                              (let ([admin (send src get-admin)])
                                (when admin 
                                  (let ([wb (box 0.0)]
                                        [hb (box 0.0)]
                                        [xb (box 0.0)]
                                        [yb (box 0.0)])
                                    (send admin get-view #f #f wb hb)
                                    (send src position-location (- pos 1) xb yb #t #f #t)
                                    (let ([w (unbox wb)]
                                          [h (unbox hb)]
                                          [x (unbox xb)]
                                          [y (unbox yb)])
                                      (send src scroll-editor-to 
                                            (max 0 (- x (* .1 w)))
                                            (max 0 (- y (* .1 h)))
                                            w h
                                            #t
                                            'none)))))
                              
                              (send src set-position (- pos 1) (+ pos span -1))
                              (send src end-edit-sequence))))))))))))

        
        ;; set-directory : text -> void
        ;; sets the current-directory based on the file saved in the definitions-text
        (define/private (set-directory definitions-text)
          (define tmp-b (box #f))
          (define fn (send definitions-text get-filename tmp-b))
          (define dir (get-init-dir (and (not (unbox tmp-b)) fn)))
          (current-directory dir))
        
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
        
        (define check-syntax-button
          (new switchable-button%
               [label (string-constant check-syntax)]
               [bitmap syncheck-bitmap]
               [alternate-bitmap syncheck-small-bitmap]
               [parent (get-button-panel)]
               [callback (λ (button) (syncheck:button-callback))]))
        (inherit register-toolbar-button)
        (register-toolbar-button check-syntax-button #:number 50)
        (define/public (syncheck:get-button) check-syntax-button)
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
      (send keymap map-function "c:x;d" "jump to definition (in other file)")
      
      (send keymap add-function "show/hide blue boxes in upper-right corner"
            (λ (txt evt)
              (when (is-a? txt editor<%>)
                (let loop ([ed txt])
                  (define c (send ed get-canvas))
                  (cond
                    [c (let loop ([w c])
                         (cond
                           [(is-a? w drracket:unit:frame<%>)
                            (send (send w get-definitions-text) toggle-syncheck-docs)]
                           [(is-a? w area<%>)
                            (loop (send w get-parent))]))]
                    [else
                     (define admin (send ed get-admin))
                     (when (is-a? admin editor-snip-editor-admin<%>)
                       (define admin2 (send (send admin get-snip) get-admin))
                       (when admin2
                         (loop (send admin2 get-editor))))])))))
      (send keymap map-function "f2" "show/hide blue boxes in upper-right corner"))
    
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
    (drracket:module-language-tools:register-online-expansion-pref syncheck-add-to-online-expansion-prefs-panel)
    (drracket:language:register-capability 'drscheme:check-syntax-button (flat-contract boolean?) #t)
    (drracket:get/extend:extend-definitions-text make-syncheck-text%)
    (drracket:get/extend:extend-definitions-canvas docs-editor-canvas-mixin)
    (drracket:get/extend:extend-unit-frame unit-frame-mixin #f)
    (drracket:get/extend:extend-tab tab-mixin)
    
    (drracket:module-language-tools:add-online-expansion-handler
     online-comp.rkt
     'go
     (λ (defs-text val) 
       (log-timeline
        "replace-compile-comp-trace"
        (send (send (send defs-text get-tab) get-frame)
              replay-compile-comp-trace
              defs-text 
              val))))))

(define-runtime-path online-comp.rkt "online-comp.rkt")

#lang racket/base

;; should cache the count of new snips -- dont
;; use `count-snips'; use something associated with the
;; equal hash-table

(require mrlib/graph
         "reduction-semantics.rkt"
         "matcher.rkt"
         "size-snip.rkt"
         "dot.rkt"
         racket/gui/base
         racket/class
         racket/file
         framework)

(preferences:set-default 'plt-reducer:show-bottom #t boolean?)

(define dark-pen-color (make-parameter "blue"))
(define light-pen-color (make-parameter "lightblue"))
(define dark-brush-color (make-parameter "lightblue"))
(define light-brush-color (make-parameter "white"))
(define dark-text-color (make-parameter "blue"))
(define light-text-color (make-parameter "lightblue"))

;; after (about) this many steps, stop automatic, initial reductions
(define reduction-steps-cutoff (make-parameter 20))

(define-struct term-node (snip))
(define (term-node-parents term-node) (send (term-node-snip term-node) get-one-step-parents))
(define (term-node-children term-node) (send (term-node-snip term-node) get-one-step-children))
(define (term-node-expr term-node) (send (term-node-snip term-node) get-expr))
(define (term-node-labels term-node) (send (term-node-snip term-node) get-one-step-labels))
(define (term-node-set-color! term-node r?)
  (snip/eventspace
   term-node
   (λ ()
     (send (term-node-snip term-node) set-bad r?))))
(define (term-node-color term-node) (send (term-node-snip term-node) get-bad))

(define (term-node-set-red! term-node r?)
  (term-node-set-color! term-node (and r? "pink")))

(define (term-node-set-position! term-node x y)
  (snip/eventspace/ed 
   term-node
   (λ (ed)
     (when ed
       (send ed move-to (term-node-snip term-node) x y)))))

(define (term-node-width term-node)
  (snip/eventspace/ed 
   term-node
   (λ (ed)
     (let ([lb (box 0)]
           [rb (box 0)]
           [snip (term-node-snip term-node)])
       (if (and (send ed get-snip-location snip lb #f #f)
                (send ed get-snip-location snip rb #f #t))
           (- (unbox rb) (unbox lb))
           0)))))

(define (term-node-height term-node)
  (snip/eventspace/ed 
   term-node
   (λ (ed)
     (let ([tb (box 0)]
           [bb (box 0)]
           [snip (term-node-snip term-node)])
       (if (and (send ed get-snip-location snip #f tb #f)
                (send ed get-snip-location snip #f bb #t))
           (- (unbox bb) (unbox tb))
           0)))))

(define (term-node-x term-node)
  (snip/eventspace/ed 
   term-node
   (λ (ed)
     (let ([xb (box 0)]
           [snip (term-node-snip term-node)])
       (if (send ed get-snip-location snip xb #f #f)
           (unbox xb)
           0)))))

(define (term-node-y term-node)
  (snip/eventspace/ed 
   term-node
   (λ (ed)
     (let ([yb (box 0)]
           [snip (term-node-snip term-node)])
       (if (send ed get-snip-location snip #f yb #f)
           (unbox yb)
           0)))))

(define (snip/eventspace/ed term-node f)
  (snip/eventspace 
   term-node
   (λ ()
     (let* ([snip (term-node-snip term-node)]
            [admin (send snip get-admin)])
       (f (and admin (send admin get-editor)))))))

  
(define (snip/eventspace term-node thunk)
  (let* ([snip (term-node-snip term-node)]
         [eventspace (send snip get-my-eventspace)])
    (cond
      [(eq? (current-eventspace) eventspace)
       (thunk)]
      [else
       (let ([c (make-channel)])
         (parameterize ([current-eventspace eventspace])
           (queue-callback
            (λ ()
              (channel-put c (thunk)))))
         (channel-get c))])))

(define initial-font-size
  (make-parameter
   (send (send (send (editor:get-standard-style-list) 
                     find-named-style
                     "Standard")
               get-font)
         get-point-size)))

;; the initial spacing between row and columns of the reduction terms
(define default-x-spacing 15)
(define default-y-spacing 15)

(define (traces/ps reductions pre-exprs filename
                   #:multiple? [multiple? #f] 
                   #:pred [pred (λ (x) #t)] 
                   #:pp [pp default-pretty-printer] 
                   #:racket-colors? [racket-colors? #t]
                   #:scheme-colors? [scheme-colors? racket-colors?]
                   #:colors [colors '()]
                   #:layout [layout void]
                   #:edge-label-font [edge-label-font #f]
                   #:edge-labels? [edge-labels? #t]
                   #:graph-pasteboard-mixin [extra-graph-pasteboard-mixin values]
                   #:filter [term-filter (lambda (x y) #t)]
                   #:post-process [post-process void]
                   #:x-spacing [x-spacing default-x-spacing]
                   #:y-spacing [y-spacing default-x-spacing])
  (let-values ([(graph-pb canvas)
                (traces reductions pre-exprs
                        #:no-show-frame? #t
                        #:multiple? multiple? 
                        #:pred pred
                        #:pp pp
                        #:racket-colors? racket-colors?
                        #:scheme-colors? scheme-colors?
                        #:colors colors
                        #:layout layout
                        #:edge-label-font edge-label-font
                        #:edge-labels? edge-labels?
                        #:graph-pasteboard-mixin extra-graph-pasteboard-mixin
                        #:filter term-filter
                        #:x-spacing x-spacing
                        #:y-spacing y-spacing)])
    (post-process graph-pb)
    (print-to-ps graph-pb canvas filename)))

(define (print-to-ps graph-pb canvas filename)
  (let ([admin (send graph-pb get-admin)]
        [printing-admin (new printing-editor-admin% [ed graph-pb])])
    (send canvas set-editor #f)
    (send graph-pb set-admin printing-admin)
    
    (dynamic-wind
     void
     (λ ()
       (send graph-pb size-cache-invalid)
       
       (send graph-pb re-run-layout)
       
       (let ([ps-setup (make-object ps-setup%)])
         (send ps-setup copy-from (current-ps-setup))
         (send ps-setup set-file filename)
         (send ps-setup set-mode 'file)
         (parameterize ([current-ps-setup ps-setup])
           (send graph-pb print #f #f 'postscript #f #f #t))))
     
     (λ ()
       (send graph-pb set-admin admin)
       (send canvas set-editor graph-pb)
       (send printing-admin shutdown) ;; do this early
       (let loop ([snip (send graph-pb find-first-snip)])
         (when snip
           (send snip size-cache-invalid)
           (loop (send snip next))))
       (send graph-pb size-cache-invalid)
       (send graph-pb re-run-layout)))))

(define printing-editor-admin%
  (class editor-admin%
    
    (init-field ed)
    
    (define temp-file (make-temporary-file "redex-size-snip-~a"))
    
    (define ps-dc
      (let ([ps-setup (make-object ps-setup%)])
        (send ps-setup copy-from (current-ps-setup))
        (send ps-setup set-file temp-file)
        (parameterize ([current-ps-setup ps-setup])
          (make-object post-script-dc% #f #f #f #t))))
    
    (send ps-dc start-doc "fake dc")
    (send ps-dc start-page)
    (super-new)
    
    (define/public (shutdown)
      (send ps-dc end-page)
      (send ps-dc end-doc)
      (delete-file temp-file))
       
    
    (define/override (get-dc [x #f] [y #f])
      (super get-dc x y)
      ps-dc)
    (define/override (get-max-view x y w h [full? #f])
      (get-view x y w h full?))
    (define/override (get-view x y w h [full? #f])
      (when x (set-box! x 0.0))
      (when y (set-box! x 0.0))
      (when (box? w) (set-box! w 500))
      (when (box? h) (set-box! h 500)))
    
    ;; the following methods are not overridden; they all default to doing nothing.
    ;;   grab-caret
    ;;   modified
    ;;   needs-update
    ;;   popup-menu
    ;;   refresh-delayed?
    ;;   resized
    ;;   scroll-to
    ;;   update-cursor
    ))

(define (traces reductions pre-exprs 
                #:multiple? [multiple? #f] 
                #:pred [pred (λ (x) #t)] 
                #:pp [pp default-pretty-printer] 
                #:colors [colors '()]
                #:racket-colors? [racket-colors? #t]
                #:scheme-colors? [scheme-colors? racket-colors?]
                #:layout [layout void]
                #:edge-label-font [edge-label-font #f]
                #:edge-labels? [edge-labels? #t]
                #:filter [term-filter (lambda (x y) #t)]
                #:graph-pasteboard-mixin [extra-graph-pasteboard-mixin values]
                #:no-show-frame? [no-show-frame? #f]
                #:x-spacing [x-spacing default-x-spacing]
                #:y-spacing [y-spacing default-y-spacing])
  (define exprs (if multiple? pre-exprs (list pre-exprs)))
  (define main-eventspace (current-eventspace))
  (define saved-parameterization (current-parameterization))
  (define graph-pb
    (let ([pb (new (extra-graph-pasteboard-mixin graph-pasteboard%)
                   [layout layout] [edge-label-font edge-label-font]
                   [edge-labels? edge-labels?])])
      (send pb set-flip-labels? #f)
      pb))
  (define user-char-width (initial-char-width))
  (define f (instantiate red-sem-frame% ()
              (label "PLT Redex Reduction Graph")
              (style '(toolbar-button))
              (graph-pb graph-pb)
              (width 600)
              (height 400)
              (toggle-panel-callback
               (lambda ()
                 (send remove-my-contents-panel
                       change-children
                       (lambda (l)
                         (preferences:set 'plt-reducer:show-bottom (null? l))
                         (if (null? l)
                             (list bottom-panel)
                             null)))))))
  (define ec (make-object editor-canvas% (send f get-area-container) graph-pb))
  (define remove-my-contents-panel (new vertical-panel%
                                        (parent (send f get-area-container))
                                        (stretchable-height #f)))
  (define bottom-panel (new vertical-panel%
                            (parent remove-my-contents-panel)
                            (stretchable-height #f)))
  (define font-size (instantiate slider% ()
                      (label "Font Size")
                      (min-value 1)
                      (init-value (initial-font-size))
                      (max-value 127)
                      (parent bottom-panel)
                      (callback (lambda (slider evt) 
                                  (send graph-pb begin-edit-sequence)
                                  (set-font-size (send slider get-value))
                                  (send graph-pb end-edit-sequence)))))
  (define lower-panel (instantiate horizontal-panel% ()
                        (parent bottom-panel)
                        (stretchable-height #f)))
  (define dot-panel (instantiate horizontal-panel% ()
                        (parent bottom-panel)
                        (stretchable-height #f)))
  (define reduce-button (make-object button% 
                          "Reducing..." 
                          lower-panel
                          (lambda (x y)
                            (reduce-button-callback #f))))
  (define status-message (instantiate message% ()
                           (label "")
                           (parent lower-panel)
                           (stretchable-width #t)))
  (define dot (new button% 
                   [parent dot-panel] 
                   [label "Fix Layout"] 
                   [callback 
                    (λ (x y)
                      (set! dot? (not dot?))
                      (dot-callback))]))
  (define dot-mode (new choice%
                        [parent dot-panel]
                        [label #f]
                        [callback
                         (λ x
                           (send dot-overlap set-label
                                 (if (equal? 0 (send dot-mode get-selection))
                                     "Top to Bottom"
                                     "No Overlap"))
                           (when dot?
                             (dot-callback)))]
                        [choices (list dot-label neato-label neato-hier-label neato-ipsep-label)]))
  (define dot-overlap (new check-box%
                           [value #t]
                           [callback
                            (λ x
                              (when dot?
                                (dot-callback)))]
                           [parent dot-panel]
                           [label "Top to Bottom"]))
                   
  (define snip-cache (make-hash))
  
  ;; call-on-eventspace-main-thread : (-> any) -> any
  ;; =reduction thread=
  (define (call-on-eventspace-main-thread thnk)
    (parameterize ([current-eventspace main-eventspace])
      (let ([s (make-semaphore 0)]
            [ans #f])
        (queue-callback
         (lambda ()
           (call-with-parameterization
            saved-parameterization
            (λ ()
              (set! ans (thnk))))
           (semaphore-post s)))
        (semaphore-wait s)
        ans)))
  
  (define default-colors (list (dark-pen-color) (light-pen-color) 
                               (dark-text-color) (light-text-color)
                               (dark-brush-color) (light-brush-color)))
  
  (define code-colors? (and racket-colors? scheme-colors?))
  
  ;; only changed on the reduction thread
  ;; frontier : (listof (is-a?/c graph-editor-snip%))
  (define frontier 
    (filter
     (λ (x) x)
     (map (lambda (expr) (apply build-snip
                                snip-cache #f expr pred pp #f code-colors?
                                (get-user-char-width user-char-width expr)
                                default-colors))
          exprs)))
  
  
  ;; set-font-size : number -> void
  ;; =eventspace main thread=
  (define (set-font-size size)
    (let* ([standard (send (editor:get-standard-style-list) find-named-style
                           "Standard")]
           [delta (make-object style-delta%)])
      (send standard get-delta delta)
      (send delta set-size-mult 0)
      (send delta set-size-add size)
      (send standard set-delta delta)
      (let loop ([snip (send graph-pb find-first-snip)])
        (when snip
          (when (is-a? snip reflowing-snip<%>)
            (send snip reflow-program))
          (loop (send snip next))))))
  
  ;; fill-out : (listof X) (listof X) -> (listof X)
  ;; produces a list whose length matches defaults but
  (define (fill-out l defaults)
    (let loop ([l l]
               [defaults defaults])
      (cond
        [(null? l) defaults]
        [else 
         (cons (car l) (loop (cdr l) (cdr defaults)))])))
  
  (define name->color-ht 
    (let ((ht (make-hash)))
      (for-each 
       (λ (c)
         (hash-set! ht (car c) (fill-out (cdr c) default-colors)))
       colors)
      ht))
  
  ;; red->colors : string -> (values string string string string string string)
  (define (red->colors reduction-name)
    (apply values (hash-ref name->color-ht
                            reduction-name 
                            default-colors)))
  
  ;; reduce-frontier : -> void
  ;; =reduction thread=
  ;; updates frontier with the new snip after a single reduction
  (define (reduce-frontier)
    (let ([col #f])
      (let loop ([snips frontier]
                 [new-frontier null]
                 [y 0])
        (cond
          [(null? snips)
           (set! frontier new-frontier)]
          [else
           (let* ([snip (car snips)]
                  [new-snips 
                   (filter 
                    (lambda (x) x)
                    (map (lambda (red+sexp)
                           (let-values ([(name sexp) (apply values red+sexp)])
                             (call-on-eventspace-main-thread
                              (λ ()
                                (and (term-filter sexp name)
                                     (let-values ([(dark-arrow-color light-arrow-color dark-label-color light-label-color
                                                                     dark-pen-color
                                                                     light-pen-color) 
                                                   (red->colors name)])
                                       (build-snip snip-cache snip sexp pred pp name code-colors? 
                                                   (get-user-char-width user-char-width sexp)
                                                   light-arrow-color dark-arrow-color dark-label-color light-label-color
                                                   dark-pen-color light-pen-color)))))))
                         (apply-reduction-relation/tag-with-names reductions (send snip get-expr))))]
                  [new-y 
                   (call-on-eventspace-main-thread
                    (lambda () ; =eventspace main thread=
                      (send graph-pb begin-edit-sequence)
                      (unless col  ;; only compute col here, incase user moves snips
                        (set! col (+ x-spacing (find-rightmost-x graph-pb))))
                      (begin0
                        (insert-into col y graph-pb new-snips y-spacing)
                        (send graph-pb end-edit-sequence)
                        (send status-message set-label
                              (string-append (term-count (count-snips)) "...")))))])
             (loop (cdr snips)
                   (append new-frontier new-snips)
                   new-y))]))))
  
  ;; count-snips : -> number
  ;; =eventspace main thread=
  ;; counts the snips in `pb'. 
  (define (count-snips)
    (let loop ([n 0]
               [snip (send graph-pb find-first-snip)])
      (cond
        [snip (loop (+ n 1) (send snip next))]
        [else n])))
  
  ;; dot-callback : -> void
  (define dot? #f)
  (define (dot-callback)
    (cond
      [(not (find-dot))
       (message-box "PLT Redex"
                    "Could not find the dot binary")]
      [dot?
       (dot-positioning graph-pb 
                        (send dot-mode get-string-selection)
                        (not (send dot-overlap get-value))) ;; refreshes the display
       (send graph-pb immobilize)
       (send dot set-label "Unlock")
       (send reduce-button enable #f)
       (send font-size enable #f)]
      [else
       (out-of-dot-state)]))
  
  (define (out-of-dot-state)
    (send graph-pb mobilize)
    (send graph-pb set-dot-callback #f)
    (send graph-pb invalidate-bitmap-cache)
    (send dot set-label "Fix Layout")
    (send reduce-button enable #t)
    (send font-size enable #t))
  
  ;; reduce-button-callback : boolean -> void
  ;; =eventspace main thread=
  (define (reduce-button-callback show-all-at-once?)
    (when show-all-at-once? (send graph-pb begin-edit-sequence))
    (send reduce-button enable #f)
    (send reduce-button set-label "Reducing...")
    (thread
     (lambda ()
       (let ([update-gui
              (λ (failed?)
                (queue-callback
                 (lambda () ;; =eventspace main thread=
                   (send graph-pb begin-edit-sequence)
                   (send graph-pb re-run-layout)
                   (send graph-pb end-edit-sequence)
                   (when show-all-at-once? (send graph-pb end-edit-sequence))
                   (scroll-to-rightmost-snip)
                   (send reduce-button set-label "Reduce")
                   (cond
                     [failed? 
                      (send status-message set-label "Error while reducing")]
                     [(null? frontier)
                      (send status-message set-label (term-count (count-snips)))]
                     [else
                      (send status-message set-label 
                            (string-append (term-count (count-snips))
                                           " (possibly more to find)"))
                      (send reduce-button enable #t)]))))])
         (with-handlers ((exn:fail? (λ (x) (update-gui #t) (raise x))))
           (do-some-reductions)
           (update-gui #f))))))
  
  #;
  (define (reduce-button-callback show-all-at-once?)
    (when show-all-at-once? (send graph-pb begin-edit-sequence))
    (send reduce-button enable #f)
    (send reduce-button set-label "Reducing...")
    (thread
     (lambda ()
       (let ([update-gui
              (λ (failed?)
                (queue-callback
                 (lambda () ;; =eventspace main thread=
                   (scroll-to-rightmost-snip)
                   (cond
                     [failed? 
                      (send status-message set-label "Error while reducing")
                      (send reduce-button set-label "Reduce")]
                     [else
                      (send reduce-button set-label "Reduce")
                      (cond
                        [(null? frontier)
                         (send status-message set-label (term-count (count-snips)))]
                        [else
                         (send status-message set-label 
                               (string-append (term-count (count-snips))
                                              "(possibly more to find)"))
                         (send reduce-button enable #t)])]))))])
         (with-handlers ((exn:fail? (λ (x) (update-gui #t) (raise x))))
           (do-some-reductions)
           (update-gui #f))))))
  
  (define (term-count n)
    (format "found ~a term~a" n (if (equal? n 1) "" "s")))
  
  ;; do-some-reductions : -> void
  ;; =reduction thread=
  ;; reduces some number of times, 
  ;; adding at least reduction-steps-cutoff steps
  ;; before stopping (unless there aren't that many left)
  (define (do-some-reductions)
    (let ([initial-size (call-on-eventspace-main-thread count-snips)])
      (let loop ()
        (cond
          [(null? frontier) (void)]
          [((call-on-eventspace-main-thread count-snips) . >= . (+ initial-size (reduction-steps-cutoff)))
           (void)]
          [else
           (reduce-frontier)
           (loop)]))))
  
  ;; scroll-to-rightmost-snip : -> void
  ;; =eventspace main thread=
  (define (scroll-to-rightmost-snip)
    (let ([rightmost-snip (send graph-pb find-first-snip)])
      (let loop ([rightmost-snip rightmost-snip]
                 [rightmost-y (get-right-edge rightmost-snip)]
                 [snip (send rightmost-snip next)])
        (cond
          [(not snip) (make-snip-visible rightmost-snip)]
          [else
           (let ([snip-y (get-right-edge snip)])
             (if (<= rightmost-y snip-y)
                 (loop snip snip-y (send snip next))
                 (loop rightmost-snip rightmost-y (send snip next))))]))))
  
  ;; make-snip-visisble : snip -> void
  ;; =eventspace-main-thread=
  (define (make-snip-visible snip)
    (let ([bl (box 0)]
          [bt (box 0)]
          [br (box 0)]
          [bb (box 0)])
      (send graph-pb get-snip-location snip bl bt #f)
      (send graph-pb get-snip-location snip br bb #t)
      (send graph-pb scroll-to 
            snip
            0
            0
            (- (unbox br) (unbox bl))
            (- (unbox bb) (unbox bt))
            #t)))
  
  ;; get-right-edge : snip -> void
  ;; =eventspace-main-thread=
  (define (get-right-edge snip)
    (let ([br (box 0)])
      (send graph-pb get-snip-location snip br #f #t)
      (unbox br)))
  
  (send remove-my-contents-panel
        change-children
        (lambda (l)
          (if (preferences:get 'plt-reducer:show-bottom)
              (list bottom-panel)
              null)))
  (out-of-dot-state) ;; make sure the state is initialized right
  (set-font-size (initial-font-size)) ;; have to call this before 'insert-into' or else it triggers resizing
  (insert-into init-rightmost-x 0 graph-pb frontier y-spacing)
  (cond
    [no-show-frame?
     (let ([s (make-semaphore)]) 
       (thread (λ () 
                 (do-some-reductions)
                 (semaphore-post s)))
       (yield s))
     (values graph-pb ec)]
    [else
     (reduce-button-callback #t)
     (send f show #t)]))

(define red-sem-frame%
  (class (frame:standard-menus-mixin (frame:basic-mixin frame%))
    (init-field graph-pb toggle-panel-callback)
    (define/override (file-menu:create-save?) #f)
    (define/override (on-toolbar-button-click) (toggle-panel-callback))
    (define/override (file-menu:between-save-as-and-print file-menu)
      (make-object menu-item% "Print..."
        file-menu
        (lambda (item evt) (send graph-pb print)))
      (make-object menu-item% "Export as Encapsulted PostScript..."
        file-menu
        (lambda (item evt) (send graph-pb print #t #f 'postscript this #f)))
      (make-object menu-item% "Export as PostScript..."
        file-menu
        (lambda (item evt) (send graph-pb print #t #f 'postscript this)))
      (make-object menu-item% 
        "Toggle bottom stuff"
        file-menu
        (lambda (item evt) (toggle-panel-callback))))
    (super-new)))

(define graph-pasteboard% 
  (class (resizing-pasteboard-mixin
          (graph-pasteboard-mixin pasteboard%))
    
    (init-field layout) ;; (-> (listof term-node) void)
    ;; this is the function supplied by the :#layout argument to traces or traces/ps
    
    (define dot-callback #f)
    (define/public (set-dot-callback cb) (set! dot-callback cb))
    (define/override (draw-edges dc left top right bottom dx dy)
      (if dot-callback
          (dot-callback this dc left top right bottom dx dy)
          (super draw-edges dc left top right bottom dx dy)))
    
    (define mobile? #t)
    (define/public (immobilize) (set! mobile? #f))
    (define/public (mobilize) (set! mobile? #t))
    
    (define/augment (can-interactive-move? evt) mobile?)
    (define/augment (can-interactive-resize? evt) mobile?)
    
    (inherit find-first-snip)
    (define/public (re-run-layout)
      (layout 
       (let loop ([snip (find-first-snip)])
         (cond
           [(not snip) '()]
           [(is-a? snip reflowing-snip<%>)
            (cons (send snip get-term-node)
                  (loop (send snip next)))]
           [else (loop (send snip next))]))))
    
    (super-new)))

(define graph-editor-snip%
  (class* (graph-snip-mixin size-editor-snip%) (reflowing-snip<%>)
    (init-field my-eventspace)
    (inherit get-expr)
    (define bad-color #f)
    (inherit get-admin)
    (define/public (get-my-eventspace) my-eventspace)
    (define/public (set-bad color)
      (send (get-editor) set-bad color)
      (set! bad-color color)
      (let ([admin (get-admin)])
        (when admin 
          (let ([wb (box 0)]
                [hb (box 0)])
            (send admin get-view-size wb hb)
            (send admin needs-update this 0 0 (unbox wb) (unbox hb))))))
    (define/public (get-bad) bad-color)
    
    (define names-to-here '())
    ;; might have the same parent twice with a different name
    ;; might have different parens with the same name.
    ;; just record this in a list.
    (define/public (record-edge-label parent name)
      (set! names-to-here (cons (list parent name) names-to-here)))
    (define/public (get-one-step-labels)
      (map cadr names-to-here))
    (define/public (get-one-step-parents) (map (λ (x) (send (car x) get-term-node)) names-to-here))
    (define term-node #f)
    (define/public (get-term-node) 
      (unless term-node
        (set! term-node (make-term-node this)))
      term-node)
    
    (inherit get-children)
    (define/public (get-one-step-children)
      (map (λ (x) (send x get-term-node)) (get-children)))
    
    (inherit get-editor)
    (inherit get-extent)
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (when bad-color
        (let ([bw (box 0)]
              [bh (box 0)]
              [pen (send dc get-pen)]
              [brush (send dc get-brush)])
          (get-extent dc x y bw bh #f #f #f #f)
          (send dc set-pen (send the-pen-list find-or-create-pen bad-color 1 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush bad-color 'solid))
          (send dc draw-rectangle x y (unbox bw) (unbox bh))
          (send dc set-pen pen)
          (send dc set-brush brush)))
      (super draw dc x y left top right bottom dx dy draw-caret))
    
    (super-new)))

(define program-text%
  (class size-text%
    (define bad-color #f)
    (define/public (set-bad color) (set! bad-color color))
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when (and bad-color before?)
        (let ([pen (send dc get-pen)]
              [brush (send dc get-brush)])
          (send dc set-pen (send the-pen-list find-or-create-pen bad-color 1 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush bad-color 'solid))
          (send dc draw-rectangle (+ dx left) (+ dy top) (- right left) (- bottom top))
          (send dc set-pen pen)
          (send dc set-brush brush)))
      (super on-paint before? dc left top right bottom dx dy draw-caret))
    (super-new)))

(define lines-pen (send the-pen-list find-or-create-pen "black" 1 'solid))

;; where the first snips are inserted
(define init-rightmost-x 25)

;; insert-into : number number pasteboard (listof snip%) -> number
;; inserts the snips into the pasteboard vertically 
;; aligned, starting at (x,y). Returns
;; the y coordinate where another snip might be inserted.
(define (insert-into x y pb exprs y-spacing)
  (let loop ([exprs exprs]
             [y y])
    (cond
      [(null? exprs) y]
      [else
       (let ([es (car exprs)])
         (send pb insert es x y)
         (loop (cdr exprs)
               (+ y (find-snip-height pb es) y-spacing)))])))

;; build-snip : hash-table
;;              (union #f (is-a?/c graph-snip<%>)) 
;;              sexp
;;              sexp -> boolean
;;              (any port number -> void)
;;              (union #f string)
;;              number
;;              color^6
;;           -> (union #f (is-a?/c graph-editor-snip%))
;; returns #f if a snip corresponding to the expr has already been created.
;; also adds in the links to the parent snip
;; =eventspace main thread=
(define (build-snip cache parent-snip expr pred pp name code-colors? cw
                    light-arrow-color dark-arrow-color dark-label-color light-label-color
                    dark-brush-color light-brush-color)
  (let-values ([(snip new?)
                (let/ec k
                  (values (hash-ref
                           cache
                           expr
                           (lambda ()
                             (let ([new-snip (make-snip parent-snip expr pred pp code-colors? cw)])
                               (hash-set! cache expr new-snip)
                               (k new-snip #t))))
                          #f))])

    (when parent-snip
      (send snip record-edge-label parent-snip name)
      (add-links/text-colors parent-snip snip
                             (send the-pen-list find-or-create-pen dark-arrow-color 0 'solid)
                             (send the-pen-list find-or-create-pen light-arrow-color 0 'solid)
                             (send the-brush-list find-or-create-brush dark-brush-color 'solid)
                             (send the-brush-list find-or-create-brush light-brush-color 'solid)
                             (if (is-a? dark-label-color color%)
                                 dark-label-color
                                 (make-object color% dark-label-color))
                             (if (is-a? light-label-color color%)
                                 light-label-color
                                 (make-object color% light-label-color))
                             0 0
                             name)
      (update-badness pred parent-snip (send parent-snip get-expr)))
    
    (update-badness pred snip expr)
    
    (and new? snip)))

(define (update-badness pred snip expr)
  (let ([good? 
         (if (procedure-arity-includes? pred 2)
             (pred expr (send snip get-term-node))
             (pred expr))])
    (send snip set-bad (cond
                         [(or (string? good?) 
                              (is-a? good? color%))
                          good?]
                         [(not good?) "pink"]
                         [else #f]))))

;; make-snip : (union #f (is-a?/c graph-snip<%>)) 
;;             sexp 
;;             sexp -> boolean
;;             (any port number -> void)
;;             boolean
;;             number
;;          -> (is-a?/c graph-editor-snip%)
;; unconditionally creates a new graph-editor-snip
;; =eventspace main thread=
(define (make-snip parent-snip expr pred pp code-colors? cw)
  (let* ([text (new program-text%)]
         [es (instantiate graph-editor-snip% ()
               (char-width cw)
               (editor text)
               (my-eventspace (current-eventspace))
               (pp pp)
               (expr expr))])
    (send text set-autowrap-bitmap #f)
    (send text set-max-width 'none)
    (send text freeze-colorer)
    (unless code-colors?
      (send text stop-colorer #t))
    (send es format-expr)
    es))

;; find-rightmost-x : pasteboard -> number
(define (find-rightmost-x pb) 
  (let ([first-snip (send pb find-first-snip)])
    (if first-snip
        (let loop ([snip first-snip]
                   [max-x (find-snip-right-edge pb first-snip)])
          (cond
            [snip
             (loop (send snip next)
                   (max max-x (find-snip-right-edge pb snip)))]
            [else max-x]))
        init-rightmost-x)))

;; find-snip-right-edge : editor snip -> number
(define (find-snip-right-edge ed snip)
  (let ([br (box 0)])
    (send ed get-snip-location snip br #f #t)
    (unbox br)))

(provide traces
         traces/ps
         print-to-ps
         term-node?
         term-node-parents
         term-node-children
         term-node-labels 
         term-node-set-red!
         term-node-set-color!
         term-node-color
         term-node-set-position!
         term-node-x
         term-node-y
         term-node-width
         term-node-height
         term-node-expr)

(provide reduction-steps-cutoff initial-font-size
         dark-pen-color light-pen-color dark-brush-color light-brush-color
         dark-text-color light-text-color)

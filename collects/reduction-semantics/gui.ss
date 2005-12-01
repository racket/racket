;; should cache the count of new snips -- dont
;; use `count-snips'; use something associated with the
;; equal hash-table

(module gui mzscheme
  (require (lib "etc.ss")
           (lib "graph.ss" "reduction-semantics")
           "reduction-semantics.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "pretty.ss")
           (lib "class.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "match.ss"))

  (provide/contract
   [traces (opt-> (compiled-lang?
                   (listof red?)
                   any/c)
                  (procedure? (listof any/c))
                  any)]
   [traces/pred (opt-> (compiled-lang?
                        (listof red?)
                        (listof any/c)
                        (any/c . -> . boolean?))
                       (procedure? (listof any/c))
                       any)]
   [traces/multiple (opt-> (compiled-lang?
                            (listof red?)
                            (listof any/c))
                           (procedure? (listof any/c))
                           any)])
               
   
  (provide reduction-steps-cutoff initial-font-size initial-char-width
           dark-pen-color light-pen-color dark-brush-color light-brush-color
           dark-text-color light-text-color
           (rename default-pp default-pretty-printer))

  (preferences:set-default 'plt-reducer:show-bottom #t boolean?)
  
  (define dark-pen-color (make-parameter "blue"))
  (define light-pen-color (make-parameter "lightblue"))
  (define dark-brush-color (make-parameter "lightblue"))
  (define light-brush-color (make-parameter "white"))
  (define dark-text-color (make-parameter "blue"))
  (define light-text-color (make-parameter "lightblue"))
  
  
  ;; after (about) this many steps, stop automatic, initial reductions
  (define reduction-steps-cutoff (make-parameter 20))
  
  
  
  (define initial-font-size
    (make-parameter
     (send (send (send (editor:get-standard-style-list) 
                       find-named-style
                       "Standard")
                 get-font)
           get-point-size)))
  
  (define initial-char-width (make-parameter 30))
  
  ;; the initial spacing between row and columns of the reduction terms
  (define x-spacing 15)
  (define y-spacing 15)
  
  (define (default-pp v port w spec)
    (parameterize ([pretty-print-columns w])
      (pretty-print v port)))
  
  (define traces
    (opt-lambda (lang reductions expr [pp default-pp] [colors '()])
      (traces/multiple lang reductions (list expr) pp colors)))
      
  (define traces/multiple
    (opt-lambda (lang reductions exprs [pp default-pp] [colors '()])
      (traces/pred lang reductions exprs (lambda (x) #t) pp colors)))
  
  (define traces/pred
    (opt-lambda (lang reductions exprs pred [pp default-pp] [colors '()])
      (define main-eventspace (current-eventspace))
      (define graph-pb (make-object graph-pasteboard%))
      (define f (instantiate red-sem-frame% ()
                  (label "Reduction Graph")
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
                          (callback (lambda (slider evt) (set-font-size (send slider get-value))))))
      (define lower-panel (instantiate horizontal-panel% ()
                             (parent bottom-panel)
                             (stretchable-height #f)))
      (define reduce-button (make-object button% 
                              "Reducing..." 
                              lower-panel
                              (lambda (x y)
                                (reduce-button-callback))))
      (define status-message (instantiate message% ()
                               (label "")
                               (parent lower-panel)
                               (stretchable-width #t)))
      
      (define snip-cache (make-hash-table 'equal))
      
      ;; call-on-eventspace-main-thread : (-> any) -> any
      ;; =reduction thread=
      (define (call-on-eventspace-main-thread thnk)
        (parameterize ([current-eventspace main-eventspace])
          (let ([s (make-semaphore 0)]
                [ans #f])
            (queue-callback
             (lambda ()
               (set! ans (thnk))
               (semaphore-post s)))
            (semaphore-wait s)
            ans)))
        
      ;; only changed on the reduction thread
      ;; frontier : (listof (is-a?/c graph-editor-snip%))
      (define frontier (map (lambda (expr) (build-snip snip-cache #f expr pred pp 
                                                       (dark-pen-color) (light-pen-color)
                                                       (dark-text-color) (light-text-color) #f))
                            exprs))

      ;; set-font-size : number -> void
      ;; =eventspace main thread=
      (define (set-font-size size)
        (let* ([scheme-standard (send (editor:get-standard-style-list) find-named-style
				      "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (send scheme-delta set-size-mult 0)
          (send scheme-delta set-size-add size)
          (send scheme-standard set-delta scheme-delta)))
      
      ;; color-spec-list->color-scheme : (list (union string? #f)^4) -> (list string?^4)
      ;; converts a list of user-specified colors (including false) into a list of color strings, filling in
      ;; falses with the default colors
      (define (color-spec-list->color-scheme l)
        (map (λ (c d) (or c d))
             l 
             (list (dark-pen-color) (light-pen-color) (dark-text-color) (light-text-color))))
        
        
      (define name->color-ht 
        (let ((ht (make-hash-table 'equal)))
          (for-each 
           (λ (c)
             (hash-table-put! ht (car c) 
                              (color-spec-list->color-scheme
                               (match (cdr c)
                                 [(color)
                                  (list color color (dark-text-color) (light-text-color))]
                                 [(dark-arrow-color light-arrow-color)
                                  (list dark-arrow-color light-arrow-color (dark-text-color) (light-text-color))]
                                 [(dark-arrow-color light-arrow-color text-color) 
                                  (list dark-arrow-color light-arrow-color text-color text-color)]
                                 [(_ _ _ _)
                                  (cdr c)]))))
           colors)
          ht))

      ;; red->colors : reduction -> (values string string string string)
      (define (red->colors red)
        (apply values (hash-table-get name->color-ht (reduction->name red) 
                                      (λ () (list (dark-pen-color) (light-pen-color) (dark-text-color) (light-text-color))))))
      
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
                               (let-values ([(red sexp) (apply values red+sexp)])
                                 (call-on-eventspace-main-thread
                                  (λ ()
                                    (let-values ([(dark-arrow-color light-arrow-color dark-label-color light-label-color) (red->colors red)])
                                      (build-snip snip-cache snip sexp pred pp light-arrow-color dark-arrow-color dark-label-color light-label-color
                                                  (reduction->name red)))))))
                             (reduce/tag-with-reduction reductions (send snip get-expr))))]
                      [new-y 
                       (call-on-eventspace-main-thread
                        (lambda () ; =eventspace main thread=
                          (send graph-pb begin-edit-sequence)
                          (unless col  ;; only compute col here, incase user moves snips
                            (set! col (+ x-spacing (find-rightmost-x graph-pb))))
                          (begin0
                            (insert-into col y graph-pb new-snips)
                            (send graph-pb end-edit-sequence)
                            (send status-message set-label
                                  (string-append (term-count (count-snips)) "...")))))])
                 (loop (cdr snips)
                       (append new-frontier new-snips)
                       new-y))]))))
      
      ;; count-snips : -> number
      ;; =eventspace main thread=
      ;; counts the snips in `pb'. If connections?
      ;; is #t, also counts the number of connections 
      ;; and returns the sum of the connections and snips
      (define (count-snips)
        (let loop ([n 0]
                   [snip (send graph-pb find-first-snip)])
          (cond
            [snip (loop (+ n 1) (send snip next))]
            [else n])))
      
      ;; reduce-button-callback : -> void
      ;; =eventspace main thread=
      (define (reduce-button-callback)
        (send reduce-button enable #f)
        (send reduce-button set-label "Reducing...")
        (thread
         (lambda ()
           (do-some-reductions)
           (queue-callback
            (lambda () ;; =eventspace main thread=
              (scroll-to-rightmost-snip)
              (send reduce-button set-label "Reduce")
              (cond
                [(null? frontier)
                 (send status-message set-label (term-count (count-snips)))]
                [else
                 (send status-message set-label 
                       (string-append (term-count (count-snips))
                                      "(possibly more to find)"))
                 (send reduce-button enable #t)]))))))
      
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
      (insert-into init-rightmost-x 0 graph-pb frontier)
      (set-font-size (initial-font-size))
      (reduce-button-callback)
      (send f show #t)))
  
  (define red-sem-frame%
    (class (frame:standard-menus-mixin (frame:basic-mixin frame%))
      (init-field graph-pb toggle-panel-callback)
      (define/override (file-menu:create-save?) #f)
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
      (super-instantiate ())))
  
  (define (resizing-pasteboard-mixin pb%)
    (class pb%
      
      (define/augment (on-interactive-resize snip)
        (when (is-a? snip reflowing-snip<%>)
          (send snip reflow-program))
        #;(super on-interactive-resize snip))
      
      (define/augment (after-interactive-resize snip)
        (when (is-a? snip reflowing-snip<%>)
          (send snip reflow-program))
        #;(super after-interactive-resize snip))
      
      (define/override (interactive-adjust-resize snip w h)
        (super interactive-adjust-resize snip w h)
        (when (is-a? snip reflowing-snip<%>)
          (send snip reflow-program)))
      (super-instantiate ())))
  
  (define reflowing-snip<%>
    (interface ()
      reflow-program))
  
  (define graph-pasteboard% 
    (resizing-pasteboard-mixin
     (graph-pasteboard-mixin pasteboard%)))
  
  (define graph-editor-snip%
    (class* (graph-snip-mixin editor-snip%) (reflowing-snip<%>)
      (init-field expr pp char-width bad?)
      (define/public (get-expr) expr)
      
      (inherit get-editor)
      (define/public (reflow-program)
        (let ([ed (get-editor)])
          (when ed
            (let ([ed-ad (send ed get-admin)])
              (when ed-ad
                (let ([dc (send ed get-dc)]
                      [wb (box 0)]
                      [std-style (send (editor:get-standard-style-list) find-named-style "Standard")])
                  (send ed-ad get-view #f #f wb #f)
                  (let-values ([(tw _1 _2 _3) (send dc get-text-extent "w"
                                                    (and std-style
                                                         (send std-style get-font)))])
                    (let ([new-width (max 1 (inexact->exact (floor (/ (- (unbox wb) 2) tw))))])
                      (unless (equal? new-width char-width)
                        (set! char-width new-width)
                        (format-expr))))))))))
      
      (inherit get-extent)
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (when bad?
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
      
      (define/public (format-expr)
        (let* ([text (get-editor)]
               [port (make-output-port
                      'graph-port
                      always-evt
                      (lambda (bytes start end buffering? enable-breaks?)
                        (send text insert (bytes->string/utf-8 (subbytes bytes start end))
                              (send text last-position)
                              (send text last-position))
                        (- end start))
                      void)])
          (send text begin-edit-sequence)
          (send text thaw-colorer)
          (send text set-styles-sticky #f)
          (send text erase)
          (pp expr port char-width text)
          (when (char=? #\newline (send text get-character (- (send text last-position) 1)))
            (send text delete (- (send text last-position) 1) (send text last-position)))
          (send text freeze-colorer)
          (when bad?
            (send text change-style bad-style-delta 0 (send text last-position)))
          (send text end-edit-sequence)))

      (super-instantiate ())))
  
  (define program-text%
    (class scheme:text%
      (init-field bad?)
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (when (and bad? before?)
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
  (define bad-style-delta (make-object style-delta% 'change-italic))
  (define bad-color "pink")
      
  ;; where the first snips are inserted
  (define init-rightmost-x 25)
  
  ;; insert-into : number number pasteboard (listof snip%) -> number
  ;; inserts the snips into the pasteboard vertically 
  ;; aligned, starting at (x,y). Returns
  ;; the y coordinate where another snip might be inserted.
  (define (insert-into x y pb exprs)
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
  ;;              color
  ;;              (union #f string)
  ;;           -> (union #f (is-a?/c graph-editor-snip%))
  ;; returns #f if a snip corresponding to the expr has already been created.
  ;; also adds in the links to the parent snip
  ;; =eventspace main thread=
  (define (build-snip cache parent-snip expr pred pp light-arrow-color dark-arrow-color dark-label-color light-label-color label)
    (let-values ([(snip new?)
                  (let/ec k
                    (k
                     (hash-table-get
                      cache
                      expr
                      (lambda ()
                        (let ([new-snip (make-snip parent-snip expr pred pp)])
                          (hash-table-put! cache expr new-snip)
                          (k new-snip #t))))
                     #f))])
      (when parent-snip
        (add-links/text-colors parent-snip snip
                   (send the-pen-list find-or-create-pen dark-arrow-color 0 'solid)
                   (send the-pen-list find-or-create-pen light-arrow-color 0 'solid)
                   (send the-brush-list find-or-create-brush (dark-brush-color) 'solid)
                   (send the-brush-list find-or-create-brush (light-brush-color) 'solid)
                   (make-object color% dark-label-color)
                   (make-object color% light-label-color)
                   0 0
                   label))
      (and new? snip)))
  
  ;; make-snip : (union #f (is-a?/c graph-snip<%>)) 
  ;;             sexp 
  ;;             sexp -> boolean
  ;;             (any port number -> void)
  ;;          -> (is-a?/c graph-editor-snip%)
  ;; unconditionally creates a new graph-editor-snip
  ;; =eventspace main thread=
  (define (make-snip parent-snip expr pred pp)
    (let* ([bad? (not (pred expr))]
           [text (new program-text% (bad? bad?))]
           [es (instantiate graph-editor-snip% ()
                 (char-width (initial-char-width))
                 (editor text)
                 (pp 
                  (if (procedure-arity-includes? pp 4)
                      pp
                      (lambda (v port w spec) (display (pp v) port))))
                 (expr expr)
                 (bad? bad?))])
      (send text set-autowrap-bitmap #f)
      (send text freeze-colorer)
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
  
  ;; find-snip-height : editor snip -> number
  (define (find-snip-height ed snip)
    (let ([bt (box 0)]
          [bb (box 0)])
      (send ed get-snip-location snip #f bt #f)
      (send ed get-snip-location snip #f bb #t)
      (- (unbox bb)
         (unbox bt)))))

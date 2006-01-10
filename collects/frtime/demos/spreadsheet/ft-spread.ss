(module ft-spread (lib "frtime-big.ss" "frtime")
  ;; TODO
  ;; 2) scroll/row & col labels
  ;; 3) copy/paste/multiple selection
  ;; 
  ;; Make namespace safer
  ;; letters
  
  
  
  (require (lib "simple.ss" "frtime" "demos" "gui"))
  (require "ss-canvas.ss")
  (require "ss-database.ss")
  (require (lib "string.ss"))
  (require (as-is:unchecked mzscheme make-hash-table hash-table-get hash-table-put!
                            open-input-string open-output-file open-input-file
                            write read delete-file close-output-port close-input-port
                            flush-output
                            current-namespace))

  (require (rename (lib "frp-core.ss" "frtime") do-in-manager do-in-manager))
  (require (rename (lib "frp-core.ss" "frtime") super-lift super-lift))
  (require (rename (lib "frp-core.ss" "frtime") current-custs current-custs))
  (require (rename (lib "mred.ss" "mred") bitmap-dc% bitmap-dc%)
           (rename (lib "mred.ss" "mred") bitmap% bitmap%))
  (require (lib "mod-mrpanel.ss" "frtime" "demos" "gui"))
  (require (all-except (lib "mred.ss" "mred") send-event))
  (require (lib "unit.ss"))
  
  
  ;(rename mzscheme current-namespace current-namespace)
  (require (as-is:unchecked (lib "plt-pretty-big-text.ss" "lang") namespace-set-variable-value!))
  
  ;;;;;;;;;;;;
  ;; Constants
  
  ;; Initial and maximum dimensions of the spreadhseet
  (define INIT_VIEW_WIDTH 800)
  (define INIT_VIEW_HEIGHT 500)
  (define MAX_VIEW_WIDTH 800)
  (define MAX_VIEW_HEIGHT 500)
  
  ;; Cell dimensions
  (define COL_WIDTH 120)
  (define ROW_HEIGHT 21)
  
  ;; Number of visible columns and rows
  (define VIS_COLS (round (/ MAX_VIEW_WIDTH COL_WIDTH)))
  (define VIS_ROWS (round (/ MAX_VIEW_HEIGHT ROW_HEIGHT)))
  
  ;; Cell value placement (padding from cell border)
  (define VERT_BUFF 3)
  (define HORIZ_BUFF 3)
  
  ;; Label constants
  (define LBL_WIDTH 60)
  (define LBL_FONT (make-object font% 10 'default))
  
  
  ;; Constant grid background used
  (define GRID_BACKGROUND
    (let r-loop ([c-row 0] [r-lst '()])
      (if (> c-row VIS_ROWS)
          r-lst
          (let c-loop ([c-col 0] [c-lst '()])
            (if (> c-col VIS_COLS)
                (r-loop (add1 c-row)
                        (cons (make-line
                               #f
                               0
                               (* c-row ROW_HEIGHT)
                               MAX_VIEW_WIDTH)
                              (append c-lst 
                                      r-lst)))
                (c-loop (add1 c-col)
                        (cons (make-line
                               #t
                               (* c-col COL_WIDTH)
                               0
                               MAX_VIEW_HEIGHT)
                              c-lst)))))))
  
  ;; customized toString
  (define (custom->string x)
    (if (undefined? x)
        "<undefined>"
        (if (string? x)
            x
            (lift-strict expr->string x))))
  
  ;;;;;;;;;;;;;;;;;
  ;; Key Generation 
  ; -- used to uniquely identify each cell --
  
  ; produces a key given a row and column
  (define (rowXcol->key r c)
    (string->symbol (format "~ax~a" r c)))
  
  ; produces a key given a posn struct
  (define (posn->key p)
    (string->symbol (format "~ax~a" (posn-x p) (posn-y p))))
  
  ;; Namespace manipulation to bind values appropriately
  (define (parameterize-namespace row col get-cell-val data thunk)
    (parameterize ([current-namespace (current-namespace)])
      (namespace-set-variable-value! 'row row)
      (namespace-set-variable-value! 'col col)
      (namespace-set-variable-value! 'get-cell-val get-cell-val)
      (namespace-set-variable-value! 'data data)
      (thunk)))
  
  ;; Creates a list of formatted strings
  ;; for use as row and column label strings
  (define (make-loc-string str base max)
    (build-list
     max
     (lambda (i)
       (format str (+ i base)))))
  
  ;; Creates a string representation of the current
  ;; state of the cells
  (define (flush-text data)
    (let r-loop ([c-row 0] [r-lst '()])
      (if (>= c-row VIS_ROWS)
          r-lst
          (let c-loop ([c-col 0] [c-lst '()])
            (if (>= c-col VIS_COLS)
                (r-loop (add1 c-row) (append c-lst r-lst))
                (c-loop (add1 c-col)
                        (let ([vnd (value-now (data (rowXcol->key c-row c-col)))])
                          (if (string=? vnd "")
                              c-lst
                              (cons (list (rowXcol->key c-row c-col)
                                          vnd)
                                    c-lst)))))))))
  
  ; add global hashtable mapping window to its parent object
  ;; Spreadsheet object
  (define spreadsheet%
    (class object%
      (init (load-from-file #f))
      (super-new)
      
#|      (define filename-str (new-cell 
                            (if load-from-file
                                load-from-file
                                "Untitled")))|#
      
      ;; List of cell address and values loaded from the file specified
      (define binding-lst (if load-from-file
                              (read (open-input-file load-from-file))
                              '()))
      
      ;; parameters for the current cell row and column
      ; -- available in cell formulas --
      (define row (make-parameter -1))
      (define col (make-parameter -1))
      
      
      
      
      ;; establish the root window
      (current-widget-parent 
       (new ft-frame% (label "Spreadsheet") 
            (width MAX_VIEW_WIDTH) 
            (height MAX_VIEW_HEIGHT)
            (key-events-event-processor split-key-events/type))#;(default-parent))
      
      (send (current-widget-parent) show #t)
      
      ;; Used to determine if there is multiple selection
      (define control-down?
        (hold (merge-e
               (map-e (lambda (_) #t) ((send (current-widget-parent) get-key-events) 'control))
               (map-e (lambda (_) #f) ((send (current-widget-parent) get-key-events) 'release)))
              #f))
      
      
      ;; Spreadsheet content
      (define-values-rec
        ;;;;;;;;;;;;;;;;;;;
        ;; Menu bar & items
        [menu-bar (new menu-bar% (parent (current-widget-parent)))]
        [file-menu (new menu% (label "File") (parent menu-bar))]
        [load-events (value-e (new ft-menu-item% (label "Load...") (parent file-menu)))]
        [save-events (value-e (new ft-menu-item% (label "Save As...") (parent file-menu)))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;
        ;; Formula entry widget
        [formula (mode widget ft-text-field% (label "Formula:")
                       ;(init-val "")
                       (value-set (merge-e
                                   last-selected-cell-text-e
                                   (map-e (lambda (_) (value-now copy-buffer))
                                          paste-e)))
                       (key-events-event-processor split-key-events/type)
                       (focus-when selecting-clicks))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;
        ;; Organizational Panes
        
        ; canvas, row labels, and column label master container
        [can-and-all-lbls-pane (new vertical-pane% (parent (current-widget-parent)))]
        ; holds column labels
        [col-lbl-pane (new free-horiz-pane% (parent can-and-all-lbls-pane) (stretchable-height #f) (alignment '(left top)))]
        ; holds row labels pane and canvas
        [row-lbl-and-can-pane (new horizontal-pane% (parent can-and-all-lbls-pane))]
        ; holds row labels
        [row-lbl-pane (new free-vert-pane% (parent row-lbl-and-can-pane) 
                           (min-width LBL_WIDTH)
                           (alignment '(right top))
                           (stretchable-width #f)
                           )]
        
        ;;;;;;;;;;;;;;;;;;
        ;; Formula Storage
        
        [data 
         (let ([d 
                (make-accessor/initial-bindings (send formula get-value-b) 
                                                commit-e 
                                                (map posn->key currently-selected-cells)
                                                binding-lst)
                ])
           (lambda (k)
             (super-lift
              d
              k)))]
        
        
        ;;;;;;;;;;;;;;;;;;;;;
        ;; Formula Evaluation
        
        [eval-it
         (lambda (r c)
           (let ([s (data (rowXcol->key r c))])
             (if (or (undefined? s) (string=? s ""))
                 ""
                 (parameterize-namespace
                  row
                  col
                  get-cell-val
                  data
                  (lambda ()
                    (super-lift
                     (lambda (v)
                       (eval
                        (read
                         (open-input-string
                          (string-append
                           (format 
                            "(parameterize ([row ~a][col ~a])"
                            (cadr v) (caddr v))
                           (string-append
                            (car v)
                            ")"))))))
                     (list s r c)))))))]
        
        ;; Events for committing the formula to formula storage
        [commit-e ((send formula get-key-events) #\return)]
        ;; Events for putting the copy buffer into the formula widget
        [paste-e ((send formula get-key-events) 'f2)]
        ;; List of cells that are currently selected
        [currently-selected-cells
         (hold
          (collect-e
           selecting-clicks
           '()
           (lambda (evt accum)
             (if (value-now control-down?)
                 (cons evt accum)
                 (list evt))))
          '())]
        
        ;; An event stream carrying an occurence when a cell is selected,
        ;; whose value is the formula of that cell
        [last-selected-cell-text-e
         (map-e
          (lambda (evt)
            (let ([vn (value-now (data (posn->key evt)))])
              (if (undefined? vn)
                  ""
                  vn)))
          selecting-clicks)]
        
        ;; Behavior storing the last copied formula
        [copy-buffer 
         (let ([f-v (send formula get-value-b)])
           (hold 
            (map-e
             (lambda (_) (value-now f-v))
             ((send formula get-key-events) 'f1)) 
            ""))]
        
        ;;;;;;;;;;;;;;;;;
        ;; Value Accessor
        ; -- is available in formulas --
        [get-cell-val (lambda (c r) (if (and (= r (row)) (= c (col))) 
                                        (begin
                                          (error 'get-cell-val "cannot read own value!")
                                          undefined)
                                        ;(let ([the-data (data (rowXcol->key r c))])
                                        ;  (if (string=? the-data "")
                                        ;      undefined
                                        (eval-it #;(data (rowXcol->key r c)) r c)
                                        ;                        ))
                                        ))]
        
        ;; List of blue boxes to be used to indicate selection
        [selected-cell-bg (map
                           (lambda (elt)
                             (let ([is-valid? (not (or (empty? currently-selected-cells)
                                                       (undefined? 
                                                        (car currently-selected-cells))))])
                               (make-select-box
                                (if is-valid?
                                    (* COL_WIDTH (- (posn-y elt) hscroll-b))
                                    (+ MAX_VIEW_WIDTH COL_WIDTH))
                                (if is-valid?
                                    (* ROW_HEIGHT (- (posn-x elt) vscroll-b))
                                    (+ MAX_VIEW_HEIGHT ROW_HEIGHT))
                                (add1 COL_WIDTH)
                                (add1 ROW_HEIGHT))))
                           currently-selected-cells)]
        
        ;;;;;;;;;
        ;; Canvas
        ; -- used to draw the cells, values, and selections
        [can (new spread-canvas%
                  (parent row-lbl-and-can-pane)
                  (grid-lines GRID_BACKGROUND)
                  (content all-val-pics)
                  (min-width INIT_VIEW_WIDTH)
                  (min-height INIT_VIEW_HEIGHT)
                  (style '(vscroll hscroll))
                  (select-area selected-cell-bg))]
        
        ;; vertical scrolling offset (behavior)
        [vscroll-b (hold (map-e (lambda (evt)
                                  (send evt get-position))
                                ((send can get-scroll-events) 'vertical)) 0)]
        
        ;; horizontal scrolling offset (behavior)
        [hscroll-b (hold (map-e (lambda (evt)
                                  (send evt get-position))
                                ((send can get-scroll-events) 'horizontal)) 0)]
        
        
        ;; Column Labels
        ;; spacer is used to align column labels
        [spacer (new ft-message% (parent col-lbl-pane) (min-width LBL_WIDTH))]
        ;; list of labels indicating columns
        [col-labels (map 
                     (lambda (str) (parameterize ([current-widget-parent col-lbl-pane])
                                     (mode widget ft-message% (label str)
                                           (min-width COL_WIDTH)
                                           (stretchable-width #f)
                                           (horiz-margin 0)
                                           (font LBL_FONT))))
                     (make-loc-string "(~a, )" hscroll-b VIS_COLS))]
        
        ;; Row Labels
        ;; list of labels indicating the row
        [row-labels (map (lambda (str) (parameterize ([current-widget-parent row-lbl-pane])
                                         (mode widget ft-message% (label str)
                                               (vert-margin 0)
                                               (min-height 0)
                                               (min-width LBL_WIDTH)
                                               (stretchable-width #f)
                                               (font LBL_FONT))))
                         (make-loc-string "( ,~a)" vscroll-b VIS_ROWS))]
        
        ;; List of values (with spacial information) for drawing in the canvas
        [all-val-pics
         (let r-loop ([c-row 0] [r-lst '()])
           (if (>= c-row VIS_ROWS)
               r-lst
               (let c-loop ([c-col 0] [c-lst '()])
                 (if (>= c-col VIS_COLS)
                     (r-loop (add1 c-row) (append c-lst r-lst))
                     (c-loop (add1 c-col)
                             (cons 
                              (make-text-disp
                               (+ HORIZ_BUFF (* c-col COL_WIDTH))
                               (+ VERT_BUFF (* c-row ROW_HEIGHT))
                               (custom->string (eval-it (+ c-row vscroll-b)
                                                      (+ c-col hscroll-b))))
                              c-lst))))))]
        
        ;; Mouse click events that indicate a new/additional selection
        [selecting-clicks (map-e 
                           (lambda (evt)
                             (let ([m-x (value-now (send can get-mouse-x))]
                                   [m-y (value-now (send can get-mouse-y))]
                                   [x-off (value-now hscroll-b)]
                                   [y-off (value-now vscroll-b)])
                               (make-posn
                                (+ y-off (floor (/ m-y ROW_HEIGHT)))
                                (+ x-off (floor (/ m-x COL_WIDTH))))))
                           (send can get-l-clicks))])

      ;; Handle loading events
      (for-each-e!
       load-events
       (lambda (le)
         (thread (lambda ()
                   (cond [(finder:get-file)
                          =>
                          (lambda (filename)
                            (new spreadsheet% (load-from-file filename)))])))))
      
      ;; Handle saving events
      (for-each-e!
       save-events
       (lambda (se)
         (thread (lambda ()
                   (cond [(finder:put-file)
                          =>
                          (lambda (filename)
                            (when (file-exists? filename)
                              (delete-file filename))
                            (let ([p (open-output-file filename)])
                              (write (flush-text data) p )
                              (flush-output p)
                              (close-output-port p)))])))))
      
      (send can set-scroll-range 'vertical 3000)
      (send can set-scroll-range 'horizontal 3000)
      (send (current-widget-parent) show #t)
      
      ))
  
  ;; start up a spredsheet when module is required
  (define s (new spreadsheet%))
  
  )

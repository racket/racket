(module xelda-com mzscheme
  
  (require (lib "mysterx.ss" "mysterx"))
  (require (lib "list.ss"))
  (require (lib "class.ss"))
  
  (require "xl-util.ss")
  
  (provide load-xl-file           ;; filename -> void
           set-gui!               ;; gui -> void
           ;; gui is an instance of the class %xelda-frame% in xelda.ss
           close-xl-workbook      ;; void -> void
           clear-cell-precedents  ;; cellref -> void
           ;; cellref as a symbol representing a cell (ex: 'a1 for cell A1)
           get-cell-text          ;; cellref -> string
           get-cell-value         ;; cellref -> cell-value
           get-cell-formula       ;; cellref -> string
           get-cell-name          ;; cellref -> string
           get-cell-row-col       ;; cellref -> (N , N)
           ;; cell-value is defined as per Excel specifications for "Value" property
           get-cell-color         ;; cellref -> color
           set-cell-color!        ;; cellref color -> void
           get-cell-comment       ;; cellref -> string
           set-cell-comment!      ;; cellref string -> void
           delete-cell-comment!   ;; cellref -> void
           iterate-over-worksheet ;; (cellref -> V) (V -> bool) -> (Listof (cellref , V))
           work-sheet-enabled?    ;; void -> bool
           )
  
  (define *cached-ws* empty)
  (define *wb* #f)
  (define *ws* #f)
  (define *excel-progid* "Excel.Application")
  (define *xl* (cci/progid *excel-progid*))
  (define *gui* #f)
  
  (define (range-from-coords row col)
    (string-append (number->cell-alpha (sub1 col))
                   (number->string row)))
  
  (define (set-gui! gui) (set! *gui* gui))
  
  (define (work-sheet-enabled?)
    (not (equal? *ws* #f)))
  
  (define (iterate-over-worksheet f pred?)
    (when (empty? *cached-ws*)
      (let* ([ur (com-get-property *ws* "UsedRange")]
             [cells (com-get-property ur "Cells")]
             [first-row (com-get-property cells "Row")]
             [first-col (com-get-property cells "Column")]
             [num-rows (com-get-property cells "Rows" "Count")]
             [num-cols (com-get-property cells "Columns" "Count")]
             [last-row (sub1 (+ first-row num-rows))]
             [last-col (sub1 (+ first-col num-cols))])
        (let row-loop ([curr-row first-row]
                       [row-results null])
          (if (> curr-row last-row)
              (set! *cached-ws* row-results)
              (let col-loop ([curr-col first-col]
                             [col-results null])
                (if (> curr-col last-col)
                    (row-loop (add1 curr-row) 
                              (append col-results 
                                      row-results))
                    (col-loop (add1 curr-col)
                              (cons (string->symbol (range-from-coords curr-row curr-col))
                                    col-results))))))))
    (foldl (lambda (cached-cell selected-cells)
             (let ([cell-content (f cached-cell)])
               (if (pred? cell-content)
                   (cons (list cached-cell cell-content) selected-cells)
                   selected-cells)))
           empty *cached-ws*))
  
  (define (get-cell-name cell)
    (with-handlers
        ([void (lambda _ "")])
      (com-get-property (com-get-property (cellref->rng cell) "Name") "Name")))
  
  (define (get-cell-formula cell)
    (com-get-property (cellref->rng cell) "Formula"))
  
  (define (get-cell-row-col cell)
    (let* ([rng (cellref->rng cell)]
           [M (com-get-property rng "CurrentArray")]
           [first-row (com-get-property M "Row")]
           [first-col (com-get-property M "Column")]
           [cell-row (com-get-property rng "Row")]
           [cell-col (com-get-property rng "Column")])
      (list (- cell-row first-row) (- cell-col first-col))))
  
  
  (define (set-cell-comment! cell comment-text)
    (let* ([rng (cellref->rng cell)]
           [comment-obj (com-get-property rng "Comment")])
      (with-handlers ([void (lambda _ "")])
        (com-invoke comment-obj "Delete"))
      (com-invoke rng "AddComment" (format "~a" comment-text))))
  
  (define (delete-cell-comment! cell)
    (let* ([rng (cellref->rng cell)]
           [comment-obj (com-get-property rng "Comment")])
      (with-handlers ([void (lambda _ "")])
        (com-invoke comment-obj "Delete"))))
  
  (define (get-cell-comment cell)
    (with-handlers ([void (lambda _ "")])
      (com-invoke (com-get-property (cellref->rng cell) "Comment") "Text")))
  
  (define (get-cell-color cellref)
    (com-get-property (com-get-property (cellref->rng cellref) "Interior") "Color"))
  
  (define (set-cell-color! cellref rgb)
    (let ([rng (cellref->rng cellref)])
      (com-set-property! 
       (com-get-property rng "Interior")
       "Color" rgb)
      (com-set-property!
       (com-get-property rng "Borders")
       "Color" 11842740)))
  
  (define (get-cell-text cellref)
    (com-get-property (cellref->rng cellref) "Text"))
  
  (define (get-cell-value cellref)
    (com-get-property (cellref->rng cellref) "Value"))
  
  (define (get-cell row col)
    (com-get-property 
     *ws* 
     `("Range" ,(range-from-coords row col))))
  
  (define (cellref->rng cell)
    (let ([xy (cellref->numbers cell)])
      (get-cell (cadr xy) (car xy))))
  
  (define (clear-cell-precedents cellref)
    (com-invoke (cellref->rng cellref) "ShowPrecedents" #t))
  
  (define (open-xl-workbook xl fname)
    (let ([wbs (com-get-property xl "Workbooks")])
      (with-handlers
          (((lambda (_) #t) (lambda (_) #f)))
        (com-invoke wbs "Open" fname))))
  
  (define (close-xl-workbook)
    (let ([wbs (com-get-property *xl* "Workbooks")])
      (with-handlers
          (((lambda (_) #t) (lambda (_) #f)))
        (com-invoke wbs "Close"))))
  
  (define (add-xl-workbook xl)
    (let* ([wbs (com-get-property xl "Workbooks")])
      (com-invoke wbs "Add")))
  
  (define (load-xl-file filename)
    (set! *cached-ws* empty)
    (com-set-property! *xl* "Visible" #t)
    (set! *wb* (open-xl-workbook *xl* filename))
    (set! *ws* (com-get-property *wb* "ActiveSheet"))
    
    ;;
    ;; Event handlers
    ;;  
    (com-register-event-handler
     *wb* "SheetActivate"
     (lambda (wb-arg)
       (when *ws*
         (send *gui* unmark-errors)
         (com-unregister-event-handler *ws* "SelectionChange"))
       (set! *ws* (com-get-property *wb* "ActiveSheet"))
       (set! *cached-ws* empty)
       (com-register-event-handler
        *ws* "SelectionChange"
        (lambda (rng)
          (when (send *gui* file-opened?)
            (let* ([row (com-get-property rng "Row")]
                   [col (com-get-property rng "Column")]
                   [rows (com-get-property rng "Rows" "Count")]
                   [cols (com-get-property rng "Columns" "Count")]
                   [left-top (numbers->cellref (list (sub1 col) row))]
                   [right-bottom (numbers->cellref (list (- (+ col cols) 2)
                                                         (sub1 (+ row rows))))])
              (send *gui* erase-cell-selections)
              (send *gui* set-cell-selections (format "~a:~a" left-top right-bottom))))))))
    
    (com-register-event-handler
     *ws* "SelectionChange"
     (lambda (rng)
       (when (send *gui* file-opened?)
         (let* ([row (com-get-property rng "Row")]
                [col (com-get-property rng "Column")]
                [rows (com-get-property rng "Rows" "Count")]
                [cols (com-get-property rng "Columns" "Count")]
                [left-top (numbers->cellref (list (sub1 col) row))]
                [right-bottom (numbers->cellref (list (- (+ col cols) 2)
                                                      (sub1 (+ row rows))))])
           (send *gui* erase-cell-selections)
           (send *gui* set-cell-selections (format "~a:~a" left-top right-bottom))))))
    
    (com-register-event-handler
     *wb* "BeforeClose"
     (lambda (cancel) (when (send *gui* file-opened?) (send *gui* close-frame!)))))
  
  (com-register-event-handler 
   *xl* "SheetBeforeRightClick"
   (lambda (ws rng b)
     (let* ([row (com-get-property rng "Row")]
            [col (com-get-property rng "Column")]
            [cell-ref (numbers->cellref (list (sub1 col) row))])
       (when (send *gui* error-cell? cell-ref)
         (set-box! b #t)
         (let ([dependencies (send *gui* get-error-cell-dependencies cell-ref)])
           (if (eq? (send *gui* get-error-cell-state cell-ref) 'off)
               (begin
                 (send *gui* set-error-cell-state cell-ref 'on)
                 (send *gui* color-dependents dependencies 'off)
                 (com-invoke rng "ShowPrecedents" #t))
               (begin
                 (send *gui* set-error-cell-state cell-ref 'off)
                 (send *gui* color-dependents dependencies 'on)
                 (com-invoke rng "ShowPrecedents")))))))))



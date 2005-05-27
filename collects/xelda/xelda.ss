(module xelda mzscheme
  (require (lib "class.ss"))
  (require (lib "list.ss"))
  (require (lib "mred.ss" "mred"))
  
  (require "private/xelda-com.ss")
  (require "private/xelda-checker.ss")
  (require "private/parser.ss")
  (require "private/xl-util.ss")
  (require "private/formula.ss")
  
  (define-struct dependent (count orig-color) (make-inspector))
  
  (define xelda-frame%
    (class frame%
      (field
       [bad-format-cells empty]
       [bad-format-cells-colors #f]
       [colored-dependents #f]
       [error-cells #f]
       [file-checked #f]
       [filename #f]
       [file-opened #f]
       [parser (make-parser empty)]
       [status-text-editor #f]
       [range-text-editor #f]
       [status-panel #f]
       [status-text #f]
       [status-message #f]
       [assign-panel #f]
       [assign-button #f]
       [range-text #f]
       [units-text #f]
       [buttons-panel #f]
       [load-button #f]
       [quit-button #f]
       [close-button #f]
       [clear-button #f]
       [check-button #f])
      (public*
       [error-cell? (lambda (cell) (and (hash-table? error-cells)
                                        (in-hash? error-cells cell)))]
       [get-error-cell-dependencies (lambda (cell) (second (hash-table-get error-cells cell)))]
       [get-error-cell-state (lambda (cell) (first (hash-table-get error-cells cell)))]
       [set-error-cell-state 
        (lambda (cell state)
          (hash-table-put! error-cells cell 
                           (list state (second (hash-table-get error-cells cell)))))] 
       [set-cell-selections (lambda (str) (send range-text-editor insert str))]
       [erase-cell-selections (lambda () (send range-text-editor erase))]
       [file-opened? (lambda () file-opened)]
       [set-status (lambda (str)
                     (send status-text-editor erase)
                     (send status-text-editor insert str))]
       [update-status (lambda (str) (send status-text-editor insert str))]
       [next-update-status (lambda (str) (update-status (format "done~n~a.... " str)))]
       [set-bad-format-cells (lambda (bad-cells) (set! bad-format-cells bad-cells))]
       [set-computation-errors 
        (lambda (errors)
          (update-status (format "+++ERROR REPORTING+++~n"))
          (set! error-cells (make-hash-table))
          (for-each
           (lambda (cell-unit-str-dependencies)
             (let ([cell (first cell-unit-str-dependencies)]
                   [unit (second cell-unit-str-dependencies)]
                   [str (third cell-unit-str-dependencies)]
                   [dependencies (fourth cell-unit-str-dependencies)])
               (cond [(equal? 'error/propagated (first (first unit)))
                      (set-cell-color! cell (rgb 209 35 238))
                      (update-status
                       (format "Cell ~a error propagation from cell~a~n"
                               cell
                               (second (first unit))))]
                     [else
                      (set-cell-color! cell (rgb 255 255 51))
                      (update-status (format "Cell ~a error in computed units!~n" cell))])
               (append-cell-comment! cell str)
               (hash-table-put! error-cells cell (list 'on dependencies))))
           errors))]
       [set-mismatch-errors 
        (lambda (errors)
          (for-each 
           (lambda (cell-actual-str-dependencies)         
             (let ([cell (first cell-actual-str-dependencies)]
                   [actual (second cell-actual-str-dependencies)]
                   [str (third cell-actual-str-dependencies)]
                   [dependencies (fourth cell-actual-str-dependencies)])
               (set-cell-color! cell (rgb 255 153 51))
               (append-cell-comment! cell str)
               (hash-table-put! error-cells cell (list 'on dependencies))
               (update-status 
                (format "Cell ~a mismatch between annotated and computed units!~n"
                        cell))))
           errors))]
       [close-frame!
        (lambda()
          (set! file-opened #f)
          (set-status (format "Load Excel file~n"))
          (send load-button enable #t)
          (send close-button enable #f)
          (send check-button enable #f)
          (send clear-button enable #f)
          (send assign-button enable #f)
          (send range-text enable #f)
          (send units-text enable #f)
          (close-xl-workbook))]
       [color-dependents 
        (lambda (l action)
          (when (not (empty? l))
            (set-cell-color! (first l) (get-dependent-color (first l) action))
            (color-dependents (rest l) action)))]
       [unmark-errors
        (lambda ()
          (when (hash-table? error-cells)
            (let ([orig-col (get-cell-color 'a1)])
              (hash-table-for-each 
               error-cells
               (lambda (k v)
                 (let ([state (first v)]
                       [dependencies (second v)])
                   (when (eq? state 'off)
                     (color-dependents dependencies 'off)
                     (clear-cell-precedents k))
                   (let ([orig (original-comment (get-cell-comment k))])
                     (cond 
                       ((and (>= (string-length orig) 2)
                             (string=? "()" (substring orig 0 2)))
                        (delete-cell-comment! k))
                       (else (set-cell-comment! k orig))))
                   (set-cell-color! k orig-col))))
              (set! error-cells #f)
              (set! colored-dependents #f))))]
       [reset-frame!
        (lambda()
          (for-each (lambda (panel)
                      (when panel (send panel show #f)))
                    (list status-panel buttons-panel))
          (set! buttons-panel
                (instantiate horizontal-panel% ()
                  (parent this)
                  (stretchable-height #f)
                  (vert-margin 2)
                  (alignment '(center center))))
          (set! status-panel
                (instantiate vertical-panel% ()
                  (parent this)
                  (border 5)
                  (horiz-margin 5)
                  (vert-margin 10)
                  (stretchable-height #t)
                  (style '(border))))
          (set! assign-panel
                (instantiate vertical-panel% ()
                  (parent this)
                  (stretchable-height #f)
                  (style '(border))
                  (border 5)
                  (spacing 3)
                  (horiz-margin 5)
                  (vert-margin 5)
                  (alignment '(center center))))
          (set! status-message
                (instantiate message% ()
                  (label "XeLda Status:")
                  (parent status-panel)))
          (set! status-text
                (instantiate text-field% ()
                  (label #f)
                  (init-value (format "Ready to load Excel file~n"))
                  (parent status-panel)
                  (style (list 'multiple))
                  (stretchable-height #t)
                  (enabled #t)
                  (callback (lambda (txt-fld cntrl-evnt) ()))))
          (set! status-text-editor (send status-text get-editor))
          (set! range-text
                (instantiate text-field% ()
                  (label "Cell(s) Range(s):")
                  (parent assign-panel)
                  (enabled #f)
                  (callback (lambda (txg-fld cntrl-evnt) ()))))
          (set! range-text-editor (send range-text get-editor))
          (set! units-text
                (instantiate text-field% ()
                  (label "      Cell(s) Units:")
                  (parent assign-panel)
                  (enabled #f)
                  (callback (lambda (txt-fld cntrl-evnt) ()))))
          (set! assign-button
                (instantiate button% ()
                  (label "Assign Units")
                  (parent assign-panel)
                  (enabled #f)
                  (callback 
                   (lambda (b env)
                     (let* ([range-editor (send range-text get-editor)]
                            [units-editor (send units-text get-editor)]
                            [range-txt (send range-editor get-text)]
                            [units-txt (send units-editor get-text)]
                            [unit-list (get-ranges-from-txt range-txt parser)]
                            [unit (get-units-from-txt units-txt)])
                       (cond
                         ((empty? unit)
                          (message-box "Invalid Input"
                                       "Improper unit format"
                                       this
                                       '(ok)))
                         ((empty? unit-list)
                          (message-box "Invalid Input"
                                       "Improper range format"
                                       this
                                       '(ok)))
                         (else
                          (send range-editor erase)
                          (send units-editor erase)
                          (for-each (lambda (us)
                                      (for-each (lambda (c) 
                                                  (set-cell-comment! c units-txt))
                                                us))
                                    unit-list))))))))
          (set! load-button
                (instantiate button% ()
                  (label "Load File")
                  (parent buttons-panel)
                  (callback (lambda (b ev)
                              (let ([file-name (get-filename)])
                                (when file-name
                                  (set! file-checked #f)
                                  (set! file-opened #t)
                                  (set! filename file-name)
                                  (update-status (format "Loaded file: ~a~n" filename))
                                  (open-frame!)))))))
          (set! close-button
                (instantiate button% ()
                  (label "Close File")
                  (parent buttons-panel)
                  (enabled #f)
                  (callback (lambda (b ev)
                              (close-frame!)))))
          (set! check-button
                (instantiate button% ()
                  (label "Analyze")
                  (parent buttons-panel)
                  (enabled #f)
                  (callback (lambda (b ev)
                              (send quit-button enable #f)
                              (send close-button enable #f)
                              (send clear-button enable #f)
                              (send check-button enable #f)
                              (send assign-button enable #f)
                              (send units-text enable #f)
                              (send range-text enable #f)
                              (thread (lambda()
                                        (time
                                         (begin
                                           (when file-checked (unmark-errors))
                                           (unit-check-xl-file)
                                           (set! file-checked #t)
                                           (send clear-button enable #t)
                                           (send check-button enable #t)
                                           (send close-button enable #t)
                                           (send assign-button enable #t)
                                           (send units-text enable #t)
                                           (send range-text enable #t)
                                           (send quit-button enable #t)))))))))
          (set! clear-button
                (instantiate button% ()
                  (label "Clear")
                  (parent buttons-panel)
                  (enabled #f)
                  (callback (lambda (b ev)
                              (unmark-errors)
                              (clear-all-precedents)))))
          (set! quit-button
                (instantiate button% ()
                  (label "Quit Xelda")
                  (parent buttons-panel)
                  (callback (lambda (b ev)
                              (when file-opened (close-xl-workbook))
                              (send this show #f))))))])
      (private*
       [unit-check-xl-file
        (lambda ()
          (when (not (empty? bad-format-cells))
            (clear-bad-format-cells))
          (cond [(empty? bad-format-cells) 
                 (unit-check this)
                 (set! colored-dependents (make-hash-table))
                 (update-status (format "+++XeLda DONE!+++~n"))]
                [else (mark-bad-format-cells)]))]
       [clear-bad-format-cells
        (lambda ()
          (for-each
           (lambda (bad-cell-with-unit)
             (let* ([bad-cell (first bad-cell-with-unit)]
                    [bad-unit (second bad-cell-with-unit)]
                    [orig-col (hash-table-get bad-format-cells-colors bad-cell)])
               (set-cell-color! bad-cell orig-col)))
           bad-format-cells)
          (set! bad-format-cells-colors #f)
          (set! bad-format-cells empty))]
       [mark-bad-format-cells
        (lambda ()
          (when (not (empty? bad-format-cells))
            (set! bad-format-cells-colors (make-hash-table))
            (update-status 
             (format "+++PREPROCESSING ERROR++~nImproper unit format for the following cells:~n"))
            (for-each 
             (lambda (bad-cell-with-unit)
               (let* ([bad-cell (first bad-cell-with-unit)]
                      [bad-unit (second bad-cell-with-unit)]
                      [orig-col (get-cell-color bad-cell)])
                 (update-status (format "cell: ~a - Improper unit format~n" bad-cell))
                 (hash-table-put! bad-format-cells-colors bad-cell orig-col)
                 (set-cell-color! bad-cell (rgb 120 170 120))))
             bad-format-cells)))]
       [clear-all-precedents
        (lambda ()
          (when (hash-table? error-cells)
            (hash-table-for-each
             error-cells
             (lambda (cell v)
               (let ([state (first v)]
                     [dependencies (second v)])
                 (when (eq? state 'off)
                   (hash-table-put! error-cells cell (list 'on dependencies))
                   (color-dependents dependencies 'off)
                   (clear-cell-precedents cell)))))))]
       [get-dependent-color 
        (lambda (loc action)
          (if (eq? action 'on)
              (begin
                (if (in-hash? colored-dependents loc)
                    (let ([dep (hash-table-get colored-dependents loc)])
                      (hash-table-put! colored-dependents loc
                                       (make-dependent (+ (dependent-count dep) 1)
                                                       (dependent-orig-color dep))))
                    (hash-table-put! colored-dependents loc
                                     (make-dependent 
                                      0
                                      (get-cell-color loc))))
                (rgb 250 0 0))
              (let ([dep (hash-table-get colored-dependents loc)])
                (if (= 0 (dependent-count dep))
                    (begin
                      (hash-table-remove! colored-dependents loc)
                      (dependent-orig-color dep))
                    (begin
                      (hash-table-put! colored-dependents loc
                                       (make-dependent (sub1 (dependent-count dep))
                                                       (dependent-orig-color dep)))
                      (rgb 250 0 0))))))]  
       [open-frame!
        (lambda()
          (send load-button enable #f)
          (send close-button enable #t)
          (send check-button enable #t)
          (send assign-button enable #t)
          (send range-text enable #t)
          (send units-text enable #t)
          (load-xl-file filename))])
      (super-instantiate())
      (reset-frame!)))
  
  (define *gui*
    (instantiate xelda-frame% ()
      (label "XeLda")
      (width 400)
      (height 550)))
  
  (send *gui* center)
  (send *gui* show #t)
  (set-gui! *gui*)
  
  (define (get-filename)
    (get-file ".xls files"
              *gui*
              (collection-path "xelda")
              #f
              "xls"
              '()
              '(("Excel Files" "*.xls"))))
  
  (define (rgb r g b) 
    (+ (* 65536 b) (* 256 g) r))
  
  (define (original-comment comment)
    (letrec ([loop (lambda (str i max)
                     (cond [(= i max) str]
                           [(equal? #\; (string-ref str i)) (substring str 0 (sub1 i))]
                           [else (loop str (+ i 1) max)]))])
      (loop comment 0 (string-length comment))))
  
  (define (append-cell-comment! cellref scheme-comment)
    (let ([orig-comment (get-cell-comment cellref)])
      (cond [(string=? "" orig-comment)
             (set-cell-comment! cellref (format "()~n;; ~a" scheme-comment))]
            [else
             (set-cell-comment! cellref (format "~a~n;; ~a" orig-comment scheme-comment))])))
  
  (define (get-ranges-from-txt str parser)
    (let* ([s (list->string (filter (lambda (c) (not (char-whitespace? c)))
                                    (string->list str)))]
           [ranges-txt (split-string s #\,)]
           [ranges (map (lambda (r) (split-string r #\:)) ranges-txt)]
           [valid-ranges (filter (lambda (r) (not (empty? r)))
                                 (map (lambda (r)
                                        (filter (lambda (c)
                                                  (cell-ref? (call-parser parser c)))
                                                r))
                                      ranges))])
      (map (lambda (r)
             (cond ((= 2 (length r)) (get-range-cells (first r) (second r)))
                   (else r)))
           (map (lambda (r)
                  (map (lambda (c) (string->symbol (to-lower c))) r))
                valid-ranges))))
  
  (define (get-units-from-txt str)
    (with-handlers ([void (lambda _ '())])
      (parse-unit str)))
  )


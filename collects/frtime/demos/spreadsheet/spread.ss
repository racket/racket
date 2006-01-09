(module spread (lib "frtime-big.ss" "frtime")
  
  (require (lib "class.ss")
           (all-except (lib "mred.ss" "mred") send-event)
           (rename mzscheme mz:define-struct define-struct)
           "preprocessor2.ss"
           (lifted "ss-funcs.ss" inflate-data)
           "quotes.ss"
           (as-is:unchecked (lib "match.ss") match-lambda)
           (as-is:unchecked (lib "frp-core.ss" "frtime") signal-value
                            proc->signal)
           (lib "framework.ss" "framework")
           (as-is:unchecked (lib "string.ss") expr->string)
           (as-is:unchecked (lib "etc.ss") build-vector)
           ;(lifted mzscheme regexp-match)
           (as-is:unchecked mzscheme make-hash-table hash-table-put! hash-table-get
                            hash-table-remove! let*-values vector-set! make-string
                            exn?
                            open-input-file open-output-file read write hash-table-map
                            file-exists? delete-file open-input-string eof
                            flush-output close-output-port dynamic-require))
  ;;
  ;; TO DO:
  ;; 
  ;; rewrite cleanly
  ;; case-insensitive and relative identifier expansion with ranges
  ;; select multiple cells
  ;; fill row or column
  ;; copy and paste formula
  ;; disable text field when selection empty
  ;; allow resizing of columns
  
  ;; KNOWN BUGS:
  ;; when loading file, expression text field does not update
  
  (define stock-price
    (opt-lambda (name-string [seconds-between 1200])
      (lift-strict (lambda (name _) (stock-quote name)) name-string (quotient seconds seconds-between))))
  
  (define-syntax for
    (syntax-rules (=)
      [(_ (var = init) condn delta proc ...)
       (let loop ([var init])
         (when condn
           proc ...
           (loop (delta var))))]))
  
  (set-cell! raise-exceptions #t)
  
  (mz:define-struct ss-loc (row col))
  
  (define (ss-format val)
    (if (or (and (signal? val)
                 (undefined? (signal-value val)))
            (and (not (signal? val))
                 (undefined? val)))
        ""
        (format "~a" (signal-value val))))
  
  (define (@e r c)
    (ss-get-cell-value/force r c))
  
  (define (@c r0 r1 c)
    (build-list (add1 (abs (- r1 r0)))
                (lambda (i)
                  (@e (+ i (min r1 r0)) c))))
  
  (define (@r r c0 c1)
    (build-list (add1 (abs (- c1 c0)))
                (lambda (i)
                  (@e r (+ i (min c1 c0))))))
  
  (define (@m r0 r1 c0 c1)
    (build-list (add1 (abs (- c1 c0)))
                (lambda (i)
                  (@c r0 r1 (+ i (min c1 c0))))))
  
  (define frame
    (instantiate frame% ("Spreadsheet") (width 600) (height 400)))
  
  (define menu-bar
    (instantiate menu-bar% (frame)))
  
  (define file-menu
    (instantiate menu% ("File" menu-bar)))
  
  (define open-item
    (instantiate menu-item%
      ("Open..."
       file-menu
       (lambda (_ event) 
         (cond
           [(finder:get-file)
            =>
            (lambda (filename)
              (let ([p (open-input-file filename)])
                (for (i = 0) (< i cols) add1
                     (vector-set! vec i (make-hash-table))
                     (for-each (lambda (elt) #;(printf "adding ~a ~a ~a~n" (first elt) i (second elt))
                                 (ss-set-cell-processed-expr! (first elt) i (process (second elt) '@e '@r '@c '@m (first elt) i))) (read p))))
              (send canvas refresh))])))
      (shortcut #\O)))
  
  (define save-item
    (instantiate menu-item%
      ("Save as..."
       file-menu
       (lambda (_ event)
         (cond
           [(finder:put-file)
            =>
            (lambda (filename)
              (when (file-exists? filename)
                (delete-file filename))
              (let ([p (open-output-file filename)])
                (for (i = 0) (i . < . cols) add1
                     (let ([v (hash-table-map (vector-ref vec i) (lambda (row cell) (list row (ss-cell-expr cell))))])
                       #;(printf "~a~n" v)
                       (write v p)))
                (flush-output p)
                (close-output-port p)))])))
      (shortcut #\S)))
  
  (define edit-menu
    (instantiate menu% ("Edit" menu-bar)))
  
  (define text-field
    (instantiate text-field%
      ("Formula:"
       frame
       (lambda (this control-event)
         (case (send control-event get-event-type)
           [(text-field-enter) (send canvas new-expression (send this get-value))])))))
  
  (define value-field
    (instantiate text-field%
      ("Value:" frame void)))
  (send value-field enable #f)
  
  (define rows 1000)
  (define cols 100)
  
  (define vec
    (build-vector
     cols
     (lambda (_) (make-hash-table))))
  
  (mz:define-struct ss-cell (expr value updater))
  
  (define (ss-get-cell-text row col)
    (cond
      [(hash-table-get (vector-ref vec col) row (lambda () #f))
       => (lambda (cell)
            (let ([expr (unprocess (ss-cell-expr cell) '@e '@r '@c '@m row col)])
              (if (eq? expr 'undefined)
                  ""
                  (expr->string expr))))]
      [else ""]))
  
  (define (ss-get-cell-value row col)
    (cond
      [(hash-table-get (vector-ref vec col) row (lambda () #f))
       => ss-cell-value]
      [else undefined]))
  
  (define (fresh-ss-cell row col)
    (let* ([value (new-cell)]
           [ret (make-ss-cell
                 'undefined value
                 (proc->signal
                  (lambda () (send canvas draw-cell row col))
                  value))])
      (hash-table-put! (vector-ref vec col) row ret)
      ret))
  
  (define (ss-get-cell-value/force row col)
    (ss-cell-value (hash-table-get (vector-ref vec col) row (lambda () (fresh-ss-cell row col)))))
  
  (define (text->processed-expr txt row col)
    (let* ([expr
            (with-handlers
                ([exn? (lambda (exn)
                         (message-box
                          "Error"
                          (format "The expression you entered is invalid:~n~a"
                                  (exn-message exn))
                          frame
                          '(ok stop))
                         eof)])
              (read (open-input-string txt)))])
      (if (eof-object? expr)
          'undefined
          (process expr '@e '@r '@c '@m row col))))
  
  ;; should not (and does not) remove when cells are emptied
  ;; should not reset when expression is the same
  (define (ss-set-cell-processed-expr! row col processed-expr)
    (let* ([cell
            (hash-table-get
             (vector-ref vec col) row
             (lambda ()
               (fresh-ss-cell row col)))])
      (when (not (equal? (ss-cell-expr cell) processed-expr))
        (set-ss-cell-expr! cell processed-expr)
        (set-cell! (ss-cell-value cell)
                   (with-handlers
                       ([exn? (lambda (exn)
                                (message-box
                                 "Error"
                                 (format "The following error occurred while evaluating a formula:~n~a"
                                         (exn-message exn))
                                 frame
                                 '(ok stop))
                                "#<Error>")])
                     (eval `(let ([row ,row]
                                  [col ,col])
                              ,processed-expr))))
        ;(synchronize)
        (send canvas draw-cell row col))
      (send canvas focus)))
  
  (define chars-per-cell 14)
  
  (define (take-upto n lst)
    (if (and (positive? n)
             (cons? lst))
        (cons (first lst) (take-upto (sub1 n) (rest lst)))
        empty))
  
  (define (history-e n b)
    (collect-e (changes b) (list (value-now b)) (lambda (ev acc) (take-upto n (cons ev acc)))))
  
  (define (clip lo x hi)
    (max lo (min x hi)))
  
  (define (between x y z)
    (or (<= x y z)
        (<= z y x)))
  
  (define ss-canvas%
    (class canvas%
      (super-instantiate ())
      
      (inherit
        refresh
        get-dc
        get-scroll-pos
        get-client-size
        set-scroll-range
        set-scroll-page
        init-manual-scrollbars)
      
      (override
        set-scroll-pos
        on-event
        on-paint
        on-scroll
        on-size
        on-char)
      
      (field
       [can-refresh? #t]
       [offscreen-dc (new bitmap-dc% (bitmap (make-object bitmap% 1280 1024 #f)))]
       
       [char-width (inexact->exact (send offscreen-dc get-char-width))]
       [cell-width (* chars-per-cell char-width)]
       [cell-height (+ 2 (inexact->exact (send offscreen-dc get-char-height)))]
       
       [left-margin (* 5 char-width)]
       [top-margin cell-height]
       
       [canvas-width-rcvr (event-receiver)]
       [canvas-height-rcvr (event-receiver)]
       [h-scroll-rcvr (event-receiver)]
       [v-scroll-rcvr (event-receiver)]
       [mouse-x-rcvr (event-receiver)]
       [mouse-y-rcvr (event-receiver)]
       [left-clicks (event-receiver)]
       [left-releases (event-receiver)]
       [key-events (event-receiver)]
       
       [canvas-width~ (hold canvas-width-rcvr)]
       [canvas-height~ (hold canvas-height-rcvr)]
       
       [mouse-x~ (hold mouse-x-rcvr 0)]
       [mouse-y~ (hold mouse-y-rcvr 0)]
       
       [left-button-down~ (hold (merge-e (left-clicks   . -=> . #t)
                                         (left-releases . -=> . #f))
                                #f)]
       
       [h-chars-per-page~ (quotient (- canvas-width~ left-margin) char-width)]
       [v-cells-per-page~ (quotient (- canvas-height~ top-margin) cell-height)]
       [h-scroll-range~ (max 0 (- (* cols chars-per-cell) h-chars-per-page~))]
       [v-scroll-range~ (max 0 (- rows v-cells-per-page~))]
       
       [h-scroll-pos~ (hold h-scroll-rcvr 0)]
       [v-scroll-pos~ (hold v-scroll-rcvr 0)]
       [h-scroll-cells~ (quotient h-scroll-pos~ chars-per-cell)]
       [h-scroll-offset~ (* char-width (remainder h-scroll-pos~ chars-per-cell))]
       [v-scroll-cells~ v-scroll-pos~]
       
       [mouse-row~ (y->row mouse-y~)]
       [mouse-col~ (x->col mouse-x~)]
       
       [first-vis-row~ (y->row (add1 top-margin))]
       [last-vis-row~ (y->row (sub1 canvas-height~))]
       [first-vis-col~ (x->col (add1 left-margin))]
       [last-vis-col~ (x->col (sub1 canvas-width~))]
       
       [start-sel-row~
        (accum-b
         (merge-e
          (left-clicks . -=> . (lambda (_) (value-now mouse-row~)))
          (key-events  . ==> . (lambda (key)
                                 (lambda (prev)
                                   (case (send key get-key-code)
                                     [(up) (max 0 (sub1 prev))]
                                     [(down) (min (sub1 rows) (add1 prev))]
                                     [else prev])))))
         0)]
       [start-sel-col~
        (accum-b
         (merge-e
          (left-clicks . -=> . (lambda (_) (value-now mouse-col~)))
          (key-events  . ==> . (lambda (key)
                                 (lambda (prev)
                                   (case (send key get-key-code)
                                     [(left) (max 0 (sub1 prev))]
                                     [(right) (min (sub1 cols) (add1 prev))]
                                     [else prev])))))
         0)]
       
       [cur-sel-row~
        (hold (merge-e
               (changes start-sel-row~)
               ((changes start-sel-col~) . -=> . (value-now start-sel-row~))
               ((changes mouse-row~) . =#> . (lambda (_)
                                               left-button-down~))) 0)]
       [cur-sel-col~
        (hold (merge-e
               (changes start-sel-col~)
               ((changes start-sel-row~) . -=> . (value-now start-sel-col~))
               ((changes mouse-col~) . =#> . (lambda (_)
                                               left-button-down~))) 0)]
       
       [scrollbar-updater
        (list
         (lift-strict (lambda (pg) (set-scroll-page 'horizontal (clip 1 (- pg chars-per-cell -1) 10000))) h-chars-per-page~)
         (lift-strict (lambda (pg) (set-scroll-page 'vertical (clip 1 (sub1 pg) 10000))) v-cells-per-page~)
         (lift-strict (lambda (rng) (set-scroll-range 'horizontal (clip 1 rng 10000))) h-scroll-range~)
         (lift-strict (lambda (rng) (set-scroll-range 'vertical (clip 1 rng 10000))) v-scroll-range~))]
       
       [scroller ((merge-e (changes h-scroll-pos~)
                           (changes v-scroll-pos~)) . -=> . (refresh))]
       
       [v-auto-scroller (merge-e
                         ((while-e (and left-button-down~
                                        (>= cur-sel-row~ last-vis-row~)
                                        (< cur-sel-row~ (sub1 rows))
                                        (not (= cur-sel-row~ start-sel-row~))) 50)
                          . -=> . (set-scroll-pos 'vertical (add1 (value-now v-scroll-pos~))))
                         ((while-e (and left-button-down~
                                        (<= cur-sel-row~ first-vis-row~)
                                        (> cur-sel-row~ 0)
                                        (not (= cur-sel-row~ start-sel-row~))) 50)
                          . -=> . (set-scroll-pos 'vertical (sub1 (value-now v-scroll-pos~))))
                         (key-events
                          . ==> .
                          (lambda (ev)
                            (case (send ev get-key-code)
                              [(prior) (set-scroll-pos 'vertical (max 0 (- (value-now v-scroll-pos~) (value-now v-cells-per-page~))))]
                              [(next) (set-scroll-pos 'vertical (min (value-now v-scroll-range~)
                                                                     (+ (value-now v-scroll-pos~) (value-now v-cells-per-page~))))]))))]
       
       [h-auto-scroller (merge-e
                         ((while-e (and left-button-down~
                                        (>= cur-sel-col~ last-vis-col~)
                                        (< h-scroll-pos~ h-scroll-range~)) 50)
                          . -=> . (set-scroll-pos 'horizontal (+ 3 (value-now h-scroll-pos~))))
                         ((while-e (and left-button-down~
                                        (<= cur-sel-col~ first-vis-col~)
                                        (> h-scroll-pos~ 0)) 50)
                          . -=> . (set-scroll-pos 'horizontal (+ -3 (value-now h-scroll-pos~)))))]
       
       [highlighter (merge-e
                     ((history-e 2 (list mouse-row~ mouse-col~))
                      . ==> .
                      (lambda (lst)
                        (for-each
                         (lambda (p)
                           (draw-cell (first p) (second p)))
                         lst)))
                     ((history-e 2 (list start-sel-row~ start-sel-col~ cur-sel-row~ cur-sel-col~))
                      . ==> .
                      (match-lambda
                        [((r01 c01 rf1 cf1) (r00 c00 rf0 cf0))
                         (cond
                           [(and (= r01 rf1) (= c01 cf1))
                            ; fresh selection: clear old selection, redraw new cell
                            (draw-cell-block r00 rf0 c00 cf0)
                            (draw-cell r01 c01)]
                           [else
                            ; extended selection, so r00 = r01 and c00 = c01
                            (draw-cell-block rf0 rf1 (min c00 cf0 cf1) (max c00 cf0 cf1))
                            (draw-cell-block (min r00 rf0 rf1) (max r00 rf0 rf1) cf0 cf1)
                            (draw-cell-block rf0 rf1 cf0 cf1)])])))]
       
       [focuser ((key-events . =#> . (lambda (ev) (eq? #\return (send ev get-key-code))))
                 . -=> . (send text-field focus))]
       
       [text-field-switcher (lift-strict (lambda (row col)
                                       (unless (or (negative? row)
                                                   (negative? col))
                                         (send text-field set-value (ss-get-cell-text row col))))
                                  start-sel-row~ start-sel-col~)]
       
       [light-steel-blue (make-object color% "LightSteelBlue")]
       [lavender (make-object color% "Lavender")]
       [white (make-object color% "White")]
       [line-pen (make-object pen% (make-object color% "DimGray") 1 'solid)]
       [light-gray (make-object color% "LightGray")]
       [trans-pen (make-object pen%)]
       [default-font (send offscreen-dc get-font)]
       [label-font (make-object font% 11 'roman 'normal 'bold)]
       [gray-brush (make-object brush% light-gray 'solid)]
       [highlight-brush (make-object brush% lavender 'solid)]
       [selected-brush (make-object brush% light-steel-blue 'solid)]
       [clear-brush (make-object brush% white 'solid)])
      
      (send trans-pen set-style 'transparent)
      
      (define (set-scroll-pos which pos)
        (super set-scroll-pos which pos)
        (send-event
         (case which
           [(horizontal) h-scroll-rcvr]
           [(vertical) v-scroll-rcvr]) pos))
      
      (define/private (x->col x)
        (if (> x left-margin)
            (+ h-scroll-cells~ (quotient (+ (- x left-margin) h-scroll-offset~) cell-width))
            -1))
      
      (define/private (y->row y)
        (if (> y top-margin)
            (+ v-scroll-cells~ (quotient (- y top-margin) cell-height))
            -1))
      
      (define/private (row->y-top row)
        (snapshot/sync (v-scroll-cells~)
                       (+ (* cell-height (- row v-scroll-cells~))
                          top-margin)))
      
      (define/private (col->x-left col)
        (snapshot/sync (h-scroll-cells~ h-scroll-offset~)
                       (+ (* (- col h-scroll-cells~) cell-width)
                          (- h-scroll-offset~)
                          left-margin)))
      
      #;(define foo (lift #t printf "~a ~a ~a ~a~n" cur-sel-row~ cur-sel-col~ start-sel-row~ start-sel-col~))
      
      (define/public (draw-cell-block r0 rf c0 cf)
        (let ([r0 (min r0 rf)]
              [rf (max r0 rf)]
              [c0 (min c0 cf)]
              [cf (max c0 cf)])
          (for (i = r0) (i . <= . rf) add1
               (for (j = c0) (j . <= . cf) add1
                    (draw-cell-offscreen i j)))
          (let ([x0 (col->x-left c0)]
                [y0 (row->y-top r0)]
                [xf (col->x-left (add1 cf))]
                [yf (row->y-top (add1 rf))])
            (send (get-dc)
                  draw-bitmap-section (send offscreen-dc get-bitmap)
                  x0 y0 x0 y0 (- xf x0) (- yf y0)))))
      
      (define/public (draw-cell-block-offscreen r0 rf c0 cf)
        (let ([r0 (min r0 rf)]
              [rf (max r0 rf)]
              [c0 (min c0 cf)]
              [cf (max c0 cf)])
          (for (i = r0) (i . <= . rf) add1
               (for (j = c0) (j . <= . cf) add1
                    (draw-cell-offscreen i j)))))
      
      (define/public (new-expression text)
        (snapshot/sync (cur-sel-row~ cur-sel-col~ start-sel-row~ start-sel-col~)
                       (let ([r0 (min cur-sel-row~ start-sel-row~)]
                             [r1 (max cur-sel-row~ start-sel-row~)]
                             [c0 (min cur-sel-col~ start-sel-col~)]
                             [c1 (max cur-sel-col~ start-sel-col~)]
                             [processed-expr (text->processed-expr text start-sel-row~ start-sel-col~)])
                         (for (row = r0) (row . <= . r1) add1
                              (for (col = c0) (col . <= . c1) add1
                                   (ss-set-cell-processed-expr! row col processed-expr))))
                       (send canvas focus)))
      
      (define (draw-cell-offscreen row col)
        (snapshot/sync (first-vis-row~
                        last-vis-row~
                        first-vis-col~ last-vis-col~
                        mouse-row~ mouse-col~
                        start-sel-row~ start-sel-col~
                        cur-sel-row~ cur-sel-col~)
                       (let ([x (col->x-left col)]
                             [y (row->y-top row)])
                         (when (and (< -1 row rows)
                                    (< -1 col cols))
                           (let ([text (ss-format (ss-get-cell-value row col))])
                             (when (and (= row start-sel-row~)
                                        (= col start-sel-col~))
                               (send value-field set-value text))
                             (when (and (<= first-vis-row~ row last-vis-row~)
                                        (<= first-vis-col~ col last-vis-col~))
                               (send offscreen-dc set-clipping-rect
                                     (max x (+ left-margin 1)) y cell-width cell-height)
                               (send offscreen-dc set-brush
                                     (cond
                                       [(and (between start-sel-row~ row cur-sel-row~)
                                             (between start-sel-col~ col cur-sel-col~)) selected-brush]
                                       [(and (= row mouse-row~)
                                             (= col mouse-col~)) highlight-brush]
                                       [else clear-brush]))
                               (send offscreen-dc draw-rectangle x y (+ cell-width 1) (+ cell-height 1))
                               (send offscreen-dc draw-text text
                                     (- (+ x cell-width) 2
                                        (let-values ([(width height descent space)
                                                      (send offscreen-dc get-text-extent text #f #f 0)])
                                          width))
                                     (+ y 1) #f 0 0)
                               (send offscreen-dc set-clipping-region #f)))))))
      
      (define/public (draw-cell row col)
        (draw-cell-offscreen row col)
        (let ([x (col->x-left col)]
              [y (row->y-top row)])
          (send (get-dc)
                draw-bitmap-section (send offscreen-dc get-bitmap)
                x y x y cell-width cell-height)))
      
      (define (get-text-width dc text)
        (let-values ([(width height descent space)
                      (send dc get-text-extent text #f #f 0)])
          width))
      
      (define (num->char n)
        (integer->char (+ n (char->integer #\A))))
      
      (define (column->string col)
        (list->string
         (if (< col 26)
             (list (num->char col))
             (list (num->char (sub1 (quotient col 26)))
                   (num->char (remainder col 26))))))
      
      (define (on-char event)
        (send-event key-events event)
        (synchronize))
      
      (define (on-scroll scroll-event)
        (case (send scroll-event get-direction)
          [(vertical) (send-event v-scroll-rcvr (send scroll-event get-position))]
          [(horizontal) (send-event h-scroll-rcvr (send scroll-event get-position))])
        (synchronize))
      
      (define (on-event event)
        (case (send event get-event-type)
          [(enter motion)
           (send-event mouse-x-rcvr (send event get-x))
           (send-event mouse-y-rcvr (send event get-y))]
          [(leave)
           (send-event mouse-x-rcvr -1)
           (send-event mouse-y-rcvr -1)]
          [(left-down) (send-event left-clicks #t)]
          [(left-up) (send-event left-releases #t)])
        (synchronize))
      
      (define (on-size width height)
        (let-values ([(width height) (get-client-size)])
          (send-event canvas-width-rcvr width)
          (send-event canvas-height-rcvr height)
          (synchronize)))
      
      (define (on-paint)
        (snapshot/sync (canvas-width~
                        canvas-height~
                        first-vis-row~ last-vis-row~
                        first-vis-col~ last-vis-col~
                        h-scroll-cells~ h-scroll-offset~ v-scroll-cells~)
                       (let ([dc offscreen-dc])
                         (send dc set-clipping-region #f)
                         (send dc clear)
                         ;(send dc set-pen line-pen)
                         ;(send dc set-brush highlight-brush)
                         (send dc set-pen trans-pen)
                         (send dc set-brush gray-brush)
                         (send dc draw-rectangle 0 0 left-margin canvas-height~)
                         (send dc draw-rectangle 0 0 canvas-width~ top-margin)
                         (send dc set-pen line-pen)
                         (send dc draw-line 0 0 0 canvas-height~)
                         (send dc draw-line 0 0 canvas-width~ 0)
                         (send dc set-brush clear-brush)
                         (send dc set-font label-font)
                         ;; draw horizontal rules and row labels
                         (for (row = first-vis-row~) (row . <= . (min last-vis-row~ (sub1 rows))) add1
                              (let ([y (row->y-top row)]
                                    [text (number->string row)])
                                (send dc draw-line 0 y canvas-width~ y)
                                (send dc draw-text text (- left-margin (get-text-width dc text) 2) (add1 y) #f 0 0)))
                         ;; draw vertical rules and column labels
                         (send dc draw-line left-margin 0 left-margin canvas-height~)
                         (send dc set-clipping-rect (+ left-margin 1) 0 (- canvas-width~ left-margin 1) canvas-height~)
                         (for (col = first-vis-col~) (col . <= . (min last-vis-col~ (sub1 cols))) add1
                              (let ([x (col->x-left col)]
                                    [text (column->string col)])
                                (send dc draw-text text (+ x (quotient (- cell-width (get-text-width dc text)) 2)) 0 #f 0 0)
                                (send dc draw-line x 0 x canvas-height~)))
                         (send dc set-font default-font)
                         (draw-cell-block-offscreen first-vis-row~ last-vis-row~ first-vis-col~ last-vis-col~)
                         (send (get-dc) draw-bitmap-section (send dc get-bitmap) 0 0 0 0 canvas-width~ canvas-height~))))
      
      (let-values ([(width height) (get-client-size)])
        (send-event canvas-width-rcvr width)
        (send-event canvas-height-rcvr height))
      (synchronize)
      (init-manual-scrollbars 1 1 1 1 0 0)
      (send offscreen-dc set-pen line-pen)
      (send offscreen-dc set-brush highlight-brush)))
  
  (define canvas
    (instantiate ss-canvas% (frame) (style (list 'hscroll 'vscroll))))
  
  (send frame show #t)
  (send canvas focus))
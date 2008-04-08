
#lang scheme/base
(require scheme/class
         scheme/gui
         scheme/match
         "params.ss"
         "pretty-printer.ss"
         "interfaces.ss"
         "util.ss")
(provide print-syntax-to-editor
         code-style)

;; print-syntax-to-editor : syntax text controller<%> -> display<%>
(define (print-syntax-to-editor stx text controller)
  (new display% (syntax stx) (text text) (controller controller)))

;; FIXME: assumes text never moves

;; display%
(define display%
  (class* object% (display<%>)
    (init ((stx syntax)))
    (init-field text)
    (init-field controller)

    (define start-anchor (new anchor-snip%))
    (define end-anchor (new anchor-snip%))
    (define range #f)
    (define extra-styles (make-hasheq))

    ;; render-syntax : syntax -> void
    (define/public (render-syntax stx)
      (with-unlock text
        (send text delete (get-start-position) (get-end-position))
        (set! range
              (print-syntax stx text controller
                            (lambda () (get-start-position))
                            (lambda () (get-end-position))))
        (apply-primary-partition-styles))
      (refresh))

    ;; refresh : -> void
    ;; Clears all highlighting and reapplies all non-foreground styles.
    (define/public (refresh)
      (with-unlock text
        (send* text 
          (begin-edit-sequence)
          (change-style unhighlight-d (get-start-position) (get-end-position)))
        (apply-extra-styles)
        (let ([selected-syntax (send controller get-selected-syntax)])
          (apply-secondary-partition-styles selected-syntax)
          (apply-selection-styles selected-syntax))
        (send* text
          (end-edit-sequence))))

    ;; cached-start-position : number
    (define cached-start-position #f)

    ;; get-start-position : -> number
    (define/public-final (get-start-position)
      (unless cached-start-position
        (set! cached-start-position (send text get-snip-position start-anchor)))
      cached-start-position)

    ;; get-end-position : -> number
    (define/public-final (get-end-position)
      (send text get-snip-position end-anchor))

    ;; relative->text-position : number -> number
    ;; FIXME: might be slow to find start every time!
    (define/public-final (relative->text-position pos)
      (+ pos (get-start-position)))

    ;; Styling

    ;; get-range : -> range<%>
    (define/public (get-range) range)
    
    ;; highlight-syntaxes : (list-of syntax) string -> void
    (define/public (highlight-syntaxes stxs hi-color)
      (let ([style-delta (highlight-style-delta hi-color #f)])
        (for-each (lambda (stx) (hash-set! extra-styles stx style-delta))
                  stxs))
      (refresh))

    ;; apply-extra-styles : -> void
    ;; Applies externally-added styles (such as highlighting)
    (define/private (apply-extra-styles)
      (hash-for-each
       extra-styles
       (lambda (hi-stx style-delta)
         (let ([rs (send range get-ranges hi-stx)])
           (for-each (lambda (r) (restyle-range r style-delta)) rs)))))

    ;; apply-secondary-partition-styles : selected-syntax -> void
    ;; If the selected syntax is an identifier, then styles all identifiers
    ;; in the same partition in blue.
    (define/private (apply-secondary-partition-styles selected-syntax)
      (when (identifier? selected-syntax)
        (let ([partition (send controller get-secondary-partition)])
          (when partition
            (for-each (lambda (id)
                        (when (send partition same-partition? selected-syntax id)
                          (draw-secondary-connection id)))
                      (send range get-identifier-list))))))

    ;; apply-selection-styles : syntax -> void
    ;; Styles subterms eq to the selected syntax
    (define/private (apply-selection-styles selected-syntax)
      (let ([rs (send range get-ranges selected-syntax)])
        (for-each (lambda (r) (restyle-range r select-highlight-d)) rs)))

    ;; draw-secondary-connection : syntax -> void
    (define/private (draw-secondary-connection stx2)
      (let ([rs (send range get-ranges stx2)])
        (for-each (lambda (r) (restyle-range r select-sub-highlight-d)) rs)))

    ;; restyle-range : (cons num num) style-delta% -> void
    (define/private (restyle-range r style)
      (send text change-style style
            (relative->text-position (car r))
            (relative->text-position (cdr r))))

    ;; Primary styles

    ;; apply-primary-partition-styles : -> void
    ;; Changes the foreground color according to the primary partition.
    ;; Only called once, when the syntax is first drawn.
    (define/private (apply-primary-partition-styles)
      (define (color-style color)
        (let ([delta (new style-delta%)])
          (send delta set-delta-foreground color)
          delta))
      (define color-styles (list->vector (map color-style (current-colors))))
      (define overflow-style (color-style "darkgray"))
      (define color-partition (send controller get-primary-partition))
      (define offset (get-start-position))
      (for-each
       (lambda (range)
         (let ([stx (range-obj range)]
               [start (range-start range)]
               [end (range-end range)])
           (send text change-style
                 (primary-style stx color-partition color-styles overflow-style)
                 (+ offset start)
                 (+ offset end))))
       (send range all-ranges)))

    ;; primary-style : syntax partition (vector-of style-delta%) style-delta%
    ;;               -> style-delta%
    (define/private (primary-style stx partition color-vector overflow)
      (let ([n (send partition get-partition stx)])
        (cond [(< n (vector-length color-vector))
               (vector-ref color-vector n)]
              [else
               overflow])))

    ;; Initialize
    (super-new)
    (send text insert start-anchor)
    (send text insert end-anchor)
    (render-syntax stx)
    (send controller add-syntax-display this)))

;; print-syntax : syntax controller (-> number) (-> number)
;;                -> range%
(define (print-syntax stx text controller
                      get-start-position get-end-position)
  (define primary-partition (send controller get-primary-partition))
  (define real-output-port (make-text-port text get-end-position))
  (define output-port (open-output-string))

  (port-count-lines! output-port)
  (let ([range (pretty-print-syntax stx output-port primary-partition)])
    (write-string (get-output-string output-port) real-output-port)
    (let ([end (get-end-position)])
      ;; Pretty printer always inserts final newline; we remove it here.
      (send text delete (sub1 end) end))
    (let ([offset (get-start-position)])
      (fixup-parentheses text range offset)
      (for-each
       (lambda (range)
         (let* ([stx (range-obj range)]
                [start (range-start range)]
                [end (range-end range)])
           (send text set-clickback (+ offset start) (+ offset end)
                 (lambda (_1 _2 _3)
                   (send controller set-selected-syntax stx)))))
       (send range all-ranges)))
    ;; Set font to standard
    (send text change-style
          (code-style text)
          (get-start-position)
          (get-end-position))
    range))

;; fixup-parentheses : text range -> void
(define (fixup-parentheses text range offset)
  (define (fixup r)
    (let ([stx (range-obj r)]
          [start (+ offset (range-start r))]
          [end (+ offset (range-end r))])
      (when (and (syntax? stx) (pair? (syntax-e stx)))
        (case (syntax-property stx 'paren-shape)
          ((#\[)
           (replace start #\[)
           (replace (sub1 end) #\]))
          ((#\{) 
           (replace start #\{)
           (replace (sub1 end) #\}))))))
  (define (replace pos char)
    (send text insert char pos (add1 pos)))
  (for-each fixup (send range all-ranges)))

;; code-style : text<%> -> style<%>
(define (code-style text)
  (let* ([style-list (send text get-style-list)]
         [style (send style-list find-named-style "Standard")]
         [font-size (current-syntax-font-size)])
    (if font-size
        (send style-list find-or-create-style
              style
              (make-object style-delta% 'change-size font-size))
        style)))

;; anchor-snip%
(define anchor-snip%
  (class snip%
    (define/override (copy)
      (make-object string-snip% ""))
    (super-instantiate ())))

;; Styles

(define (highlight-style-delta color em?)
  (let ([sd (new style-delta%)])
    (unless em? (send sd set-delta-background color))
    (when em? (send sd set-weight-on 'bold))
    (unless em? (send sd set-underlined-off #t)
      (send sd set-weight-off 'bold))
    sd))

(define selection-color "yellow")
(define subselection-color "yellow")

(define select-highlight-d (highlight-style-delta selection-color #t))
(define select-sub-highlight-d (highlight-style-delta subselection-color #f))

(define unhighlight-d (highlight-style-delta "white" #f))


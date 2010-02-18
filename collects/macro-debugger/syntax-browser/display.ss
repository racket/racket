#lang scheme/base
(require scheme/class
         scheme/gui
         scheme/list
         (rename-in unstable/class-iop
                    [send/i send:]
                    [init-field/i init-field:])
         (only-in mzlib/etc begin-with-definitions)
         "pretty-printer.ss"
         "interfaces.ss"
         "util.ss")
(provide print-syntax-to-editor
         code-style)

(define TIME-PRINTING? #f)

(define-syntax-rule (now)
  (if TIME-PRINTING?
      (current-inexact-milliseconds)
      0))

(define eprintf
  (if TIME-PRINTING?
      (let ([eport (current-error-port)])
        (lambda (fmt . args) (apply fprintf eport fmt args)))
      void))

;; FIXME: assumes text never moves

;; print-syntax-to-editor : syntax text controller<%> config number number
;;                       -> display<%>
(define (print-syntax-to-editor stx text controller config columns insertion-point)
  (begin-with-definitions
   (define output-port (open-output-string/count-lines))
   (define range
     (pretty-print-syntax stx output-port 
                          (send: controller controller<%> get-primary-partition)
                          (length (send: config config<%> get-colors))
                          (send: config config<%> get-suffix-option)
                          (send config get-pretty-styles)
                          columns))
   (define output-string (get-output-string output-port))
   (define output-length (sub1 (string-length output-string))) ;; skip final newline
   (fixup-parentheses output-string range)
   (send text begin-edit-sequence #f)
   (send text insert output-length output-string insertion-point)
   (define display
     (new display%
          (text text)
          (controller controller)
          (config config)
          (range range)
          (start-position insertion-point)
          (end-position (+ insertion-point output-length))))
   (send display initialize)
   (send text end-edit-sequence)
   display))

;; display%
(define display%
  (class* object% (display<%>)
    (init-field: [controller controller<%>]
                 [config config<%>]
                 [range range<%>])
    (init-field text
                start-position
                end-position)

    (define base-style
      (code-style text (send: config config<%> get-syntax-font-size)))

    (define extra-styles (make-hasheq))

    ;; initialize : -> void
    (define/public (initialize)
      (send text change-style base-style start-position end-position #f)
      (apply-primary-partition-styles)
      (add-clickbacks)
      (refresh))

    ;; add-clickbacks : -> void
    (define/private (add-clickbacks)
      (define (the-clickback editor start end)
        (send: controller selection-manager<%> set-selected-syntax
               (clickback->stx
                (- start start-position) (- end start-position))))
      (for ([range (send: range range<%> all-ranges)])
        (let ([stx (range-obj range)]
              [start (range-start range)]
              [end (range-end range)])
          (send text set-clickback (+ start-position start) (+ start-position end)
                the-clickback))))

    ;; clickback->stx : num num -> syntax
    ;; FIXME: use vectors for treerange-subs and do binary search to narrow?
    (define/private (clickback->stx start end)
      (let ([treeranges (send: range range<%> get-treeranges)])
        (let loop* ([treeranges treeranges])
          (for/or ([tr treeranges])
            (cond [(and (= (treerange-start tr) start)
                        (= (treerange-end tr) end))
                   (treerange-obj tr)]
                  [(and (<= (treerange-start tr) start)
                        (<= end (treerange-end tr)))
                   (loop* (treerange-subs tr))]
                  [else #f])))))

    ;; refresh : -> void
    ;; Clears all highlighting and reapplies all non-foreground styles.
    (define/public (refresh)
      (with-unlock text
        (send* text 
          (begin-edit-sequence #f)
          (change-style unhighlight-d start-position end-position))
        (apply-extra-styles)
        (let ([selected-syntax
               (send: controller selection-manager<%>
                      get-selected-syntax)])
          (apply-secondary-partition-styles selected-syntax)
          (apply-selection-styles selected-syntax))
        (send* text
          (end-edit-sequence))))

    ;; get-range : -> range<%>
    (define/public (get-range) range)

    ;; get-start-position : -> number
    (define/public (get-start-position) start-position)

    ;; get-end-position : -> number
    (define/public (get-end-position) end-position)

    ;; highlight-syntaxes : (list-of syntax) string -> void
    (define/public (highlight-syntaxes stxs hi-color)
      (let ([style-delta (highlight-style-delta hi-color #f)])
        (for ([stx stxs])
          (add-extra-styles stx (list style-delta))))
      (refresh))

    ;; underline-syntaxes : (listof syntax) -> void
    (define/public (underline-syntaxes stxs)
      (for ([stx stxs])
        (add-extra-styles stx (list underline-style-delta)))
      (refresh))

    ;; add-extra-styles : syntax (listof style) -> void
    (define/public (add-extra-styles stx styles)
      (hash-set! extra-styles stx
                 (append (hash-ref extra-styles stx null)
                         styles)))

    ;; Primary styles
    ;; (Done once on initialization, never repeated)

    ;; apply-primary-partition-styles : -> void
    ;; Changes the foreground color according to the primary partition.
    ;; Only called once, when the syntax is first drawn.
    (define/private (apply-primary-partition-styles)
      (define style-list (send text get-style-list))
      (define (color-style color)
        (let ([delta (new style-delta%)])
          (send delta set-delta-foreground color)
          (send style-list find-or-create-style base-style delta)))
      (define color-styles
        (list->vector (map color-style (send: config config<%> get-colors))))
      (define overflow-style (color-style "darkgray"))
      (define color-partition
        (send: controller mark-manager<%> get-primary-partition))
      (define offset start-position)
      ;; Optimization: don't call change-style when new style = old style
      (let tr*loop ([trs (send: range range<%> get-treeranges)] [old-style #f])
        (for ([tr trs])
          (define stx (treerange-obj tr))
          (define start (treerange-start tr))
          (define end (treerange-end tr))
          (define subs (treerange-subs tr))
          (define new-style
            (primary-style stx color-partition color-styles overflow-style))
          (unless (eq? old-style new-style)
            (send text change-style new-style (+ offset start) (+ offset end) #f))
          (tr*loop subs new-style)))
      (void))

    ;; primary-style : syntax partition (vector-of style-delta%) style-delta%
    ;;               -> style-delta%
    (define/private (primary-style stx partition color-vector overflow)
      (let ([n (send: partition partition<%> get-partition stx)])
        (cond [(< n (vector-length color-vector))
               (vector-ref color-vector n)]
              [else
               overflow])))

    ;; Secondary Styling
    ;; May change in response to user actions

    ;; apply-extra-styles : -> void
    ;; Applies externally-added styles (such as highlighting)
    (define/private (apply-extra-styles)
      (for ([(stx style-deltas) extra-styles])
        (for ([r (send: range range<%> get-ranges stx)])
          (for ([style-delta style-deltas])
            (restyle-range r style-delta)))))

    ;; apply-secondary-partition-styles : selected-syntax -> void
    ;; If the selected syntax is an identifier, then styles all identifiers
    ;; in the same partition in blue.
    (define/private (apply-secondary-partition-styles selected-syntax)
      (when (identifier? selected-syntax)
        (let ([partition
               (send: controller secondary-partition<%>
                      get-secondary-partition)])
          (when partition
            (for ([id (send: range range<%> get-identifier-list)])
              (when (send: partition partition<%>
                           same-partition? selected-syntax id)
                (draw-secondary-connection id)))))))

    ;; apply-selection-styles : syntax -> void
    ;; Styles subterms eq to the selected syntax
    (define/private (apply-selection-styles selected-syntax)
      (for ([r (send: range range<%> get-ranges selected-syntax)])
        (restyle-range r select-highlight-d)))

    ;; draw-secondary-connection : syntax -> void
    (define/private (draw-secondary-connection stx2)
      (for ([r (send: range range<%> get-ranges stx2)])
        (restyle-range r select-sub-highlight-d)))

    ;; restyle-range : (cons num num) style-delta% -> void
    (define/private (restyle-range r style)
      (send text change-style style
            (relative->text-position (car r))
            (relative->text-position (cdr r))))

    ;; relative->text-position : number -> number
    (define/private (relative->text-position pos)
      (+ pos start-position))

    ;; Initialize
    (super-new)
    (send: controller controller<%> add-syntax-display this)))

;; fixup-parentheses : string range -> void
(define (fixup-parentheses string range)
  (for ([r (send: range range<%> all-ranges)])
    (let ([stx (range-obj r)]
          [start (range-start r)]
          [end (range-end r)])
      (when (and (syntax? stx) (pair? (syntax-e stx)))
        (case (syntax-property stx 'paren-shape)
          ((#\[)
           (string-set! string start #\[)
           (string-set! string (sub1 end) #\]))
          ((#\{) 
           (string-set! string start #\{)
           (string-set! string (sub1 end) #\})))))))

(define (open-output-string/count-lines)
  (let ([os (open-output-string)])
    (port-count-lines! os)
    os))

;; code-style : text<%> number/#f -> style<%>
(define (code-style text font-size)
  (let* ([style-list (send text get-style-list)]
         [style (send style-list find-named-style "Standard")])
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

(define underline-style-delta
  (let ([sd (new style-delta%)])
    (send sd set-underlined-on #t)
    sd))

(define selection-color "yellow")
(define subselection-color "yellow")

(define select-highlight-d (highlight-style-delta selection-color #t))
(define select-sub-highlight-d (highlight-style-delta subselection-color #f))

(define unhighlight-d (highlight-style-delta "white" #f))

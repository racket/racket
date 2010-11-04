#lang racket/base
(require racket/class
         racket/gui/base
         racket/list
         racket/pretty
         framework
         unstable/class-iop
         "pretty-printer.rkt"
         "interfaces.rkt"
         "prefs.rkt"
         "util.rkt")
(provide print-syntax-to-editor
         code-style)

(define-syntax-rule (uninterruptible e ...)
  ;; (coarsely) prevent breaks within editor operations
  (parameterize-break #f (begin e ...))
  #|
  (parameterize-break #f
    (let ([ta (now)])
      (begin0 (begin e ...)
        (let ([tb (now)])
          (eprintf "****\n")
          (pretty-write '(begin e ...) (current-error-port))
          (eprintf "  -- ~s ms\n\n" (- tb ta))))))
  |#)

(define (now) (current-inexact-milliseconds))

;; FIXME: assumes text never moves

;; print-syntax-to-editor : syntax text controller<%> config number number
;;                       -> display<%>
;; Note: must call display<%>::refresh to finish styling.
(define (print-syntax-to-editor stx text controller config columns
                                [insertion-point (send text last-position)])
  (define output-port (open-output-string/count-lines))
  (define range
    (pretty-print-syntax stx output-port 
                         (send/i controller controller<%> get-primary-partition)
                         (length (send/i config config<%> get-colors))
                         (send/i config config<%> get-suffix-option)
                         (send config get-pretty-styles)
                         columns))
  (define output-string (get-output-string output-port))
  (define output-length (sub1 (string-length output-string))) ;; skip final newline
  (fixup-parentheses output-string range)
  (with-unlock text
    (uninterruptible
     (send text insert output-length output-string insertion-point))
    (new display%
         (text text)
         (controller controller)
         (config config)
         (range range)
         (start-position insertion-point)
         (end-position (+ insertion-point output-length)))))

;; display%
;; Note: must call refresh method to finish styling.
(define display%
  (class* object% (display<%>)
    (init-field/i [controller controller<%>]
                  [config config<%>]
                  [range range<%>])
    (init-field text
                start-position
                end-position)

    (define base-style
      (code-style text (send/i config config<%> get-syntax-font-size)))

    (define extra-styles (make-hasheq))

    (define auto-refresh? #f) ;; FIXME: delete or make init arg

    ;; initialize : -> void
    (define/private (initialize)
      (uninterruptible
       (send text change-style base-style start-position end-position #f))
      (uninterruptible (apply-primary-partition-styles))
      (uninterruptible (add-clickbacks))
      (when auto-refresh? (refresh)))

    ;; add-clickbacks : -> void
    (define/private (add-clickbacks)
      (define (the-clickback editor start end)
        (send/i controller selection-manager<%> set-selected-syntax
               (clickback->stx
                (- start start-position) (- end start-position))))
      (for ([range (send/i range range<%> all-ranges)])
        (let ([stx (range-obj range)]
              [start (range-start range)]
              [end (range-end range)])
          (send text set-clickback (+ start-position start) (+ start-position end)
                the-clickback))))

    ;; clickback->stx : num num -> syntax
    ;; FIXME: use vectors for treerange-subs and do binary search to narrow?
    (define/private (clickback->stx start end)
      (let ([treeranges (send/i range range<%> get-treeranges)])
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
      (uninterruptible
       (with-unlock text
         (send text change-style (unhighlight-d) start-position end-position)
         (apply-extra-styles)
         (let ([selected-syntax
                (send/i controller selection-manager<%>
                        get-selected-syntax)])
           (apply-secondary-relation-styles selected-syntax)
           (apply-selection-styles selected-syntax)))))

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
      (when auto-refresh? (refresh)))

    ;; underline-syntaxes : (listof syntax) -> void
    (define/public (underline-syntaxes stxs)
      (for ([stx stxs])
        (add-extra-styles stx (list underline-style-delta)))
      (when auto-refresh? (refresh)))

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
        (list->vector
         (map color-style
              (map translate-color
                   (send/i config config<%> get-colors)))))
      (define overflow-style (color-style (translate-color "darkgray")))
      (define color-partition
        (send/i controller mark-manager<%> get-primary-partition))
      (define offset start-position)
      ;; Optimization: don't call change-style when new style = old style
      (let tr*loop ([trs (send/i range range<%> get-treeranges)] [old-style #f])
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
      (let ([n (send/i partition partition<%> get-partition stx)])
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
        (for ([r (send/i range range<%> get-ranges stx)])
          (for ([style-delta style-deltas])
            (restyle-range r style-delta)))))

    ;; apply-secondary-relation-styles : selected-syntax -> void
    ;; If the selected syntax is an identifier, then styles all identifiers
    ;; in the relation with it.
    (define/private (apply-secondary-relation-styles selected-syntax)
      (when (identifier? selected-syntax)
        (let* ([name+relation
                (send/i controller secondary-relation<%>
                        get-identifier=?)]
               [relation (and name+relation (cdr name+relation))])
          (when relation
            (for ([id (send/i range range<%> get-identifier-list)])
              (when (relation selected-syntax id)
                (draw-secondary-connection id)))))))

    ;; apply-selection-styles : syntax -> void
    ;; Styles subterms eq to the selected syntax
    (define/private (apply-selection-styles selected-syntax)
      (for ([r (send/i range range<%> get-ranges selected-syntax)])
        (restyle-range r (select-highlight-d))))

    ;; draw-secondary-connection : syntax -> void
    (define/private (draw-secondary-connection stx2)
      (for ([r (send/i range range<%> get-ranges stx2)])
        (restyle-range r (select-sub-highlight-d))))

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
    (send/i controller controller<%> add-syntax-display this)
    (initialize)))

;; fixup-parentheses : string range -> void
(define (fixup-parentheses string range)
  (for ([r (send/i range range<%> all-ranges)])
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
         [style (send style-list find-named-style (editor:get-default-color-style-name))])
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

;; Color translation

;; translate-color : color-string -> color%
(define (translate-color color-string)
  (let ([c (make-object color% color-string)])
    (if (pref:invert-colors?)
        (let-values ([(r* g* b*)
                      (lightness-invert (send c red) (send c green) (send c blue))])
          #|
          (printf "translate: ~s -> ~s\n"
                  (list (send c red) (send c green) (send c blue))
                  (list r* g* b*))
          |#
          (make-object color% r* g* b*))
        c)))

;; lightness-invert : uint8 uint8 uint8 -> (values uint8 uint8 uint8)
(define (lightness-invert r g b)
  (define (c x)
    (/ (exact->inexact x) 255.0))
  (define (d x)
    (inexact->exact (round (* x 255))))
  (let-values ([(r g b) (lightness-invert* (c r) (c g) (c b))])
    (values (d r) (d g) (d b))))

(define (lightness-invert* R G B)
  (let-values ([(Hp Sl L) (rgb->hsl* R G B)])
    (hsl*->rgb Hp Sl (- 1.0 L))))

(define (rgb->hsl* R G B)
  (define M (max R G B))
  (define m (min R G B))
  (define C (- M m))
  (define Hp
    (cond [(zero? C)
           ;; Undefined, but use 0
           0.0]
          [(= M R)
           (realmod* (/ (- G B) C) 6)]
          [(= M G)
           (+ (/ (- B R) C) 2)]
          [(= M B)
           (+ (/ (- R G) C) 4)]))
  (define L (* 0.5 (+ M m)))
  (define Sl
    (cond [(zero? C) 0.0]
          [(>= L 0.5) (/ C (* 2 L))]
          [else (/ C (- 2 (* 2 L)))]))
  
  (values Hp Sl L))

(define (hsl*->rgb Hp Sl L)
  (define C
    (cond [(>= L 0.5) (* 2 L Sl)]
          [else (* (- 2 (* 2 L)) Sl)]))
  (define X (* C (- 1 (abs (- (realmod Hp 2) 1)))))
  (define-values (R1 G1 B1)
    (cond [(< Hp 1) (values C X 0)]
          [(< Hp 2) (values X C 0)]
          [(< Hp 3) (values 0 C X)]
          [(< Hp 4) (values 0 X C)]
          [(< Hp 5) (values X 0 C)]
          [(< Hp 6) (values C 0 X)]))
  (define m (- L (* 0.5 C)))
  (values (+ R1 m) (+ G1 m) (+ B1 m)))

;; realmod : real integer -> real
;; Adjusts a real number to [0, base]
(define (realmod x base)
  (define xint (ceiling x))
  (define m (modulo xint base))
  (realmod* (- m (- xint x)) base))

;; realmod* : real real -> real
;; Adjusts a number in [-base, base] to [0,base]
;; Not a real mod, but faintly reminiscent.
(define (realmod* x base)
  (if (negative? x)
      (+ x base)
      x))

;; Styles

(define (highlight-style-delta raw-color em?
                               #:translate-color? [translate-color? #t])
  (let* ([sd (new style-delta%)])
    (unless em?
      (send sd set-delta-background
            (if translate-color? (translate-color raw-color) raw-color)))
    (when em? (send sd set-weight-on 'bold))
    (unless em?
      ;; (send sd set-underlined-off #t)
      (send sd set-weight-off 'bold))
    sd))

(define underline-style-delta
  (let ([sd (new style-delta%)])
    (send sd set-underlined-on #t)
    sd))

(define (mk-2-constant-style bow-color em? [wob-color (translate-color bow-color)])
  (let ([wob-version (highlight-style-delta wob-color em? #:translate-color? #f)]
        [bow-version (highlight-style-delta bow-color em? #:translate-color? #f)])
    (λ ()
      (if (pref:invert-colors?)
          wob-version
          bow-version))))

(define select-highlight-d
  (mk-2-constant-style "yellow" #t "darkgoldenrod"))
(define select-sub-highlight-d
  (mk-2-constant-style "yellow" #f "darkgoldenrod"))

(define unhighlight-d (mk-2-constant-style "white" #f #|"black"|#))

#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         framework
         unstable/class-iop
         macro-debugger/syntax-browser/interfaces
         "util.rkt"
         macro-debugger/util/mpi
         macro-debugger/util/stxobj)
(provide properties-view%
         properties-snip%)

(define color-text-default-style-name
  "macro-debugger/syntax-browser/properties color-text% basic")

(define color-text%
  (class (editor:standard-style-list-mixin text:basic%)
    (inherit get-style-list)
    (define/override (default-style-name)
      color-text-default-style-name)
    (super-new)
    (let* ([sl (get-style-list)]
           [standard
            (send sl find-named-style (editor:get-default-color-style-name))]
           [basic
            (send sl find-or-create-style standard
                  (make-object style-delta% 'change-family 'default))])
      (send sl new-named-style color-text-default-style-name basic))))

;; properties-view-base-mixin
(define properties-view-base-mixin
  (mixin () ()
    ;; controller : controller<%>
    (init-field controller)

    ;; selected-syntax : syntax
    (field (selected-syntax #f))

    ;; mode : maybe symbol in '(term stxobj)
    (define mode 'term)

    ;; text : text%
    (field (text (new color-text%)))
    (field (pdisplayer (new properties-displayer% (text text))))

    (send/i controller selection-manager<%> listen-selected-syntax
            (lambda (stx)
              (set! selected-syntax stx)
              (refresh)))
    (super-new)

    ;; get-mode : -> symbol
    (define/public (get-mode) mode)

    ;; set-mode : symbol -> void
    (define/public (set-mode m)
      (set! mode m)
      (refresh))

    ;; refresh : -> void
    (define/public (refresh)
      (with-unlock text
        (send text erase)
        (if (syntax? selected-syntax)
            (refresh/mode mode)
            (refresh/mode #f)))
      (send text scroll-to-position 0))

    ;; refresh/mode : symbol -> void
    (define/public (refresh/mode mode)
      (case mode
        ((term) (send pdisplayer display-meaning-info selected-syntax))
        ((stxobj) (send pdisplayer display-stxobj-info selected-syntax))
        ((#f) (send pdisplayer display-null-info))
        (else (error 'properties-view-base:refresh
                     "internal error: no such mode: ~s" mode))))

    (send text set-styles-sticky #f)
    #;(send text hide-caret #t)
    (send text lock #t)
    (refresh)))


;; properties-snip%
(define properties-snip%
  (class (properties-view-base-mixin editor-snip%)
    (inherit-field text)
    (inherit-field pdisplayer)
    (inherit set-mode)

    (define/private outer:insert
      (case-lambda
       [(obj)
        (outer:insert obj style:normal)]
       [(text style)
        (outer:insert text style #f)]
       [(text style clickback)
        (let ([start (send outer-text last-position)])
          (send outer-text insert text)
          (let ([end (send outer-text last-position)])
            (send outer-text change-style style start end #f)
            (when clickback
                  (send outer-text set-clickback start end clickback))))]))

    (define outer-text (new text%))
    (super-new (editor outer-text))
    (outer:insert "Term" style:hyper (lambda _ (set-mode 'term)))
    (outer:insert " ")
    (outer:insert "Syntax Object" style:hyper (lambda _ (set-mode 'stxobj)))
    (outer:insert "\n")
    (outer:insert (new editor-snip% (editor text)))
    (send outer-text hide-caret #t)
    (send outer-text lock #t)))

;; properties-view%
(define properties-view%
  (class* (properties-view-base-mixin object%) ()
    (init parent)
    (inherit-field text)
    (inherit-field pdisplayer)
    (inherit set-mode)

    ;; get-tab-choices : (listof (cons string thunk))
    ;; Override to add or remove panels
    (define/public (get-tab-choices)
      (list (cons "Term" 'term)
            (cons "Syntax Object" 'stxobj)))

    (super-new)
    (define tab-choices (get-tab-choices))
    (define tab-panel
      (new tab-panel% 
           (choices (map car tab-choices))
           (parent parent)
           (callback
            (lambda (tp e)
              (set-mode (cdr (list-ref tab-choices (send tp get-selection))))))))
    (define ecanvas (new canvas:color% (editor text) (parent tab-panel)))))

;; properties-displayer%
(define properties-displayer%
  (class* object% ()
    (init-field text)

    ;; display-null-info : -> void
    (define/public (display-null-info)
      (display "No syntax selected\n" n/a-sd))

    ;; display-meaning-info : syntax -> void
    (define/public (display-meaning-info stx)
      (when (and (identifier? stx)
                 (uninterned? (syntax-e stx)))
        (display "Uninterned symbol!\n\n" key-sd))
      (display-binding-info stx)
      (display-indirect-binding-info stx))

    ;; display-binding-info : syntax -> void
    (define/private (display-binding-info stx)
      (display "Apparent identifier binding\n" key-sd)
      (display-bindings stx))

    ;; display-indirect-binding-info : syntax -> void
    (define/private (display-indirect-binding-info stx)
      (cond
       [(identifier? stx)
        (display "Binding if used for #%top\n" key-sd)
        (display-bindings (datum->syntax stx '#%top))]
       [(and (syntax? stx) (pair? (syntax-e stx)))
        (display "Binding if used for #%app\n" key-sd)
        (display-bindings (datum->syntax stx '#%app))]
       [else
        (display "Binding if used for #%datum\n" key-sd)
        (display-bindings (datum->syntax stx '#%datum))]))

    ;; display-bindings : syntax -> void
    (define/private (display-bindings stx)
      (define phases-to-search '(0 1 -1 #f 2 3 4 5 -2 -3 -4 -5))
      (unless (identifier? stx)
        (display "Not applicable\n\n" n/a-sd))
      (when (identifier? stx)
        (cond [(eq? (identifier-binding stx) 'lexical)
               (display "lexical (all phases)\n" #f)]
              [else
               (let ([bindings (for/hash ([phase (in-list phases-to-search)])
                                 (values phase (identifier-binding stx phase)))])
                 (cond [(for/or ([(p b) (in-hash bindings)]) b)
                        (for ([phase (in-list phases-to-search)])
                          (display-binding-kvs phase (hash-ref bindings phase #f) stx))]
                       [else (display "none\n" #f)]))])
        (display "\n" #f)))

    ;; display-binding-kvs : phase bindinginfo identifier -> void
    (define/private (display-binding-kvs phase v stx)
      (when v
        (display (format "in phase ~a~a:"
                         phase
                         (case phase
                           ((1) " (transformer phase)")
                           ((-1) " (template phase)")
                           ((#f) " (label phase)")
                           (else "")))
                 sub-key-sd)
        (display "\n" #f)
        (match v
          [(list* def-mpi def-sym imp-mpi imp-sym defined-at-phase _)
           (display-subkv "  defined in" (mpi->string def-mpi))
           (unless (eq? def-sym (syntax-e stx))
             (display-subkv "    as" def-sym))
           (display-subkv "  imported from" (mpi->string imp-mpi))
           (unless (eq? imp-sym (syntax-e stx))
             (display-subkv "    provided as" (list-ref v 3)))
           (unless (zero? defined-at-phase)
             (display-subkv "  defined at phase" defined-at-phase))]
          [_ (void)])))

    ;; display-stxobj-info : syntax -> void
    (define/public (display-stxobj-info stx)
      (display-source-info stx)
      (display-extra-source-info stx)
      (display-symbol-property-info stx)
      (display-marks stx)
      ;; Disable until correct:
      (when #f (display-taint stx)))

    ;; display-source-info : syntax -> void
    (define/private (display-source-info stx)
      (define s-source (syntax-source stx))
      (define s-line (syntax-line stx))
      (define s-column (syntax-column stx))
      (define s-position (syntax-position stx))
      (define s-span (syntax-span stx))
      (define s-span-known? (not (memv s-span '(0 #f))))
      (display "Source location\n" key-sd)
      (if (or s-source s-line s-column s-position s-span-known?)
          (begin
            (display-subkv "source" (prettify-source s-source))
            (display-subkv "line" s-line)
            (display-subkv "column" s-column)
            (display-subkv "position" s-position)
            (display-subkv "span" s-span))
          (display "No source location available\n" n/a-sd))
      (display "\n" #f))

    ;; display-extra-source-info : syntax -> void
    (define/private (display-extra-source-info stx)
      (display "Built-in properties\n" key-sd)
      (display-subkv "source module"
                     (let ([mod (syntax-source-module stx)])
                       (and mod (mpi->string mod))))
      (display-subkv "original?" (syntax-original? stx))
      (display "\n" #f))

    ;; display-symbol-property-info : syntax -> void
    (define/private (display-symbol-property-info stx)
      (let ([keys (syntax-property-symbol-keys stx)])
        (display "Additional properties\n" key-sd)
        (when (null? keys)
          (display "No additional properties available.\n" n/a-sd))
        (when (pair? keys)
          (for-each (lambda (k) (display-subkv/value k (syntax-property stx k)))
                    keys))
        (display "\n" #f)))

    ;; display-marks : syntax -> void
    (define/private (display-marks stx)
      (display "Marks: " key-sd)
      (display (format "~s\n" (get-marks stx)) #f)
      (display "\n" #f))

    ;; display-taint : syntax -> void
    (define/private (display-taint stx)
      (define (syntax-armed? stx)
        (syntax-tainted? (datum->syntax stx 'dummy)))
      (display "Tamper status: " key-sd)
      (display (cond [(syntax-tainted? stx)
                      "tainted"]
                     [(syntax-armed? stx)
                      "armed"]
                     [else "clean"])
               #f))

    ;; display-kv : any any -> void
    (define/private (display-kv key value)
      (display (format "~a\n" key) key-sd)
      (display (format "~s\n\n" value) #f))

    ;; display-subkv : any any -> void
    (define/public (display-subkv k v)
      (display (format "~a: " k) sub-key-sd)
      (display (format "~a\n" v) #f))

    (define/public (display-subkv/value k v)
      (display-subkv k v)
      #;
      (begin
        (display (format "~a:\n" k) sub-key-sd)
        (let* ([value-text (new text:standard-style-list% (auto-wrap #t))]
               [value-snip (new editor-snip% (editor value-text))]
               [value-port (make-text-port value-text)])
          (set-interactive-write-handler value-port)
          (set-interactive-print-handler value-port)
          (set-interactive-display-handler value-port)
          (write v value-port)
          (send value-text lock #t)
          (send text insert value-snip)
          (send text insert "\n")
          #;(send ecanvas add-wide-snip value-snip))))

    ;; display : string style-delta -> void
    (define/private (display item sd)
      (let ([p0 (send text last-position)])
        (send text insert item)
        (let ([p1 (send text last-position)])
          (send text change-style sd p0 p1))))

    (super-new)))


;; lift/id : (identifier -> void) 'a -> void
(define (lift/id f)
  (lambda (stx) (when (identifier? stx) (f stx))))

(define (uninterned? s)
  (not (eq? s (string->symbol (symbol->string s)))))

(define (prettify-source s)
  (cond [(is-a? s editor<%>)
         'editor]
        [else s]))

;; Styles
  
(define key-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "blue")
    (send sd set-weight-on 'bold)
    sd))

(define sub-key-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "blue")
    sd))

(define n/a-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "gray")
    sd))

(define style:normal (make-object style-delta% 'change-normal))

(define style:hyper
  (let ([s (make-object style-delta% 'change-normal)])
    (send s set-delta 'change-toggle-underline)
    (send s set-delta-foreground "blue")
    s))

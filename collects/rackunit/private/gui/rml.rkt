#lang racket/base
(require racket/class
         racket/gui/base
         framework
         "interfaces.rkt")

(provide insert-text
         ext:text%
         rackunit-style-map)

;; insert-text : text% string style-delta% -> void
(define (insert-text e text style)
  (let ([a (send e last-position)])
    (send e insert text)
    (let ([b (send e last-position)])
      (send e change-style style a b))))

(define text<%> (class->interface text%))

(define ext:text-mixin
  (mixin (text<%>) ()
    (init-field (style-map rackunit-style-map))
    (inherit last-position
             change-style
             set-clickback
             insert
             get-canvas
             set-styles-sticky
             set-autowrap-bitmap)

    (super-new (auto-wrap #t))
    (set-styles-sticky #f)
    (set-autowrap-bitmap #f)

    ;; insert/styles : (list-of style-delta%) string ... -> void
    ;; A list of styles to be applied. The first style is the last applied.
    (define/public (insert/styles styles . texts)
      (unless (andmap (lambda (x) (or (string? x) (is-a? x snip%))) texts)
        (raise-type-error 'insert/styles "list of strings" texts))
      (let-values ([(a b) (put texts)])
        (for-each (lambda (style) (change-style (resolve style) a b))
                  (reverse styles))))

    ;; insert/styles+click : (list-of style-delta%) (?? -> void) string ...-> void
    (define/public (insert/styles+click styles clickback . texts)
      (unless (andmap (lambda (x) (or (string? x) (is-a? x snip%))) texts)
        (raise-type-error 'insert/styles+click "list of strings" texts))
      (let-values ([(a b) (put texts)])
        (for-each (lambda (style) (change-style (resolve style) a b))
                  (reverse styles))
        (set-clickback a b clickback)))

    ;; put : (list-of string) -> int int
    (define/private (put texts)
      (let ([a (last-position)])
        (let loop ([texts texts] [where a])
          (if (pair? texts)
              (begin (insert (car texts) where 'same #f)
                     (loop (cdr texts) (last-position)))
              (values a where)))))

    (define/private (resolve style)
      (if (symbol? style)
          (send style-map get-style style)
          style))

    ;; newline : -> void
    (define/public (newline)
      (insert/styles '() "\n"))

    ;; insert-wide-box : (ext:text<%> -> void) -> void
    (define/public (insert-wide-box p)
      (internal-insert-box p #t)
      (newline))

    ;; internal-insert-box : (ext:text<%> -> void) boolean? -> void
    (define/private (internal-insert-box p wide?)
      (let* ([seditor (new ext:text%)]
             [snip (new editor-snip% (editor seditor))])
        (p seditor)
        (let [(canvas (get-canvas))]
          (when (and (is-a? canvas canvas:wide-snip<%>) wide?)
            (send canvas add-wide-snip snip)))
        (insert snip)
        (send seditor lock #t)))

    ))

(define ext:text%
  (text:wide-snip-mixin
   (ext:text-mixin
    text:hide-caret/selection%)))

(define style:no-change (make-object style-delta% 'change-nothing))
(define style:normal (make-object style-delta% 'change-normal))
(define style:large (make-object style-delta% 'change-nothing))
(void (send style:large set-size-mult 1.5))

(define style:blue (make-object style-delta% 'change-nothing))
(void (send style:blue set-delta-foreground "Blue"))

(define style:red (make-object style-delta% 'change-nothing))
(void (send style:red set-delta-foreground "Red"))

(define style:green (make-object style-delta% 'change-nothing))
(void (send style:green set-delta-foreground "ForestGreen"))

(define style:purple (make-object style-delta% 'change-nothing))
(void (send style:purple set-delta-foreground "Purple"))

(define style:gray (make-object style-delta% 'change-nothing))
(void (send style:gray set-delta-foreground "DimGray"))

(define style:darkblue (make-object style-delta% 'change-nothing))
(void (send style:darkblue set-delta-foreground "DarkBlue"))

(define style:clickback (make-object style-delta% 'change-underline #t))
(void (send style:clickback set-delta-foreground "blue"))

(define style:bold (make-object style-delta% 'change-nothing))
(void (send style:bold set-delta 'change-weight 'bold))

(define style:italic (make-object style-delta% 'change-nothing))
(void (send style:italic set-delta 'change-style 'italic))

(define basic-styles
  `([no-change . ,style:no-change]
    [normal    . ,style:normal]
    [large     . ,style:large]
    [clickback . ,style:clickback]
    [red       . ,style:red]
    [blue      . ,style:blue]
    [green     . ,style:green]
    [purple    . ,style:purple]
    [darkblue  . ,style:darkblue]
    [bold      . ,style:bold]
    [italic    . ,style:italic]
    [error     . ,style:red]
    [value     . ,style:darkblue]))

(define rackunit-styles
  `([test-unexecuted . ,style:gray]
    [test-success    . ,style:green]
    [test-failure    . ,style:red]
    [test-error      . ,style:red]
    
    [exn-type        . ,style:darkblue]
    [exn-message     . ,style:red]
    [exn-value       . ,style:darkblue]
    [fail-type       . ,style:darkblue]))


;; -- style-map classes

(define extended-style-map%
  (class* object% (style-map<%>)
    (init-field styles
                base)
    (define/public (get-style sym)
      (cond [(assq sym styles) => cdr]
            [else (send base get-style sym)]))
    (super-new)))

(define empty-style-map%
  (class* object% (style-map<%>)
    (define/public (get-style sym)
      (error 'get-style "unknown style: ~s" sym))
    (super-new)))

;; extend-style-map : style-map<%> styles -> style-map<%>
(define (extend-style-map base styles)
  (new extended-style-map% (base base) (styles styles)))

;; empty-style-map : style-map<%>
(define empty-style-map
  (new empty-style-map%))

;; basic-style-map : style-map<%>
(define basic-style-map
  (extend-style-map empty-style-map
                    basic-styles))

;; rackunit-style-map : style-map<%>
(define rackunit-style-map
  (extend-style-map basic-style-map 
                    rackunit-styles))

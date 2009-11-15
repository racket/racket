#lang scheme/base
(require scheme/class
         (rename-in unstable/class-iop
                    [send/i send:])
         scheme/match
         scheme/list
         mzlib/string
         mred
         framework
         unstable/gui/notify
         "interfaces.ss"
         "display.ss"
         "controller.ss"
         "keymap.ss"
         "properties.ss"
         "partition.ss"
         "prefs.ss")

(provide syntax-snip%
         syntax-value-snip%)

(define syntax-snip-config-base%
  (class prefs-base%
    (notify-methods props-shown?)
    (super-new)))

(define syntax-snip-config%
  (class syntax-snip-config-base%
    (define/override (init-props-shown?) (new notify-box% (value #f)))
    (super-new)))

;; syntax-value-snip%
(define syntax-value-snip%
  (class* editor-snip% (readable-snip<%>)
    (init-field ((stx syntax)))
    (init-field (controller (new controller%)))
    (init-field (config (new syntax-snip-config%)))

    (inherit set-margin
             set-inset)

    (define text (new text:standard-style-list%))
    (super-new (editor text) (with-border? #f))

    (set-margin 0 0 0 0)
    ;;(set-inset 2 2 2 2)
    ;;(set-margin 2 2 2 2)
    (set-inset 0 0 0 0)

    (send text begin-edit-sequence)
    (send text change-style (make-object style-delta% 'change-alignment 'top))
    (define display
      (print-syntax-to-editor stx text controller config))
    (send text lock #t)
    (send text end-edit-sequence)
    (send text hide-caret #t)

    (setup-keymap text)

    (define/public (setup-keymap text)
      (new syntax-keymap%
           (controller controller)
           (config config)
           (editor text)))

    ;; snip% Methods
    (define/override (copy)
      (new syntax-value-snip%
           (config config)
           (controller controller)
           (syntax stx)))

    ;; read-special : any number/#f number/#f number/#f -> syntax
    ;; Produces 3D syntax to preserve eq-ness of syntax
    ;; #'#'stx would be lose identity when wrapped
    (define/public (read-special src line col pos)
      (with-syntax ([p (lambda () stx)])
        #'(p)))
    ))

(define top-aligned
  (make-object style-delta% 'change-alignment 'top))

(define-struct styled (contents style clickback))

;; clicky-snip%
(define clicky-snip%
  (class* editor-snip% ()

    (init-field [open-style '(border)]
                [closed-style '(tight-text-fit)])

    (inherit set-margin
             set-inset
             set-snipclass
             set-tight-text-fit
             show-border
             get-admin)

    (define -outer (new text%))
    (super-new (editor -outer) (with-border? #f))
    (set-margin 2 2 2 2)
    (set-inset 2 2 2 2)
    ;;(set-margin 3 0 0 0)
    ;;(set-inset 1 0 0 0)
    ;;(set-margin 0 0 0 0)
    ;;(set-inset 0 0 0 0)

    (define/public (closed-contents) null)
    (define/public (open-contents) null)

    (define open? #f)

    (define/public (refresh-contents)
      (send* -outer
        (begin-edit-sequence)
        (lock #f)
        (erase))
      (do-style (if open? open-style closed-style))
      (outer:insert (if open? (hide-icon) (show-icon))
                    style:hyper
                    (if open?
                        (lambda _
                          (set! open? #f)
                          (refresh-contents))
                        (lambda _
                          (set! open? #t)
                          (refresh-contents))))
      (for-each (lambda (s) (outer:insert s))
                (if open? (open-contents) (closed-contents)))
      (send* -outer
        (change-style top-aligned 0 (send -outer last-position))
        (lock #t)
        (end-edit-sequence)))

    (define/private (do-style style)
      (show-border (memq 'border style))
      (set-tight-text-fit (memq 'tight-text-fit style)))

    (define/private outer:insert
      (case-lambda
       [(obj)
        (if (styled? obj)
            (outer:insert (styled-contents obj)
                          (styled-style obj)
                          (styled-clickback obj))
            (outer:insert obj style:normal))]
       [(text style)
        (outer:insert text style #f)]
       [(text style clickback)
        (let ([start (send -outer last-position)])
          (send -outer insert text)
          (let ([end (send -outer last-position)])
            (send -outer change-style style start end #f)
            (when clickback
                  (send -outer set-clickback start end clickback))))]))

    (send -outer hide-caret #t)
    (send -outer lock #t)
    (refresh-contents)
    ))

;; syntax-snip%
(define syntax-snip%
  (class* clicky-snip% (readable-snip<%>)
    (init-field ((stx syntax)))
    (init-field [controller (new controller%)])
    (init-field [config (new syntax-snip-config%)])

    (inherit set-snipclass
             refresh-contents)

    (define the-syntax-snip
      (new syntax-value-snip%
           (syntax stx)
           (controller controller)
           (config config)))
    (define the-summary
      (let* ([t (new text%)]
             [es (new editor-snip% (editor t) (with-border? #f))])
        (send es set-margin 0 0 0 0)
        (send es set-inset 0 0 0 0)
        (send t insert (format "~s" stx))
        es))

    (define properties-snip
      (new properties-container-snip%
           (controller controller)))

    (define/override (closed-contents)
      (list the-summary))

    (define/override (open-contents)
      (list " "
            the-syntax-snip
            " "
            properties-snip))

    ;; Snip methods
    (define/override (copy)
      (new syntax-snip% (syntax stx)))
    (define/override (write stream)
      (send stream put
            (string->bytes/utf-8
             (format "~s" (marshall-syntax stx)))))
    (define/public (read-special src line col pos)
      (send the-syntax-snip read-special src line col pos))

    (send: config config<%> listen-props-shown?
           (lambda (?) (refresh-contents)))

    (super-new)
    (set-snipclass snip-class)
    ))

(define properties-container-snip%
  (class clicky-snip%
    (init controller)

    (define properties-snip
      (new properties-snip% (controller controller)))

    (define/override (open-contents)
      (list #;(show-properties-icon)
            properties-snip))

    (define/override (closed-contents)
      (list (show-properties-icon)))

    (super-new (open-style '())
               (closed-style '()))))

(define style:normal (make-object style-delta% 'change-normal))
(define style:hyper
  (let ([s (make-object style-delta% 'change-normal)])
    (send s set-delta 'change-toggle-underline)
    (send s set-delta-foreground "blue")
    s))
(define style:green
  (let ([s (make-object style-delta% 'change-normal)])
    (send s set-delta-foreground "darkgreen")
    s))
(define style:bold
  (let ([s (make-object style-delta% 'change-normal)])
    (send s set-delta 'change-bold)
    s))

(define (show-icon)
  (make-object image-snip%
    (build-path (collection-path "icons") "turn-up.png")))
(define (hide-icon)
  (make-object image-snip%
    (build-path (collection-path "icons") "turn-down.png")))

(define (show-properties-icon)
  (make-object image-snip%
    (build-path (collection-path "icons") "syncheck.png")))

;; marshall-syntax : syntax -> printable
(define (marshall-syntax stx)
  (unless (syntax? stx)
    (error 'marshall-syntax "not syntax: ~s\n" stx))
  `(syntax
    (source ,(marshall-object (syntax-source stx)))
    (source-module ,(marshall-object (syntax-source-module stx)))
    (position ,(syntax-position stx))
    (line ,(syntax-line stx))
    (column ,(syntax-column stx))
    (span ,(syntax-span stx))
    (original? ,(syntax-original? stx))
    (properties 
     ,@(map (λ (x) `(,x ,(marshall-object (syntax-property stx x))))
            (syntax-property-symbol-keys stx)))
    (contents
     ,(marshall-object (syntax-e stx)))))

;; marshall-object : any -> printable
;; really only intended for use with marshall-syntax
(define (marshall-object obj)
  (cond
    [(syntax? obj) (marshall-syntax obj)]
    [(pair? obj) 
     `(pair ,(cons (marshall-object (car obj))
                   (marshall-object (cdr obj))))]
    [(or (symbol? obj)
         (char? obj)
         (number? obj)
         (string? obj)
         (boolean? obj)
         (null? obj))
     `(other ,obj)]
    [else (string->symbol (format "unknown-object: ~s" obj))]))

;; COPIED AND MODIFIED from mrlib/syntax-browser.ss
(define syntax-snipclass%
  (class snip-class%
    (define/override (read stream)
      (make-object syntax-snip%
        (unmarshall-syntax (read-from-string (send stream get-bytes)))))
    (super-instantiate ())))

(define snip-class (make-object syntax-snipclass%))
(send snip-class set-version 2)
(send snip-class set-classname
      (format "~s" '(lib "implementation.ss" "macro-debugger" "syntax-browser")))
(send (get-the-snip-class-list) add snip-class)

(define (unmarshall-syntax stx)
  (match stx
    [`(syntax
       (source ,src)
       (source-module ,source-module) ;; marshalling
       (position ,pos)
       (line ,line)
       (column ,col)
       (span ,span)
       (original? ,original?)
       (properties . ,properties)
       (contents ,contents))
      (foldl
       add-properties
       (datum->syntax
        #'here ;; ack
        (unmarshall-object contents)
        (list (unmarshall-object src)
              line
              col
              pos
              span))
       properties)]
    [else #'unknown-syntax-object]))

;; add-properties : syntax any -> syntax
(define (add-properties prop-spec stx)
  (match prop-spec
    [`(,(and sym (? symbol?))
        ,prop)
      (syntax-property stx sym (unmarshall-object prop))]
    [else stx]))

(define (unmarshall-object obj)
  (let ([unknown (lambda () (string->symbol (format "unknown: ~s" obj)))])
    (if (and (pair? obj)
             (symbol? (car obj)))
        (case (car obj)
          [(pair) 
           (if (pair? (cdr obj))
               (let ([raw-obj (cadr obj)])
                 (if (pair? raw-obj)
                     (cons (unmarshall-object (car raw-obj))
                           (unmarshall-object (cdr raw-obj)))
                     (unknown)))
               (unknown))]
          [(other) 
           (if (pair? (cdr obj))
               (cadr obj)
               (unknown))]
          [(syntax) (unmarshall-syntax obj)]
          [else (unknown)])
        (unknown))))

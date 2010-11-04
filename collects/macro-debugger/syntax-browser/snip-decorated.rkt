#lang racket/base
(require racket/class
         racket/gui/base
         (only-in mzlib/string read-from-string)
         unstable/class-iop
         "interfaces.rkt"
         "controller.rkt"
         "properties.rkt"
         "prefs.rkt"
         "util.rkt"
         (except-in "snip.rkt"
                    snip-class))

(provide decorated-syntax-snip%
         snip-class)

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
      (with-unlock -outer
        (send -outer erase)
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
        (send -outer change-style top-aligned 0 (send -outer last-position))))

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

;; decorated-syntax-snip%
(define decorated-syntax-snip%
  (class* clicky-snip% (readable-snip<%>)
    (init-field ((stx syntax)))
    (init-field [controller (new controller%)])
    (init-field [config (new syntax-prefs%)])

    (inherit set-snipclass
             refresh-contents)

    (define the-syntax-snip
      (new syntax-snip%
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
      (new decorated-syntax-snip%
           (syntax stx)
           (controller controller)
           (config config)))
    (define/override (write stream)
      (send stream put
            (string->bytes/utf-8
             (format "~s" (marshall-syntax stx)))))
    (define/public (read-special src line col pos)
      (send the-syntax-snip read-special src line col pos))

    (send/i config config<%> listen-props-shown?
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


;; SNIPCLASS

;; COPIED AND MODIFIED from mrlib/syntax-browser.rkt
(define decorated-syntax-snipclass%
  (class snip-class%
    (define/override (read stream)
      (new decorated-syntax-snip%
           (syntax (unmarshall-syntax
                    (read-from-string (send stream get-bytes))))))
    (super-new)))

(define snip-class (make-object decorated-syntax-snipclass%))
(send snip-class set-version 2)
(send snip-class set-classname
      (format "~s" '(lib "macro-debugger/syntax-browser/snip-decorated.rkt")))

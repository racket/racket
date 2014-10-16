#lang racket/base
(require racket/class
         racket/gui/base
         racket/match
         (only-in mzlib/string read-from-string)
         framework
         "display.rkt"
         "controller.rkt"
         "keymap.rkt"
         "util.rkt"
         "text.rkt"
         "prefs.rkt")

(provide syntax-snip%
         marshall-syntax
         unmarshall-syntax
         snip-class)

;; syntax-snip%
(define syntax-snip%
  (class* editor-snip% (readable-snip<%>)
    (init-field ((stx syntax)))
    (init-field (controller (new controller%)))
    (init-field (config (new syntax-prefs/readonly%)))
    (init-field (columns 40))

    (inherit set-margin
             set-inset
             set-snipclass)

    (define text (new browser-text%))
    (super-new (editor text) (with-border? #f))

    (set-margin 0 0 0 0)
    ;;(set-inset 2 2 2 2)
    ;;(set-margin 2 2 2 2)
    (set-inset 0 0 0 0)

    (define display
      (with-unlock text
        (send text change-style (make-object style-delta% 'change-alignment 'top))
        (print-syntax-to-editor stx text controller config columns)))
    (send text hide-caret #t)

    (setup-keymap text)

    (define/public (setup-keymap text)
      (new syntax-keymap%
           (controller controller)
           (config config)
           (editor text)))

    ;; snip% Methods
    (define/override (copy)
      (new syntax-snip%
           (config config)
           (controller controller)
           (syntax stx)))

    ;; read-special : any number/#f number/#f number/#f -> syntax
    ;; Produces 3D syntax to preserve eq-ness of syntax
    ;; #'#'stx would be lose identity when wrapped
    (define/public (read-special src line col pos)
      (with-syntax ([p (lambda () stx)])
        #'(p)))
    
    (define/override (write stream)
      (send stream put
            (string->bytes/utf-8
             (format "~s" (marshall-syntax stx)))))

    (set-snipclass snip-class)))

;; Marshalling stuff

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

;; SNIPCLASS

;; COPIED AND MODIFIED from mrlib/syntax-browser.rkt
(define syntax-snipclass%
  (class snip-class%
    (define/override (read stream)
      (make-object syntax-snip%
        (unmarshall-syntax (read-from-string (send stream get-bytes)))))
    (super-instantiate ())))

(define snip-class (new syntax-snipclass%))
(send snip-class set-version 2)
(send snip-class set-classname
      (format "~s" '(lib "macro-debugger/syntax-browser/snip.rkt")))

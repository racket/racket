
(module syntax-snip mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "match.ss")
           (lib "list.ss")
           (lib "string.ss")
           "interfaces.ss"
           "display.ss"
           "controller.ss"
           "properties.ss"
           "partition.ss")

  (provide syntax-value-snip%)

  ;; syntax-value-snip%
  (define syntax-value-snip%
    (class* editor-snip% (readable-snip<%>)
      (init-field ((stx syntax)))
      (init-field host)
      (inherit set-margin
               set-inset)

      (define text (new text:standard-style-list%))
      (super-new (editor text) (with-border? #f))
      (set-margin 0 0 0 0)
      (set-inset 2 2 2 2)
      (send text begin-edit-sequence)
      (send text change-style (make-object style-delta% 'change-alignment 'top))
      (define display
        (print-syntax-to-editor stx text (send host get-controller)))
      (send text lock #t)
      (send text end-edit-sequence)
      (send text hide-caret #t)

      (send host add-keymap text this)

      ;; snip% Methods
      (define/override (copy)
        (new syntax-value-snip% (host host) (syntax stx)))

      ;; read-special : any number/#f number/#f number/#f -> syntax
      ;; Produces 3D syntax to preserve eq-ness of syntax
      ;; #'#'stx would be lose identity when wrapped
      (define/public (read-special src line col pos)
        (with-syntax ([p (lambda () stx)])
          #'(p)))
      ))

  ;; syntax-snip%
  #;
  (define syntax-snip%
    (class* editor-snip% (readable-snip<%>)
      (init-field ((stx syntax)))
      (init-field primary-partition)
      (inherit set-margin
               set-inset
               set-snipclass
               set-tight-text-fit
               show-border
               get-admin)

      (define properties-snip (new properties-snip%))

      (define -outer (new text%))
      (super-new (editor -outer) (with-border? #f))
      (set-margin 0 0 0 0)
      (set-inset 0 0 0 0)
      (set-snipclass snip-class)
      (send -outer select-all)

      (define the-syntax-snip
        (new syntax-value-snip%
             (syntax stx)
             (controller controller)
             ;; FIXME
             #;(syntax-keymap% syntax-keymap%)
             ))
      (define the-summary
        (let ([line (syntax-line stx)]
              [col (syntax-column stx)])
          (if (and line col)
              (format "#<syntax:~s:~s>" line col)
              "#<syntax>")))

      (define shown? #f)
      (define/public (refresh)
        (if shown?
            (refresh/shown)
            (refresh/hidden)))

      (define/private (refresh/hidden)
        (send* -outer
          (begin-edit-sequence)
          (lock #f)
          (erase))
        (set-tight-text-fit #t)
        (show-border #f)
        (outer:insert (show-icon) style:hyper 
                      (lambda _ (set! shown? #t) (refresh)))
        (outer:insert the-summary)
        (send* -outer 
          (lock #t)
          (end-edit-sequence)))

      (define/private (refresh/shown)
        (send* -outer
          (begin-edit-sequence)
          (lock #f)
          (erase))
        (set-tight-text-fit #f)
        (show-border #t)
        (outer:insert (hide-icon) style:hyper
                      (lambda _ (set! shown? #f) (refresh)))
        (outer:insert " ")
        (outer:insert the-syntax-snip)
        (outer:insert " ")
        (if (props-shown?)
            (begin (outer:insert "<" style:green (lambda _ (show #f)))
                   (outer:insert properties-snip))
            (begin (outer:insert ">" style:green (lambda _ (show #t)))))
        (send* -outer
          (change-style (make-object style-delta% 'change-alignment 'top)
                        0
                        (send -outer last-position))
          (lock #t)
          (end-edit-sequence)))

      (define/private outer:insert
        (case-lambda
         [(obj)
          (outer:insert obj style:normal)]
         [(text style)
          (outer:insert text style #f)]
         [(text style clickback)
          (let ([start (send -outer last-position)])
            (send -outer insert text)
            (let ([end (send -outer last-position)])
              (send -outer change-style style start end #f)
              (when clickback
                    (send -outer set-clickback start end clickback))))]))

      ;; Snip methods
      (define/override (copy)
        (new syntax-snip% (syntax stx)))
      (define/override (write stream)
        (send stream put (string->bytes/utf-8 (format "~s" (marshall-syntax stx)))))
      (define/public (read-special src line col pos)
        (send the-syntax-snip read-special src line col pos))

      (define/private (find-primary-partition)
        #;(define editor (send (get-admin) get-editor))
        (new-bound-partition))


      ;; syntax-properties-controller methods
      (define properties-shown? #f)
      (define/public (props-shown?)
        properties-shown?)
      (define/public (show ?)
        (set! properties-shown? ?)
        (refresh))
      (define/public (set-syntax stx)
        (send properties-snip set-syntax stx))

      (refresh)
      (send -outer hide-caret #t)
      (send -outer lock #t)
      ))

  ;; independent-properties-controller%
  #;
  (define independent-properties-controller%
    (class* object% (syntax-properties-controller<%>)
      (init-field controller)
      (init-field ((stx syntax) #f))

      ;; Properties display
      (define parent
        (new frame% (label "Properties") (height (pref:height))
             (width (floor (* (pref:props-percentage) (pref:width))))))
      (define pv (new properties-view% (parent parent)))

      (define/private (show-properties)
        (unless (send parent is-shown?)
          (send parent show #t)))

      (define/public (set-syntax stx)
        (send pv set-syntax stx))
      (define/public (show ?)
        (send parent show ?))
      (define/public (props-shown?)
        (send parent is-shown?))

      (super-new)))


  #;
  (define snip-keymap-extension@
    (unit
      (import (prefix pre: keymap^))
      (export keymap^)

      (define syntax-keymap%
        (class pre:syntax-keymap%
          (init-field snip)
          (inherit add-function)
          (super-new (controller (send snip get-controller)))

          (add-function "show-syntax-properties"
                        (lambda (i e)
                          (send snip show-props)))))))



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
  #;
  (define syntax-snipclass%
    (class snip-class%
      (define/override (read stream)
        (make-object syntax-snip%
          (unmarshall-syntax (read-from-string (send stream get-bytes)))))
      (super-instantiate ())))

  #;(define snip-class (make-object syntax-snipclass%))
  #;(send snip-class set-version 2)
  #;(send snip-class set-classname
        (format "~s" '(lib "implementation.ss" "macro-debugger" "syntax-browser")))
  #;(send (get-the-snip-class-list) add snip-class)
  
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
         (properties ,@(properties ...))
         (contents ,contents))
        (foldl
         add-properties
         (datum->syntax-object
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

  )

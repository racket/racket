
(module syntax-snip mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "controller.ss"
           "properties.ss"
           "typesetter.ss"
           "partition.ss")
  (provide snip@
           snip-keymap-extension@)

  ;; Every snip has its own controller and properties-controller
  ;; (because every snip now displays its own properties)

  (define snip@
    (unit
      (import prefs^
              keymap^
              context-menu^
              snipclass^)
      (export snip^)

      ;; syntax-snip : syntax -> snip
      (define (syntax-snip stx)
        (new syntax-snip% (syntax stx)))
      
      ;; syntax-value-snip%
      (define syntax-value-snip%
        (class* editor-snip% (readable-snip<%>)
          (init-field ((stx syntax)))
          (init-field controller)
          (inherit set-margin
                   set-inset)

          (define -outer (new text:standard-style-list%))
          (super-new (editor -outer) (with-border? #f))
          (set-margin 0 0 0 0)
          (set-inset 2 2 2 2)
          (send -outer change-style (make-object style-delta% 'change-alignment 'top))
          (new syntax-keymap%
               (editor -outer)
               (snip this))
          (refresh)
          
          (define/public (get-controller) controller)

          (define/private (refresh)
            (send -outer begin-edit-sequence)
            (send -outer erase)
            (new typesetter-for-text%
                 (syntax stx)
                 (controller controller)
                 (text -outer))
            (send -outer lock #t)
            (send -outer end-edit-sequence)
            (send -outer hide-caret #t))
          
          (define/public (show-props)
            (send (send controller get-properties-controller)
                  show #t))
          
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
          
          ;; snip% Methods
          (define/override (copy)
            (new syntax-value-snip% (controller controller) (syntax stx)))
          
          ;; read-special : any number/#f number/#f number/#f -> syntax
          ;; Produces 3D syntax to preserve eq-ness of syntax
          ;; #'#'stx would be lose identity when wrapped
          (define/public (read-special src line col pos)
            (with-syntax ([p (lambda () stx)])
              #'(p)))
          ))


      ;; syntax-snip%
      (define syntax-snip%
        (class* editor-snip% (readable-snip<%>)
          (init-field ((stx syntax)))
          (inherit set-margin
                   set-inset
                   set-snipclass
                   set-tight-text-fit
                   show-border
                   get-admin)

          (define controller
            (new syntax-controller% (primary-partition (find-primary-partition))))
          (define properties-snip (new properties-snip%))
          (send controller set-properties-controller this)

          (define -outer (new text%))
          (super-new (editor -outer) (with-border? #f))
          (set-margin 0 0 0 0)
          (set-inset 0 0 0 0)
          (set-snipclass snip-class)
          (send -outer select-all)

          (define the-syntax-snip
            (new syntax-value-snip% (syntax stx) (controller controller)))
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
      ))

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
  )

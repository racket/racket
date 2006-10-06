
(module syntax-snip mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "controller.ss"
           "properties.ss"
           "typesetter.ss")
  (provide snip@
           snip-context-menu-extension@)
  
  (define snip@
    (unit/sig snip^
      (import prefs^
              keymap^
              context-menu^
              snipclass^)
      
      ;; syntax-snip : syntax -> snip
      (define (syntax-snip stx)
        (new syntax-snip% (syntax stx)))
      
      (define *syntax-controller* #f)
      
      (define (the-syntax-controller)
        (let ([controller *syntax-controller*])
          (or controller
              (let* ([controller (new syntax-controller%)]
                     [props (new independent-properties-controller% (controller controller))])
                (send controller set-properties-controller props)
                (set! *syntax-controller* controller)
                controller))))
      
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
               (context-menu (new context-menu% (snip this))))
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
          
          (define/public (read-special src line col pos)
            #;(datum->syntax-object #f
                                    `(,#'quote-syntax ,stx)
                                    (list src line col pos 1))
            #`(force '#,(delay stx)))
          ))
      
      (define syntax-snip%
        (class* editor-snip% (readable-snip<%>)
          (init-field ((stx syntax)))
          (init-field (controller (the-syntax-controller)))
          (inherit set-margin
                   set-inset
                   set-snipclass
                   set-tight-text-fit
                   show-border)
          
          (define -outer (new text%))
          (super-new (editor -outer) (with-border? #f))
          (set-margin 0 0 0 0)
          (set-inset 0 0 0 0)
          (set-snipclass snip-class)
          (send -outer select-all)
          (send -outer change-style (make-object style-delta% 'change-alignment 'top)
                0
                (send -outer last-position))
          
          (define the-syntax-snip
            (new syntax-value-snip% (syntax stx) (controller controller)))
          (define the-summary
            (let ([line (syntax-line stx)]
                  [col (syntax-column stx)])
              (if (and line col)
                  (format "#<syntax:~s:~s>" line col)
                  "#<syntax>")))
          
          (define/private (hide-me)
            (send* -outer
              (begin-edit-sequence)
              (lock #f)
              (erase))
            (set-tight-text-fit #t)
            (show-border #f)
            (outer:insert (show-icon) style:hyper (lambda _ (show-me)))
            (outer:insert the-summary)
            (send* -outer 
              (lock #t)
              (end-edit-sequence)))
          
          (define/private (show-me)
            (send* -outer
              (begin-edit-sequence)
              (lock #f)
              (erase))
            (set-tight-text-fit #f)
            (show-border #t)
            (outer:insert (hide-icon) style:hyper (lambda _ (hide-me)))
            (outer:insert " ")
            (outer:insert the-syntax-snip)
            (send* -outer
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
            (new syntax-snip% (controller controller) (syntax stx)))
          (define/override (write stream)
            (send stream put (string->bytes/utf-8 (format "~s" (marshall-syntax stx)))))
          (define/public (read-special src line col pos)
            (send the-syntax-snip read-special src line col pos))
          
          (hide-me)
          (send -outer hide-caret #t)
          (send -outer lock #t)
          ))
      
      ;; independent-properties-controller%
      (define independent-properties-controller%
        (class* object% (syntax-properties-controller<%>)
          (init-field controller)
          
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

  (define snip-context-menu-extension@
    (unit/sig context-menu^
      (import (pre : context-menu^))
      
      (define context-menu%
        (class pre:context-menu%
          (init-field snip)

          (define/override (after-selection-items)
            (super after-selection-items)
            (new menu-item% (label "Show syntax properties")
                 (parent this)
                 (callback (lambda _ (send snip show-props))))
            (void))
          
          (super-new (controller (send snip get-controller)))))))
  
  
  (define style:normal (make-object style-delta% 'change-normal))
  (define style:hyper
    (let ([s (make-object style-delta% 'change-normal)])
      (send s set-delta 'change-toggle-underline)
      (send s set-delta-foreground "blue")
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

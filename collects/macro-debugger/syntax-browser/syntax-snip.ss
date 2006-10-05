
(module syntax-snip mzscheme
  (require (lib "class.ss")
           (lib "match.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "string.ss")
           "interfaces.ss"
           "partition.ss"
           "typesetter.ss"
           "widget.ss"
           "syntax-browser.ss")
  (provide syntax-snip
           snip-class
           syntax-value-snip%
           syntax-snip%)

  ;; syntax-snip : syntax -> snip
  (define (syntax-snip stx)
    (new syntax-snip% (syntax stx)))

  (define current-syntax-controller (make-parameter #f))

  (define (the-syntax-controller)
    (let ([controller (current-syntax-controller)])
      (or controller
          (let ([controller (new syntax-controller%)])
            (current-syntax-controller controller)
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
      (refresh)

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

      (define/private (show-props)
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
      
      ;; BEGIN COPIED from widget.ss
      ;; WITH MODIFICATIONS
      ;; Set up keymap
      (let ([keymap (send -outer get-keymap)])
        (send keymap map-function "rightbutton" "popup-context-window")
        (send keymap add-function "popup-context-window"
              (lambda (editor event)
                (do-popup-context-window editor event))))
      (define/private (do-popup-context-window editor event)
        (define-values (x y)
          (send editor dc-location-to-editor-location
                (send event get-x)
                (send event get-y)))
        (define admin (send editor get-admin))
        (send admin popup-menu context-menu x y))
      (define context-menu
        (let ([context-menu (new popup-menu%)])
          (new menu-item% (label "Copy") (parent context-menu)
               (callback (lambda (i e)
                           (define stx (send controller get-selected-syntax))
                           (send the-clipboard set-clipboard-string
                                 (if stx 
                                     (format "~s" (syntax-object->datum stx))
                                     "")
                                 (send e get-time-stamp)))))
          ;; ADDED
          (new menu-item% (label "Copy syntax") (parent context-menu)
               (callback (lambda (i e)
                           (define stx (send controller get-selected-syntax))
                           (define t (new text%))
                           (send t insert
                                 (new syntax-snip%
                                      (syntax stx)
                                      (controller controller)))
                           (send t select-all)
                           (send t copy))))
          ;; FIXME: Add option for "formatted" copy/paste?
          (new menu-item%
               (label "Clear selection")
               (parent context-menu)
               (callback (lambda _ (send controller select-syntax #f))))
          (new separator-menu-item% (parent context-menu))
          ;; properties (MODIFIED)
          (new menu-item% 
               (label "Show syntax properties")
               (parent context-menu)
               (callback (lambda _ (show-props))))
          ;; syntax browser (ADDED)
          (new menu-item%
               (label "Show in browser frame")
               (parent context-menu)
               (callback (lambda _ (browse-syntax stx))))
          ;; primary selection
          (let ([secondary (new menu% (label "identifier=?") (parent context-menu))])
            (for-each
             (lambda (name func)
               (let ([this-choice
                      (new checkable-menu-item%
                           (label name)
                           (parent secondary)
                           (callback 
                            (lambda (i e)
                              (send controller on-update-identifier=? name func))))])
                 (send controller add-identifier=?-listener
                       (lambda (new-name new-id=?) 
                         (send this-choice check (eq? name new-name))))))
             (map car (identifier=-choices))
             (map cdr (identifier=-choices))))
          context-menu))
      ;; END COPIED
      
      ;; snip% Methods
      (define/override (copy)
        (new syntax-value-snip% (controller controller) (syntax stx)))
      
      (define/public (read-special src line col pos)
        (datum->syntax-object #f
                              `(,#'quote-syntax ,stx)
                              (list src line col pos 1)))

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
      (set-margin 2 0 0 0)
      (set-inset 3 0 0 0)
      (set-snipclass snip-class)

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
  
  ;; COPIED AND MODIFIED from mrlib/syntax-browser.ss
  
  (define syntax-snipclass%
    (class snip-class%
      (define/override (read stream)
        (let ([str (send stream get-bytes)])
          (make-object syntax-snip% 
            (unmarshall-syntax (read-from-string (bytes->string/utf-8 str))))))
      (super-instantiate ())))
  
  (define snip-class (make-object syntax-snipclass%))
  (send snip-class set-version 2)
  (send snip-class set-classname
        (format "~s" '(lib "syntax-snip.ss" "macro-debugger" "syntax-browser")))
  (send (get-the-snip-class-list) add snip-class)
  
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
    (let ([unknown (λ () (string->symbol (format "unknown: ~s" obj)))])
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
          (unknown)))))

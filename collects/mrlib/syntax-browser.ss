#|

needed to really make this work:

  - marshallable syntax objects (compile and write out the compiled form)
  - support for generic ports that are editors

|#

(module syntax-browser mzscheme
  (require (lib "pretty.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "match.ss")
           (lib "string.ss")
	   "include-bitmap.ss")
  
  (provide render-syntax/snip render-syntax/window snip-class)
  
  (define (render-syntax/window syntax)
    (let ([es (render-syntax/snip syntax)])
      (define f (make-object frame% "frame" #f 850 500))
      (define mb (make-object menu-bar% f))
      (define edit-menu (make-object menu% "Edit" mb))
      (define t (make-object text%))
      (define ec (make-object editor-canvas% f t))
      (append-editor-operation-menu-items edit-menu)
      (send t insert es)
      (send f show #t)))

  (define (render-syntax/snip stx) (make-object syntax-snip% stx))
  
  (define syntax-snipclass%
    (class snip-class%
      (define/override (read stream)
        (let ([str (send stream get-bytes)])
          (make-object syntax-snip% (unmarshall-syntax (read-from-string (bytes->string/utf-8 str))))))
      (super-instantiate ())))
  
  (define snip-class (make-object syntax-snipclass%))
  (send snip-class set-version 1)
  (send snip-class set-classname (format "~s" '(lib "syntax-browser.ss" "mrlib")))
  (send (get-the-snip-class-list) add snip-class)
  
  (define-struct range (obj start end))

  (define syntax-snip%
    (class editor-snip%
      (init-field main-stx)
      
      (unless (syntax? main-stx)
        (error 'syntax-snip% "got non-syntax object"))
      
      (define/public (get-syntax) main-stx)
      
      (define/override (copy) (make-object syntax-snip% main-stx))
      (define/override (write stream)
        (send stream put (string->bytes/utf-8 (format "~s" (marshall-syntax main-stx)))))
      
      (define-values (datum stx-ht) (syntax-object->datum/ht main-stx))
      
      (define output-text (make-object text%))
      (define output-port (make-text-port output-text))
      (define info-text (make-object text%))
      (define info-port (make-text-port info-text))
      
      ;; assume that there aren't any eq? sub structures, only eq? flat stuff (symbols, etc)
      ;; this is guaranteed by syntax-object->datum/ht
      
      ;; range-start-ht : hash-table[obj -o> number]
      (define range-start-ht (make-hash-table))

      ;; range-ht : hash-table[obj -o> (listof (cons number number))]
      (define range-ht (make-hash-table))
      
      (define/private (make-modern text)
        (send text change-style
              (make-object style-delta% 'change-family 'modern)
              0
              (send text last-position)))
      
      (let ([range-pretty-print-pre-hook 
             (λ (x v)
               (hash-table-put! range-start-ht x (send output-text last-position)))]
            [range-pretty-print-post-hook 
             (λ (x port)
               (let ([range-start (hash-table-get range-start-ht x (λ () #f))])
                 (when range-start
                   (hash-table-put! range-ht x 
                                    (cons
                                     (cons
                                      range-start
                                      (send output-text last-position))
                                     (hash-table-get range-ht x (λ () null)))))))])
        (parameterize ([current-output-port output-port]
                       [pretty-print-pre-print-hook range-pretty-print-pre-hook]
                       [pretty-print-post-print-hook range-pretty-print-post-hook]
                       [pretty-print-columns 30])
          (pretty-print datum)
          (make-modern output-text)))
      
      (define/private (show-info stx)
        (insert/big "General Info\n")
        (piece-of-info "Source" (syntax-source stx))
        (piece-of-info "Source module" (syntax-source-module stx))
        (piece-of-info "Position" (syntax-position stx))
        (piece-of-info "Line" (syntax-line stx))
        (piece-of-info "Column" (syntax-column stx))
        (piece-of-info "Span" (syntax-span stx))
        (piece-of-info "Original?" (syntax-original? stx))
        (when (identifier? stx)
          (piece-of-info "Identifier-binding" (identifier-binding stx))
          (piece-of-info "Identifier-transformer-binding" (identifier-transformer-binding stx)))
        
	(let ([properties (syntax-property-symbol-keys stx)])
	  (unless (null? properties)
	    (insert/big "Known properties\n")
	    (for-each
	     (λ (prop) (show-property stx prop))
	     properties))))
      
      (define/private (render-mpi mpi)
        (string-append
         "#<module-path-index "
         (let loop ([mpi mpi])
           (cond
             [(module-path-index? mpi)
              (let-values ([(x y) (module-path-index-split mpi)])
                (string-append
                 "("
                 (format "~s" x)
                 " . "
                 (loop y)
                 ")"))]
             [else (format "~s" mpi)]))
         ">"))
      
      (define/private (show-property stx prop)
        (piece-of-info (format "'~a" prop) (syntax-property stx prop)))
      
      (define/private (piece-of-info label info)
        (let ([small-newline 
               (λ (port text)
                 (let ([before-newline (send text last-position)])
                   (newline port)
                   (send info-text change-style small-style before-newline (+ before-newline 1))))])
          
          (insert/bold label)
          (newline info-port)
          
          ;; should just be using generic `print'
          ;; but won't work without built-in support for
          ;; editors as output ports
          (parameterize ([pretty-print-size-hook
                          (λ (val d/p port)
                            (if (is-a? val syntax-snip%)
                                (+ (string-length (format "~a" (send val get-syntax))) 2)
                                #f))]
                         [pretty-print-print-hook
                          (λ (val d/p port)
                            (send info-text insert (send val copy) 
                                  (send info-text last-position)
                                  (send info-text last-position)))])
            (pretty-print (replace-syntaxes info) info-port))
          
          (optional-newline)
          (small-newline info-port info-text)))
      
      (define/private (replace-syntaxes obj)
        (cond
          [(cons? obj) (cons (replace-syntaxes (car obj))
                             (replace-syntaxes (cdr obj)))]
          [(syntax? obj) (make-object syntax-snip% obj)]
          [else obj]))
      
      (define/private (insert/bold str)
        (let ([pos (send info-text last-position)])
          (send info-text insert str 
                (send info-text last-position)
                (send info-text last-position))
          (send info-text change-style
                (make-object style-delta% 'change-bold)
                pos 
                (send info-text last-position))))
      
      (define/private (insert/big str)
        (let ([sd (make-object style-delta% 'change-bold)])
          (send sd set-delta-foreground "Navy")
          (let ([pos (send info-text last-position)])
            (send info-text insert str 
                  (send info-text last-position)
                  (send info-text last-position))
            (send info-text change-style
                  sd
                  pos 
                  (send info-text last-position)))))
      
      (define/private (optional-newline)
        (unless (equal?
                 (send info-text get-character (- (send info-text last-position) 1))
                 #\newline)
          (send info-text insert "\n" (send info-text last-position))))
      
      (define/private (show-range stx start end)
        (send output-text begin-edit-sequence)
        (send output-text lock #f)
        (send output-text change-style black-style-delta 0 (send output-text last-position))
        (send output-text change-style green-style-delta start end)
        (send output-text lock #t)
        (send output-text end-edit-sequence)
        
        (send info-text begin-edit-sequence)
        (send info-text lock #f)
        (send info-text erase)
        (show-info stx)
        (make-modern info-text)
        (send info-text lock #t)
        (send info-text end-edit-sequence))
      
      (define outer-t (make-object text%))
      
      (super-instantiate ()
        (editor outer-t)
        (with-border? #f)
        (left-margin 3)
        (top-margin 0)
        (right-margin 0)
        (bottom-margin 0)
        (left-inset 1)
        (top-inset 0)
        (right-inset 0)
        (bottom-inset 0))
      
      (define inner-t (make-object text%))
      (define inner-es (instantiate editor-snip% ()
                         (editor inner-t)
                         (with-border? #f)
                         (left-margin 0)
                         (top-margin 0)
                         (right-margin 0)
                         (bottom-margin 0)
                         (left-inset 0)
                         (top-inset 0)
                         (right-inset 0)
                         (bottom-inset 0)))
      
      (define details-shown? #t)
      
      (inherit show-border set-tight-text-fit)
      (define/private (hide-details)
        (when details-shown?
          (send outer-t lock #f)
          (show-border #f)
          (set-tight-text-fit #t)
          (send outer-t release-snip inner-es)
          (send outer-t delete (send outer-t last-position))
          (send outer-t lock #t)
          (set! details-shown? #f)))
      
      (define/private (show-details)
        (unless details-shown?
          (send outer-t lock #f)
          (show-border #t)
          (set-tight-text-fit #f)
          (send outer-t insert #\newline
                (send outer-t last-position)
                (send outer-t last-position))
          (send outer-t insert inner-es
                (send outer-t last-position)
                (send outer-t last-position))
          (send outer-t lock #t)
          (set! details-shown? #t)))
      
      (let ([ranges
             (sort
              (apply append
                     (hash-table-map
                      range-ht
                      (λ (k vs)
                        (map (λ (v) (make-range k (car v) (cdr v)))
                             vs))))
              (λ (x y)
                (>= (- (range-end x) (range-start x))
                    (- (range-end y) (range-start y)))))])
        (for-each
         (λ (range)
           (let* ([obj (range-obj range)]
                  [stx (hash-table-get stx-ht obj (λ () #f))]
                  [start (range-start range)]
                  [end (range-end range)])
             (when (syntax? stx)
               (send output-text set-clickback start end 
                     (λ (_1 _2 _3)
                       (show-range stx start end))))))
         ranges)
        
        (send outer-t insert (new turn-snip% 
                                  [on-up (λ () (hide-details))]
                                  [on-down (λ () (show-details))]))
        (send outer-t insert (format "~s\n" main-stx))
        (send outer-t insert inner-es)
        (make-modern outer-t)
        
        (send inner-t insert (instantiate editor-snip% ()
                               (editor output-text)
                               (with-border? #f)
                               (left-margin 0)
                               (top-margin 0)
                               (right-margin 0)
                               (bottom-margin 0)
                               (left-inset 0)
                               (top-inset 0)
                               (right-inset 0)
                               (bottom-inset 0)))
        (send inner-t insert (make-object editor-snip% info-text))
        (send inner-t change-style (make-object style-delta% 'change-alignment 'top) 0 2)
        
        (send info-text auto-wrap #t)
        (send info-text set-styles-sticky #f)
        (let/ec k
          (when (null? ranges)
            (k (void)))
          (let* ([rng (car ranges)]
                 [obj (hash-table-get stx-ht (range-obj rng) 
                                      (λ ()
                                        (k (void))))])
            (show-range obj (range-start rng) (range-end rng)))))
      
      (send output-text hide-caret #t)
      (send info-text hide-caret #t)
      (send inner-t hide-caret #t)
      (send outer-t hide-caret #t)
      (send output-text lock #t)
      (send info-text lock #t)
      (send inner-t lock #t)
      (send outer-t lock #t)
      
      (hide-details)
      
      (inherit set-snipclass)
      (set-snipclass snip-class)))
  
  (define black-style-delta (make-object style-delta% 'change-normal-color))
  (define green-style-delta (make-object style-delta%))
  (send green-style-delta set-delta-foreground "forest green")
  (define small-style (make-object style-delta% 'change-size 4))
  
  (define turn-snip%
    (class snip%
      
      (init-field on-up on-down)
      
      ;; state : (union 'up 'down 'up-click 'down-click))
      (init-field [state 'up])
      
      (define/override (copy)
        (instantiate turn-snip% ()
          (on-up on-up)
          (on-down on-down)
          (state state)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([bitmap (case state
                        [(up) up-bitmap]
                        [(down) down-bitmap]
                        [(up-click) up-click-bitmap]
                        [(down-click) down-click-bitmap])])
          (cond
            [(send bitmap ok?)
             (send dc draw-bitmap bitmap x y)]
            [(send dc draw-rectangle x y 10 10)
             (send dc drawline x y 10 10)])))
             

      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0)
        (set-box/f! w arrow-snip-width)
        (set-box/f! h arrow-snip-height))
      
      (define/override (on-event dc x y editorx editory evt)
        (let ([snip-evt-x (- (send evt get-x) x)]
              [snip-evt-y (- (send evt get-y) y)])
          (cond
            [(send evt button-down? 'left)
             (set-state (case state
                          [(up) 'up-click]
                          [(down) 'down-click]
                          [else 'down-click]))]
            [(and (send evt button-up? 'left)
                  (<= 0 snip-evt-x arrow-snip-width)
                  (<= 0 snip-evt-y arrow-snip-height))
             (set-state (case state
                          [(up up-click) 
                           (on-down)
                           'down]
                          [(down down-click)
                           (on-up)
                           'up]
                          [else 'down]))]
            [(send evt button-up? 'left)
             (set-state (case state
                          [(up up-click) 'up]
                          [(down down-click) 'down]
                          [else 'up]))]
            [(and (send evt get-left-down)
                  (send evt dragging?)
                  (<= 0 snip-evt-x arrow-snip-width)
                  (<= 0 snip-evt-y arrow-snip-height))
             (set-state (case state
                          [(up up-click) 'up-click]
                          [(down down-click) 'down-click]
                          [else 'up-click]))]
            [(and (send evt get-left-down)
                  (send evt dragging?))
             (set-state (case state
                          [(up up-click) 'up]
                          [(down down-click) 'down]
                          [else 'up-click]))]
            [else
             (super on-event dc x y editorx editory evt)])))

      (inherit get-admin)
      (define/private (set-state new-state)
        (unless (eq? state new-state)
          (set! state new-state)
          (let ([admin (get-admin)])
            (when admin
              (send admin needs-update this 0 0 arrow-snip-width arrow-snip-height)))))
      
      (define/override (adjust-cursor dc x y editorx editory event) arrow-snip-cursor)
      
      (super-instantiate ())
      
      (inherit get-flags set-flags)
      (set-flags (cons 'handles-events (get-flags)))))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  (define down-bitmap (include-bitmap (lib "turn-down.png" "icons") 'png))
  (define up-bitmap (include-bitmap (lib "turn-up.png" "icons") 'png))
  (define down-click-bitmap (include-bitmap (lib "turn-down-click.png" "icons") 'png))
  (define up-click-bitmap (include-bitmap (lib "turn-up-click.png" "icons") 'png))
  (define arrow-snip-height
    (max 10
         (send up-bitmap get-height)
         (send down-bitmap get-height)
         (send up-click-bitmap get-height)
         (send down-click-bitmap get-height)))
  (define arrow-snip-width
    (max 10
         (send up-bitmap get-width)
         (send down-bitmap get-width)
         (send up-click-bitmap get-width)
         (send down-click-bitmap get-width)))
  (define arrow-snip-cursor (make-object cursor% 'arrow))
  
  ;; syntax-object->datum/ht : stx -> (values any hash-table[any -o> syntax])
  ;; the resulting hash-table maps from the each sub-object's to it's syntax.
  (define (syntax-object->datum/ht stx)
    (let ([ht (make-hash-table)])
      (values (let loop ([obj stx])
                (cond
                  [(syntax? obj)
                   (let ([lp-datum (loop (syntax-e obj))])
                     (hash-table-put! ht lp-datum obj)
                     lp-datum)]
                  [(pair? obj) 
                   (cons (loop (car obj))
                         (loop (cdr obj)))]
                  [(vector? obj) 
                   (list->vector (map loop (vector->list obj)))]
                  [else 
                   obj]))
              ht)))
  
  (define (syntax-properties stx)
    (let ([is-property? (λ (prop) (syntax-property stx prop))])
      (filter is-property?
              '(inferred-name
                bound-in-source
                origin
                disappeared-binding
                disappeared-use
                bind-as-variable
                module-variable-provides
                module-syntax-provides
                module-indirect-provides
                module-kernel-reprovide-hint
                module-self-path-index))))

  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text)
    (make-output-port #f
                      always-evt
                      (λ (s start end flush? breaks?) 
                        (send text insert (bytes->string/utf-8 (subbytes s start end))
                              (send text last-position)
                              (send text last-position))
                        (- end start))
                      void))
  
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


(module color mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           "interfaces.ss"
           "pretty-printer.ss"
           "util.ss")
  (provide syntax-text-colorer%)
  
  (define colors 
    (list "black" "red" "blue"
          "mediumforestgreen" "darkgreen" 
          "darkred"
          "cornflowerblue" "royalblue" "steelblue" "darkslategray" "darkblue"
          "indigo" "purple" 
          "orange" "salmon" "darkgoldenrod" "olive"))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  ;; syntax-text-colorer%
  (define syntax-text-colorer%
    (class object%
      ;; text : text%
      (init-field text)
      (init-field start-anchor)
      (init-field end-anchor)

      ;; syntax-pp : syntax-pp<%>
      (init-field syntax-pp)
      
      ;; controller : color-controller<%>
      (init-field controller)

      ;; Initialized in apply-styles
      (define range #f)
      (define identifier-list #f)
      (define color-partition #f)

      (define selected-syntax #f)
      (define extra-styles (make-hash-table))
      
      (define/public (apply-styles)
        (set! range (send syntax-pp get-range))
        (set! identifier-list (send syntax-pp get-identifier-list))
        (set! color-partition (send controller get-primary-partition))
        (apply-primary-partition-styles))

      ;; select-syntax : syntax -> void
      (define/public (select-syntax stx)
        (set! selected-syntax stx)
        (refresh))

      ;; highlight-syntaxes : (list-of syntax) string -> void
      (define/public (highlight-syntaxes stxs hi-color)
        (let ([style-delta (highlight-style-delta hi-color #f)])
          (for-each (lambda (stx) (hash-table-put! extra-styles stx style-delta))
                    stxs))
        (refresh))

      ;;

      ;; refresh : -> void
      ;; Clears all highlighting and reapplies all non-foreground styles.
      (define/public (refresh)
        (with-unlock text
          (send* text 
            (begin-edit-sequence)
            (change-style unhighlight-d
                          (get-start-position)
                          (get-end-position)))
          (apply-extra-styles)
          (when selected-syntax
            (apply-secondary-partition-styles)
            (apply-selection-styles))
          (send* text
            (end-edit-sequence))))

      ;; apply-extra-styles : -> void
      ;; Applies externally-added styles (such as highlighting)
      (define/private (apply-extra-styles)
        (hash-table-for-each
         extra-styles
         (lambda (hi-stx style-delta)
           (let ([rs (send range get-ranges hi-stx)])
             (for-each (lambda (r) (restyle-range r style-delta)) rs)))))

      ;; apply-secondary-partition-styles : -> void
      ;; If the selected syntax is an identifier, then styles all identifiers
      ;; in the same partition in blue.
      (define/private (apply-secondary-partition-styles)
        (when (identifier? selected-syntax)
          (let ([partition (send controller get-secondary-partition)])
            (when partition
              (for-each (lambda (id)
                          (when (send partition same-partition? selected-syntax id)
                            (draw-secondary-connection id)))
                        identifier-list)))))

      ;; apply-selection-styles : -> void
      ;; Styles subterms eq to the selected syntax in grey.
      (define/private (apply-selection-styles)
        (let ([rs (send range get-ranges selected-syntax)])
          (for-each (lambda (r) (restyle-range r select-highlight-d)) rs)))
      
      (define/private (draw-secondary-connection stx2)
        (let ([rs (send range get-ranges stx2)])
          (for-each (lambda (r) (restyle-range r select-sub-highlight-d)) rs)))

      ;; apply-primary-partition-styles : -> void
      ;; Changes the foreground color according to the primary partition.
      ;; Only called once, when the syntax is first drawn.
      (define/private (apply-primary-partition-styles)
        (for-each 
         (lambda (range)
           (let ([stx (range-obj range)]
                 [start (range-start range)]
                 [end (range-end range)])
             (send text change-style (primary-style stx) start end)))
         (send range all-ranges)))

      ;; primary-style : syntax -> style-delta%
      (define/private (primary-style stx)
        (let ([delta (new style-delta%)])
          (let ([n (send color-partition get-partition stx)])
            (cond [(< n (length colors))
                   (send delta set-delta-foreground (list-ref colors n))]
                  [else
                   (send delta set-delta-foreground "gray")
                   #;(begin (send* delta
                              (set-delta-foreground "white")
                              (set-delta-background "black")))]))
          delta))
      
      ;; restyle-range : (cons num num) style-delta% -> void
      (define/private (restyle-range r style)
        (send text change-style style (car r) (cdr r)))
      
      (define/private (get-start-position)
        (send text get-snip-position start-anchor))

      (define/private (get-end-position)
        (send text get-snip-position end-anchor))

      (super-new)))

  (define (highlight-style-delta color em?)
    (let ([sd (new style-delta%)])
      (unless em? (send sd set-delta-background color))
      (when em? 
        #;(send sd set-underlined-on #t)
        (send sd set-weight-on 'bold))
      (unless em?
        #;(send sd set-underlined-off #t)
        (send sd set-weight-off 'bold))
      sd))
  
  (define selection-color "yellow" #;"yellow" #;"lightgray")
  (define subselection-color "yellow" #;"lightyellow" #;"whitesmoke")
  
  (define select-highlight-d (highlight-style-delta selection-color #t))
  (define select-sub-highlight-d (highlight-style-delta subselection-color #f))

  (define unhighlight-d (highlight-style-delta "white" #f))
  
  )

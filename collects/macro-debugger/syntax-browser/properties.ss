
(module properties mzscheme
  (require "interfaces.ss"
           "util.ss"
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  (provide properties-view%)

  ;; properties-view%
  (define properties-view%
    (class* object% ()
      (init parent)
      (define selected-syntax #f)
      
      (define tab-choices (get-tab-choices))
      (define tab-panel (new tab-panel% 
                             (choices (map car tab-choices))
                             (parent parent)
                             (callback (lambda _ (refresh)))))
      
      (define text (new text%))
      (send text set-styles-sticky #f)
      (define ecanvas (new editor-canvas% (editor text) (parent tab-panel)))

      (define/public (set-syntax stx)
        (set! selected-syntax stx)
        (refresh))
      
      ;; get-tab-choices : (listof (cons string thunk))
      ;; Override to add or remove panels
      (define/public (get-tab-choices)
        (list (cons "Term" (lambda () (display-meaning-info)))
              (cons "Syntax Object" (lambda () (display-stxobj-info)))))
      
      (define/private (refresh)
        (send* text
          (lock #f)
          (begin-edit-sequence)
          (erase))
        (when (syntax? selected-syntax)
          (let ([tab (send tab-panel get-item-label (send tab-panel get-selection))])
            (cond [(assoc tab tab-choices) => (lambda (p) ((cdr p)))]
                  [else (error 'properties-view%:refresh "internal error: no such tab: ~s" tab)])))
        (send* text
          (end-edit-sequence)
          (lock #t)
          (scroll-to-position 0)))

      (define/pubment (display-meaning-info)
        (when (and (identifier? selected-syntax)
                   (uninterned? (syntax-e selected-syntax)))
          (display "Uninterned symbol!\n\n" key-sd))
        (display-binding-info)
        (inner (void) display-meaning-info))
      
      
      (define/private (display-binding-info)
        (display "Apparent identifier binding\n" key-sd)
        (unless (identifier? selected-syntax)
          (display "Not applicable\n\n" n/a-sd))
        (when (identifier? selected-syntax)
          (if (eq? (identifier-binding selected-syntax) 'lexical)
              (display "lexical (all phases)\n" #f)
              (for-each (lambda (p) (display-binding-kvs (car p) ((cdr p) selected-syntax)))
                        binding-properties))
          (display "\n" #f)))
      
      (define/private (display-binding-kvs k v)
        (display k sub-key-sd)
        (display "\n" #f)
        (cond [(eq? v #f)
               (display "  top-level or unbound\n" #f)]
              [(list? v)
               (display-subkv "  defined in" (mpi->string (list-ref v 0)))
               (display-subkv "  as" (list-ref v 1))
               (display-subkv "  imported from" (mpi->string (list-ref v 2)))
               (display-subkv "  as" (list-ref v 3))
               (if (list-ref v 4)
                   (display "  via define-for-syntax" sub-key-sd))]))
        
      (define/pubment (display-stxobj-info)
        (display-source-info)
        (display-extra-source-info)
        (inner (void) display-stxobj-info)
        (display-symbol-property-info))

      (define/private (display-source-info)
        (define s-source (syntax-source selected-syntax))
        (define s-line (syntax-line selected-syntax))
        (define s-column (syntax-column selected-syntax))
        (define s-position (syntax-position selected-syntax))
        (define s-span0 (syntax-span selected-syntax))
        (define s-span (if (zero? s-span0) #f s-span0))
        (display "Source location\n" key-sd)
        (if (or s-source s-line s-column s-position s-span)
            (begin
              (display-subkv "source" (prettify-source s-source))
              (display-subkv "line" s-line)
              (display-subkv "column" s-column)
              (display-subkv "position" s-position)
              (display-subkv "span" s-span0))
            (display "No source location available\n" n/a-sd))
        (display "\n" #f))
      
      (define/private (display-extra-source-info)
        (display "Built-in properties\n" key-sd)
        (display-subkv "source module"
                       (let ([mod (syntax-source-module selected-syntax)])
                         (and mod (mpi->string mod))))
        (display-subkv "original?" (syntax-original? selected-syntax))
        (display "\n" #f))
      
      (define/private (display-symbol-property-info)
        (let ([keys (syntax-property-symbol-keys selected-syntax)])
          (display "Additional properties\n" key-sd)
          (when (null? keys)
            (display "No additional properties available.\n" n/a-sd))
          (when (pair? keys)
            (for-each (lambda (k) (display-subkv k (syntax-property selected-syntax k)))
                      keys))))
      
      (define/private (display-kv key value)
        (display (format "~a~n" key) key-sd)
        (display (format "~s~n~n" value) #f))

      (define/public (display-subkv k v)
        (display (format "~a: " k) sub-key-sd)
        (display (format "~a~n" v) #f))
      
      (define/private (display item sd)
        (let ([p0 (send text last-position)])
          (send text insert item)
          (let ([p1 (send text last-position)])
            (send text change-style sd p0 p1))))

      (send text lock #t)
      (super-new)))
  
  ;; lift/id : (identifier -> void) 'a -> void
  (define (lift/id f)
    (lambda (stx) (when (identifier? stx) (f stx))))

  ;; binding-properties : (listof (cons string (syntax -> any)))
  (define binding-properties
    (list (cons "in the standard phase"
                (lift/id identifier-binding))
          (cons "in the transformer phase (\"for-syntax\")"
                (lift/id identifier-transformer-binding))
          (cons "in the template phase (\"for-template\")"
                (lift/id identifier-template-binding))))
  
  (define (uninterned? s)
    (not (eq? s (string->symbol (symbol->string s)))))

  (define (prettify-source s)
    (cond [(is-a? s editor<%>)
           'editor]
          [else s]))
  
  ;; Styles
    
  (define key-sd
    (let ([sd (new style-delta%)])
      (send sd set-delta-foreground "blue")
      (send sd set-weight-on 'bold)
      sd))

  (define sub-key-sd
    (let ([sd (new style-delta%)])
      (send sd set-delta-foreground "blue")
      sd))
  
  (define n/a-sd
    (let ([sd (new style-delta%)])
      (send sd set-delta-foreground "gray")
      sd))
  
  )


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
      
      (define tab-panel (new tab-panel% 
                             (choices (list "Binding" "Source" "Properties"))
                             (parent parent)
                             (callback (lambda _ (refresh)))))
      (define text (new text%))
      (send text set-styles-sticky #f)
      (define ecanvas (new editor-canvas% (editor text) (parent tab-panel)))

      (define/public (set-syntax stx)
        (set! selected-syntax stx)
        (refresh))
      
      (define/private (refresh)
        (send* text
          (lock #f)
          (begin-edit-sequence)
          (erase))
        (when (syntax? selected-syntax)
          (let ([s (send tab-panel get-item-label (send tab-panel get-selection))])
            (cond [(equal? s "Binding")
                   (display-binding-info)]
                  [(equal? s "Source")
                   (display-source-info)]
                  [(equal? s "Properties")
                   (display-properties)])))
        (send* text
          (end-edit-sequence)
          (lock #t)
          (scroll-to-position 0)))

      (define/private (display-binding-info)
        (for-each (lambda (p) (display-binding-kv (car p) ((cdr p) selected-syntax)))
                  binding-properties))

      (define/private (display-binding-kv k v)
        (display (format "~a~n" k) key-sd)
        (cond [(eq? v 'lexical)
               (display "lexical\n" #f)]
              [(eq? v #f)
               (display "#f (top-level or unbound)\n" #f)]
              [(list? v)
               (display-subkv "source module" (mpi->string (list-ref v 0)))
               (display-subkv "source id" (list-ref v 1))
               (display-subkv "nom. module" (mpi->string (list-ref v 2)))
               (display-subkv "nom. id" (list-ref v 3))
               (if (list-ref v 4)
                   (display-subkv "phase" "via define-for-syntax"))]
              [(void? v)
               (display "Not applicable\n" n/a-sd)])
        (display "\n" #f))

      (define/private (display-subkv k v)
        (display (format "~a: " k) sub-key-sd)
        (display (format "~a~n" v) #f))
      
      (define/private (display-source-info)
        (for-each (lambda (p) (display-subkv (car p) ((cdr p) selected-syntax)))
                  source-properties))
      
      (define/private (display-properties)
        (let ([keys (syntax-property-symbol-keys selected-syntax)])
          (if (null? keys)
              (display "No properties available" n/a-sd)
              (for-each (lambda (k) (display-kv k (syntax-property selected-syntax k)))
                        keys))))

      (define/private (display-kv key value)
        (display (format "~a~n" key) key-sd)
        (display (format "~s~n~n" value) #f))
      
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
    (list (cons "identifier-binding"
                (lift/id identifier-binding))
          (cons "identifier-transformer-binding"
                (lift/id identifier-transformer-binding))
          (cons "identifier-template-binding"
                (lift/id identifier-template-binding))))

  ;; source-properties : (listof (cons string (syntax -> any)))
  (define source-properties
    (list (cons "syntax-source" syntax-source)
          (cons "syntax-source-module"
                (lambda (stx) (mpi->string (syntax-source-module stx))))
          (cons "syntax-line" syntax-line)
          (cons "syntax-position" syntax-position)
          (cons "syntax-span" syntax-span)
          (cons "syntax-original?" syntax-original?)))

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
(module display-break-stuff mzscheme
  
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "marks.ss")

    
  (provide display-break-stuff)
  
  ;; display-break-stuff : show the information associated with a breakpoint.  Useful for 
  ;; people building steppers for new languages
  (define (display-break-stuff break-number mark-set break-kind returned-value-list)
    (define f (instantiate frame% () [label "breakpoint information"] [width 300] [height 500]))
    (define ec (instantiate editor-canvas% () [parent f]))
    (define t (instantiate text% ()))
    (send ec set-editor t)
    (send f show #t)
    (send t insert (format "breakpoint number: ~v\n\n" break-number))
    (send t insert (format "break-kind: ~v \n\n" break-kind))
    (send t insert "marks:\n")
    (if mark-set
        (map (display-breakpoint-mark t) (extract-mark-list mark-set))
        (send t insert " no mark-set!\n"))
    (send t insert "\nreturned-value-list:\n")
    (send t insert (format " ~v\n" returned-value-list)))
  
  (define ((display-breakpoint-mark t) mark)
    (let* ([em (expose-mark mark)]
           [source (car em)]
           [label (cadr em)]
           [binding-set (caddr em)])
      (send t insert "\n")
      (send t insert (format " label: ~v\n" label))
      ;; we really want one of those nice collapsible syntax-viewer thingies here:
      (send t insert (format " source : ~v\n" (syntax-object->datum source)))
      ;; here too, though this isn't a syntax object.
      (send t insert (format " bindings: ~v\n" binding-set))))
  )
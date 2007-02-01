
(module controller mzscheme
  (require (lib "class.ss")
           "interfaces.ss"
           "partition.ss")
  
  (provide syntax-controller%)
  
  ;; syntax-controller%
  (define syntax-controller%
    (class* object% (syntax-controller<%>
                     syntax-pp-snip-controller<%>
                     color-controller<%>)
      (init-field (primary-partition (new-bound-partition)))
      (init-field (properties-controller #f))

      (define colorers null)
      (define selection-listeners null)
      (define selected-syntax #f)
      (define identifier=?-listeners null)

      ;; syntax-controller<%> Methods

      (define/public (select-syntax stx)
        (set! selected-syntax stx)
        (send properties-controller set-syntax stx)
        (for-each (lambda (c) (send c select-syntax stx)) colorers)
        (for-each (lambda (p) (p stx)) selection-listeners))

      (define/public (get-selected-syntax) selected-syntax)

      (define/public (get-properties-controller) properties-controller)
      (define/public (set-properties-controller pc)
        (set! properties-controller pc))

      (define/public (add-view-colorer c)
        (set! colorers (cons c colorers))
        (send c select-syntax selected-syntax))

      (define/public (get-view-colorers) colorers)

      (define/public (add-selection-listener p)
        (set! selection-listeners (cons p selection-listeners)))
      
      (define/public (on-update-identifier=? name id=?)
        (set! secondary-partition 
              (and id=? (new partition% (relation id=?))))
        (for-each (lambda (c) (send c refresh)) colorers)
        (for-each (lambda (f) (f name id=?)) identifier=?-listeners))
      
      (define/public (add-identifier=?-listener f)
        (set! identifier=?-listeners
              (cons f identifier=?-listeners)))

      (define/public (erase)
        (set! colorers null))

      ;; syntax-pp-snip-controller<%> Methods

      (define/public (on-select-syntax stx)
        (select-syntax stx))

      ;; color-controller<%> Methods

      (define secondary-partition #f)

      (define/public (get-primary-partition) primary-partition)
      (define/public (get-secondary-partition) secondary-partition)

      ;; Initialization
      (super-new)
      ))
  
  )

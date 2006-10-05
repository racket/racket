
(module syntax-browser mzscheme
  (require (lib "class.ss")
           "interfaces.ss"
           "widget.ss")
  (provide browse-syntax
           browse-syntaxes
           syntax-browser<%>
           make-syntax-browser)
  
  ;; browse-syntax : syntax -> void
  (define (browse-syntax stx)
    (browse-syntaxes (list stx)))
  
  ;; browse-syntaxes : (list-of syntax) -> void
  (define (browse-syntaxes stxs)
    (let ((w (make-syntax-browser)))
      (for-each (lambda (stx)
                  (send w add-syntax stx)
                  (send w add-separator))
                stxs)))
  
  ;; make-syntax-browser : -> syntax-browser<%>
  (define (make-syntax-browser)
    (let* ([view (new syntax-browser-frame%)])
      (send view show #t)
      (send view get-widget)))
  
  )

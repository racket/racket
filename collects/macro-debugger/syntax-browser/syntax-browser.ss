
(module syntax-browser mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "prefs.ss"
           "syntax-snip.ss"
           "widget.ss")
  (provide browse-syntax
           browse-syntaxes
           syntax-browser<%>
           make-syntax-browser
           syntax-snip)
  
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
  
  ;; syntax-snip : syntax -> snip
  (define (syntax-snip stx)
    (new super-syntax-snip% (syntax stx)))
  
;  ;; syntaxes-snip : syntaxes -> snip
;  (define (syntaxes-snip stxs)
;    (let* ([controller (new syntax-controller%)]
;           [view (new syntax-snip% (controller controller))])
;      (let loop ([stxs stxs])
;        (cond [(null? stxs) (void)]
;              [(null? (cdr stxs))
;               (send controller add-syntax (car stxs))]
;              [else
;               (send controller add-syntax (car stxs))
;               #;(send controller add-separator)
;               (loop (cdr stxs))]))
;      view))
  
  )

#lang racket/base
(require racket/class
         racket/gui/base
         racket/contract/base
         framework
         "by-source.rkt"
         "by-list.rkt"
         "by-installed.rkt"
         mrlib/terminal
         string-constants)

(provide
 (contract-out
  [make-pkg-gui
   (->* ()
        (#:wrap-terminal-action 
         (-> (-> any) any) 
         #:initial-tab 
         (or/c 'by-source 'from-list 'installed))
        (is-a?/c top-level-window<%>))]
  [make-pkg-installer
   (->* ()
        (#:parent
         (or/c #f (is-a?/c top-level-window<%>))
         #:wrap-terminal-action 
         (-> (-> any) any)
         #:package-to-offer 
         (or/c #f string?))
        (is-a?/c top-level-window<%>))]))

(define pkg-gui-frame% 
  (class (frame:standard-menus-mixin
          (frame:status-line-mixin
           frame:basic%))
    (super-new)
    ;; no menu separator:
    (define/override (edit-menu:between-select-all-and-find m) (void))))

(define (make-pkg-installer #:parent 
                            [parent #f]
                            #:wrap-terminal-action 
                            [wrap-terminal-action (λ (t) (t))]
                            #:package-to-offer
                            [package-to-offer #f])
  
  (define allow-close? #t)
  
  (define dlg
    (new (class dialog%
           (define/augment (can-close?)
             allow-close?)
           (super-new
            [parent parent]
            [label "Package Installer"]))))

  (define terminal #f)
  (define (in-terminal-panel abort-label thunk)
    (send dlg begin-container-sequence)
    (when terminal (send terminal close))
    (define t (in-terminal 
               #:abort-label abort-label
               #:canvas-min-height 400
               #:container dlg
               #:close-button? #f
               (λ (cust parent) (wrap-terminal-action thunk))))
    (send dlg reflow-container)
    (unless terminal (send dlg center))
    (set! terminal t)
    (disallow-close)
    (send dlg end-container-sequence)
    (yield (send t can-close-evt))
    (allow-close))
  
  (define (disallow-close)
    (set! allow-close? #f)
    (send close enable #f))
  
  (define (allow-close)
    (set! allow-close? #t)
    (send close enable #t))

  (define by-source-panel
    (new by-source-panel% 
         [parent dlg]
         [in-terminal in-terminal-panel]
         [text-field-initial-value package-to-offer]))
  
  (define close (new button% 
                     [label (string-constant close)] 
                     [parent (send by-source-panel get-button-panel)]
                     [callback
                      (λ (x y)
                        (send dlg show #f))]))
  
  (send dlg show #t)

  dlg)

(define (make-pkg-gui #:wrap-terminal-action 
                      [wrap-terminal-action (lambda (thunk) (thunk))]
                      #:initial-tab [initial-tab 'installed])

  (define frame
    (new pkg-gui-frame%
         [label "Package Manager"]
         [width 800]
         [height 600]))

  (define (update-sel-panel-active)
    (define old (send sel-panel active-child))
    (define new (list-ref (send sel-panel get-children) (send sel-tab get-selection)))
    (unless (eq? new old)
      (send sel-panel active-child new)))
  
  (define sel-tab
    (new tab-panel%
         [parent (send frame get-area-container)]
         [choices (list (string-constant install-pkg-install-by-source)
                        (string-constant install-pkg-install-from-list)
                        (string-constant install-pkg-install-installed))]
         [callback (lambda (t e)
                     (update-sel-panel-active))]))

  (define sel-panel
    (new panel:single%
         [parent sel-tab]))

  (define terminal #f)
  (define (in-terminal-panel abort-label thunk)
    (when terminal 
      (send terminal close))
    (define t (in-terminal 
               #:abort-label abort-label
               #:container (send frame get-area-container)
               (λ (cust parent) (wrap-terminal-action thunk))))
    (set! terminal t)
    (send sel-tab enable #f)
    (yield (send t can-close-evt))
    (send sel-tab enable #t))

  (new by-source-panel% 
       [parent sel-panel]
       [in-terminal in-terminal-panel])
  (new by-list-panel% 
       [parent sel-panel]
       [in-terminal in-terminal-panel])
  (new by-installed-panel%
       [parent sel-panel]
       [in-terminal in-terminal-panel])
  
  (send sel-tab set-selection
        (case initial-tab
          [(by-source) 0]
          [(from-list) 1]
          [(installed) 2]))
  (update-sel-panel-active)
  
  (send frame show #t)

  frame)

(module+ main
  (void (make-pkg-installer)))

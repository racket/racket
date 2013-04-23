#lang racket/base
(require racket/class
         racket/gui/base
         framework
         "by-source.rkt"
         "by-list.rkt"
         "by-installed.rkt"
         mrlib/terminal
         string-constants)

(define frame
  (new frame:basic%
       [label "Package Manager"]
       [width 800]
       [height 600]))

(define sel-tab
  (new tab-panel%
       [parent (send frame get-area-container)]
       [choices (list (string-constant install-pkg-install-by-source)
                      (string-constant install-pkg-install-from-list)
                      (string-constant install-pkg-install-installed))]
       [callback (lambda (t e)
                   (define old (send sel-panel active-child))
                   (define new (list-ref panels (send t get-selection)))
                   (unless (eq? new old)
                     (send sel-panel active-child new)))]))

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
             (λ (cust parent) (thunk))))
  (set! terminal t)
  (send sel-tab enable #f)
  (yield (send t can-close-evt))
  (send sel-tab enable #t))

(define panels
  (list
   (new by-source-panel% 
        [parent sel-panel]
        [in-terminal in-terminal-panel])
   (new by-list-panel% 
        [parent sel-panel]
        [in-terminal in-terminal-panel])
   (new by-installed-panel%
        [parent sel-panel]
        [in-terminal in-terminal-panel])))

(send frame show #t)

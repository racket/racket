(module gui frtime
  (require frtime/gui/fred)

  (module test racket/base)
  
  (define frame (new ft-frame% [label "GUI"] [min-height 150] [min-width 200] [shown #t]))
  
  (define (make-button str)
    (send (new ft-button% [parent frame] [label str]) get-value-e))
  
  (define (make-message str~)
    (new ft-message% [parent frame] [label str~]
         [stretchable-height #t]
         [stretchable-width #t]))
  
  (define (make-gauge rng val~)
    (new ft-gauge% [parent frame] [label ""] [range rng] [stretchable-width #t]
         [value val~]))
  
  (define (make-text str)
    (send (new ft-text-field% [parent frame] [label str] [init-value ""])
          get-value-b))
  
  (define (make-choice str los)
    (send (new ft-choice% [parent frame] [label str] [choices los])
          get-selection-b))

  (define (make-slider str min max init)
    (send (new ft-slider% [parent frame] [min-value min] [max-value max]
               [init-value init] [label str])
          get-value-b))
  
  (define make-check-box
    (opt-lambda (str [val #f])
      (send (new ft-check-box% [parent frame] [label str] [value val])
            get-value-b)))
  
  (define fresh-window
    (let ([first #t])
      (lambda ()
        (if first
            (set! first #f)
            (begin
              (set! frame (new ft-frame% [label "GUI"] [min-height 150] [min-width 200]
                               [shown #t])))))))
  
  (provide (all-defined-out)))

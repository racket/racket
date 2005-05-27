(module gui-defs mzscheme
  (require (lib "unitsig.ss") (lib "class.ss") (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           "checksigs.ss")
  (provide gui-defs@)
  (define gui-defs@
    (unit/sig defs^ (import)

      (define (run-thunk th)
        (parameterize ([current-eventspace (make-eventspace)])
          (queue-callback th)))

      ;; string (list string (listof string)) (union (listof string) #f) -> void
      (define (show-ok title captions details ok-thunk)
        (letrec ([frame
                  (instantiate frame% ()
                    [label title]
                    [min-width 50]
                    [alignment '(left center)]
                    [stretchable-height #f]
                    [stretchable-width #f]
                    [style '(no-resize-border)])]
                 [main-panel (instantiate vertical-panel% ()
                               [parent frame]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [alignment '(center center)])]
                 [panel-sep 4]
                 [msg-width 50]
                 [make-make-panel
                  (lambda (c%)
                    (lambda ()
                      (instantiate c% ()
                        [parent main-panel]
                        [vert-margin panel-sep]
                        [alignment '(center center)])))]
                 [make-hpanel (make-make-panel horizontal-panel%)]
                 [make-vpanel (make-make-panel vertical-panel%)]
                 [row-panel (make-vpanel)]
                 [make-msg
                  (lambda (msg panel)
                    (instantiate message% ()
                      [min-width msg-width] [label msg] [parent panel]))]
                 [status-msgs (map (lambda (msg) (make-msg msg row-panel))
                                   captions)]
                 [details-panel #f]
                 [showing-details #f]
                 [details-text "Details "]
                 [show-details-button-text (string-append details-text ">>")]
                 [hide-details-button-text (string-append details-text "<<")]
                 [hide-details
                  (lambda ()
                    (set! showing-details #f)
                    (send main-panel delete-child details-panel)
                    (send details-button set-label show-details-button-text)
                    (set! details-panel #f))]
                 [show-details
                  (lambda ()
                    (set! showing-details #t)
                    (send details-button set-label hide-details-button-text)
                    (set! details-button-callback hide-details)
                    (unless details-panel
                      (set! details-panel
                            (instantiate vertical-panel% ()
                              (parent main-panel)
                              (style '(border))
                              (border 2)
                              (vert-margin panel-sep)
                              (alignment '(left center))))
                      (for-each (lambda (d) (make-msg d details-panel))
                                details)))]
                 [details-button-callback
                  (lambda (e bv)
                    (if showing-details (hide-details) (show-details)))]
                 [buttons-panel (make-hpanel)]
                 [ok-button (instantiate button% ()
                              [label "OK"] [min-width 20] [parent buttons-panel]
                              [callback (lambda (b ev)
                                          (send frame show #f)
                                          (ok-thunk))])]
                 [spacer
                  (and details
                       (instantiate message% ()
                         [min-width 20] [label ""] [parent buttons-panel]))]
                 [details-button
                  (and details
                       (not (null? details))
                       (instantiate button% ()
                         [label show-details-button-text]
                         [min-width 20]
                         [parent buttons-panel]
                         [callback details-button-callback]))])
          (send frame center)
          (send frame show #t)))

      (define (show-error-ok title caption)
        (show-ok title
                 (list (format (string-constant vc-error-format) caption))
                 #f
                 void))

      (define (make-wait-dialog parent title caption close-fun)
        (let ([dialog (instantiate dialog% ()
                        [label title] [parent parent] [width 100] [height 50]
                        [stretchable-width #t] [stretchable-height #t])])
          (instantiate message% () [label caption] [parent dialog])
          (instantiate button% ()
            [label (string-constant cancel)]
            [parent dialog]
            [callback (lambda (button ce) (close-fun) (send dialog show #f))])
          dialog))

      (define (show-wait-dialog dialog)
        (send dialog center)
        (thread (lambda () (send dialog show #t)))
        (send dialog focus))

     (define (hide-wait-dialog dialog)
       (send dialog show #f)))))

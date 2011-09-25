#lang racket/base
(require racket/class
         unstable/class-iop
         unstable/gui/notify
         racket/gui/base
         framework
         mrlib/hierlist
         "interfaces.rkt"
         "config.rkt"
         "model2rml.rkt"
         "rml.rkt")

(provide make-view-frame)

(define style-map rackunit-style-map)

#|

To avoid getting sequence contract violations from editors, all editor
mutations should be done in the eventspace thread.

Can an update to a result<%> occur before its view link has been
created? Answer = yes, quite easily it seems (I tried it.)

See 'do-model-update': we yield if the view-link hasn't been created
yet, since there should be a callback queued waiting to create it.

With the 'queue-callback' calls and the one 'yield' call in place, I'm
no longer able to trigger the race condition.

----

FIXME:

Another problem: If tests are still running and a gui element "goes
away", then get errors. Eg, run (test/gui success-and-failure-tests)
and then immediately close the window.

Why are these things actually disappearing, though? Shouldn't they
still be there, just not visible?

|#


;; View
(define view%
  (class* object% (view<%>)
    (init-field parent
                controller)
    (super-new)

    (define editor (new ext:text% (style-map rackunit-style-map)))
    (define renderer
      (new model-renderer%
           (controller controller)
           (editor editor)))

    (define eventspace
      (send (send parent get-top-level-window) get-eventspace))

    (define -hpane (new panel:horizontal-dragable% (parent parent)))
    (define -lpane (new vertical-pane% (parent -hpane)))
    (define -rpane (new vertical-pane% (parent -hpane)))
    (define -details-canvas 
      (new canvas:wide-snip% (parent -rpane) (editor editor)))

    (define tree-view
      (new model-tree-view%
           (parent -lpane) ;; tree-panel
           (view this)
           (controller controller)))

    ;; Update management
    ;; Do adds in order, then updates in any order (hash).

    ;; add-queue : (listof (-> void))
    (define add-queue null)

    ;; update-queue : (imm-hashof model<%> #t)
    (define update-queue '#hasheq())

    ;; update-lock : semaphore
    (define update-lock (make-semaphore 1))

    ;; update-timer : timer%
    (define update-timer
      (new timer% (notify-callback (lambda () (process-updates)))))

    (send editor lock #t)
    (with-handlers ([exn:fail? void])
      (send -hpane set-percentages VIEW-PANE-PERCENTS))

    (send/i controller controller<%> register-view this)

    ;; View Links

    (define/private (get-view-link model)
      (send tree-view get-view-link model))

    ;; Methods

    (define/private (get-selected-model)
      (send/i controller controller<%> get-selected-model))

    (send/i controller controller<%> listen-selected-model
            (lambda (model)
              (parameterize ((current-eventspace eventspace))
                (queue-callback
                 (lambda ()
                   (let ([view-link (get-view-link model)])
                     (send view-link select #t))
                   (show-model model))))))

    ;; Update Management

    ;; create-view-link : model suite-result<%>/#f -> void
    (define/public (create-view-link model parent)
      (let ([proc (lambda () (send tree-view create-view-link model parent))])
        (semaphore-wait update-lock)
        (set! add-queue (cons proc add-queue))
        (updates-pending!)
        (semaphore-post update-lock)))

    ;; queue-for-update : model -> void
    (define/public (queue-for-update model)
      (semaphore-wait update-lock)
      (set! update-queue (hash-set update-queue model #t))
      (updates-pending!)
      (semaphore-post update-lock))

    ;; updates-pending! : -> void
    (define/private (updates-pending!)
      (send update-timer start 50 #t))

    ;; process-updates : -> void
    ;; ** Must be called from eventspace thread.
    (define/private (process-updates)
      (let-values ([(adds updates) (grab+clear-update-queue)])
        (send (send tree-view get-editor) begin-edit-sequence #f)
        (for ([add (in-list adds)])
          (add))
        (for ([model (in-hash-keys updates)])
          (do-model-update model))
        (send (send tree-view get-editor) end-edit-sequence)))

    ;; grab+clear-update-queue : -> (values list hash)
    ;; ** Must be called from eventspace thread.
    (define/private (grab+clear-update-queue)
      (semaphore-wait update-lock)
      (begin0
          (values (reverse add-queue)
                  update-queue)
        (set! add-queue null)
        (set! update-queue '#hasheq())
        (semaphore-post update-lock)))

    ;; do-model-update : model<%> -> void
    ;; ** Must be called from eventspace thread.
    (define/private (do-model-update model)
      (let ([view-link (get-view-link model)])
        (unless view-link
          ;; should not be possible
          (error 'rackunit-gui "internal error: no view-link"))
        (send tree-view update-item view-link)
        (when (eq? model (get-selected-model))
          (show-model model))))

    ;; Update display

    ;; show-model : result<%> -> void
    ;; Displays the result in the Details area.
    ;; ** Must be called from eventspace thread.
    (define/private (show-model model)
      (send* editor
        (begin-edit-sequence)
        (lock #f)
        (erase))
      (send renderer render-model/long model)
      (send* editor
        (lock #t)
        (end-edit-sequence)
        (scroll-to-position 0)))

    ;; Shutdown

    ;; shutdown : -> void
    ;; Notifies the controller that the view has hung up.
    (define/public (shutdown)
      (send/i controller controller<%> on-view-shutdown))
    ))


;; tree-view% <: hierarchical-list%
(define model-tree-view%
  (class* hierarchical-list% ()
    (init-field view
                controller)
    (super-new (style '(auto-hscroll)))

    (inherit get-items)

    ;; View Link

    (define model=>view-link (make-hasheq))

    (define/public (set-view-link model item)
      (hash-set! model=>view-link model item))
    (define/public (get-view-link model)
      (hash-ref model=>view-link model #f))

    ;; Behavior

    (define/override (on-select item)
      (let [(model (send item user-data))]
        (send/i controller controller<%> set-selected-model model)))

    (define/override (on-double-select item)
      (when (is-a? item hierarchical-list-compound-item<%>)
        (if (send item is-open?)
            (send item close)
            (send item open))))

    (define/private (ensure-tree-visible model)
      (let* [(parent (send model get-parent))
             (parent-view-link (and parent (get-view-link parent)))]
        (when (and parent (not (send parent-view-link is-open?)))
          (ensure-tree-visible parent)
          (send parent-view-link open))))

    ;; Construction

    ;; create-view-link : result<%> suite-result<%>/#f-> void
    (define/public (create-view-link model parent)
      (let* ([parent-link
              (if parent
                  (get-view-link parent)
                  this)]
             [view-link
              (cond [(is-a? model suite<%>)
                     (send parent-link new-list)]
                    [(is-a? model case<%>)
                     (send parent-link new-item)])])
        (initialize-view-link view-link model)
        (when (and (is-a? model suite<%>) (not parent))
          (send view-link open))))

    ;; initialize-view-link : result<%> (U compound-item% item%) -> void
    (define/private (initialize-view-link item model)
      (set-view-link model item)
      (send item user-data model)
      (insert-text (send item get-editor)
                   (send model get-name)
                   (send style-map get-style
                         (if (is-a? model suite<%>)
                             'bold
                             'normal))))

    ;; update-item : item% -> void
    (define/public (update-item view-link)
      (let* ([editor (send view-link get-editor)]
             [model (send view-link user-data)]
             [name (send/i model result<%> get-name)]
             [style-name
              (cond [(not (send/i model result<%> finished?)) 'test-unexecuted]
                    [(send/i model result<%> success?) 'test-success]
                    [(send/i model result<%> failure?) 'test-failure]
                    [(send/i model result<%> error?) 'test-error])]
             [style (send/i style-map style-map<%> get-style style-name)]
             [output? (send/i model result<%> has-output?)]
             [trash? (send/i model result<%> has-trash?)])
        (send editor begin-edit-sequence #f)
        (send editor delete (string-length name) (send editor last-position) #f)
        (when (or output? trash?)
          (send editor insert
                (output-icon)
                (string-length name)
                'same
                #f))
        (send editor change-style style 0 (send editor last-position) #f)
        (send editor end-edit-sequence)))))


;; view-frame% <: frame%
(define view-frame%
  (class (frame:standard-menus-mixin 
           (frame:basic-mixin frame%))

    (init-field controller)
    (init [width (pref:width)]
          [height (pref:height)])

    (inherit get-help-menu
             get-width
             get-height
             get-menu-bar
             get-area-container)

    (define-syntax override-false
      (syntax-rules ()
        [(override-false name ...)
         (begin (define/override (name . _) #f) ...)]))

    (override-false file-menu:create-new?
                    file-menu:create-open?
                    file-menu:create-open-recent?
                    file-menu:create-revert?
                    file-menu:create-save?
                    file-menu:create-save-as?
                    file-menu:create-print?
                    edit-menu:create-undo?
                    edit-menu:create-redo?
                    edit-menu:create-cut?
                    edit-menu:create-paste?
                    edit-menu:create-clear?
                    edit-menu:create-find?
                    edit-menu:create-preferences?)

    (define/augment (on-close)
      (pref:width (get-width))
      (pref:height (get-height))
      (send view shutdown)
      (inner (void) on-close))

    (super-new (width width) (height height))
    (send (get-help-menu) delete)
    (let ([rackunit-menu
           (new menu%
                (label "RackUnit")
                (parent (get-menu-bar)))])
      (menu-option/notify-box rackunit-menu
                              "Lock"
                              (get-field locked? controller)))

    (define view
      (new view%
           (controller controller)
           (parent (get-area-container))))))

;; make-view-frame : controller -> frame%
(define (make-view-frame controller)
  (let ([frame 
         (new view-frame%
              (label FRAME-LABEL)
              (controller controller))])
    (send frame show #t)
    frame))

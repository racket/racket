
(module hiding-panel mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "list.ss")
           (lib "boundmap.ss" "syntax")
           "util.ss"
           "../model/synth-engine.ss"
           "../syntax-browser/util.ss")
  (provide macro-hiding-prefs-widget%)

  (define mode:disable "Disable")
  (define mode:standard "Standard")
  (define mode:custom "Custom ...")

  (define (make-policy hide-mzscheme?
                       hide-libs?
                       hide-contracts?
                       hide-transformers?
                       specialized-policies)
    (lambda (id)
      (define now (phase))
      (define binding
        (cond [(= now 0) (identifier-binding id)]
              [(= now 1) (identifier-transformer-binding id)]
              [else #f]))
      (define-values (def-mod def-name nom-mod nom-name)
        (if (pair? binding)
            (values (car binding)
                    (cadr binding)
                    (caddr binding)
                    (cadddr binding))
            (values #f #f #f #f)))
      (let/ec return
        (let loop ([policies specialized-policies])
          (when (pair? policies)
            ((car policies) id binding return)
            (loop (cdr policies))))
        (cond [(and hide-mzscheme? (symbol? def-mod)
                    (regexp-match #rx"^#%" (symbol->string def-mod)))
               #f]
              [(and hide-libs? def-mod
                    (lib-module? def-mod))
               #f]
              [(and hide-contracts? def-name
                    (regexp-match #rx"^provide/contract-id-"
                                  (symbol->string def-name)))
               #f]
              [(and hide-transformers? (positive? now))
               #f]
              [else #t]))))

  (define standard-policy
    (make-policy #t #t #t #t null))

  ;; macro-hiding-prefs-widget%
  (define macro-hiding-prefs-widget%
    (class object%
      (init parent)
      (init-field stepper)
      (init-field config)

      (define/public (get-policy)
        (let ([mode (get-mode)])
          (cond [(not (macro-hiding-enabled?)) #f]
                [(equal? mode mode:standard) standard-policy]
                [(equal? mode mode:custom) (get-custom-policy)])))

      (define/private (get-custom-policy)
        (let ([hide-mzscheme? (send box:hide-mzscheme get-value)]
              [hide-libs? (send box:hide-libs get-value)]
              [hide-contracts? (send box:hide-contracts get-value)]
              [hide-transformers? (send box:hide-phase1 get-value)]
              [specialized-policies (get-specialized-policies)])
          (make-policy hide-mzscheme?
                       hide-libs?
                       hide-contracts?
                       hide-transformers?
                       specialized-policies)))

      (define super-panel
        (new vertical-panel%
             (parent parent)
             (stretchable-height #f)))
      (define top-line-panel
        (new horizontal-panel%
             (parent super-panel)
             (alignment '(left center))
             (stretchable-height #f)))
      (define customize-panel
        (new horizontal-panel%
             (parent super-panel)
             (stretchable-height #f)
             (alignment '(left top))
             (style '(deleted))))
      (define left-pane
        (new vertical-pane%
             (parent customize-panel)
             (stretchable-width #f)
             (alignment '(left top))))
      (define right-pane
        (new vertical-pane%
             (parent customize-panel)))

      (define mode-selector
        (choice/notify-box
         top-line-panel
         "Macro hiding: "
         (list mode:disable mode:standard mode:custom)
         (get-field macro-hiding-mode config)))
      (define top-line-inner-panel
        (new horizontal-panel%
             (parent top-line-panel)
             (alignment '(right center))
             (style '(deleted))))

      (define/private (get-mode)
        (send config get-macro-hiding-mode))

      (define/private (macro-hiding-enabled?)
        (let ([mode (get-mode)])
          (or (equal? mode mode:standard)
              (and (equal? mode mode:custom)
                   (send box:hiding get-value)))))

      (define/private (ensure-custom-mode)
        (unless (equal? (get-mode) mode:custom)
          (send config set-macro-hiding-mode mode:custom)))

      (define/private (update-visibility)
        (let ([customizing (equal? (get-mode) mode:custom)])
          (send top-line-panel change-children
                (lambda (children)
                  (append (remq top-line-inner-panel children)
                          (if customizing (list top-line-inner-panel) null))))
          (send super-panel change-children
                (lambda (children)
                  (append (remq customize-panel children)
                          (if (and customizing (send box:edit get-value))
                              (list customize-panel)
                              null))))))

      (send config listen-macro-hiding-mode
            (lambda (value)
              (update-visibility)
              (force-refresh)))

      (define box:hiding
        (new check-box%
             (label "Enable macro hiding")
             (value #t)
             (parent top-line-inner-panel)
             (callback (lambda (c e) (force-refresh)))))
      (define box:edit
        (new check-box%
             (label "Show policy editor")
             (parent top-line-inner-panel)
             (value #t)
             (callback (lambda (c e) (update-visibility)))))

      (define box:hide-mzscheme
        (new check-box%
             (label "Hide mzscheme syntax")
             (parent left-pane)
             (value #t)
             (callback (lambda (c e) (refresh)))))
      (define box:hide-libs
        (new check-box%
             (label "Hide library syntax")
             (parent left-pane)
             (value #t)
             (callback (lambda (c e) (refresh)))))
      (define box:hide-contracts
        (new check-box%
             (label "Hide contracts (heuristic)")
             (parent left-pane)
             (value #t)
             (callback (lambda (c e) (refresh)))))
      (define box:hide-phase1
        (new check-box%
             (label "Hide phase>0")
             (parent left-pane)
             (value #t)
             (callback (lambda (c e) (refresh)))))

      (define look-ctl
        (new list-box% (parent right-pane) (label "")
             (choices null) (style '(extended))
             (callback
              (lambda (c e)
                (send delete-ctl enable (pair? (send c get-selections)))))))

      (define look-button-pane
        (new horizontal-pane% (parent right-pane) (stretchable-width #f)))

      (define delete-ctl
        (new button% (parent look-button-pane) (label "Delete rule") (enabled #f)
             (callback (lambda _ (delete-selected) (refresh)))))
      (define add-hide-id-button
        (new button% (parent look-button-pane) (label "Hide macro") (enabled #f)
             (callback (lambda _ (add-hide-identifier) (refresh)))))
      (define add-show-id-button
        (new button% (parent look-button-pane) (label "Show macro") (enabled #f)
             (callback (lambda _ (add-show-identifier) (refresh)))))
      #;(new grow-box-spacer-pane% (parent right-pane))

      ;; Methods

      (define stx #f)
      (define stx-name #f)

      ;; refresh : -> void
      (define/public (refresh)
        (when (macro-hiding-enabled?)
          (send stepper refresh/resynth)))

      ;; force-refresh : -> void
      (define/private (force-refresh)
        (send stepper refresh/resynth))

      ;; set-syntax : syntax/#f -> void
      (define/public (set-syntax lstx)
        (set! stx (and (identifier? lstx) lstx))
        (when (identifier? stx)
          (let ([binding (identifier-binding stx)])
            (if (pair? binding)
                (set! stx-name (cadr binding))
                (set! stx-name (syntax-e stx)))))
        (send add-show-id-button enable (identifier? lstx))
        (send add-hide-id-button enable (identifier? lstx)))

      (define identifier-policies null)

      (define/private (get-specialized-policies)
        (map (lambda (policy)
               (define key (car policy))
               (define show? (cdr policy))
               (cond [(pair? key)
                      (lambda (id binding return)
                        (when (and (pair? binding)
                                   (equal? key (get-id-key/binding id binding)))
                          (return show?)))]
                     [else
                      (lambda (id binding return)
                        (when (module-identifier=? id key)
                          (return show?)))]))
             identifier-policies))

      (define/public (add-hide-identifier)
        (add-identifier-policy #f)
        (ensure-custom-mode))

      (define/public (add-show-identifier)
        (add-identifier-policy #t)
        (ensure-custom-mode))

      (define/private (add-identifier-policy show?)
        (when (identifier? stx)
          (let ([key (get-id-key stx)])
            (let loop ([i 0] [policies identifier-policies])
              (cond [(null? policies)
                     (set! identifier-policies
                           (cons (cons key show?) identifier-policies))
                     (send look-ctl append "")
                     (update-list-view i key show?)]
                    [(key=? key (car (car policies)))
                     (set-cdr! (car policies) show?)
                     (update-list-view i key show?)]
                    [else (loop (add1 i) (cdr policies))])))))

      (define/private (update-list-view index key show?)
        (send look-ctl set-data index key)
        (send look-ctl set-string
              index
              (string-append (if show? "show " "hide ")
                             (key->text key))))

      (define/private (delete-selected)
        (define to-delete (sort (send look-ctl get-selections) <))
        (set! identifier-policies
              (let loop ([i 0] [policies identifier-policies] [to-delete to-delete])
                (cond [(null? to-delete) policies]
                      [(= i (car to-delete))
                       (loop (add1 i) (cdr policies) (cdr to-delete))]
                      [else
                       (cons (car policies)
                             (loop (add1 i) (cdr policies) to-delete))])))
        (for-each (lambda (n) (send look-ctl delete n)) (reverse to-delete)))

      (super-new)
      (update-visibility)))

  (define (lib-module? mpi)
    (and (module-path-index? mpi)
         (let-values ([(path rel) (module-path-index-split mpi)])
           (cond [(pair? path) (memq (car path) '(lib planet))]
                 [(string? path) (lib-module? rel)]
                 [else #f]))))

  (define (get-id-key id)
    (let ([binding
           (or (identifier-binding id)
               (identifier-transformer-binding id))])
      (get-id-key/binding id binding)))

  (define (get-id-key/binding id binding)
      (cond [(pair? binding)
             binding]
            [else id]))

  (define (key=? key1 key2)
    (cond [(and (identifier? key1) (identifier? key2))
           (module-identifier=? key1 key2)]
          [(and (pair? key1) (pair? key2))
           (and (equal? (car key1) (car key2))
                (equal? (cadr key1) (cadr key2)))]
          [else #f]))

  (define (key->text key)
    (cond [(pair? key)
           (let ([name (cadddr key)]
                 [mod (caddr key)])
             (format "'~s' from ~a"
                     name
                     (mpi->string mod)))]
          [else (symbol->string (syntax-e key))]))

  )


(module stepper mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "boundmap.ss" "syntax")
           "interfaces.ss"
           "prefs.ss"
           "extensions.ss"
           "warning.ss"
           "hiding-panel.ss"
           (prefix s: "../syntax-browser/widget.ss")
           (prefix s: "../syntax-browser/params.ss")
           "../model/deriv.ss"
           "../model/deriv-util.ss"
           "../model/trace.ss"
           "../model/hide.ss"
           "../model/steps.ss"
           "cursor.ss"
           "util.ss")
  (provide macro-stepper-widget%)

  ;; Struct for one-by-one stepping

  (define-struct (prestep protostep) (foci1 e1))
  (define-struct (poststep protostep) (foci2 e2))

  (define (prestep-term1 s) (context-fill (protostep-ctx s) (prestep-e1 s)))
  (define (poststep-term2 s) (context-fill (protostep-ctx s) (poststep-e2 s)))

  ;; TermRecords

  (define-struct trec (deriv synth-deriv estx raw-steps steps definites warnings) #f)

  (define (new-trec deriv)
    (make-trec deriv #f #f #f #f #f null))
  
  ;; trec:invalidate-synth! : TermRecord -> void
  ;; Invalidates cached parts that depend on macro-hiding policy
  (define (trec:invalidate-synth! trec)
    (set-trec-synth-deriv! trec #f)
    (set-trec-estx! trec #f)
    (set-trec-raw-steps! trec #f)
    (set-trec-definites! trec #f)
    (set-trec-warnings! trec null)
    (trec:invalidate-steps! trec))

  ;; trec:invalidate-steps! : TermRecord -> void
  ;; Invalidates cached parts that depend on reductions config
  (define (trec:invalidate-steps! trec)
    (set-trec-steps! trec #f))


  ;; Macro Stepper

  ;; macro-stepper-widget%
  (define macro-stepper-widget%
    (class* object% ()
      (init-field parent)
      (init-field config)

      ;; Terms

      ;; terms : (Cursor-of TermRecord)
      (define terms (cursor:new null))

      ;; focused-term : -> TermRecord or #f
      (define (focused-term)
        (let ([term (cursor:next terms)])
          (when term (recache term))
          term))

      ;; focused-steps : -> (Cursor-of Step) or #f
      (define/private (focused-steps)
        (let ([term (focused-term)])
          (and term 
               (cursor? (trec-steps term))
               (trec-steps term))))

      ;; alpha-table : module-identifier-mapping[identifier => identifier]
      (define alpha-table (make-module-identifier-mapping))

      ;; saved-position : number/#f
      (define saved-position #f)

      ;; add-deriv : Derivation -> void
      (define/public (add-deriv d)
        (let ([needs-display? (cursor:at-end? terms)])
          (for-each (lambda (id) (module-identifier-mapping-put! alpha-table id id))
                    (extract-all-fresh-names d))
          (cursor:add-to-end! terms (list (new-trec d)))
          (trim-navigator)
          (if needs-display?
              (refresh/move)
              (update))))

      ;; remove-current-term : -> void
      (define/public (remove-current-term)
        (cursor:remove-current! terms)
        (trim-navigator)
        (refresh/move))

      (define/public (get-config) config)
      (define/public (get-controller) sbc)
      (define/public (get-view) sbview)
      (define/public (get-macro-hiding-prefs) macro-hiding-prefs)

      (define/public (reset-primary-partition)
        (send sbc reset-primary-partition)
        (update/preserve-view))

      (define area (new vertical-panel% (parent parent)))
      (define supernavigator
        (new horizontal-panel%
             (parent area)
             (stretchable-height #f)
             (alignment '(center center))))
      (define navigator
        (new horizontal-panel%
             (parent supernavigator)
             (stretchable-width #f)
             (stretchable-height #f)
             (alignment '(left center))))
      (define extra-navigator
        (new horizontal-panel%
             (parent supernavigator)
             (stretchable-width #f)
             (stretchable-height #f)
             (alignment '(left center))
             (style '(deleted))))
      
      (define warnings (new stepper-warnings% (parent area)))
      
      (define sbview (new stepper-syntax-widget% 
                          (parent area)
                          (macro-stepper this)))
      (define sbc (send sbview get-controller))
      (define control-pane
        (new vertical-panel% (parent area) (stretchable-height #f)))
      (define macro-hiding-prefs
        (new macro-hiding-prefs-widget%
             (parent control-pane)
             (stepper this)
             (config config)))

      (send config listen-show-syntax-properties?
            (lambda (show?) (send sbview show-props show?)))
      (send config listen-show-hiding-panel?
            (lambda (show?) (show-macro-hiding-prefs show?)))
      (send sbc listen-selected-syntax
            (lambda (stx) (send macro-hiding-prefs set-syntax stx)))
      (send config listen-highlight-foci?
            (lambda (_) (update/preserve-view)))
      (send config listen-highlight-frontier?
            (lambda (_) (update/preserve-view)))
      (send config listen-show-rename-steps?
            (lambda (_) (refresh/re-reduce)))
      (send config listen-one-by-one?
            (lambda (_) (refresh/re-reduce)))
      (send config listen-force-letrec-transformation?
            (lambda (_) (refresh/resynth)))
      (send config listen-extra-navigation?
            (lambda (show?) (show-extra-navigation show?)))

      (define nav:up
        (new button% (label "Previous term") (parent navigator)
             (callback (lambda (b e) (navigate-up)))))
      (define nav:start
        (new button% (label "<-- Start") (parent navigator)
             (callback (lambda (b e) (navigate-to-start)))))
      (define nav:previous
        (new button% (label "<- Step") (parent navigator)
             (callback (lambda (b e) (navigate-previous)))))
      (define nav:next
        (new button% (label "Step ->") (parent navigator)
             (callback (lambda (b e) (navigate-next)))))
      (define nav:end
        (new button% (label "End -->") (parent navigator)
             (callback (lambda (b e) (navigate-to-end)))))
      (define nav:down
        (new button% (label "Next term") (parent navigator)
             (callback (lambda (b e) (navigate-down)))))

      (define/private (trim-navigator)
        (if (> (length (cursor->list terms)) 1)
            (send navigator change-children
                  (lambda _
                    (list nav:up
                          nav:start
                          nav:previous
                          nav:next
                          nav:end
                          nav:down)))
            (send navigator change-children
                  (lambda _
                    (list nav:start
                          nav:previous
                          nav:next
                          nav:end)))))

      (define/public (show-macro-hiding-prefs show?)
        (send area change-children
              (lambda (children)
                (if show?
                    (append (remq control-pane children) (list control-pane))
                    (remq control-pane children)))))

      (define/private (show-extra-navigation show?)
        (send supernavigator change-children
              (lambda (children)
                (if show?
                    (list navigator extra-navigator)
                    (list navigator)))))

      ;; Navigate

      (define/public-final (at-start?)
        (cursor:at-start? (focused-steps)))
      (define/public-final (at-end?)
        (cursor:at-end? (focused-steps)))

      (define/public-final (navigate-to-start)
        (cursor:move-to-start (focused-steps))
        (update/save-position))
      (define/public-final (navigate-to-end)
        (cursor:move-to-end (focused-steps))
        (update/save-position))
      (define/public-final (navigate-previous)
        (cursor:move-prev (focused-steps))
        (update/save-position))
      (define/public-final (navigate-next)
        (cursor:move-next (focused-steps))
        (update/save-position))

      (define/public-final (navigate-forward/count n)
        (unless (integer? n)
          (raise-type-error 'navigate-forward/count "integer" n))
        (cond [(zero? n)
               (update/save-position)]
              [(positive? n)
               (cursor:move-next (focused-steps))
               (navigate-forward/count (sub1 n))]
              [(negative? n)
               (cursor:move-prev (focused-steps))
               (navigate-forward/count (add1 n))]))

      (define/public-final (navigate-forward/pred p)
        (let* ([cursor (focused-steps)]
               [steps (and cursor (cursor:suffix->list cursor))]
               [pred (lambda (s)
                       (and (rewrite-step? s)
                            (ormap p (step-foci1 s))
                            s))]
               [step (ormap pred steps)])
          (unless step
            (error 'navigate-forward/pred "no step matching predicate"))
          (cursor:skip-to cursor step)
          (update/save-position)))

      (define/public-final (navigate-up)
        (cursor:move-prev terms)
        (refresh/move))
      (define/public-final (navigate-down)
        (cursor:move-next terms)
        (refresh/move))

      (define/public-final (navigate-down/pred p)
        (let* ([termlist (cursor:suffix->list terms)]
               [pred (lambda (trec)
                       (and (p (lift/deriv-e1 (trec-deriv trec)))
                            trec))]
               [term (ormap pred termlist)])
          (unless term
            (error 'navigate-down/pred "no term matching predicate"))
          (cursor:skip-to terms term)
          (refresh/move)))

      ;; insert-step-separator : string -> void
      (define/private (insert-step-separator text)
        (send sbview add-text "\n    ")
        (send sbview add-text
              (make-object image-snip% 
                           (build-path (collection-path "icons")
                                       "red-arrow.bmp")))
        (send sbview add-text "  ")
        (send sbview add-text text)
        (send sbview add-text "\n\n"))

      ;; insert-as-separator : string -> void
      (define/private (insert-as-separator text)
        (send sbview add-text "\n    ")
        (send sbview add-text text)
        (send sbview add-text "\n\n"))

      ;; insert-step-separator/small : string -> void
      (define/private (insert-step-separator/small text)
        (send sbview add-text "    ")
        (send sbview add-text
              (make-object image-snip% 
                           (build-path (collection-path "icons")
                                       "red-arrow.bmp")))
        (send sbview add-text "  ")
        (send sbview add-text text)
        (send sbview add-text "\n\n"))

      ;; update/preserve-view : -> void
      (define/public (update/preserve-view)
        (define text (send sbview get-text))
        (define start-box (box 0))
        (define end-box (box 0))
        (send text get-visible-position-range start-box end-box)
        (update)
        (send text scroll-to-position (unbox start-box) #f (unbox end-box) 'start))

      ;; update/save-position : -> void
      (define/private (update/save-position)
        (save-position)
        (update/preserve-lines-view))

      ;; update/preserve-lines-view : -> void
      (define/public (update/preserve-lines-view)
        (define text (send sbview get-text))
        (define start-box (box 0))
        (define end-box (box 0))
        (send text get-visible-line-range start-box end-box)
        (update)
        (send text scroll-to-position
              (send text line-start-position (unbox start-box))
              #f
              (send text line-start-position (unbox end-box))
              'start))
      
      ;; update : -> void
      ;; Updates the terms in the syntax browser to the current step
      (define/private (update)
        (define text (send sbview get-text))
        (define position-of-interest 0)
        (define multiple-terms? (> (length (cursor->list terms)) 1))
        (send text begin-edit-sequence)
        (send sbview erase-all)
        
        (update:show-prefix)
        (when multiple-terms? (send sbview add-separator))
        (set! position-of-interest (send text last-position))
        (update:show-current-step)
        (when multiple-terms? (send sbview add-separator))
        (update:show-suffix)
        (send text end-edit-sequence)
        (send text scroll-to-position
              position-of-interest
              #f
              (send text last-position)
              'start)
        (enable/disable-buttons))
      
      ;; update:show-prefix : -> void
      (define/private (update:show-prefix)
        ;; Show the final terms from the cached synth'd derivs
        (for-each (lambda (trec)
                    (recache trec)
                    (let ([e2 (trec-estx trec)]
                          [definites
                            (if (pair? (trec-definites trec))
                                (trec-definites trec)
                                null)])
                      (if e2
                          (send sbview add-syntax e2
                                #:alpha-table alpha-table
                                #:definites definites)
                          (send sbview add-text "Error\n"))))
                  (cursor:prefix->list terms)))

      ;; update:show-current-step : -> void
      (define/private (update:show-current-step)
        (define steps (focused-steps))
        (when (focused-term)
          (when steps
            (let ([step (cursor:next steps)])
              (cond [(step? step)
                     (update:show-step step)]
                    [(mono? step)
                     (update:show-mono step)]
                    [(misstep? step)
                     (update:show-misstep step)]
                    [(prestep? step)
                     (update:show-prestep step)]
                    [(poststep? step)
                     (update:show-poststep step)]
                    [(not step)
                     (update:show-final (focused-term))])))
          (unless steps
            (send sbview add-text
                  "Internal error computing reductions. Original term:\n")
            (send sbview add-syntax
                  (lift/deriv-e1 (trec-deriv (focused-term)))))))

      ;; update:show-lctx : Step -> void
      (define/private (update:show-lctx step)
        (define lctx (protostep-lctx step))
        (when (pair? lctx)
          (send sbview add-text "\n")
          (for-each (lambda (bf)
                      (send sbview add-text 
                            "while executing macro transformer in:\n")
                      (insert-syntax/redex (bigframe-term bf)
                                           (bigframe-foci bf)
                                           (protostep-definites step)
                                           (protostep-frontier step)))
                    (reverse lctx))))

      ;; update:separator : Step -> void
      (define/private (update:separator step)
        (if (not (mono? step))
            (insert-step-separator (step-type->string (protostep-type step)))
            (insert-as-separator (step-type->string (protostep-type step)))))

      ;; update:separator/small : Step -> void
      (define/private (update:separator/small step)
        (insert-step-separator/small
         (step-type->string (protostep-type step))))
      
      ;; update:show-step : Step -> void
      (define/private (update:show-step step)
        (insert-syntax/redex (step-term1 step)
                             (step-foci1 step)
                             (protostep-definites step)
                             (protostep-frontier step))
        (update:separator step)
        (insert-syntax/contractum (step-term2 step)
                                  (step-foci2 step)
                                  (protostep-definites step)
                                  (protostep-frontier step))
        (update:show-lctx step))

      ;; update:show-mono : Step -> void
      (define/private (update:show-mono step)
        (update:separator step)
        (insert-syntax/redex (mono-term1 step)
                             null
                             (protostep-definites step)
                             (protostep-frontier step))
        (update:show-lctx step))

      ;; update:show-prestep : Step -> void
      (define/private (update:show-prestep step)
        (update:separator/small step)
        (insert-syntax/redex (prestep-term1 step)
                             (prestep-foci1 step)
                             (protostep-definites step)
                             (protostep-frontier step))
        (update:show-lctx step))

      ;; update:show-poststep : Step -> void
      (define/private (update:show-poststep step)
        (update:separator/small step)
        (insert-syntax/contractum (poststep-term2 step)
                                  (poststep-foci2 step)
                                  (protostep-definites step)
                                  (protostep-frontier step))
        (update:show-lctx step))

      ;; update:show-misstep : Step -> void
      (define/private (update:show-misstep step)
        (insert-syntax/redex (misstep-term1 step)
                             (misstep-foci1 step)
                             (protostep-definites step)
                             (protostep-frontier step))
        (update:separator step)
        (send sbview add-text (exn-message (misstep-exn step)))
        (send sbview add-text "\n")
        (when (exn:fail:syntax? (misstep-exn step))
          (for-each (lambda (e) (send sbview add-syntax e
                                      #:alpha-table alpha-table
                                      #:definites (protostep-definites step)))
                    (exn:fail:syntax-exprs (misstep-exn step))))
        (update:show-lctx step))

      ;; update:show-final : TermRecord -> void
      (define/private (update:show-final trec)
        (define result (trec-estx trec))
        (when result
          (send sbview add-text "Expansion finished\n")
          (send sbview add-syntax result
                #:alpha-table alpha-table
                #:definites (let ([definites (trec-definites trec)])
                              (if (pair? definites) definites null))))
        (unless result
          (send sbview add-text "Error\n")))

      ;; update:show-suffix : -> void
      (define/private (update:show-suffix)
        (let ([suffix0 (cursor:suffix->list terms)])
          (when (pair? suffix0)
            (for-each (lambda (trec)
                        (send sbview add-syntax
                              (lift/deriv-e1 (trec-deriv trec))
                              #:alpha-table alpha-table))
                      (cdr suffix0)))))

      ;; insert-syntax/color : syntax syntaxes identifiers syntaxes string -> void
      (define/private (insert-syntax/color stx foci definites frontier hi-color)
        (send sbview add-syntax stx
              #:definites definites
              #:alpha-table alpha-table
              #:hi-color hi-color
              #:hi-stxs (if (send config get-highlight-foci?) foci null)
              #:hi2-color "WhiteSmoke"
              #:hi2-stxs (if (send config get-highlight-frontier?) frontier null)))

      ;; insert-syntax/redex : syntax syntaxes identifiers syntaxes -> void
      (define/private (insert-syntax/redex stx foci definites frontier)
        (insert-syntax/color stx foci definites frontier "MistyRose"))

      ;; insert-syntax/contractum : syntax syntaxes identifiers syntaxes -> void
      (define/private (insert-syntax/contractum stx foci definites frontier)
        (insert-syntax/color stx foci definites frontier "LightCyan"))

      ;; enable/disable-buttons : -> void
      (define/private (enable/disable-buttons)
        (define steps (focused-steps))
        (send nav:start enable (and steps (cursor:has-prev? steps)))
        (send nav:previous enable (and steps (cursor:has-prev? steps)))
        (send nav:next enable (and steps (cursor:has-next? steps)))
        (send nav:end enable (and steps (cursor:has-next? steps)))
        (send nav:up enable (cursor:has-prev? terms))
        (send nav:down enable (cursor:has-next? terms)))

      ;; --
      
      ;; refresh/resynth : -> void
      ;; Macro hiding policy has changed; invalidate cached parts of trec
      (define/public (refresh/resynth)
        (for-each trec:invalidate-synth! (cursor->list terms))
        (refresh))
      
      ;; refresh/re-reduce : -> void
      ;; Reduction config has changed; invalidate cached parts of trec
      (define/private (refresh/re-reduce)
        (for-each trec:invalidate-steps! (cursor->list terms))
        (refresh))
      
      ;; refresh/move : -> void
      ;; Moving between terms; clear the saved position
      (define/private (refresh/move)
        (clear-saved-position)
        (refresh))
      
      ;; refresh : -> void
      (define/public (refresh)
        (restore-position)
        (display-warnings (focused-term))
        (update))
      
      ;; display-warnings : TermRecord -> void
      (define/private (display-warnings trec)
        (send warnings clear)
        (when trec
          (unless (send config get-suppress-warnings?)
            (for-each (lambda (tag+message)
                        (let ([tag (car tag+message)]
                              [message (cdr tag+message)])
                          (send warnings add-warning tag message)))
                      (trec-warnings trec)))))
      
      ;; recache : TermRecord -> void
      (define/private (recache trec)
        (unless (trec-synth-deriv trec)
          (set-trec-warnings! trec null)
          (with-handlers ([(lambda (e) #t)
                           (lambda (e)
                             (handle-recache-error e 'macro-hiding)
                             (set-trec-synth-deriv! trec 'error)
                             (set-trec-estx! trec (lift/deriv-e2 (trec-deriv trec))))])
            (recache-synth trec)))
        (unless (trec-raw-steps trec)
          (with-handlers ([(lambda (e) #t)
                           (lambda (e)
                             (handle-recache-error e 'reductions)
                             (set-trec-raw-steps! trec 'error)
                             (set-trec-definites! trec 'error))])
            (let-values ([(steps definites)
                          (reductions+definites
                           (or (trec-synth-deriv trec) (trec-deriv trec)))])
              (set-trec-raw-steps! trec steps)
              (set-trec-definites! trec definites))))
        (unless (trec-steps trec)
          (with-handlers ([(lambda (e) #t)
                           (lambda (e)
                             (handle-recache-error e 'special-reductions)
                             (set-trec-steps! trec 'error))])
            (set-trec-steps!
             trec
             (let ([raw-steps (trec-raw-steps trec)])
               (if (eq? raw-steps 'error)
                   'error
                   (let ([filtered-steps 
                          (if (send config get-show-rename-steps?)
                              raw-steps
                              (filter (lambda (x) (not (rename-step? x))) raw-steps))])
                     (cursor:new
                      (if (send config get-one-by-one?)
                          (reduce:one-by-one filtered-steps)
                          filtered-steps)))))))))

      ;; delayed-recache-errors : (list-of (cons exn string))
      (define delayed-recache-errors null)

      ;; handle-recache-error : exception string -> void
      (define/private (handle-recache-error exn part)
        (if (send config get-debug-catch-errors?)
            (begin
              (set! delayed-recache-errors 
                    (cons (cons exn part) delayed-recache-errors))
              (queue-callback
               (lambda ()
                 (when (pair? delayed-recache-errors)
                   (message-box
                    "Error"
                    (string-append 
                     "Internal errors in macro stepper:\n"
                     (if (memq 'macro-hiding (map cdr delayed-recache-errors))
                         (string-append 
                          "Macro hiding failed on one or more terms. "
                          "The macro stepper is showing the terms "
                          "with macro hiding disabled.\n")
                         "")
                     (if (memq 'reductions (map cdr delayed-recache-errors))
                         (string-append
                          "The macro stepper failed to compute the reduction sequence "
                          "for one or more terms.\n")
                         "")))
                   (set! delayed-recache-errors null)))))
            (raise exn)))

      ;; update-saved-position : num -> void
      (define/private (update-saved-position pos)
        (when pos (set! saved-position pos)))

      ;; clear-saved-position : -> void
      (define/private (clear-saved-position)
        (set! saved-position #f))

      ;; save-position : -> void
      (define/private (save-position)
        (when (cursor? (focused-steps))
          (let ([step (cursor:next (focused-steps))])
            (cond [(not step)
                   ;; At end; go to the end when restored
                   (update-saved-position +inf.0)]
                  [(protostep? step)
                   (update-saved-position
                    (extract-protostep-seq step))]))))

      ;; restore-position : number -> void
      (define/private (restore-position)
        (define steps (focused-steps))
        (define (advance)
          (let ([step (cursor:next steps)])
            (cond [(not step)
                   ;; At end; stop
                   (void)]
                  [(protostep? step)
                   (let ([step-pos (extract-protostep-seq step)])
                     (cond [(not step-pos)
                            (cursor:move-next steps)
                            (advance)]
                           [(< step-pos saved-position)
                            (cursor:move-next steps)
                            (advance)]
                           [else (void)]))])))
        (when saved-position
          (when steps
            (advance))))

      (define/private (extract-protostep-seq step)
        (match (protostep-deriv step)
          [(AnyQ mrule (_ _ (AnyQ transformation (_ _ _ _ _ _ seq)) _))
           seq]
          [else #f]))

      ;; recache-synth : TermRecord -> void
      (define/private (recache-synth trec)
        (define deriv (trec-deriv trec))
        (define-values (synth-deriv estx)
          (let ([show-macro? (get-show-macro?)])
            (if show-macro?
                (parameterize ((current-hiding-warning-handler
                                (lambda (tag message)
                                  (set-trec-warnings!
                                   trec
                                   (cons (cons tag message)
                                         (trec-warnings trec)))))
                               (force-letrec-transformation
                                (send config get-force-letrec-transformation?)))
                  (hide/policy deriv show-macro?))
                (values deriv (lift/deriv-e2 deriv)))))
        (set-trec-synth-deriv! trec synth-deriv)
        (set-trec-estx! trec estx))
      
      (define/private (reduce:one-by-one rs)
        (let loop ([rs rs])
          (match rs
            [(cons (struct step (d l t c df fr redex contractum e1 e2)) rs)
             (list* (make-prestep d l "Find redex" c df fr redex e1)
                    (make-poststep d l t c df fr contractum e2)
                    (loop rs))]
            [(cons (struct misstep (d l t c df fr redex e1 exn)) rs)
             (list* (make-prestep d l "Find redex" c df fr redex e1)
                    (make-misstep d l t c df fr redex e1 exn)
                    (loop rs))]
            ['()
             null])))
      
      (define/private (foci x) (if (list? x) x (list x)))
      
      ;; Hiding policy
      
      (define/private (get-show-macro?)
        (send macro-hiding-prefs get-policy))
      
      ;; Initialization
      
      (super-new)
      (send sbview show-props (send config get-show-syntax-properties?))
      (show-macro-hiding-prefs (send config get-show-hiding-panel?))
      (show-extra-navigation (send config get-extra-navigation?))
      (refresh/move)
      ))

  )

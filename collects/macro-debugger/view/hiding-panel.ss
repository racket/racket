
(module hiding-panel mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "list.ss")
           (lib "boundmap.ss" "syntax")
           "../model/hiding-policies.ss"
           "../syntax-browser/util.ss")
  (provide macro-hiding-prefs-widget%)
  
  ;; macro-hiding-prefs-widget%
  (define macro-hiding-prefs-widget%
    (class object%
      (init parent)
      (init-field stepper)
      (init-field policy)
      (init-field (enabled? #f))
      
      (define stx #f)
      (define stx-name #f)
      (define stx-module #f)
      
      (define super-pane
        (new horizontal-pane%
             (parent parent)
             (stretchable-height #f)))
      (define left-pane
        (new vertical-pane%
             (parent super-pane)
             (stretchable-width #f)
             (alignment '(left top))))
      (define right-pane
        (new vertical-pane%
             (parent super-pane)))
      
      (define enable-ctl
        (new check-box% 
             (label "Enable macro hiding?")
             (parent left-pane)
             (value enabled?) 
             (callback
              (lambda _
                (set! enabled? (send enable-ctl get-value))
                (force-refresh)))))
      
      (define kernel-ctl
        (new check-box%
             (label "Hide mzscheme syntax")
             (parent left-pane)
             (value (hiding-policy-opaque-kernel policy))
             (callback (lambda _
                         (if (send kernel-ctl get-value)
                             (policy-hide-kernel policy)
                             (policy-unhide-kernel policy))
                         (refresh)))))
      (define libs-ctl
        (new check-box%
             (label "Hide library syntax")
             (parent left-pane)
             (value (hiding-policy-opaque-libs policy))
             (callback (lambda _
                         (if (send libs-ctl get-value)
                             (policy-hide-libs policy)
                             (policy-unhide-libs policy))
                         (refresh)))))
      
      (define look-pane
        (new horizontal-pane% (parent right-pane) (stretchable-height #f)))
      (define look-ctl
        (new list-box% (parent look-pane) (label "") (choices null)))
      (define delete-ctl
        (new button% (parent look-pane) (label "Delete")
             (callback
              (lambda _
                (delete-selected)
                (refresh)))))
      
      (define add-pane
        (new horizontal-pane% (parent right-pane) (stretchable-height #f)))
      (define add-text
        (new text-field%
             (label "")
             (parent add-pane)
             (stretchable-width #t)))
      (define add-editor (send add-text get-editor))
      (define add-hide-module-button
        (new button% (parent add-pane) (label "Hide module") (enabled #f)
             (callback (lambda _ (add-hide-module) (refresh)))))
      (define add-hide-id-button
        (new button% (parent add-pane) (label "Hide macro") (enabled #f)
             (callback (lambda _ (add-hide-identifier) (refresh)))))
      (define add-show-id-button
        (new button% (parent add-pane) (label "Show macro") (enabled #f)
             (callback (lambda _ (add-show-identifier) (refresh)))))
      
      (send add-editor lock #t)
      
      ;; Methods
      
      ;; enable-hiding : boolean -> void
      ;; Called only by stepper, which does it's own refresh
      (define/public (enable-hiding ok?)
        (send enable-ctl set-value ok?)
        (set! enabled? ok?))
      
      ;; get-enabled?
      (define/public (get-enabled?) enabled?)
      
      ;; get-policy
      (define/public (get-policy) policy)
      
      ;; refresh
      (define/public (refresh)
        (when enabled?
          (send stepper refresh/resynth)))
      
      ;; force-refresh
      (define/private (force-refresh)
        (send stepper refresh/resynth))
      
      ;; set-syntax : syntax/#f -> void
      (define/public (set-syntax lstx)
        (set! stx lstx)
        (send add-editor lock #f)
        (send add-editor erase)
        (unless (identifier? stx)
          (send add-hide-module-button enable #f))
        (when (identifier? stx)
          (let ([binding (identifier-binding stx)])
            (send add-hide-module-button enable (pair? binding))
            (if (pair? binding)
                (begin
                  (set! stx-name (cadr binding))
                  (set! stx-module (car binding)))
                (begin
                  (set! stx-name (syntax-e stx))
                  (set! stx-module #f)))
            (update-add-text)))
        (send add-editor lock #t)
        (send add-show-id-button enable (identifier? lstx))
        (send add-hide-id-button enable (identifier? lstx)))
      
      (define/private (update-add-text)
        (send add-editor lock #f)
        (when (identifier? stx)
          (send add-editor insert (identifier-text "" stx)))
        (send add-editor lock #t))
      
      (define/public (add-hide-module)
        (when stx-module
          (policy-hide-module policy stx-module)
          (update-list-view)))
      
      (define/public (add-hide-identifier)
        (when (identifier? stx)
          (policy-hide-id policy stx)
          (update-list-view)))
      
      (define/public (add-show-identifier)
        (when (identifier? stx)
          (policy-show-id policy stx)
          (update-list-view)))
      
      (define/private (delete-selected)
        (for-each (lambda (n)
                    (let ([d (send look-ctl get-data n)])
                      (case (car d)
                        ((identifier) (policy-unhide-id policy (cdr d)))
                        ((show-identifier) (policy-unshow-id policy (cdr d)))
                        ((module) (policy-unhide-module policy (cdr d))))))
                  (send look-ctl get-selections))
        (update-list-view))
      
      (define/private (identifier-text prefix id)
        (let ([b (identifier-binding id)])
          (cond [(pair? b)
                 (let ([name (cadr b)]
                       [mod (car b)])
                   (format "~a'~s' from module ~a"
                           prefix
                           name
                           (mpi->string mod)))]
                [(eq? b 'lexical)
                 (format "~alexically bound '~s'"
                         prefix
                         (syntax-e id))]
                [(not b)
                 (format "~aglobal or unbound '~s'" prefix (syntax-e id))])))
      
      (define/private (update-list-view)
        (let ([opaque-modules
               (hash-table-map (hiding-policy-opaque-modules policy)
                               (lambda (k v) k))]
              [opaque-ids
               (filter values
                       (module-identifier-mapping-map
                        (hiding-policy-opaque-ids policy)
                        (lambda (k v) (and v k))))]
              [transparent-ids
               (filter values
                       (module-identifier-mapping-map
                        (hiding-policy-transparent-ids policy)
                        (lambda (k v) (and v k))))])
          (define (om s)
            (cons (format "hide from module ~a" (mpi->string s))
                  (cons 'module s)))
          (define (*i prefix tag id)
            (cons (identifier-text prefix id)
                  (cons tag id)))
          (define (oid id) (*i "hide " 'identifier id))
          (define (tid id) (*i "show " 'show-identifier id))
          (let ([choices
                 (sort (append (map om opaque-modules)
                               (map oid opaque-ids)
                               (map tid transparent-ids))
                       (lambda (a b)
                         (string<=? (car a) (car b))))])
            (send look-ctl clear)
            (for-each (lambda (c) (send look-ctl append (car c) (cdr c)))
                      choices))))
      
      (super-new)))
  
  )
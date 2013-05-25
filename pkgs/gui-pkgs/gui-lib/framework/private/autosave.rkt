#lang scheme/unit

  (require racket/class
           racket/file
           "sig.rkt"
           "../gui-utils.rkt"
           "../preferences.rkt"
           mred/mred-sig
           string-constants)
  
  (import mred^
          [prefix exit: framework:exit^]
          [prefix frame: framework:frame^]
          [prefix racket: framework:racket^]
          [prefix editor: framework:editor^]
          [prefix text: framework:text^]
          [prefix finder: framework:finder^]
          [prefix group: framework:group^])
  
  (export framework:autosave^)
  
  (define autosavable<%>
    (interface ()
      do-autosave))
  
  (define objects null)
  
  (define toc-path
    (build-path (find-system-path 'pref-dir)
                (case (system-type)
                  [(unix) ".plt-autosave-toc.rktd"]
                  [else "PLT-autosave-toc.rktd"])))
  
  (define autosave-toc-save-filename
    (build-path (find-system-path 'pref-dir)
                (case (system-type)
                  [(unix) ".plt-autosave-toc-save.rktd"]
                  [else "PLT-autosave-toc-save.rktd"])))
  
  (define autosave-timer%
    (class timer%
      (inherit start)
      (field [last-name-mapping #f])
      (define/override (notify)
        (when (preferences:get 'framework:autosaving-on?)
          (let-values ([(new-objects new-name-mapping) (rebuild-object-list)])
            (set! objects new-objects)
            (unless (equal? last-name-mapping new-name-mapping)
              (set! last-name-mapping new-name-mapping)
              (when (file-exists? autosave-toc-save-filename)
                (delete-file autosave-toc-save-filename))
              (when (file-exists? toc-path)
                (copy-file toc-path autosave-toc-save-filename))
              (call-with-output-file toc-path
                (λ (port)
                  (write new-name-mapping port))
                #:exists 'truncate
                #:mode 'text))))
        (let ([seconds (preferences:get 'framework:autosave-delay)])
          (start (* 1000 seconds) #t)))
      (super-new)
      (let ([seconds (preferences:get 'framework:autosave-delay)])
        (start (* 1000 seconds) #t))))
  
  ;; rebuild-object-list : -> (values (listof (weak-box (is-a?/c editor<%>)))
  ;;                                  (listof (list (union #f string[filename]) string[filename]))
  (define (rebuild-object-list)
    (let loop ([orig-objects objects]
               [name-mapping null]
               [new-objects null])
      (if (null? orig-objects)
          (values new-objects name-mapping)
          (let* ([object-wb (car orig-objects)]
                 [object (weak-box-value object-wb)])
            (if object
                (let* ([new-filename (send object do-autosave)]
                       [tmp-box (box #f)]
                       [filename (send object get-filename tmp-box)])
                  (loop (cdr orig-objects)
                        (if new-filename
                            (cons (list (and (not (unbox tmp-box)) 
                                             filename
                                             (path->bytes filename))
                                        (and new-filename 
                                             (path->bytes new-filename)))
                                  name-mapping)
                            name-mapping)
                        (cons object-wb new-objects)))
                (loop (cdr orig-objects)
                      name-mapping
                      new-objects))))))
  
  (define timer #f)
  ;; when the autosave delay is changed then we
  ;; trigger an autosave right away and let the
  ;; callback trigger the next one at the right interval
  (preferences:add-callback
   'framework:autosave-delay
   (λ (k v)
     (when timer
       (send timer stop)
       (send timer start 0 #t))))
  

  
  (define (register b)
    (unless timer
      (set! timer (make-object autosave-timer%)))
    (set! objects
          (let loop ([objects objects])
            (cond
              [(null? objects) (list (make-weak-box b))]
              [else (let ([weak-box (car objects)])
                      (if (weak-box-value weak-box)
                          (cons weak-box (loop (cdr objects)))
                          (loop (cdr objects))))]))))
  
  ;; restore-autosave-files/gui : -> (union #f (is-a?/c top-level-window<%>))
  ;; opens a frame that lists the autosave files that have changed.
  (define (restore-autosave-files/gui)
    
    ;; main : -> void
    ;; start everything going
    (define (main)
      (when (file-exists? toc-path)
        ;; Load table from file, and check that the file was not corrupted
        (let* ([table (let ([v (with-handlers ([exn:fail? (λ (x) null)])
                                 (call-with-input-file toc-path read))])
                        (if (and (list? v)
                                 (andmap (λ (i)
                                           (and (list? i) 
                                                (= 2 (length i))
                                                (or (not (car i))
                                                    (bytes? (car i)))
                                                (bytes? (cadr i))))
                                         v))
                            (map (λ (ent) (list (if (bytes? (list-ref ent 0))
                                                    (bytes->path (list-ref ent 0))
                                                    #f)
                                                (bytes->path (list-ref ent 1))))
                                 v)
                            null))]
               ;; assume that the autosave file was deleted due to the file being saved
               [filtered-table
                (filter (λ (x) (file-exists? (cadr x))) table)])
          (unless (null? filtered-table)
            (let* ([dlg (new (frame:focus-table-mixin dialog%)
                             (label (string-constant recover-autosave-files-frame-title)))]
                   [t (new text% (auto-wrap #t))]
                   [ec (new editor-canvas%
                            (parent dlg)
                            (editor t)
                            (line-count 2)
                            (stretchable-height #f)
                            (style '(no-hscroll)))]
                   [hp (new horizontal-panel% 
                            [parent dlg]
                            [stretchable-height #f])]
                   [vp (new vertical-panel%
                            [parent hp]
                            [stretchable-height #f])]
                   [details-parent (new horizontal-panel% [parent dlg])])
              (send vp set-alignment 'right 'center)
              (make-object grow-box-spacer-pane% hp)
              (send t insert (string-constant autosave-explanation))
              (send t hide-caret #t)
              (send t set-position 0 0)
              (send t lock #t)
              
              (for-each (add-table-line vp dlg details-parent) filtered-table)
              (make-object button%
                (string-constant autosave-done)
                vp
                (λ (x y)
                  (when (send dlg can-close?)
                    (send dlg on-close)
                    (send dlg show #f))))
              (send dlg show #t)
              (void))))))
    
    ;; add-table-line : (is-a? area-container<%>)
    ;;                  (or/c #f (is-a?/c top-level-window<%>))
    ;;                  (is-a? area-container<%>)
    ;;               -> (list/c (or/c #f path?) path?)
    ;;               -> void?
    ;; adds in a line to the overview table showing this pair of files.
    (define (add-table-line area-container dlg show-details-parent)
      (λ (table-entry)
        (letrec ([orig-file (car table-entry)]
                 [backup-file (cadr table-entry)]
                 [hp (new horizontal-panel%
                          (parent area-container)
                          (style '(border))
                          (stretchable-height #f))]
                 [vp (new vertical-panel%
                          (parent hp))]
                 [msg1-panel (new horizontal-panel%
                                  (parent vp))]
                 [msg1-label (new message%
                                  (parent msg1-panel)
                                  (label (string-constant autosave-original-label:)))]
                 [msg1 (new message%
                            (label (if orig-file (path->string orig-file) (string-constant autosave-unknown-filename)))
                            (stretchable-width #t)
                            (parent msg1-panel))]
                 [msg2-panel (new horizontal-panel%
                                  (parent vp))]
                 [msg2-label (new message%
                                  (parent msg2-panel)
                                  (label (string-constant autosave-autosave-label:)))]
                 [msg2 (new message%
                            (label (path->string backup-file))
                            (stretchable-width #t)
                            (parent msg2-panel))]
                 [details
                  (make-object button% (string-constant autosave-details) hp
                    (λ (x y)
                      (show-files table-entry show-details-parent dlg)))]
                 [delete
                  (make-object button%
                    (string-constant autosave-delete-button)
                    hp
                    (λ (delete y)
                      (when (delete-autosave table-entry)
                        (disable-line)
                        (send msg2 set-label (string-constant autosave-deleted)))))]
                 [recover
                  (make-object button% 
                    (string-constant autosave-recover)
                    hp
                    (λ (recover y)
                      (let ([filename-result (recover-file dlg table-entry)])
                        (when filename-result
                          (disable-line)
                          (send msg2 set-label (string-constant autosave-recovered!))
                          (send msg1 set-label (gui-utils:quote-literal-label
                                                (path->string filename-result)
                                                #:quote-amp? #f))))))]
                 [disable-line
                  (λ ()
                    (send recover enable #f)
                    (send details enable #f)
                    (send delete enable #f))])
          (let ([w (max (send msg1-label get-width) (send msg2-label get-width))])
            (send msg1-label min-width w)
            (send msg2-label min-width w))
          (void))))

    ;; delete-autosave : (list (union #f string[filename]) string[filename]) -> boolean
    ;; result indicates if delete occurred
    (define (delete-autosave table-entry)
      (let ([autosave-file (cadr table-entry)])
        (and (gui-utils:get-choice
              (format (string-constant are-you-sure-delete?)
                      autosave-file)
              (string-constant autosave-delete-title)
              (string-constant cancel)
              (string-constant warning)
              #f)
             (with-handlers ([exn:fail?
                              (λ (exn)
                                (message-box
                                 (string-constant warning)
                                 (format (string-constant autosave-error-deleting)
                                         autosave-file
                                         (if (exn? exn)
                                             (format "~a" (exn-message exn))
                                             (format "~s" exn))))
                                #f)])
               (delete-file autosave-file)
               #t))))
    
    ;; show-files : (list (or/c #f path?) path?) (is-a?/c area-container<%>) (is-a?/c dialog%) -> void
    (define (show-files table-entry show-details-parent dlg)
      (let ([file1 (list-ref table-entry 0)]
            [file2 (list-ref table-entry 1)])
        (send dlg begin-container-sequence)
        (define had-children? #f)
        (send show-details-parent change-children (λ (x) 
                                                    (set! had-children? (not (null? x)))
                                                    '()))
        (when file1
          (add-file-viewer file1 show-details-parent (string-constant autosave-original-label)))
        (add-file-viewer file2 show-details-parent (string-constant autosave-autosave-label))
        (send dlg end-container-sequence)
        (unless had-children?
          (send dlg center))))
    
    ;; add-file-viewer : path? -> void
    (define (add-file-viewer filename parent label)
      (define vp (make-object vertical-panel% parent))
      (define t (make-object show-files-text%))
      (define msg1 (make-object message% label vp))
      (define msg2 (new message% 
                        [label (gui-utils:quote-literal-label
                                (path->string filename)
                                #:quote-amp? #f)]
                        [parent vp]))
      (define ec (make-object editor-canvas% vp t))
      (send ec min-height 400)
      (send t load-file filename)
      (send t hide-caret #t)
      (send t lock #t))
    
    (define show-files-frame% frame:basic%)
    (define show-files-text% text:keymap%)
    
    (main))
  
  ;; recover-file : (union #f (is-a?/c toplevel-window<%>)) 
  ;;                (list (union #f string[filename]) string)
  ;;             -> (union #f string)
  (define (recover-file parent table-entry)
    (let ([orig-name (or (car table-entry)
                         (parameterize ([finder:dialog-parent-parameter parent])
                           (finder:put-file #f #f #f
                                            (string-constant autosave-restore-to-where?))))])
      (and orig-name
           (let ([autosave-name (cadr table-entry)])
             (let ([tmp-name (and (file-exists? orig-name)
                                  (make-temporary-file "autosave-repair~a" orig-name))])
               (when (file-exists? orig-name)
                 (delete-file orig-name))
               (copy-file autosave-name orig-name)
               (delete-file autosave-name)
               (when tmp-name
                 (delete-file tmp-name))
               orig-name)))))

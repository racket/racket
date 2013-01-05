#lang scheme/unit

  (require mzlib/class
           string-constants
           "sig.rkt"
           "../preferences.rkt"
           "../gui-utils.rkt"
           mzlib/etc
           mred/mred-sig
           racket/path)
  
  (import mred^
          [prefix autosave: framework:autosave^]
          [prefix finder: framework:finder^]
          [prefix path-utils: framework:path-utils^]
          [prefix keymap: framework:keymap^]
          [prefix icon: framework:icon^]
          [prefix text: framework:text^]
          [prefix pasteboard: framework:pasteboard^]
          [prefix frame: framework:frame^]
          [prefix handler: framework:handler^])
  (export (rename framework:editor^
                  [-keymap<%> keymap<%>]))
  (init-depend mred^ framework:autosave^)
  
  ;; renaming, for editor-mixin where get-file is shadowed by a method.
  (define mred:get-file get-file) 
  
  (define basic<%>
    (interface (editor<%>)
      has-focus?
      local-edit-sequence?
      run-after-edit-sequence
      get-top-level-window
      save-file-out-of-date?
      save-file/gui-error
      load-file/gui-error
      on-close
      can-close?
      close
      get-filename/untitled-name
      
      get-pos/text
      get-pos/text-dc-location))
  
  (define basic-mixin
    (mixin (editor<%>) (basic<%>)
      
      (define/pubment (can-close?) (inner #t can-close?))
      (define/pubment (on-close) (inner (void) on-close))
      (define/public (close) (if (can-close?)
                                 (begin (on-close) #t)
                                 #f))
      
      (define/public (get-pos/text event)
        (get-pos/text-dc-location (send event get-x) (send event get-y)))
      
      (define/public (get-pos/text-dc-location event-x event-y)
        (let ([on-it? (box #f)])
          (let loop ([editor this])
            (let-values ([(x y) (send editor dc-location-to-editor-location event-x event-y)])
              (cond
                [(is-a? editor text%)
                 (let ([pos (send editor find-position x y #f on-it?)])
                   (cond
                     [(not (unbox on-it?)) (values #f #f)]
                     [else
                      (let ([snip (send editor find-snip pos 'after-or-none)])
                        (if (and snip
                                 (is-a? snip editor-snip%))
                            (loop (send snip get-editor))
                            (values pos editor)))]))]
                [(is-a? editor pasteboard%)
                 (let ([snip (send editor find-snip x y)])
                   (if (and snip
                            (is-a? snip editor-snip%))
                       (loop (send snip get-editor))
                       (values #f editor)))]
                [else (values #f #f)])))))
      
      ;; get-filename/untitled-name : -> string
      ;; returns a string representing the visible name for this file,
      ;; or "Untitled <n>" for some n.
      (define untitled-name #f)
      (define/public (get-filename/untitled-name)
        (let ([filename (get-filename)])
          (if filename
              (path->string filename)
              (begin
                (unless untitled-name
                  (set! untitled-name (gui-utils:next-untitled-name)))
                untitled-name))))
      
      (inherit get-filename save-file)
      (define/public save-file/gui-error
        (opt-lambda ([input-filename #f]
                     [fmt 'same]
                     [show-errors? #t])
          (let ([filename (if (or (not input-filename)
                                  (equal? input-filename ""))
                              (let ([internal-filename (get-filename)])
                                (if (or (not internal-filename)
                                        (equal? internal-filename ""))
                                    (put-file #f #f)
                                    internal-filename))
                              input-filename)])
            (with-handlers ([exn:fail?
                             (λ (exn)
                               (message-box
                                (string-constant error-saving)
                                (string-append
                                 (format (string-constant error-saving-file/name) 
                                         filename)
                                 "\n\n"
                                 (format-error-message exn))
                                #f
                                '(stop ok))
                               #f)])
              (when filename
                (save-file filename fmt #f))
              #t))))
      
      (inherit load-file)
      (define/public load-file/gui-error
        (opt-lambda ([input-filename #f]
                     [fmt 'guess]
                     [show-errors? #t])
          (let ([filename (if (or (not input-filename)
                                  (equal? input-filename ""))
                              (let ([internal-filename (get-filename)])
                                (if (or (not internal-filename)
                                        (equal? internal-filename ""))
                                    (get-file #f)
                                    internal-filename))
                              input-filename)])
            (with-handlers ([exn:fail?
                             (λ (exn)
                               (message-box 
                                (string-constant error-loading)
                                (string-append
                                 (format (string-constant error-loading-file/name)
                                         filename)
                                 "\n\n"
                                 (format-error-message exn))
                                #f
                                '(stop ok))
                               #f)])
              (load-file input-filename fmt show-errors?)
              #t))))
      
      (define/private (format-error-message exn)
        (if (exn? exn)
            (format "~a" (exn-message exn))
            (format "uncaught exn: ~s" exn)))
      
      (inherit refresh-delayed? 
               get-canvas
               get-admin)
      
      (define/augment (can-save-file? filename format)
        (and (if (equal? filename (get-filename))
                 (if (save-file-out-of-date?)
                     (gui-utils:get-choice 
                      (string-constant file-has-been-modified)
                      (string-constant overwrite-file-button-label)
                      (string-constant cancel)
                      (string-constant warning)
                      #f
                      (get-top-level-window))
                     #t)
                 #t)
             (inner #t can-save-file? filename format)))
      
      (define last-saved-file-time #f)
      
      (define/augment (after-save-file success?)
        ;; update recently opened file names
        (let* ([temp-b (box #f)]
               [filename (get-filename temp-b)])
          (unless (unbox temp-b)
            (when filename
              (handler:add-to-recent filename))))
        
        ;; update last-saved-file-time
        (when success?
          (let ([filename (get-filename)])
            (set! last-saved-file-time
                  (and filename
                       (file-exists? filename)
                       (file-or-directory-modify-seconds filename)))))
        
        (inner (void) after-save-file success?))
      
      (define/augment (after-load-file success?)
        (when success?
          (let ([filename (get-filename)])
            (set! last-saved-file-time
                  (and filename
                       (file-exists? filename)
                       (file-or-directory-modify-seconds filename)))))
        (inner (void) after-load-file success?))
      
      (define/public (save-file-out-of-date?)
        (and last-saved-file-time
             (let ([fn (get-filename)])
               (and fn
                    (file-exists? fn)
                    (let ([ms (file-or-directory-modify-seconds fn)])
                      (< last-saved-file-time ms))))))
      
      (define has-focus #f)
      (define/override (on-focus x)
        (set! has-focus x)
        (super on-focus x))
      (define/public (has-focus?) has-focus)
      
      (define/public (get-top-level-window)
        (let loop ([text this])
          (let ([editor-admin (send text get-admin)])
            (cond
              [(is-a? editor-admin editor-snip-editor-admin<%>)
               (let* ([snip (send editor-admin get-snip)]
                      [snip-admin (send snip get-admin)])
                 (loop (send snip-admin get-editor)))]
              [(send text get-canvas) 
               => 
               (λ (canvas)
                 (send canvas get-top-level-window))]
              [else #f]))))
      
      [define edit-sequence-queue null]
      [define edit-sequence-ht (make-hasheq)]
      [define in-local-edit-sequence? #f]
      [define/public local-edit-sequence? (λ () in-local-edit-sequence?)]
      [define/public run-after-edit-sequence
        (case-lambda 
          [(t) (run-after-edit-sequence t #f)]
          [(t sym)
           (unless (and (procedure? t)
                        (= 0 (procedure-arity t)))
             (error 'editor:basic::run-after-edit-sequence
                    "expected procedure of arity zero, got: ~s\n" t))
           (unless (or (symbol? sym) (not sym))
             (error 'editor:basic::run-after-edit-sequence
                    "expected second argument to be a symbol or #f, got: ~s\n"
                    sym))
           (if (refresh-delayed?)
               (if in-local-edit-sequence?
                   (cond
                     [(symbol? sym)
                      (hash-set! edit-sequence-ht sym t)]
                     [else (set! edit-sequence-queue
                                 (cons t edit-sequence-queue))])
                   (let ([snip-admin (get-admin)])
                     (cond
                       [(not snip-admin)
                        (t)] ;; refresh-delayed? is always #t when there is no admin.
                       [(is-a? snip-admin editor-snip-editor-admin<%>)
                        (let loop ([ed this])
                          (let ([snip-admin (send ed get-admin)])
                            (if (is-a? snip-admin editor-snip-editor-admin<%>)
                                (let ([up-one
                                       (send (send (send snip-admin get-snip) get-admin) get-editor)])
                                  (if (is-a? up-one basic<%>)
                                      (send up-one run-after-edit-sequence t sym)
                                      (loop up-one)))
                                
                                ;; here we are in an embdedded editor that is not
                                ;; in an edit sequence and the "parents" of the embdedded editor
                                ;; are all non-basic<%> objects, so we just run the thunk now.
                                (t))))]
                       [else
                        '(message-box "run-after-edit-sequence error"
                                      (format "refresh-delayed? is #t but snip admin, ~s, is not an editor-snip-editor-admin<%>"
                                              snip-admin))
                        '(t)
                        (void)])))
               (t))
           (void)])]
      [define/public extend-edit-sequence-queue
        (λ (l ht)
          (hash-for-each ht (λ (k t) (hash-set! edit-sequence-ht k t)))
          (set! edit-sequence-queue (append l edit-sequence-queue)))]
      (define/augment (on-edit-sequence)
        (set! in-local-edit-sequence? #t)
        (inner (void) on-edit-sequence))
      (define/augment (after-edit-sequence)
        (set! in-local-edit-sequence? #f)
        (let ([queue edit-sequence-queue]
              [ht edit-sequence-ht]
              [find-enclosing-editor
               (λ (editor)
                 (let ([admin (send editor get-admin)])
                   (cond
                     [(is-a? admin editor-snip-editor-admin<%>)
                      (send (send (send admin get-snip) get-admin) get-editor)]
                     [else #f])))])
          (set! edit-sequence-queue null)
          (set! edit-sequence-ht (make-hash))
          (let loop ([editor (find-enclosing-editor this)])
            (cond
              [(and editor 
                    (is-a? editor basic<%>)
                    (not (send editor local-edit-sequence?)))
               (loop (find-enclosing-editor editor))]
              [(and editor
                    (is-a? editor basic<%>))
               (send editor extend-edit-sequence-queue queue ht)]
              [else
               (hash-for-each ht (λ (k t) (t)))
               (for-each (λ (t) (t)) queue)])))
        (inner (void) after-edit-sequence))
      
      (define/override (on-new-box type)
        (cond
          [(eq? type 'text) (make-object editor-snip% (make-object text:basic%))]
          [else (make-object editor-snip% (make-object pasteboard:basic%))]))
      
      (define/override (on-new-image-snip filename kind relative-path? inline?)
        (super on-new-image-snip 
               filename
               (if (eq? kind 'unknown) 'unknown/mask kind) 
               relative-path? 
               inline?))
      
      (define/override (get-file d)
        (parameterize ([finder:dialog-parent-parameter
                        (get-top-level-window)])
          (finder:get-file d)))
      (define/override (put-file d f)
        (parameterize ([finder:dialog-parent-parameter
                        (get-top-level-window)])
          (finder:put-file f d)))
      
      (super-new)))
  
  (define standard-style-list (new style-list%))
  (define (get-standard-style-list) standard-style-list)
  
  (define default-color-style-name "framework:default-color")
  (define (get-default-color-style-name) default-color-style-name)
  
  (let ([delta (make-object style-delta% 'change-normal)])
    (send delta set-delta 'change-family 'modern)
    (let ([style (send standard-style-list find-named-style "Standard")])
      (if style
          (send style set-delta delta)
          (send standard-style-list new-named-style "Standard"
                (send standard-style-list find-or-create-style
                      (send standard-style-list basic-style)
                      delta)))))
  
  (let ([delta (make-object style-delta%)]
        [style (send standard-style-list find-named-style default-color-style-name)])
    (if style
        (send style set-delta delta)
        (send standard-style-list new-named-style default-color-style-name
              (send standard-style-list find-or-create-style
                    (send standard-style-list find-named-style "Standard")
                    delta))))
  
  (define (set-default-font-color color)
    (let* ([scheme-standard (send standard-style-list find-named-style default-color-style-name)]
           [scheme-delta (make-object style-delta%)])
      (send scheme-standard get-delta scheme-delta)
      (send scheme-delta set-delta-foreground color)
      (send scheme-standard set-delta scheme-delta)))
  
  (define (set-font-size size)
    (update-standard-style
     (λ (scheme-delta)
       (send scheme-delta set-size-mult 0)
       (send scheme-delta set-size-add size))))
  
  (define (set-font-name name)
    (update-standard-style
     (λ (scheme-delta)
       (send scheme-delta set-delta-face name)
       (send scheme-delta set-family 'modern))))
  
  (define (set-font-smoothing sym)
    (update-standard-style
     (λ (scheme-delta)
       (send scheme-delta set-smoothing-on sym))))
  
  (define (update-standard-style cng-delta)
    (let* ([scheme-standard (send standard-style-list find-named-style "Standard")]
           [scheme-delta (make-object style-delta%)])
      (send scheme-standard get-delta scheme-delta)
      (cng-delta scheme-delta)
      (send scheme-standard set-delta scheme-delta)))
  
  (define standard-style-list<%>
    (interface (editor<%>)
      ))
  
  (define standard-style-list-mixin
    (mixin (editor<%>) (standard-style-list<%>)
      (super-instantiate ())
      (inherit set-style-list set-load-overwrites-styles)
      (set-style-list standard-style-list)
      (set-load-overwrites-styles #f)))
  
  (define (set-standard-style-list-pref-callbacks)
    (set-font-size (preferences:get 'framework:standard-style-list:font-size))
    (set-font-name (preferences:get 'framework:standard-style-list:font-name))
    (set-font-smoothing (preferences:get 'framework:standard-style-list:smoothing))
    (preferences:add-callback 'framework:standard-style-list:font-size (λ (p v) (set-font-size v)))
    (preferences:add-callback 'framework:standard-style-list:font-name (λ (p v) (set-font-name v)))
    (preferences:add-callback 'framework:standard-style-list:smoothing (λ (p v) (set-font-smoothing v)))
    
    (unless (member (preferences:get 'framework:standard-style-list:font-name) (get-face-list))
      (preferences:set 'framework:standard-style-list:font-name (get-family-builtin-face 'modern))))
  
  ;; set-standard-style-list-delta : string (is-a?/c style-delta<%>) -> void
  (define (set-standard-style-list-delta name delta)
    (let* ([style-list (get-standard-style-list)]
           [style (send style-list find-named-style name)])
      (if style
          (send style set-delta delta)
          (send style-list new-named-style name
                (send style-list find-or-create-style
                      (send style-list find-named-style "Standard")
                      delta)))
      (void)))
  
  (define -keymap<%> (interface (basic<%>) get-keymaps))
  (define keymap-mixin
    (mixin (basic<%>) (-keymap<%>)
      (define/public (get-keymaps)
        (list (keymap:get-user) (keymap:get-global)))
      (inherit set-keymap)
      
      (super-new)
      (let ([keymap (make-object keymap:aug-keymap%)])
        (set-keymap keymap)
        (for-each (λ (k) (send keymap chain-to-keymap k #f))
                  (get-keymaps)))))
  
  (define (add-after-user-keymap km kms)
    (let loop ([kms kms])
      (cond
        [(null? kms) (list km)]
        [else
         (let ([f (car kms)])
           (if (eq? f (keymap:get-user))
               (list* f km (cdr kms))
               (cons f (loop (cdr kms)))))])))
  
  (define autowrap<%> (interface (basic<%>)))
  (define autowrap-mixin
    (mixin (basic<%>) (autowrap<%>)
      (inherit auto-wrap)
      (super-instantiate ())
      (auto-wrap 
       (preferences:get
        'framework:auto-set-wrap?))))
  
  (define file<%> 
    (interface (-keymap<%>)
      get-can-close-parent
      update-frame-filename
      allow-close-with-no-filename?))
  
  (define file-mixin
    (mixin (-keymap<%>) (file<%>)
      (inherit get-filename lock get-style-list 
               is-modified? set-modified 
               get-top-level-window)
      
      (inherit get-canvases get-filename/untitled-name)
      (define/public (update-frame-filename)
        (let* ([filename (get-filename)]
               [name (if filename
                         (path->string 
                          (file-name-from-path 
                           filename))
                         (get-filename/untitled-name))])
          (for-each (λ (canvas)
                      (let ([tlw (send canvas get-top-level-window)])
                        (when (and (is-a? tlw frame:editor<%>)
                                   (eq? this (send tlw get-editor)))
                          (send tlw set-label name))))
                    (get-canvases))))
      
      (define/override set-filename
        (case-lambda
          [(name) (set-filename name #f)]
          [(name temp?)
           (super set-filename name temp?)
           (unless temp?
             (update-frame-filename))]))
      
      (inherit save-file)
      (define/public (allow-close-with-no-filename?) #f)
      (define/augment (can-close?)
        (and (user-saves-or-not-modified?)
             (inner #t can-close?)))
      
      (define/public (user-saves-or-not-modified? [allow-cancel? #t])
        (or (not (is-modified?))
            (and (not (get-filename))
                 (allow-close-with-no-filename?))
            (case (gui-utils:unsaved-warning
                   (get-filename/untitled-name)
                   (string-constant dont-save)
                   #t
                   (or (get-top-level-window)
                       (get-can-close-parent))
                   allow-cancel?)
              [(continue) #t]
              [(save) (save-file)]
              [else #f])))
      
      (define/public (get-can-close-parent) #f)
      
      (define/override (get-keymaps)
        (add-after-user-keymap (keymap:get-file) (super get-keymaps)))
      (super-new)))
  
  (define backup-autosave<%>
    (interface (basic<%>)
      backup?
      autosave?
      do-autosave
      remove-autosave))
  
  (define backup-autosave-mixin
    (mixin (basic<%>) (backup-autosave<%> autosave:autosavable<%>)
      (inherit is-modified? get-filename save-file)
      [define auto-saved-name #f]
      [define auto-save-out-of-date? #t]
      [define auto-save-error? #f]
      (define/private (file-old? filename)
        (if (and filename
                 (file-exists? filename))
            (let ([modified-seconds (file-or-directory-modify-seconds filename)]
                  [old-seconds (- (current-seconds) (* 7 24 60 60))])
              (< modified-seconds old-seconds))
            #t))
      (define/public (backup?) (preferences:get 'framework:backup-files?))
      (define/augment (on-save-file name format)
        (when (and (backup?)
                   (not (eq? format 'copy))
                   (file-exists? name))
          (let ([back-name (path-utils:generate-backup-name name)])
            (when (or (not (file-exists? back-name))
                      (file-old? back-name))
              (with-handlers ([exn:fail? 
                               (λ (exn)
                                 (log-debug "failed to clean up autosave file.1: ~a" back-name))])
                (when (file-exists? back-name)
                  (delete-file back-name))
                (copy-file name back-name)))))
        (inner (void) on-save-file name format))
      (define/augment (after-save-file success?)
        (when success?
          (set! auto-save-error? #f))
        (inner (void) after-save-file success?))

      (define/augment (on-close)
        (remove-autosave)
        (set! do-autosave? #f)
        (inner (void) on-close))
      (define/augment (on-change)
        (set! auto-save-out-of-date? #t)
        (inner (void) on-change))
      (define/override (set-modified modified?)
        (when auto-saved-name
          (if modified?
              (set! auto-save-out-of-date? #t)
              (remove-autosave)))
        (super set-modified modified?))
      
      [define do-autosave? #t]
      (define/public (autosave?) do-autosave?)
      
      (define/public (do-autosave)
        (cond
          [(and (autosave?)
                (not auto-save-error?)
                (is-modified?)
                (or (not auto-saved-name)
                    auto-save-out-of-date?))
           (let* ([orig-name (get-filename)]
                  [old-auto-name auto-saved-name]
                  [auto-name (path-utils:generate-autosave-name orig-name)]
                  [orig-format (and (is-a? this text%)
                                    (send this get-file-format))])
             (when (is-a? this text%)
               (send this set-file-format 'standard))
             (with-handlers ([exn:fail?
                              (λ (exn)
                                (show-autosave-error exn orig-name)
                                (set! auto-save-error? #t)
                                (when (is-a? this text%)
                                  (send this set-file-format orig-format))
                                #f)])
               (save-file auto-name 'copy #f)
               (when (is-a? this text%)
                 (send this set-file-format orig-format))
               (when old-auto-name
                 (when (file-exists? old-auto-name)
                   (delete-file old-auto-name)))
               (set! auto-saved-name auto-name)
               (set! auto-save-out-of-date? #f)
               auto-name))]
          [else auto-saved-name]))
      
      ;; show-autosave-error : any (union #f string) -> void
      ;; opens a message box displaying the exn and the filename
      ;; to the user.
      (define/private (show-autosave-error exn orig-name)
        (message-box 
         (string-constant warning)
         (apply
          string-append
          (format (string-constant error-autosaving)
                  (or orig-name (string-constant untitled)))
          "\n"
          (string-constant autosaving-turned-off)
          "\n\n"
          (if (exn? exn)
              (format "~a" (exn-message exn))
              (format "~s" exn))
          "\n\n"
          (if (and (exn? exn)
                   (continuation-mark-set? (exn-continuation-marks exn)))
              (for/list ([fr (in-list (continuation-mark-set->context 
                                       (exn-continuation-marks exn)))])
                (format "   ~s\n" fr))
              '()))
         #f
         '(caution ok)))
      
      (define/public (remove-autosave)
        (when auto-saved-name
          (when (file-exists? auto-saved-name)
            (with-handlers ([exn:fail? 
                             (λ (exn)
                               (log-debug "failed to clean up autosave file.2: ~a" 
                                          auto-saved-name))])            
              (delete-file auto-saved-name)
              (set! auto-saved-name #f)))))
      (super-new)
      (autosave:register this)))
  
  (define info<%> (interface (basic<%>)))
  (define info-mixin
    (mixin (basic<%>) (info<%>)
      (inherit get-top-level-window run-after-edit-sequence)
      (define callback-running? #f)
      (define/override (lock x)
        (super lock x)
        (run-after-edit-sequence
         (rec send-frame-update-lock-icon
           (λ ()
             (unless callback-running?
               (set! callback-running? #t)
               (queue-callback
                (λ ()
                  (let ([frame (get-top-level-window)])
                    (when (is-a? frame frame:info<%>)
                      (send frame lock-status-changed)))
                  (set! callback-running? #f))
                #f))))
         'framework:update-lock-icon))
      (super-new)))

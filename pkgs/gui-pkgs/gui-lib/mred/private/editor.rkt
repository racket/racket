#lang racket/base

(require racket/class
         racket/list
         racket/file
         racket/path
         (for-syntax racket/base)
         (prefix-in wx: "kernel.rkt")
         (prefix-in wx: racket/snip/private/style)
         (prefix-in wx: racket/snip/private/snip)
         (prefix-in wx: "wxme/keymap.rkt")
         (prefix-in wx: "wxme/editor.rkt")
         (prefix-in wx: "wxme/text.rkt")
         (prefix-in wx: "wxme/pasteboard.rkt")
         (prefix-in wx: "wxme/editor-snip.rkt")
         (prefix-in wx: (only-in "wxme/cycle.rkt"
                                 set-extended-editor-snip%!
                                 set-extended-text%!
                                 set-extended-pasteboard%!))
         "seqcontract.rkt"
         "lock.rkt"
         "check.rkt"
         "const.rkt"
         "helper.rkt"
         "cycle.rkt"
         "wx.rkt"
         "wxtop.rkt"
         "wxitem.rkt"
         "wxcanvas.rkt"
         "mrwindow.rkt"
         "mrtop.rkt"
         "mrcanvas.rkt"
         "mrpopup.rkt"
         "mrmenuintf.rkt"
         "mrmenu.rkt")

(provide editor<%>
         text%
         pasteboard%
         editor-snip%
         current-text-keymap-initializer
         append-editor-operation-menu-items
         append-editor-font-menu-items)

(define editor<%>
  (interface (wx:editor<%>)
    get-canvases
    get-active-canvas set-active-canvas
    get-canvas
    add-canvas remove-canvas
    auto-wrap get-max-view-size
    save-file))

(define-local-member-name
  -format-filter
  -format-filter/save
  -get-current-format
  -get-file-format
  -set-file-format
  -set-position
  -set-format)

(define (check-format who format)
  (unless (memq format '(guess standard text text-force-cr same copy))
    (raise-argument-error (who->name who)
                          "(or/c 'guess 'standard 'text 'text-force-cr 'same 'copy)"
                          format)))

(define-syntax (augmentize stx)
  (syntax-case stx ()
    [(_ (result id arg ...) ...)
     #'(begin
         (define/overment (id arg ...)
           (and (super id arg ...)
                (inner result id arg ...)))
         ...)]))

(define (make-editor-buffer% % can-wrap? get-editor%)
                                        ; >>> This class is instantiated directly by the end-user <<<
  (class* % (editor<%> internal-editor<%>)
    (init-rest args)
    (rename-super [super-get-view-size get-view-size]
                  [super-begin-edit-sequence begin-edit-sequence]
                  [super-end-edit-sequence end-edit-sequence]
                  [super-insert-port insert-port]
                  [super-save-port save-port]
                  [super-erase erase]
                  [super-clear-undos clear-undos]
                  [super-get-load-overwrites-styles get-load-overwrites-styles]
                  [super-get-filename get-filename])
    (inherit get-max-width set-max-width get-admin
             get-keymap get-style-list
             set-modified set-filename
             get-file put-file
             get-max-undo-history)
    (define canvases null)
    (define active-canvas #f)
    (define auto-set-wrap? #f)
    (define use-text-mode? #t)
    (private*
     [max-view-size
      (lambda ()
        (let ([wb (box 0)]
              [hb (box 0)])
          (super-get-view-size wb hb)
          (unless (or (null? canvases) (null? (cdr canvases)))
            (for-each
             (lambda (canvas)
               (send canvas call-as-primary-owner
                     (lambda ()
                       (let ([wb2 (box 0)]
                             [hb2 (box 0)])
                         (super-get-view-size wb2 hb2)
                         (set-box! wb (max (unbox wb) (unbox wb2)))
                         (set-box! hb (max (unbox hb) (unbox hb2)))))))
             canvases))
          (values (unbox wb) (unbox hb))))])
    (public*
     [-format-filter (lambda (f) f)]
     [-format-filter/save (lambda (f) f)]
     [-set-file-format (lambda (f) (void))]
     [-set-position (lambda () (void))]
     [-get-file-format (lambda () 'standard)])

    (override*
     [insert-file
      (lambda (file [format 'guess] [show-errors? #t])
        (let ([who '(method editor<%> insert-file)])
          (check-path who file)
          (check-format who format))
        (do-load-file file format #f))]

     [load-file
      (lambda ([file #f] [format 'guess] [show-errors? #t])
        (do-load-file file format #t))])

    (public*
     [use-file-text-mode
      (case-lambda
        [() use-text-mode?]
        [(v?) (set! use-text-mode? (and v? #t))])])

    (private*
     [do-load-file
      (lambda (file format load?)
        (let ([who '(method editor<%> load-file)])
          (unless (equal? file "")
            (check-path/false who file))
          (check-format who format))
        (let* ([temp-filename?-box (box #f)]
               [old-filename (super-get-filename temp-filename?-box)])
          (let* ([file (cond
                        [(or (not (path-string? file))
                             (equal? file ""))
                         (if (or (equal? file "") (not old-filename) (unbox temp-filename?-box))
                             (let ([path (if old-filename
                                             (path-only old-filename)
                                             #f)])
                               (get-file path))
                             old-filename)]
                        [(path? file) file]
                        [else (string->path file)])])
            (and
             file
             (or (not load?)
                 (can-load-file? file (-format-filter format)))
             (begin
               (or (not load?)
                   (on-load-file file (-format-filter format)))
               (let ([port #f]
                     [finished? #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (set! port (open-input-file file))
                     (wx:begin-busy-cursor)
                     (super-begin-edit-sequence)
                     (dynamic-wind
                       void
                       (lambda ()
                         (when load?
                           (super-erase)
                           (unless (and (not (unbox temp-filename?-box))
                                        (equal? file old-filename))
                             (set-filename file #f)))
                         (let ([format (if (eq? format 'same)
                                           (-get-file-format)
                                           format)])
                           (let ([new-format
                                  (with-handlers ([values (lambda (x)
                                                            (set-filename #f #f)
                                                            (raise x))])
                                    (super-insert-port port
                                                       (-format-filter format)
                                                       (and load?
                                                            (super-get-load-overwrites-styles))))])
                             (close-input-port port) ; close as soon as possible
                             (when load?
                               (-set-file-format new-format)
                               (-set-position))))) ; text% only
                       (lambda ()
                         (super-end-edit-sequence)
                         (wx:end-busy-cursor)))
                     (when load?
                       (super-clear-undos)
                       (set-modified #f))
                     (set! finished? #t)
                     #t)
                   (lambda ()
                     ;; In case it wasn't closed before:
                     (when port (close-input-port port))
                     (when load?
                       (after-load-file finished?))))))))))])
    (public*
     [save-file
      (lambda ([file #f] [format 'same] [show-errors? #t])
        (let ([who '(method editor<%> save-file)])
          (unless (equal? file "")
            (check-path/false who file))
          (check-format who format))
        (let* ([temp-filename?-box (box #f)]
               [old-filename (super-get-filename temp-filename?-box)])
          (let* ([file (cond
                        [(or (not (path-string? file))
                             (equal? file ""))
                         (if (or (equal? file "") (not old-filename) (unbox temp-filename?-box))
                             (let ([path (if old-filename
                                             (path-only old-filename)
                                             #f)])
                               (put-file path (and old-filename
                                                   (file-name-from-path old-filename))))
                             old-filename)]
                        [(path? file) file]
                        [else (string->path file)])]
                 [f-format (-format-filter/save format)])
            (and
             file
             (can-save-file? file f-format)
             (begin
               (on-save-file file f-format)
               (let* ([actual-format (if (memq f-format '(copy same))
                                         (-get-file-format)
                                         f-format)]
                      [text? (memq actual-format '(text text-force-cr))]
                      [text-mode? (and text? use-text-mode?)])
                 (let ([port #f]
                       [finished? #f])
                   (dynamic-wind
                     void
                     (lambda ()
                       (set! port (open-output-file file
                                                    #:mode (if text-mode? 'text 'binary)
                                                    #:exists 'truncate/replace))
                       (wx:file-creator-and-type file #"mReD" (if text? #"TEXT" #"WXME"))
                       (wx:begin-busy-cursor)
                       (dynamic-wind
                         void
                         (lambda ()
                           (super-save-port port format #t)
                           (close-output-port port)     ; close as soon as possible
                           (unless (or (eq? format 'copy)
                                       (and (not (unbox temp-filename?-box))
                                            (equal? file old-filename)))
                             (set-filename file #f))
                           (unless (eq? format 'copy)
                             (-set-file-format actual-format))) ; text% only
                         (lambda ()
                           (wx:end-busy-cursor)))
                       (unless (eq? format 'copy)
                         (set-modified #f))
                       (set! finished? #t)
                       #t)
                     (lambda ()
                       ;; In case it wasn't closed before:
                       (when port (close-output-port port))
                       (after-save-file finished?))))))))))])

    (public*
     [get-canvases (entry-point (lambda () (map wx->mred canvases)))]
     [get-active-canvas (entry-point (lambda () (and active-canvas (wx->mred active-canvas))))]
     [get-canvas
      (entry-point
       (lambda ()
         (let ([c (or active-canvas
                      (and (not (null? canvases))
                           (car canvases)))])
           (and c (wx->mred c)))))]
     [set-active-canvas
      (entry-point
       (lambda (new-canvas)
         (check-instance '(method editor<%> set-active-canvas) editor-canvas% 'editor-canvas% #t new-canvas)
         (set! active-canvas (mred->wx new-canvas))))]

     [add-canvas
      (entry-point
       (lambda (new-canvas)
         (check-instance '(method editor<%> add-canvas) editor-canvas% 'editor-canvas% #f new-canvas)
         (let ([new-canvas (mred->wx new-canvas)])
           (unless (memq new-canvas canvases)
             (set! canvases (cons new-canvas canvases))))))]

     [remove-canvas
      (entry-point
       (lambda (old-canvas)
         (check-instance '(method editor<%> remove-canvas) editor-canvas% 'editor-canvas% #f old-canvas)
         (let ([old-canvas (mred->wx old-canvas)])
           (when (eq? old-canvas active-canvas)
             (set! active-canvas #f))
           (set! canvases (remq old-canvas canvases)))))]

     [auto-wrap (case-lambda
                  [() auto-set-wrap?]
                  [(on?) (as-entry
                          (lambda ()
                            (set! auto-set-wrap? (and on? #t))
                            (as-exit
                             (lambda ()
                               (if on?
                                   (on-display-size)
                                   (set-max-width 'none))))))])]
     [get-max-view-size (entry-point (lambda () (max-view-size)))])
    (override*
     [copy-self
      (lambda () (let ([e (make-object (get-editor%))])
                   (copy-self-to e)
                   e))]
     [copy-self-to
      (lambda (e)
        (super copy-self-to e)
        (send e auto-wrap auto-set-wrap?))])

    (overment*
     [on-display-size
      (entry-point
       (lambda ()
         (as-exit (lambda () (super on-display-size)))
         (when (as-exit (lambda () (get-admin)))
           (when (and can-wrap? auto-set-wrap?)
             (let-values ([(current-width) (as-exit (lambda () (get-max-width)))]
                          [(new-width new-height) (max-view-size)])
               (when (and (not (equal? current-width new-width))
                          (< 0 new-width))
                 (as-exit (lambda () (set-max-width new-width)))))))
         (as-exit (lambda () (inner (void) on-display-size)))))])

    (augmentize ((void) on-change)
                ((void) on-snip-modified snip x)
                (#t can-save-file? p t)
                ((void) on-save-file p t)
                ((void) after-save-file t)
                (#t can-load-file? p t)
                ((void) on-load-file p t)
                ((void) after-load-file t)
                ((void) on-edit-sequence)
                ((void) after-edit-sequence))

    (private*
     [sp (lambda (x y z f b? eps?)
           ;; let super method report z errors:
           (let ([zok? (memq z '(standard postscript))])
             (when zok?
               (check-top-level-parent/false '(method editor<%> print) f))
             (let ([p (and zok? f (mred->wx f))])
               (as-exit (lambda () (super print x y z p b? eps?))))))])

    (override*
     [print
      (entry-point
       (case-lambda
         [() (sp #t #t 'standard #f #t #f)]
         [(x) (sp x #t 'standard #f #t #f)]
         [(x y) (sp x y 'standard #f #t #f)]
         [(x y z) (sp x y z #f #t #f)]
         [(x y z f) (sp x y z f #t #f)]
         [(x y z f b?) (sp x y z f b? #f)]
         [(x y z f b? eps?) (sp x y z f b? eps?)]))]

     [on-new-box
      (entry-point
       (lambda (type)
         (unless (memq type '(text pasteboard))
           (raise-argument-error (who->name '(method editor<%> on-new-box)) "(or/c 'text 'pasteboard)" type))
         (make-object editor-snip%
                      (let ([e (make-object (cond
                                             [(eq? type 'pasteboard) pasteboard%]
                                             [else text%]))])
                        (send e set-keymap (get-keymap))
                        (send e set-style-list (get-style-list))
                        (send e set-max-undo-history (get-max-undo-history))
                        e))))])

    (apply super-make-object args)))

(define text%
  (class (lock-contract-mixin
          (es-contract-mixin
           (make-editor-buffer% wx:text% #t  (lambda () text%))))
    (init [line-spacing 1.0]
          [tab-stops null]
          [(aw? auto-wrap) #f])
    (rename-super [super-get-file-format get-file-format]
                  [super-set-file-format set-file-format]
                  [super-set-position set-position]
                  [super-auto-wrap auto-wrap])
    (override*
     [-get-file-format (lambda ()
                         (super-get-file-format))]
     [-set-file-format (lambda (format)
                         (super-set-file-format format))]
     [-set-position (lambda ()
                      (super-set-position 0 0))])

    (augmentize (#t can-insert? s e)
                ((void) on-insert s e)
                ((void) after-insert s e)
                (#t can-delete? s e)
                ((void) on-delete s e)
                ((void) after-delete s e)
                (#t can-change-style? s e)
                ((void) on-change-style s e)
                ((void) after-change-style s e)
                ((void) after-set-position)
                (#t can-set-size-constraint?)
                ((void) on-set-size-constraint)
                ((void) after-set-size-constraint)
                ((void) after-split-snip s)
                ((void) after-merge-snips s)
                ((void) on-reflow))

    (super-make-object line-spacing tab-stops)
    (when aw?
      (super-auto-wrap #t))))

(define pasteboard%
  (class (es-contract-mixin (make-editor-buffer% wx:pasteboard% #f (lambda () pasteboard%)))
    (override*
     [-format-filter (lambda (f) 'standard)]
     [-format-filter/save (lambda (f) (if (eq? f 'copy)
                                          f
                                          'standard))])
    (augmentize (#t can-insert? s s2 x y)
                ((void) on-insert s s2 x y)
                ((void) after-insert s s2 x y)
                (#t can-delete? s)
                ((void) on-delete s)
                ((void) after-delete s)
                (#t can-move-to? s x y ?)
                ((void) on-move-to s x y ?)
                ((void) after-move-to s x y ?)
                (#t can-resize? s x y)
                ((void) on-resize s x y)
                ((void) after-resize s x y ?)
                (#t can-reorder? s s2 ?)
                ((void) on-reorder s s2 ?)
                ((void) after-reorder s s2 ?)
                (#t can-select? s ?)
                ((void) on-select s ?)
                ((void) after-select s ?)

                (#t can-interactive-move? e)
                ((void) on-interactive-move e)
                ((void) after-interactive-move e)
                (#t can-interactive-resize? s)
                ((void) on-interactive-resize s)
                ((void) after-interactive-resize s))
    (super-new)))

(define editor-snip%
  (class wx:editor-snip% (init [editor #f]
                               [with-border? #t]
                               [left-margin 5]
                               [top-margin 5]
                               [right-margin 5]
                               [bottom-margin 5]
                               [left-inset 1]
                               [top-inset 1]
                               [right-inset 1]
                               [bottom-inset 1]
                               [min-width 'none]
                               [max-width 'none]
                               [min-height 'none]
                               [max-height 'none])
    (super-make-object (or editor (make-object text%))
                       with-border?
                       left-margin
                       top-margin
                       right-margin
                       bottom-margin
                       left-inset
                       top-inset
                       right-inset
                       bottom-inset
                       min-width
                       max-width
                       min-height
                       max-height)))

(wx:set-extended-editor-snip%! editor-snip%)
(wx:set-extended-text%! text%)
(wx:set-extended-pasteboard%! pasteboard%)

;; ----------------------- Keymap ----------------------------------------

(define std-keymap (make-object wx:keymap%))

(let* ([k std-keymap]
       [mouse-paste (lambda (edit event)
                      (when (send event button-down?)
                        (cond
                         [(is-a? edit wx:text%)
                          (let ([x-box (box (send event get-x))]
                                [y-box (box (send event get-y))]
                                [eol-box (box #f)])
                            (send edit global-to-local x-box y-box)
                            (let ([click-pos (send edit find-position
                                                   (unbox x-box)
                                                   (unbox y-box)
                                                   eol-box)])
                              (send edit set-position click-pos)))]
                         [else (void)])
                        (send edit paste-x-selection)))]
       [mouse-popup-menu (lambda (edit event)
                           (when (send event button-up?)
                             (let ([a (send edit get-admin)])
                               (when a
                                 (let ([m (make-object popup-menu%)])
                                   (append-editor-operation-menu-items m)
                                   ;; Remove shortcut indicators (because they might not be correct)
                                   (for-each
                                    (lambda (i)
                                      (when (is-a? i selectable-menu-item<%>)
                                        (send i set-shortcut #f)))
                                    (send m get-items))
                                   (let-values ([(x y) (send edit
                                                             dc-location-to-editor-location
                                                             (send event get-x)
                                                             (send event get-y))])
                                     (send a popup-menu m (+ x 5) (+ y 5))))))))])
  (wx:add-text-keymap-functions k)
  (send k add-function "mouse-paste" mouse-paste)
  (send k add-function "mouse-popup-menu" mouse-popup-menu)
  (map
   (lambda (key func) (send k map-function key func))
   (append
    (case (system-type)
      [(windows) '(":c:c" ":c:x" ":c:v" ":c:k" ":c:z" ":c:a")]
      [(macos macosx) '(":d:c" ":d:x" ":d:v" ":d:k" ":d:z" ":d:a")]
      [(unix) '(":m:w" ":c:w" ":c:y" ":c:k" ":c:s:_" ":m:a")])
    '(":middlebutton"))
   '("copy-clipboard" "cut-clipboard" "paste-clipboard" "delete-to-end-of-line"
     "undo" "select-all" "mouse-paste"))
  (send k map-function ":rightbuttonseq" "mouse-popup-menu")
  (when (eq? (system-type) 'unix)
    (send k map-function ":c:a" "beginning-of-line")
    (send k map-function ":c:e" "end-of-line")))

(define (check-installer who)
  (lambda (p)
    (unless (and (procedure? p)
                 (procedure-arity-includes? p 1))
      (raise-argument-error who
                            "(procedure-arity-includes/c 1)"
                            p))
    p))

(define current-text-keymap-initializer
  (make-parameter (let ([default-text-keymap-initializer
                          (lambda (k)
                            (check-instance 'default-text-keymap-initializer wx:keymap% 'keymap% #f k)
                            ;; Level of indirection to protect std-keymap:
                            (let ([naya (make-object wx:keymap%)])
                              (send naya chain-to-keymap std-keymap #f)
                              (send k chain-to-keymap naya #f)))])
                    default-text-keymap-initializer)
                  (check-installer 'default-text-keymap-initializer)))

(define (find-item-editor item)
  (let ([o (let loop ([i item])
             (let ([p (send i get-parent)])
               (cond
                [(not p) #f]
                [(is-a? p popup-menu%)
                 (let ([p (send p get-popup-target)])
                   (if (is-a? p window<%>)
                       (let ([f (send p get-top-level-window)])
                         (and f (send f get-edit-target-object)))
                       p))]
                [(is-a? p menu%) (loop p)]
                [else (let ([f (send p get-frame)])
                        (and f (send f get-edit-target-object)))])))])
    (and (is-a? o wx:editor<%>)
         o)))

;; ------------------------- Menus ----------------------------------------

(define (append-editor-operation-menu-items m
                                            [text-only? #t]
                                            #:popup-position [popup-position #f])
  (unless (or (not popup-position)
              (and (list? popup-position)
                   (= 2 (length popup-position))
                   (is-a? (list-ref popup-position 0) text%)
                   (exact-nonnegative-integer? (list-ref popup-position 1))))
    (raise-argument-error 'append-editor-operation-menu-items
                          (format "~s" '(or/c #f (list/c (is-a?/c text%) exact-nonnegative-integer?)))
                          popup-position))
  (menu-parent-only 'append-editor-operation-menu-items m)
  (let* ([mk (lambda (name key op [special-case? (λ () #f)] [special-go void])
               (make-object (class menu-item%
                              (inherit enable)
                              (define/override (on-demand)
                                (let ([o (find-item-editor this)])
                                  (enable (and o
                                               (or (send o can-do-edit-operation? op)
                                                   (special-case?))))))
                              (super-make-object
                               name m
                               (lambda (i e)
                                 (let* ([o (find-item-editor i)])
                                   (and o
                                        (if (special-case?)
                                            (special-go)
                                            (send o do-edit-operation op)))))
                               key))))]
         [mk-sep (lambda () (make-object separator-menu-item% m))])
    (define (special-case?)
      (cond
        [popup-position
         (define snp (send (list-ref popup-position 0) find-snip 
                           (list-ref popup-position 1)
                           'after-or-none))
         (and snp (not (is-a? snp wx:string-snip%)))]
        [else
         #f]))
    (define (copy-special-go)
      (send (list-ref popup-position 0)
            copy #f 0 
            (list-ref popup-position 1)
            (+ (list-ref popup-position 1) 1)))
    (define (cut-special-go)
      (send (list-ref popup-position 0)
            cut #f 0 
            (list-ref popup-position 1)
            (+ (list-ref popup-position 1) 1)))
    (mk "&Undo" #\z 'undo)
    (mk "Redo" #f 'redo)
    (mk-sep)
    (mk "&Copy" #\c 'copy special-case? copy-special-go)
    (mk "Cu&t" #\x 'cut special-case? cut-special-go)
    (mk "&Paste" #\v 'paste)
    (if (eq? (system-type) 'windows)
        (mk "Delete" #f 'clear)
        (mk "Clear" #f 'clear))
    (mk "Select &All" #\a 'select-all)
    (unless text-only?
      (mk-sep)
      (mk "Insert Text Box" #f 'insert-text-box)
      (mk "Insert Pasteboard Box" #f 'insert-pasteboard-box)
      (mk "Insert Image..." #f 'insert-image))
    (void)))

(define (append-editor-font-menu-items m)
  (menu-parent-only 'append-editor-font-menu-items m)
  (let ([mk (lambda (name m cb)
              (make-object menu-item% name m
                           (lambda (i e)
                             (let* ([o (find-item-editor i)])
                               (and o (cb o))))))]
        [mk-sep (lambda (m) (make-object separator-menu-item% m))]
        [mk-menu (lambda (name) (make-object menu% name m))])
    (let ([family (mk-menu "Font")]
          [size (mk-menu "Size")]
          [style (mk-menu "Style")]
          [weight (mk-menu "Weight")]
          [underline (mk-menu "Underline")]
          [alignment (mk-menu "Alignment")]
          [color (mk-menu "Color")]
          [background (mk-menu "Background")])

                                        ; Font menu
      (for-each (lambda (l f)
                  (mk l family
                      (lambda (e)
                        (send e change-style (make-object wx:style-delta% 'change-family f)))))
                '("Standard" "Decorative" "Roman" "Script" "Swiss" "Fixed" "Symbol")
                '(default decorative roman script swiss modern symbol))
      (mk-sep family)
      (mk "Choose..." family (lambda (e) (let ([f ((get-get-font-from-user))])
                                           (when f
                                             (send e change-style (font->delta f))))))

                                        ; Size menu
      (let ([bigger (make-object menu% "Bigger" size)]
            [smaller (make-object menu% "Smaller" size)]
            [add-change-size
             (lambda (m ls dss xss)
               (for-each (lambda (l ds xs)
                           (mk l m (lambda (e)
                                     (let ([d (make-object wx:style-delta%)])
                                       (send d set-size-add ds)
                                       (send d set-size-mult xs)
                                       (send e change-style d)))))
                         ls dss xss))])
        (add-change-size bigger
                         '("+1" "+2" "+4" "+8" "+16" "+32")
                         '(1 2 4 8 16 32)
                         '(1 1 1 1 1  1))
        (mk-sep bigger)
        (add-change-size bigger
                         '("x2" "x3" "x4" "x5")
                         '(0    0    0    0)
                         '(2    3    4    5))

        (add-change-size smaller
                         '("-1" "-2" "-4" "-8" "-16" "-32")
                         '(1 -2 -4 -8 -16 -32)
                         '(1 1   1  1  1  1))
        (mk-sep smaller)
        (add-change-size smaller
                         '("/2" "/3" "/5" "/5")
                         '(0    0    0    0)
                         '(#i1/2 #i1/3 #i1/4 #i1/5))

        (for-each (lambda (s)
                    (mk (number->string s) size (lambda (e)
                                                  (let ([d (make-object wx:style-delta%)])
                                                    (send d set-size-add s)
                                                    (send d set-size-mult 0)
                                                    (send e change-style d)))))
                  '(9 10 12 14 16 24 32 48)))


      (let ([mk-cg (lambda (cmd arg)
                     (lambda (e) (send e change-style (make-object wx:style-delta% cmd arg))))])

                                        ; Style
        (for-each (lambda (name s)
                    (mk name style (mk-cg 'change-style s)))
                  '("Normal" "Italic" "Slant")
                  '(normal italic slant))

                                        ; Weight
        (for-each (lambda (name s)
                    (mk name weight (mk-cg 'change-weight s)))
                  '("Normal" "Bold" "Light")
                  '(normal bold light))

                                        ; Underline
        (mk "No Underline" underline (mk-cg 'change-underline #f))
        (mk "Underline" underline (mk-cg 'change-underline #t))
        (mk "Toggle" underline (lambda (e) (send e change-style (make-object wx:style-delta% 'change-toggle-underline))))

                                        ; Alignment
        (for-each (lambda (name s)
                    (mk name alignment (mk-cg 'change-alignment s)))
                  '("Top" "Center" "Bottom")
                  '(top center bottom))

        (let ([colors '("Black" "White" "Red" "Orange" "Yellow" "Green" "Blue" "Purple" "Cyan" "Magenta" "Grey")])

                                        ; Colors
          (for-each (lambda (c)
                      (mk c color (lambda (e) (let ([d (make-object wx:style-delta%)])
                                                (send d set-delta-foreground c)
                                                (send e change-style d)))))
                    colors)

                                        ; Background
          (mk "Transparent" background (lambda (e) (let ([d (make-object wx:style-delta%)])
                                                     (send d set-transparent-text-backing-on #t)
                                                     (send e change-style d))))
          (for-each (lambda (c)
                      (mk c background (lambda (e) (let ([d (make-object wx:style-delta%)])
                                                     (send d set-delta-background c)
                                                     (send e change-style d)))))
                    colors))))))

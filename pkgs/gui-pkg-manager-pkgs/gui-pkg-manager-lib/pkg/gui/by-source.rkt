#lang racket/base

(require racket/gui/base 
         racket/class
         racket/format
         string-constants
         pkg/name
         pkg/lib
         pkg
         racket/list
         framework
         net/url
	 "common.rkt")

(provide by-source-panel%)

(define sc-install-pkg-dialog-title (string-constant install-pkg-dialog-title))
(define sc-install-pkg-source-label (string-constant install-pkg-source-label))
(define sc-install-pkg-type-label (string-constant install-pkg-type-label))
(define sc-install-pkg-infer (string-constant install-pkg-infer))
(define sc-install-pkg-file (string-constant install-pkg-file))
(define sc-install-pkg-dir (string-constant install-pkg-dir))
(define sc-install-pkg-dir-url (string-constant install-pkg-dir-url))
(define sc-install-pkg-file-url (string-constant install-pkg-file-url))
(define sc-install-pkg-github (string-constant install-pkg-github))
(define sc-install-pkg-name (string-constant install-pkg-name))
(define sc-install-pkg-inferred-as (string-constant install-pkg-inferred-as))
(define sc-install-pkg-force? (string-constant install-pkg-force?))
(define sc-install-pkg-replace? (string-constant install-pkg-replace?))
(define sc-install-pkg-command-line (string-constant install-pkg-command-line))

(define sc-install-pkg-action-label (string-constant install-pkg-action-label))
(define sc-install-pkg-install (string-constant install-pkg-install))
(define sc-install-pkg-update (string-constant install-pkg-update))
(define sc-action-inferred-to-be-update (string-constant install-pkg-action-inferred-to-be-update))
(define sc-action-inferred-to-be-install (string-constant install-pkg-action-inferred-to-be-install))

(define sc-install-pkg-default (string-constant install-pkg-default))
(define sc-install-pkg-scope-label (string-constant install-pkg-scope-label))
(define sc-install-pkg-installation (string-constant install-pkg-installation))
(define sc-install-pkg-user (string-constant install-pkg-user))
(define sc-install-pkg-set-as-default (string-constant install-pkg-set-as-default))
(define sc-install-pkg-scope-is (string-constant install-pkg-scope-is))
  
(define sc-install-pkg-browse (string-constant browse...))

(define sc-install-pkg-abort-set-scope (string-constant install-pkg-abort-set-scope))

(define sc-install-pkg-dependencies-fail (string-constant install-pkg-dependencies-fail))
(define sc-install-pkg-dependencies-force (string-constant install-pkg-dependencies-force))
(define sc-install-pkg-dependencies-search-ask (string-constant install-pkg-dependencies-search-ask))
(define sc-install-pkg-dependencies-search-auto (string-constant install-pkg-dependencies-search-auto))
(define sc-install-pkg-dependencies-search-auto+update (string-constant install-pkg-dependencies-search-auto+update))

(define sc-install-pkg-dependencies-mode (string-constant install-pkg-dependencies-mode))

(define sc-install-pkg-dependencies-search-ask-not-supported-in-gui
  (string-constant install-pkg-dependencies-search-ask-not-supported-in-gui))
(define sc-install-pkg-deps-is (string-constant install-pkg-deps-is))

(preferences:set-default 'drracket:gui-installer-pkg-source "" string?)

(define by-source-panel%
  (class vertical-panel%
    (init-field [in-terminal in-terminal])
    (init [text-field-initial-value #f]
          [(details-initially-shown? details-shown?) #f])
    (super-new)
    
    (inherit get-top-level-window)

    (define source-panel (new horizontal-panel% 
                              [parent this] 
                              [stretchable-height #f]))

    (define tf 
      (new text-field% 
           [parent source-panel] 
           [min-width 600]
           [label (~a sc-install-pkg-source-label ":")]
           [callback (λ (_1 _2) 
                       (preferences:set 'drracket:gui-installer-pkg-source (send tf get-value))
                       (adjust-all))]))
    (send tf set-value (or text-field-initial-value 
                           (preferences:get 'drracket:gui-installer-pkg-source)))

    (define (browse-callback b e)
      (let/ec esc
        (define mode (send choice get-string-selection))
        (define dir? (or (equal? mode sc-install-pkg-dir)
                         (and (not (equal? mode sc-install-pkg-file))
                              (let ([v (message-box/custom 
                                        (string-constant browse...)
                                        (string-constant install-pkg-file-or-dir?)
                                        (string-constant install-pkg-file)
                                        (string-constant install-pkg-dir)
                                        (string-constant cancel)
                                        (get-top-level-window)
                                        '(default=1))])
                                (when (or (not v) (= v 3)) (esc (void)))
                                (= v 2)))))
        
        (define f
          (cond
           [dir?
            (get-directory (string-constant install-pkg-select-package-directory)
                           (get-top-level-window))]
           [else
            (parameterize ([finder:default-filters
                            '(("Package" "*.zip;*.plt;*.tgz;*.tar")
                              ("Any" "*.*"))])
              (finder:get-file #f (string-constant install-pkg-select-package-file)
                               #f "bad"
                               (get-top-level-window)))]))
        (when f
          (send tf set-value 
                ;; Simplified paths on no platform should start "[a-z]*://":
                (path->string (simplify-path
                               (if dir?
                                   (path->directory-path f) 
                                   f))))
          (adjust-all))))
    (define browse-button (new button%
                               [parent source-panel]
                               [label (string-constant browse...)]
                               [font small-control-font]
                               [callback browse-callback]
                               [vert-margin 0]))

    (new message% 
         [parent this]
         [label (string-constant install-pkg-package-source-desc)]
         [stretchable-width #t]
         [font small-control-font])
    
    (define button-panel (new horizontal-panel% 
                              [parent this] 
                              [stretchable-height #f]))
    (define details-parent (new vertical-panel% [parent this]))
    (define details-panel (new group-box-panel% 
                               [label (string-constant autosave-details)]
                               [parent details-parent]
                               [alignment '(left center)]
                               [stretchable-height #f]))

    (define/private (reset-installed-pkgs!)
      (define scope (selected-scope))
      (set! currently-installed-pkgs (installed-pkg-names #:scope scope))
      (set! currently-installed-pkgs-scope scope))
    
    (define details-shown? details-initially-shown?)
    (define details-button (new button% 
                                [label (if details-initially-shown?
                                           (string-constant hide-details-button-label)
                                           (string-constant show-details-button-label))]
                                [parent button-panel]
                                [callback
                                 (λ (a b)
                                   (set! details-shown? (not details-shown?))
                                   (adjust-all))]))
    (unless details-initially-shown?
      (send details-parent change-children (λ (l) '())))

    (new horizontal-panel% [parent button-panel])
    (define ok-button
      (new button%
           [label (pick-wider normal-control-font
			      sc-install-pkg-install
			      sc-install-pkg-update)]
           [parent button-panel]
           [style '(border)]
           [callback (lambda (b e)
                       (define res (compute-cmd-line))
                       (define action (case (cmdline-which res)
                                        [(install) pkg-install-command]
                                        [(update) pkg-update-command]))
                       (in-terminal
                        (case (cmdline-which res)
                          [(install) (string-constant install-pkg-abort-install)]
                          [(update) (string-constant install-pkg-abort-update)])
                        (lambda ()
                          (keyword-apply action 
                                         (cmdline-kwds res)
                                         (cmdline-kwd-args res)
                                         (cmdline-args res))))
                       (reset-installed-pkgs!)
                       (adjust-all))]))

    (define/public (get-close-button-panel) button-panel)
    
    (define name-panel (new horizontal-panel% 
                            [parent details-panel]
                            [stretchable-height #f]))
    (define name-choice (new radio-box%
                             [label (~a (string-constant install-pkg-package-name) ":")]
                             [parent name-panel]
                             [style '(horizontal)]
                             [stretchable-width #f]
                             [choices (list (string-constant install-pkg-infer)
                                            (~a (string-constant install-pkg-use) ":"))]
                             [callback (lambda (cb e) (adjust-all))]))
    (define name-message (new message%
                              [label ""]
                              [parent name-panel]
                              [stretchable-width #t]))
    (define name-field (new text-field%
                            [label #f]
                            [parent name-panel]
                            [callback (lambda (t e) (adjust-all))]))
    ;; Make the panel height the same whether we show the message or field:
    (let-values ([(w h) (send name-panel get-graphical-min-size)])
      (send name-panel min-height h))

    (define type-panel (new horizontal-panel% 
                            [parent details-panel]
                            [stretchable-height #f]))
    (define choice (new choice%
                        [label (~a sc-install-pkg-type-label ":")]
                        [parent type-panel]
                        [stretchable-width #t]
                        [callback (λ (x y) (adjust-all))]
                        [choices (list sc-install-pkg-infer
                                       sc-install-pkg-file
                                       sc-install-pkg-dir
                                       sc-install-pkg-file-url
                                       sc-install-pkg-dir-url
                                       sc-install-pkg-github
                                       sc-install-pkg-name)]))
    (define link-dir-checkbox (new check-box%
                                   [parent type-panel]
                                   [label (string-constant install-pkg-link-dirs)]
                                   [value #t]
                                   [callback (lambda (b e) (adjust-cmd-line))]))
    
    (define inferred-msg-parent (new horizontal-panel% 
                                     [parent details-panel]
                                     [stretchable-height #f]
                                     [alignment '(right center)]))
    (define inferred-msg (new message% [label ""] [parent inferred-msg-parent] [auto-resize #t]))
    
    (define action-choice (new choice%
                               [label (~a sc-install-pkg-action-label ":")]
                               [parent details-panel]
                               [stretchable-width #t]
                               [callback (λ (x y) (adjust-all))]
                               [choices (list sc-install-pkg-infer
                                              sc-install-pkg-install
                                              sc-install-pkg-update)]))
    (define inferred-action-msg-parent (new horizontal-panel% 
                                            [parent details-panel]
                                            [stretchable-height #f]
                                            [alignment '(right center)]))
    (define inferred-action-msg (new message% [label ""] 
                                     [parent inferred-action-msg-parent]
                                     [auto-resize #t]))

    
    (define scope-panel (new horizontal-panel%
                             [parent details-panel]
                             [stretchable-height #f]))
    (define scope-choice (new choice%
                              [label (~a sc-install-pkg-scope-label ":")]
                              [parent scope-panel]
                              [stretchable-width #t]
                              [callback (λ (x y) (adjust-all))]
                              [choices (list sc-install-pkg-default
                                             sc-install-pkg-installation
                                             sc-install-pkg-user)]))
    (define/private (selected-scope) (case (send scope-choice get-selection)
                                       [(0) (default-pkg-scope)]
                                       [(1) 'installation]
                                       [(2) 'user]))
    (define scope-default-button (new button% 
                                      [label sc-install-pkg-set-as-default]
                                      [font small-control-font]
                                      [vert-margin 0]
                                      [parent scope-panel]
                                      [callback
                                       (lambda (b e)
                                         (define scope (selected-scope))
                                         (in-terminal
                                          sc-install-pkg-abort-set-scope
                                          (lambda ()
                                            (pkg-config-command #:scope 'user
                                                                #:set #t
                                                                "default-scope"
                                                                (~a scope))
                                            (printf "Default scope successfully changed to ~a"
                                                    scope)))
                                         (adjust-all))]))
    (define inferred-scope-msg-parent (new horizontal-panel% 
                                           [parent details-panel]
                                           [stretchable-height #f]
                                           [alignment '(right center)]))
    (define scope-msg (new message% [label ""] [parent inferred-scope-msg-parent] [auto-resize #t]))
    
    (define deps-panel (new horizontal-panel% [parent details-panel] [stretchable-height #f]))
    (define (deps-choice-callback b e)
      (case (send deps-choice get-selection)
        [(3) 
         (send deps-choice set-selection 0)
         (adjust-all)
         (message-box 
          sc-install-pkg-dependencies-mode
          sc-install-pkg-dependencies-search-ask-not-supported-in-gui)]
        [else 
         (adjust-all)]))
              
    (define deps-choice (new choice%
                             [label sc-install-pkg-dependencies-mode]
                             [parent deps-panel]
                             [choices (list sc-install-pkg-default
                                            sc-install-pkg-dependencies-fail
                                            sc-install-pkg-dependencies-force
                                            sc-install-pkg-dependencies-search-ask
                                            sc-install-pkg-dependencies-search-auto
                                            sc-install-pkg-dependencies-search-auto+update)]
                             [stretchable-width #t]
                             [callback deps-choice-callback]))
    (define deps-msg-parent (new horizontal-panel% 
                                 [parent details-panel]
                                 [stretchable-height #f]
                                 [alignment '(right center)]))
    (define deps-msg (new message% [label ""] [parent deps-msg-parent] [auto-resize #t]))

    (define checkboxes-panel (new horizontal-panel%
                                  [parent details-panel]
                                  [stretchable-height #f]
                                  [alignment '(left center)]))
    
    (define cb (new check-box% 
                    [label sc-install-pkg-force?] 
                    [parent checkboxes-panel]
                    [callback (λ (a b) (adjust-all))]))

    (define overwrite-cb (new check-box% 
                              [label sc-install-pkg-replace?]
                              [parent checkboxes-panel]
                              [callback (λ (a b) (adjust-all))]))

    (new message% [parent details-panel] [label " "]) ; a spacer

    (new message% [parent details-panel] [label sc-install-pkg-command-line])
    (define cmdline-panel (new horizontal-panel% [parent details-panel] [stretchable-height #f]))
    (new horizontal-panel% [parent cmdline-panel] [min-width 12] [stretchable-width #f])
    (define cmdline-msg (new message% 
                             [parent cmdline-panel]
                             [auto-resize #t]
                             [label ""]
                             [font (send (send (editor:get-standard-style-list)
                                               find-named-style
                                               "Standard")
                                         get-font)]))
    (new horizontal-panel% [parent cmdline-panel] [stretchable-width #t])
    
    (define/private (selected-type)
      (case (send choice get-selection)
        [(0) #f]
        [(1) 'file]
        [(2) 'dir]
        [(3) 'file-url]
        [(4) 'dir-url]
        [(5) 'github]
        [(6) 'name]))
    
    (define/private (type->str type)
      (case type
        [(file) sc-install-pkg-file]
        [(name) sc-install-pkg-name]
        [(dir) sc-install-pkg-dir]
        [(github) sc-install-pkg-github]
        [(file-url) sc-install-pkg-file-url]
        [(dir-url) sc-install-pkg-dir-url]
        [else (error 'type->str "unknown type ~s\n" type)]))
    
    (define currently-installed-pkgs-scope #f)
    (define currently-installed-pkgs '())
    (define/private (get-current-action)
      (case (send action-choice get-selection)
        [(0) 
         (define current-name (get-name))
         (cond
          [(and current-name (member current-name currently-installed-pkgs))
           'update]
          [else 
           'install])]
        [(1) 'install]
        [(2) 'update]))

    (define/private (infer-package-name?)
      (= 0 (send name-choice get-selection)))

    (define/private (get-name)
      (if (infer-package-name?)
          (or (package-source->name (send tf get-value) (selected-type))
              "???")
          (send name-field get-value)))

    (define/private (adjust-all)
      (unless (eq? currently-installed-pkgs-scope
                   (selected-scope))
        (reset-installed-pkgs!))
      (adjust-name)
      (adjust-inferred)
      (adjust-link-dir)
      (adjust-inferred-action)
      (adjust-checkbox)
      (adjust-cmd-line)
      (adjust-details-shown)
      (adjust-browse)
      (adjust-scope)
      (adjust-deps)
      (adjust-ok))

    (define/private (adjust-name)
      (define infer? (infer-package-name?))
      (send name-panel change-children
            (lambda (l)
              (list name-choice
                    (if infer? name-message name-field))))
      (when infer?
        (define name (get-name))
        (send name-message set-label (gui-utils:trim-string name 200))
        (send name-field set-value name)))
    
    (define/private (adjust-checkbox)
      (void))
    
    (define/private (adjust-inferred-action)
      (define action (get-current-action))
      (define new-lab
        (cond
         [(equal? 0 (send action-choice get-selection)) 
          (case (get-current-action)
            [(install) sc-action-inferred-to-be-install]
            [(update) sc-action-inferred-to-be-update])]
         [else ""]))
      (send inferred-action-msg set-label new-lab))
    
    (define/private (adjust-ok)
      (send ok-button set-label (case (get-current-action)
                                  [(install) sc-install-pkg-install]
                                  [(update) sc-install-pkg-update]))
      (send ok-button enable (compute-cmd-line)))

    (define/private (adjust-browse)
      (define mode (send choice get-string-selection))
      (define show? (or (equal? mode sc-install-pkg-infer)
                        (equal? mode sc-install-pkg-file)
                        (equal? mode sc-install-pkg-dir)))
      (define shown? (member browse-button (send source-panel get-children)))
      (unless (eq? (and show? #t) (and shown? #t))
        (if show?
            (send source-panel add-child browse-button)
            (send source-panel delete-child browse-button))))
    
    (define/private (adjust-details-shown)
      (define current-details-shown-state?
        (and (member details-panel (send details-parent get-children))
             #t))
      (unless (equal? current-details-shown-state? 
                      details-shown?)
        (cond
         [details-shown?
          (send details-button set-label (string-constant hide-details-button-label))
          (send details-parent change-children
                (λ (l) (list details-panel)))]
         [else
          (send details-button set-label (string-constant show-details-button-label))
          (send details-parent change-children
                (λ (l) '()))])))
    
    (define/private (adjust-inferred)
      (define inferred-actual-type (get-inferred-actual-type))
      (define new-lab
        (and inferred-actual-type
             (format sc-install-pkg-inferred-as (type->str inferred-actual-type))))
      (send inferred-msg set-label (or new-lab "")))

    (define/private (adjust-link-dir)
      (send link-dir-checkbox show (member (selected-type) '(#f dir))))
    
    (define (get-inferred-actual-type)
      (and (equal? #f (selected-type))
           (let-values ([(_ actual-type)
                         (package-source->name+type (send tf get-value) #f)])
             actual-type)))

    (define/private (adjust-scope)
      (send scope-msg set-label (format sc-install-pkg-scope-is
                                        (case (selected-scope)
                                          [(installation) sc-install-pkg-installation]
                                          [(user) sc-install-pkg-user])))
      (define is-default? (let ([v (send scope-choice get-selection)])
                            (or (zero? v)
                                (= v (case (default-pkg-scope)
                                       [(installation) 1]
                                       [(user) 2])))))
      (define deleted? (not (member scope-default-button (send scope-panel get-children))))
      (unless (equal? is-default? deleted?)
        (if is-default?
            (send scope-panel delete-child scope-default-button)
            (send scope-panel add-child scope-default-button))))
    
    (define/private (adjust-deps)
      (send deps-msg set-label
            (cond
              [(equal? 0 (send deps-choice get-selection)) 
               (format sc-install-pkg-deps-is 
                       (cadr
                        (regexp-match #rx"^(.*):" 
                                      (case (get-deps-selected-type)
                                        [(fail)
                                         (string-constant install-pkg-dependencies-fail)]
                                        [(search-auto)
                                         (string-constant install-pkg-dependencies-search-auto)]))))]
              [else ""])))
    
    (define (get-deps-selected-type)
      (case (send deps-choice get-selection)
        [(0)
         (define current-type (or (selected-type) (get-inferred-actual-type)))
         (case current-type
           [(name) 'search-auto]
           [else 'fail])]
        [(1) 'fail]
        [(2) 'force]
        [(3) 'fail] ;; shouldn't happen
        [(4 5) 'search-auto]))

    (define (get-deps-auto-update)
      (= 5 (send deps-choice get-selection)))
      
    (define/private (adjust-cmd-line)
      (define (possibly-quote-string s)
        (if (regexp-match #rx" " s)
            (string-append "\"" s "\"")
            s))
      (define cmd-line (compute-cmd-line))
      (send cmdline-msg set-label
            (gui-utils:trim-string
             (if cmd-line
                 (string-append
                  (if (eq? (system-type) 'windows)
                      "raco.exe"
                      "raco")
                  " pkg "
                  (format "~a " (cmdline-which cmd-line))
                  (apply 
                   string-append
                   (add-between
                    (map (λ (kwd kwd-arg)
                           (define flag (~a "--" (keyword->string kwd)))
                           (if (boolean? kwd-arg)
                               flag
                               (~a flag " " (~s kwd-arg))))
                         (cmdline-kwds cmd-line)
                         (cmdline-kwd-args cmd-line))
                    " "))
                  (apply string-append
                         (map (λ (x) (format " ~a" (possibly-quote-string x)))
                              (cmdline-args cmd-line))))
                 "")
             200)))
    
    (struct cmdline (which kwds kwd-args args) #:transparent)
    (define/private (compute-cmd-line)
      (define action (get-current-action))
      (define update-by-name? (and (eq? 'update action)
                                   (not (send overwrite-cb get-value))))
      (define the-pkg (if update-by-name?
                          (get-name)
                          (send tf get-value)))
      (cond
        [(equal? the-pkg "") #f]
        [else
         (define kwds '()) 
         (define kwd-args '())
         (define (add-kwd-arg kwd arg)
           (set!-values (kwds kwd-args)
                        (let loop ([ks kwds] [as kwd-args])
                          (cond
                           [(null? ks) (values (list kwd) (list arg))]
                           [(keyword<? kwd (car ks))
                            (values (cons kwd ks) (cons arg as))]
                           [else
                            (define-values (ks2 as2) (loop (cdr ks) (cdr as)))
                            (values (cons (car ks) ks2) (cons (car as) as2))]))))
         (when (and (send cb is-enabled?) (send cb get-value))
           (add-kwd-arg '#:force #t))
         (when (and (not update-by-name?)
                    (selected-type))
           (add-kwd-arg '#:type (selected-type)))
         (when (and (not update-by-name?)
                    (send link-dir-checkbox get-value)
                    (eq? 'dir (or (selected-type) (get-inferred-actual-type))))
             (add-kwd-arg '#:link #t))
         (let ([scope (selected-scope)])
           (unless (and (equal? scope (default-pkg-scope))
                        ;; Don't let `update' infer a scope itself:
                        (not (eq? action 'update)))
             (add-kwd-arg '#:scope scope)))
         (add-kwd-arg '#:deps (get-deps-selected-type))
         (when (get-deps-auto-update)
           (add-kwd-arg '#:update-deps #t))
         (unless (or update-by-name?
                     (infer-package-name?))
           (add-kwd-arg '#:name (get-name)))
         (cmdline action kwds kwd-args (list the-pkg))]))

    (define/override (on-superwindow-show on?)
      (when on?
        (reset-installed-pkgs!)
        (adjust-all)))

    (adjust-all)))

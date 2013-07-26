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

(preferences:set-default 'drracket:gui-installer-pkg-source "" string?)

(define by-source-panel%
  (class vertical-panel%
    (init-field [in-terminal in-terminal])
    (init [text-field-initial-value #f])
    (super-new)
    
    (inherit get-top-level-window)

    (define source-panel (new horizontal-panel% 
                              [parent this] 
                              [stretchable-height #f]))

    (define tf (new text-field% 
                    [parent source-panel] 
                    [min-width 600]
                    [label (~a sc-install-pkg-source-label ":")]
                    [callback (λ (_1 _2) 
                                 (preferences:set 'drracket:gui-installer-pkg-source (send tf get-value))
                                 (adjust-all))]))
    (send tf set-value (or text-field-initial-value (preferences:get 'drracket:gui-installer-pkg-source)))

    (define browse-button (new button%
                               [parent source-panel]
                               [label (string-constant browse...)]
                               [font small-control-font]
                               [callback (lambda (b e)
                                           (define mode (send choice get-string-selection))
                                           (define dir? (or (equal? mode sc-install-pkg-dir)
                                                            (equal? mode sc-install-pkg-dir-url)))
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
                                                   (url->string (path->url (if dir?
                                                                               (path->directory-path f) 
                                                                               f))))
                                             (adjust-all)))]))
    
    (define/public (get-button-panel) button-panel)
    (define button-panel (new horizontal-panel% 
                              [parent this] 
                              [stretchable-height #f]))
    (define details-parent (new vertical-panel% [parent this]))
    (define details-panel (new group-box-panel% 
                               [label (string-constant autosave-details)]
                               [parent details-parent]
                               [alignment '(left center)]
                               [stretchable-height #f]))

    (define ok-button
      (new button%
           [label (pick-wider normal-control-font
			      sc-install-pkg-install
			      sc-install-pkg-update)]
           [parent button-panel]
           [style '(border)]
           [callback (lambda (b e)
                       (define res (compute-cmd-line))
                       (in-terminal
                        (case (car res)
                          [(install) (string-constant install-pkg-abort-install)]
                          [(update) (string-constant install-pkg-abort-update)])
                        (lambda ()
                          (define action (case (car res)
                                           [(install) pkg-install-command]
                                           [(update) pkg-update-command]))
                          (apply action (cdr res))))
                       (reset-installed-pkgs!))]))

    (define/private (reset-installed-pkgs!)
      (set! currently-installed-pkgs (installed-pkg-names))
      (adjust-all))
    
    (new horizontal-panel% [parent button-panel])
    (define details-shown? #f)
    (define details-button (new button% 
                                [label (string-constant show-details-button-label)]
                                [parent button-panel]
                                [callback
                                 (λ (a b)
                                   (set! details-shown? (not details-shown?))
                                   (adjust-all))]))

    (send details-parent change-children (λ (l) '()))
    (define choice (new choice%
                        [label (~a sc-install-pkg-type-label ":")]
                        [parent details-panel]
                        [stretchable-width #t]
                        [callback (λ (x y) (adjust-all))]
                        [choices (list sc-install-pkg-infer
                                       sc-install-pkg-file
                                       sc-install-pkg-dir
                                       sc-install-pkg-file-url
                                       sc-install-pkg-dir-url
                                       sc-install-pkg-github
                                       sc-install-pkg-name)]))
    
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
    (define inferred-action-msg (new message% [label ""] [parent inferred-action-msg-parent] [auto-resize #t]))

    
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
                                      [parent scope-panel]
                                      [callback (lambda (b e)
                                                  (in-terminal
                                                   (lambda ()
                                                     (define scope (selected-scope))
                                                     (pkg-config-command #:scope 'installation #:set #t "default-scope" (~a scope))
                                                     (printf "Default scope successfully changed to ~a" scope)))
                                                  (adjust-all))]))
    (define inferred-scope-msg-parent (new horizontal-panel% 
                                           [parent details-panel]
                                           [stretchable-height #f]
                                           [alignment '(right center)]))
    (define scope-msg (new message% [label ""] [parent inferred-scope-msg-parent] [auto-resize #t]))
    
    (define cb (new check-box% 
                    [label sc-install-pkg-force?] 
                    [parent details-panel]
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
    
    (define currently-installed-pkgs (installed-pkg-names))
    (define/private (get-current-action)
      (case (send action-choice get-selection)
        [(0) 
         (define current-name (package-source->name (send tf get-value)))
         (cond
          [(and current-name (member current-name currently-installed-pkgs))
           'update]
          [else 
           'install])]
        [(1) 'install]
        [(2) 'update]))
    
    
    (define/private (adjust-all)
      (adjust-inferred)
      (adjust-inferred-action)
      (adjust-checkbox)
      (adjust-cmd-line)
      (adjust-details-shown)
      (adjust-browse)
      (adjust-scope)
      (adjust-ok))
    
    (define/private (adjust-checkbox)
      (send cb enable (equal? 'install (get-current-action))))
    
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
      (define show? (not (or (equal? mode sc-install-pkg-github)
                             (equal? mode sc-install-pkg-name))))
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
      (define new-lab
        (and (equal? #f (selected-type))
             (let-values ([(_ actual-type)
                           (package-source->name+type (send tf get-value) #f)])
               (and actual-type
                    (format sc-install-pkg-inferred-as (type->str actual-type))))))
      (send inferred-msg set-label (or new-lab "")))

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
    
    (define/private (adjust-cmd-line)
      (define (convert-to-string s)
        (cond
         [(string? s)
          (if (regexp-match #rx" " s)
              (string-append "\"" s "\"")
              s)]
         [(keyword? s) (regexp-replace #rx"^#:" (format "~a" s) "--")]
         [(symbol? s) (symbol->string s)]
         [(boolean? s) #f]
         [else (error 'convert-to-string "unk ~s" s)]))
      (define cmd-line (compute-cmd-line))
      (send cmdline-msg set-label
            (if cmd-line
                (string-append
                 (if (eq? (system-type) 'windows)
                     "raco.exe"
                     "raco")
                 " pkg "
                 (apply string-append
                        (add-between 
                         (filter values (map convert-to-string cmd-line))
                         " ")))
                "")))
    
    (define/private (compute-cmd-line)
      (define the-pkg 
        (cond
         [(and (equal? 'update (get-current-action))
               (package-source->name (send tf get-value)))
          =>
          values]
         [else (send tf get-value)]))
      (and (not (equal? the-pkg ""))
           (cons (get-current-action)
                 (append
                  (if (and (send cb is-enabled?) (send cb get-value))
                      '(#:force #t)
                      '())
                  (if (selected-type)
                      (list '#:type (selected-type))
                      '())
                  (let ([scope (selected-scope)])
                    (if (equal? scope (default-pkg-scope))
                        '()
                        (list '#:scope scope)))
                  (list the-pkg)))))

    (define/override (on-superwindow-show on?)
      (when on? 
        (reset-installed-pkgs!)))
    
    (adjust-all)))

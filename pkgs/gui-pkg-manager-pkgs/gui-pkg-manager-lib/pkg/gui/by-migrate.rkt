#lang racket/base

(require racket/gui/base 
         racket/class
         racket/format
         racket/file
         string-constants
         setup/dirs
         pkg
         "common.rkt")

(provide by-migrate-panel%)

(define by-migrate-panel%
  (class vertical-panel%
    (init-field [in-terminal in-terminal])
    (super-new)
    
    (inherit get-top-level-window)

    (define choices-panel (new horizontal-panel% 
                               [parent this] 
                               [alignment '(center top)]))
    
    (define versions (new list-box% 
                          [label (~a (string-constant install-pkg-migrate-available-installations)
                                     ":")]
                          [choices null]
                          [parent choices-panel]
                          [callback (λ (_1 _2) (adjust-all))]))

    (define button-panel (new vertical-panel% 
                              [parent choices-panel] 
                              [stretchable-height #f]
                              [stretchable-width #f]
                              [alignment '(center center)]))

    (define migrate-from-button (new button%
                                     [parent button-panel]
                                     [label (string-constant install-pkg-migrate-from)]
                                     [callback (lambda (b e)
                                                 (in-terminal
                                                  (string-constant install-pkg-abort-migrate)
                                                  (lambda ()
                                                    (pkg-migrate-command
                                                     (send versions get-string-selection)))))]
                                     [style '(border)]))

    (define remove-button (new button%
                               [parent button-panel]
                               [label (string-constant install-pkg-remove)]
                               [callback (lambda (b e)
                                           (remove-package-info
                                            (send versions get-string-selection)))]))

    (define (remove-package-info vers)
      (when (= 1 (message-box/custom (format (string-constant install-pkg-packages-for) vers)
                                     (format
                                      (string-constant install-pkg-really-remove-installation)
                                      vers)
                                     (string-constant install-pkg-remove)
                                     (string-constant install-pkg-do-not-remove)
                                     #f
                                     (get-top-level-window)
                                     '(caution default=1)))
        (delete-directory/files (build-path (find-system-path 'addon-dir) vers "pkgs"))
        (update-list!)
        (adjust-all)))

    (define (update-list!)
      (define d (find-system-path 'addon-dir))
      (define dirs (for/list ([p (in-list (directory-list d))]
                              #:when (let ([p (build-path d p "pkgs" "pkgs.rktd")])
                                       (file-exists? p))
                              #:unless (equal? (path-element->string p)
                                               (get-installation-name)))
                     (path-element->string p)))
      (send versions set dirs))

    (define (adjust-all)
      (define s (send versions get-selection))
      (send migrate-from-button enable s)
      (send remove-button enable s))
    
    (define/override (on-superwindow-show on?)
      (when on?
        (update-list!)
        (adjust-all)))
    
    (adjust-all)))

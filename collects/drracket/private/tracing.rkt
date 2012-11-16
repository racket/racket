#lang racket/base

(require racket/contract
         racket/unit
         racket/class
         racket/path
         racket/port
         racket/list
         racket/gui/base
         string-constants
         framework
         (prefix-in tr: trace/stacktrace)
         "drsig.rkt")

(provide tracing@)

(define-unit tracing@
  (import [prefix drracket:frame: drracket:frame^]
          [prefix drracket:rep: drracket:rep^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket: drracket:interface^])
  (export drracket:tracing^)
  
  (define-local-member-name
    get-tracing-text
    show-tracing
    tracing:add-line
    tracing:rest)
  (define tab-tracing<%>
    (interface ()
      get-tracing-text
      get-any-results?
      tracing:add-line
      tracing:reset))
  
  (define ellipses-cutoff 200)
  (define calltrace-key #`(quote #,(gensym 'drscheme-calltrace-key)))
  
  (define (print-call-trace inferred-name original? src args improper? depth)
    (when inferred-name
      (let ([name (cond
                    [(identifier? inferred-name) (syntax-e inferred-name)]
                    [else (object-name inferred-name)])]
            [rep (drracket:rep:current-rep)])
        (when (and name rep)
          (let ([canvas (send rep get-canvas)])
            (when canvas
              (let* ([frame (send canvas get-top-level-window)]
                     [tab (send frame get-current-tab)])
                (when (is-a? tab tab-tracing<%>)
                  (let ([sp (open-output-string)])
                    (let loop ([i depth])
                      (unless (zero? i)
                        (display " " sp)
                        (loop (- i 1))))
                    (fprintf sp "(")
                    (fprintf sp "~a" name)
                    (let loop ([args args])
                      (cond
                        [(null? args) (void)]
                        [(and (null? (cdr args)) improper?)
                         (fprintf sp " . ")
                         (fprintf sp "~v" (car args))]
                        [else
                         (let ([arg (car args)])
                           (fprintf sp " ")
                           (fprintf sp "~v" arg))
                         (loop (cdr args))]))
                    (fprintf sp ")")
                    (let ([sema (make-semaphore)])
                      ;; Disable breaks, so an exn handler can't
                      ;;  grab the DrRacket eventspacae:
                      (parameterize-break #f
                                          ;; Queue callback to write trace line ---
                                          ;; low priority, so that infinite loops don't stop the user
                                          ;;  from clicking "Break"
                                          (parameterize ([current-eventspace drracket:init:system-eventspace])
                                            (queue-callback
                                             (lambda ()
                                               (send tab tracing:add-line (get-output-string sp))
                                               (semaphore-post sema))
                                             #f)))
                      ;; Wait for the line to get written, so that the
                      ;;  trace output doesn't get too far behind (which
                      ;;  matters, again, for infinite loops)
                      (semaphore-wait sema)))))))))))
  
  (define-values/invoke-unit tr:stacktrace@
    (import tr:stacktrace-imports^) (export tr:stacktrace^))
  
  (define tab-mixin
    (mixin (drracket:unit:tab<%> drracket:rep:context<%>) (tab-tracing<%>)
      (inherit get-frame)
      
      (define tracing-visible? #f)
      (define/public (set-tracing-visible? v?) (set! tracing-visible? v?))
      (define/public (get-tracing-visible?) tracing-visible?)
      
      (define/augment (clear-annotations)
        (tracing:reset)
        (inner (void) clear-annotations))
      
      (define any-results? #f)
      (define/public (get-any-results?) any-results?)
      (define ever-been-reset? #f)
      (define/public (tracing:reset)
        (when (or any-results?
                  (not ever-been-reset?))
          (set! any-results? #f)
          (set! ever-been-reset? #t)
          (send show-tracing-text lock #f)
          (send show-tracing-text erase)
          (send show-tracing-text auto-wrap #t)
          (send show-tracing-text insert (string-constant tracing-tracing-nothing-to-show))
          (send show-tracing-text lock #t)))
      
      (define show-tracing-text (new text:hide-caret/selection%))
      (define/public (get-tracing-text) show-tracing-text)
      (send show-tracing-text lock #t)
      
      (define/public (tracing:add-line s)
        (let ([old-any? any-results?])
          (set! any-results? #t)
          (unless old-any?
            (send (get-frame) show-tracing))
          (send show-tracing-text begin-edit-sequence)
          (send show-tracing-text lock #f)
          (unless old-any?
            (send show-tracing-text erase)
            (send show-tracing-text auto-wrap #f))
          (let ([insert
                 (lambda (s)
                   (send show-tracing-text insert s (send show-tracing-text last-position) 'same #f))])
            (cond
              [(<= (string-length s) ellipses-cutoff)
               (insert s)
               (insert "\n")]
              [else
               (insert (substring s 0 ellipses-cutoff))
               (insert " ")
               (let ([ell-start (send show-tracing-text last-position)])
                 (insert "...")
                 (let ([ell-end (send show-tracing-text last-position)])
                   (let ([para (send show-tracing-text last-paragraph)])
                     (insert "\n")
                     (send show-tracing-text change-style clickback-delta ell-start ell-end)
                     (send show-tracing-text set-clickback ell-start ell-end 
                           (lambda (t x y)
                             (send show-tracing-text begin-edit-sequence)
                             (send show-tracing-text lock #f)
                             (let ([line-start (send show-tracing-text paragraph-start-position para)]
                                   [line-end (send show-tracing-text paragraph-end-position para)])
                               (send show-tracing-text delete line-start line-end #f)
                               (send show-tracing-text insert s line-start 'same #f))
                             (send show-tracing-text lock #t)
                             (send show-tracing-text end-edit-sequence))))))]))
          (send show-tracing-text lock #t)
          (send show-tracing-text end-edit-sequence)))
      
      (super-new)))
  
  (define clickback-delta (make-object style-delta%))
  (send clickback-delta set-delta-foreground "BLUE")
  (send clickback-delta set-delta 'change-underline #t)
  
  (define frame-mixin 
    (mixin (drracket:frame:<%> drracket:unit:frame<%>) ()
      (inherit get-current-tab set-show-menu-sort-key)
      (define show-tracing-menu-item #f)
      (define tracing-visible? #f)
      
      (define/augment (on-tab-change old new)
        (inner (void) on-tab-change old new)
        (send show-tracing-canvas set-editor (send new get-tracing-text))
        (cond
          [(eq? tracing-visible? (send new get-tracing-visible?))
           (void)]
          [(send new get-tracing-visible?)
           (show-tracing)]
          [else
           (hide-tracing)]))
      
      (define/override (add-show-menu-items show-menu)
        (super add-show-menu-items show-menu)
        (set! show-tracing-menu-item
              (new menu-item%
                   (parent show-menu)
                   (label (string-constant tracing-show-tracing-window))
                   (callback (lambda (x y) (toggle-tracing)))))
        (set-show-menu-sort-key show-tracing-menu-item 206))
      
      (define/public (show-tracing)
        (set! tracing-visible? #t)
        (send show-tracing-menu-item set-label (string-constant tracing-hide-tracing-window))
        (send dragable-parent begin-container-sequence)
        (send dragable-parent change-children
              (lambda (l)
                (let ([without (remq show-tracing-canvas l)])
                  (append without (list show-tracing-canvas)))))
        (send dragable-parent set-percentages '(3/4 1/4))
        (send dragable-parent end-container-sequence))
      
      (define/private (hide-tracing)
        (set! tracing-visible? #f)
        (send show-tracing-menu-item set-label (string-constant tracing-show-tracing-window))
        (send dragable-parent change-children
              (lambda (l)
                (remq show-tracing-canvas l))))
      
      (define/private (toggle-tracing)
        (if tracing-visible?
            (hide-tracing)
            (show-tracing)))
      
      (define dragable-parent #f)
      (define show-tracing-parent-panel #f)
      (define show-tracing-canvas #f)
      
      (define/override (make-root-area-container cls parent)
        (set! dragable-parent (super make-root-area-container panel:horizontal-dragable% parent))
        (let ([root (make-object cls dragable-parent)])
          (set! show-tracing-canvas (new editor-canvas% 
                                         (parent dragable-parent)
                                         (editor (send (get-current-tab) get-tracing-text))))
          (send dragable-parent change-children (lambda (l) (remq show-tracing-canvas l)))
          root))
      
      (super-new))))

#lang racket/base
(require racket/class
         racket/gui/base
         racket/pretty
         unstable/gui/notify
         "interfaces.rkt")
(provide syntax-keymap%)

(define keymap/popup%
  (class* keymap% (keymap/popup<%>)
    (init editor)
    (super-new)
    (inherit add-function
             map-function
             chain-to-keymap)

    (define/public (add-context-menu-items menu)
      (void))

    (map-function "rightbutton" "popup-context-menu")
    (add-function "popup-context-menu"
                  (lambda (editor event)
                    (popup-context-menu editor event)))

    (define/private (popup-context-menu editor event)
      (define-values (x y)
        (send editor dc-location-to-editor-location
              (send event get-x)
              (send event get-y)))
      (define admin (send editor get-admin))
      (define menu (new popup-menu%))
      (add-context-menu-items menu)
      (send admin popup-menu menu x y))

    ;; FIXME: move out of constructor to use sites
    (chain-to-keymap (send editor get-keymap) #t)
    (send editor set-keymap this)))

(define syntax-keymap%
  (class keymap/popup%
    (init-field controller
                config)
    (inherit add-function
             map-function
             call-function
             chain-to-keymap)
    (super-new)

    (define/private (selected-syntax)
      (send controller get-selected-syntax))

    ;; Functionality

    (add-function "copy-syntax-as-text"
                  (lambda (_ event)
                    (define stx (send controller get-selected-syntax))
                    (when stx
                      (send the-clipboard set-clipboard-string
                            (let ([out (open-output-string)])
                              (pretty-print (syntax->datum stx) out)
                              (get-output-string out))
                            (send event get-time-stamp)))))

    (add-function "clear-syntax-selection"
                  (lambda (i e)
                    (send controller set-selected-syntax #f)))

    (add-function "show-syntax-properties"
                  (lambda (i e)
                    (send config set-props-shown? #t)))

    (add-function "hide-syntax-properties"
                  (lambda (i e)
                    (send config set-props-shown? #f)))

    (define ((pretty-print-as sym) i e)
      (let ([stx (selected-syntax)])
        (when (identifier? stx)
          (send config set-pretty-styles
                (hash-set (send config get-pretty-styles)
                          (syntax-e stx)
                          sym)))))

    (define/override (add-context-menu-items menu)
      (new menu-item% (label "Copy") (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (and (selected-syntax) #t))))
           (callback
            (lambda (i e)
              (call-function "copy-syntax-as-text" i e))))
      (new separator-menu-item% (parent menu))
      (new menu-item%
           (label "Clear selection")
           (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (and (selected-syntax) #t))))
           (callback 
            (lambda (i e)
              (call-function "clear-syntax-selection" i e))))
      (menu-option/notify-box menu "View syntax properties"
                              (get-field props-shown? config))
      (let ([pretty-menu
             (new menu%
                  (label "Change layout")
                  (parent menu)
                  (demand-callback
                   (lambda (i)
                     (send i enable (and (identifier? (selected-syntax)) #t)))))])
        (for ([sym+desc '((and    "like and")
                          (begin  "like begin (0 up)")
                          (lambda "like lambda (1 up)")
                          (do     "like do (2 up)"))])
          (new menu-item%
               (label (format "Format identifier ~a" (cadr sym+desc)))
               (parent pretty-menu)
               (demand-callback
                (lambda (i)
                  (let ([stx (selected-syntax)])
                    (when (identifier? stx)
                      (send i set-label
                            (format "Format ~s ~a" (syntax-e stx) (cadr sym+desc)))))))
               (callback
                (pretty-print-as (car sym+desc)))))))))

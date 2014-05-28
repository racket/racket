#lang racket/base

(require racket/gui/base 
         racket/class
         racket/format
         racket/list
         racket/file
         string-constants
         setup/dirs
         pkg/lib
         net/url
         "common.rkt")

(provide settings-panel%)

(define settings-panel%
  (class vertical-panel%
    (super-new [alignment '(left center)])
    
    (inherit get-top-level-window)

    (define scope-choice 
      (new choice%
           [label (~a (string-constant install-pkg-default-scope-label)
                      ":")]
           [parent this]
           [callback (λ (c y)
                        (catch-errors
                         (lambda ()
                           (parameterize ([current-pkg-scope 'user])
                             (with-pkg-lock
                              (pkg-config #t (list "default-scope"
                                                   (if (= 0 (send c get-selection))
                                                       "installation"
                                                       "user")))))))
                        (adjust-all))]
           [choices (list (string-constant install-pkg-installation)
                          (string-constant install-pkg-user))]))

    (define catalog-panel
      (new horizontal-panel%
           [parent this]
           [alignment '(center top)]))

    (define catalog-list
      (new list-box%
           [label (~a (string-constant install-pkg-package-catalogs) ":")]
           [parent catalog-panel]
           [callback (lambda (x y) (adjust-buttons))]
           [choices null]))

    (define button-panel (new vertical-panel%
                              [parent catalog-panel] 
                              [stretchable-height #f]
                              [stretchable-width #f]
                              [alignment '(center top)]))

    (define raise-button (new button%
                              [parent button-panel]
                              [label (string-constant ml-cp-raise)]
                              [callback (lambda (b e)
                                          (adjust-catalogs
                                           (lambda (l n)
                                             (values
                                              (append
                                               (take l (sub1 n))
                                               (list (list-ref l n)
                                                     (list-ref l (sub1 n)))
                                               (drop l (add1 n)))
                                              (sub1 n)))))]))
                                           
    (define lower-button (new button%
                              [parent button-panel]
                              [label (string-constant ml-cp-lower)]
                              [callback (lambda (b e)
                                          (adjust-catalogs
                                           (lambda (l n)
                                             (values
                                              (append
                                               (take l n)
                                               (list (list-ref l (add1 n))
                                                     (list-ref l n))
                                               (drop l (+ n 2)))
                                              (add1 n)))))]))

    (send (new button% [parent button-panel] [label "1"]) show #f)

    (define add-button (new button%
                            [parent button-panel]
                            [label (string-constant ml-cp-add)]
                            [callback (lambda (b e)
                                        (define s (get-text-from-user
                                                   (string-constant install-pkg-add-package-catalog)
                                                   (string-constant install-plt-url)
                                                   (get-top-level-window)
                                                   #:validate (lambda (s)
                                                                (with-handlers ([exn:fail?
                                                                                 (lambda (exn) #f)])
                                                                  (define u (string->url s))
                                                                  (and (url-scheme u)
                                                                       (or (eq? (url-scheme u) 'file)
                                                                           (url-host u)))))
                                                   "http://..."
                                                   '(disallow-invalid)))
                                        (when s
                                          (adjust-catalogs
                                           (lambda (l n)
                                             (values (append l (list s))
                                                     (length l))))))]))

    (define remove-button (new button%
                               [parent button-panel]
                               [label (string-constant ml-cp-remove)]
                               [callback (lambda (b e)
                                           (adjust-catalogs
                                            (lambda (l n)
                                              (values
                                               (append
                                                (take l n)
                                                (drop l (add1 n)))
                                               #f))))]))

    (define/private (catch-errors thunk)
      (with-handlers ([exn:fail? (lambda (exn)
                                   (message-box
                                    (string-constant error)
                                    (exn-message exn)
                                    (get-top-level-window)
                                    '(ok stop)))])
        (thunk)))

    (define/private (adjust-catalogs cb)
      (define l (for/list ([i (send catalog-list get-number)])
                  (send catalog-list get-string i)))
      (define-values (new-l new-n) (cb l (send catalog-list get-selection)))
      (unless (equal? l new-l)
        (catch-errors
         (lambda ()
           (parameterize ([current-pkg-scope 'user])
             (with-pkg-lock
              (pkg-config #t (cons "catalogs" new-l))))      
           (send catalog-list set new-l)
           (when new-n
             (send catalog-list set-selection new-n)))))
      (adjust-buttons))

    (define/private (adjust-buttons)
      (define s (send catalog-list get-selection))
      (send remove-button enable s)
      (send raise-button enable (and s (positive? s)))
      (send lower-button enable (and s (s . < . (sub1 (send catalog-list get-number))))))

    (define/private (adjust-all)
      (send catalog-list set (pkg-config-catalogs))
      (let ([scope (default-pkg-scope)])
        (send scope-choice set-selection (if (eq? scope 'user) 1 0))
        (send scope-choice enable (symbol? scope)))
      (adjust-buttons))

    (define/override (on-superwindow-show on?)
      (when on?
        (adjust-all)))
    
    (adjust-all)))

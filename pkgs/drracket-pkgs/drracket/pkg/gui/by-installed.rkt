#lang racket/base
(require racket/class
         racket/gui/base
         racket/format
         pkg/lib
         pkg
         string-constants
         "common.rkt")

(provide by-installed-panel%)

(struct ipkg (name scope auto? checksum source))

(define (scope<? a b)
  (cond
   [(path? a)
    (or (not (path? b))
        (bytes<? (path->bytes a) (path->bytes b)))]
   [(path? b) #f]
   [else
    (case a
      [(installation) #t]
      [(user) (eq? b 'shared)]
      [else #f])]))

(define (ipkg<? a b)
  (if (string=? (ipkg-name a) (ipkg-name b))
      (scope<? (ipkg-scope a) (ipkg-scope b))
      (string<? (ipkg-name a) (ipkg-name b))))

(define by-installed-panel%
  (class vertical-panel%
    (init-field [in-terminal in-terminal])

    (super-new)

    (inherit get-top-level-window)

    (define pkg-list
      (new list-box%
           [parent this]
           [label #f]
           [choices null]
           [columns (list "Auto?" "Scope" "Name" "Checksum" "Source")]
           [style '(multiple column-headers clickable-headers)]
           [callback (lambda (lb e)
                       (when (e . is-a? . column-control-event%)
                         (define sb (send e get-column))
                         (if (= sb sort-by)
                             (set! flip? (not flip?))
                             (begin
                               (set! sort-by sb)
                               (set! flip? #f)))
                         (sort-list!))
                       (adjust-buttons!))]))

    (send pkg-list set-column-width 0 30 2 1000)

    (define sort-by 0)
    (define flip? #f)
    (define installed '())
    (define sorted-installed '#())

    (define/override (on-superwindow-show on?)
      (when on?
        (reset-installed-list!)))

    (define/private (reset-installed-list!)
      (set! installed
            (for*/list ([scope (in-list (get-scope-list))]
                        [(k v) (in-hash (installed-pkg-table #:scope scope))])
              (ipkg k scope (pkg-info-auto? v) (pkg-info-checksum v) (pkg-info-orig-pkg v))))
      (sort-list!))

    (define/private (selected-ipkgs)
      (for/list ([i (in-list (send pkg-list get-selections))])
        (vector-ref sorted-installed i)))
    
    (define button-line
      (new horizontal-panel%
           [parent this]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define remove-button
      (new button%
           [label sc-install-pkg-remove]
           [parent button-line]
           [callback (lambda (b e)
                       (define ipkgs (selected-ipkgs))
                       (define names (map ipkg-name ipkgs))
                       (when (really-remove? names #:parent (get-top-level-window))
                         (define scope (ipkg-scope (car ipkgs)))
                         (in-terminal
                          (string-constant install-pkg-abort-remove)
                          (lambda ()
                            (apply
                             pkg-remove-command
                             #:scope scope
                             names)))
                         (reset-installed-list!)))]))

    (define update-button
      (new button%
           [label (string-constant install-pkg-update)]
           [parent button-line]
           [callback (lambda (b e)
                       (define ipkgs (selected-ipkgs))
                       (define names (map ipkg-name ipkgs))
                       (define scope (ipkg-scope (car ipkgs)))
                       (in-terminal
                        (string-constant install-pkg-abort-update)
                        (lambda ()
                          (apply
                           pkg-update-command
                           #:scope scope
                           names)))
                       (reset-installed-list!))]))

    (define/private (adjust-buttons!)
      (define ipkgs (selected-ipkgs))
      (define same-scope? (and (pair? ipkgs)
                               ;; must be all in the same scope:
                               (for/and ([i (cdr ipkgs)])
                                 (equal? (ipkg-scope i) (ipkg-scope (car ipkgs))))))
      (send remove-button enable same-scope?)
      (send update-button enable (and same-scope?
                                      (for/and ([i (in-list ipkgs)])
                                        (not (eq? 'link (car (ipkg-source i))))))))

    (define/private (sort-list!)
      (define l (sort installed
                      (lambda (a b)
                        ((if flip? not values)
                         (case sort-by
                           [(0) (if (equal? (ipkg-scope a) (ipkg-scope b))
                                    (if (eq? (ipkg-auto? a) (ipkg-auto? b))
                                        (string<? (ipkg-name a) (ipkg-name b))
                                        (not (ipkg-auto? a)))
                                    (scope<? (ipkg-scope a) (ipkg-scope b)))]
                           [(1) (ipkg<? a b)]
                           [(2) (if (equal? (ipkg-checksum a) (ipkg-checksum b))
                                    (ipkg<? a b)
                                    (cond
                                     [(not (ipkg-checksum a)) #f]
                                     [(not (ipkg-checksum b)) #t]
                                     [else (string<? (ipkg-checksum a) (ipkg-checksum b))]))]
                           [(3) 
                            (define sa (ipkg-source a))
                            (define sb (ipkg-source b))
                            (if (equal? sa sb)
                                (ipkg<? a b)
                                (if (eq? (car sa) (car sb))
                                    (string<? (cadr sa) (cadr sb))
                                    (case (car sa)
                                      [(link) #t]
                                      [(catalog) (eq? b 'url)]
                                      [(url) #f])))])))))
      (set! sorted-installed (list->vector l))
      (send pkg-list set
            (for/list ([i (in-list l)])
              (if (ipkg-auto? i) "*" ""))
            (for/list ([i (in-list l)])
              (~a (ipkg-scope i)))
            (for/list ([i (in-list l)])
              (format "~a" (ipkg-name i)))
            (for/list ([i (in-list l)])
              (or (ipkg-checksum i) ""))
            (for/list ([i (in-list l)])
              (define s (ipkg-source i))
              (format "~a: ~a"
                      (case (car s)
                        [(catalog) "Catalog"]
                        [(url) "URL"]
                        [(link) "Link"]
                        [(static-link) "Static link"]
                        [(file) "File"])
                      (cadr s))))
      (adjust-buttons!))))

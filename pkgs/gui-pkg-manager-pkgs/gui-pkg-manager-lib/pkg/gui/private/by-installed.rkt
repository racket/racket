#lang racket/base
(require racket/class
         racket/gui/base
         racket/format
         setup/dirs
         pkg/lib
         pkg
         string-constants
         "filter-panel.rkt"
         "common.rkt")

(provide by-installed-panel%)

(struct ipkg (name scope auto? checksum source))

(define ((ipkg->source dir) ipkg)
  (define s (cadr (ipkg-source ipkg)))
  (if (not (eq? 'catalog (car (ipkg-source ipkg))))
      (path->string (path->complete-path s dir))
      s))

(define (source->string s)
  (format "~a: ~a"
          (case (car s)
            [(catalog) "Catalog"]
            [(url) "URL"]
            [(link) "Link"]
            [(static-link) "Static link"]
            [(file) "File"])
          (cadr s)))

(define (status-string a default-scope)
  (~a (if (ipkg-auto? a) "*" check-mark)
      (if (equal? (ipkg-scope a) default-scope)
          ""
          "!")
      (case (car (ipkg-source a))
        [(link static-link) "="]
        [(URL) "@"]
        [else ""])))

(define (scope<? a b)
  (cond
   [(path? a)
    (or (not (path? b))
        (bytes<? (path->bytes a) (path->bytes b)))]
   [(path? b) #f]
   [else
    (case a
      [(installation) #t]
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

    (define filter-panel (make-filter-panel this
                                            (lambda () (sort-list!))))

    (define status-text
      (new message%
           [parent this]
           [label install-status-desc]
           [font small-control-font]
           [stretchable-width #t]))

    (define pkg-list
      (new list-box%
           [parent this]
           [label #f]
           [choices null]
           [columns (list check-mark "Scope" "Name" "Checksum" "Source")]
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
    (send pkg-list set-column-width 2 
          (max 100 (let-values ([(w mn mx) (send pkg-list get-column-width 2)])
                     w))
          2 1000)
    (send pkg-list set-column-width 4 300 2 1000)


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

    (define update-button
      (new button%
           [label (string-constant install-pkg-update)]
           [parent button-line]
           [callback (lambda (b e)
                       (handle-packages
                        (string-constant install-pkg-abort-update)
                        (lambda (names scope)
                          (apply
                           pkg-update-command
                           #:scope scope
                           names))))]))

    (define promote-button
      (new button%
           [label (string-constant install-pkg-promote)]
           [parent button-line]
           [callback (lambda (b e)
                       (handle-packages
                        (string-constant install-pkg-abort-promote)
                        #:ipkgs? #t
                        (lambda (ipkgs scope)
                          ;; Links can be relative to scope:
                          (define dir (cond
                                       [(path? scope) scope]
                                       [(eq? scope 'installation) (find-pkgs-dir)]
                                       [else (find-user-pkgs-dir)]))
                          ;; Also preserve link kind:
                          (define kind (car (ipkg-source (car ipkgs))))
                          (apply
                           pkg-install-command
                           #:scope scope
                           #:link (eq? 'link kind)
                           #:static-link (eq? 'static-link kind)
                           (map (ipkg->source dir) ipkgs)))))]))

    (define demote-button
      (new button%
           [label (string-constant install-pkg-demote)]
           [parent button-line]
           [callback (lambda (b e)
                       (handle-packages
                        (string-constant install-pkg-abort-demote)
                        (lambda (names scope)
                          (apply
                           pkg-remove-command
                           #:demote #t
                           #:scope scope
                           names))))]))

    (define remove-button
      (new button%
           [label sc-install-pkg-remove]
           [parent button-line]
           [callback (lambda (b e)
                       (handle-packages
                        (string-constant install-pkg-abort-remove)
                        #:check (lambda (names)
                                  (really-remove? names #:parent (get-top-level-window)))
                        (lambda (names scope)
                          (apply
                           pkg-remove-command
                           #:scope scope
                           names))))]))

    (define/private (handle-packages label cb 
                                     #:ipkgs? [ipkgs? #f]
                                     #:check [check void])
      (define ipkgs (selected-ipkgs))
      (define names (map ipkg-name ipkgs))
      (when (check names)
        (define scope (ipkg-scope (car ipkgs)))
        (in-terminal
         label
         (lambda ()
           (cb (if ipkgs? ipkgs names) scope)))
        (reset-installed-list!)))

    (define/private (adjust-buttons!)
      (define ipkgs (selected-ipkgs))
      (define same-scope? (and (pair? ipkgs)
                               ;; must be all in the same scope:
                               (for/and ([i (in-list (cdr ipkgs))])
                                 (equal? (ipkg-scope i) (ipkg-scope (car ipkgs))))))
      (send remove-button enable same-scope?)
      (send demote-button enable (and same-scope?
                                      (for/and ([i (in-list ipkgs)])
                                        (not (ipkg-auto? i)))))
      (send promote-button enable (and same-scope?
                                       (for/and ([i (in-list ipkgs)])
                                         (ipkg-auto? i))
                                       ;; all 'catalog, 'link, or 'static-link
                                       (let ([kind (car (ipkg-source (car ipkgs)))])
                                         (and (memq kind '(catalog link static-link))
                                              (for/and ([i (in-list (cdr ipkgs))])
                                                (eq? kind (car (ipkg-source i))))))))
      (send update-button enable (and same-scope?
                                      (for/and ([i (in-list ipkgs)])
                                        (not (memq (car (ipkg-source i))
                                                   '(link static-link)))))))

    (define/private (sort-list!)
      (define default-scope (default-pkg-scope))
      (define show-installed (let ([rx (send filter-panel get-rx)])
                               (filter
                                (lambda (a)
                                  (or (regexp-match? rx (status-string a default-scope))
                                      (regexp-match? rx (ipkg-name a))
                                      (regexp-match? rx (~a (ipkg-scope a)))
                                      (regexp-match? rx (or (ipkg-checksum a) ""))
                                      (regexp-match? rx (source->string (ipkg-source a)))))
                                installed)))
      (send filter-panel set-result (length show-installed) (length installed))
      (define l (sort show-installed
                      (lambda (a b)
                        ((if flip? not values)
                         (case sort-by
                           [(0) (if (eq? (ipkg-auto? a) (ipkg-auto? b))
                                    (if (equal? (ipkg-scope a) (ipkg-scope b))
                                        (string<? (ipkg-name a) (ipkg-name b))
                                        (not (ipkg-auto? a)))
                                    (ipkg-auto? b))]
                           [(1) (if (equal? (ipkg-scope a) (ipkg-scope b))
                                    (if (eq? (ipkg-auto? a) (ipkg-auto? b))
                                        (string<? (ipkg-name a) (ipkg-name b))
                                        (not (ipkg-auto? a)))
                                    (scope<? (ipkg-scope a) (ipkg-scope b)))]
                           [(2) (ipkg<? a b)]
                           [(3) (if (equal? (ipkg-checksum a) (ipkg-checksum b))
                                    (ipkg<? a b)
                                    (cond
                                     [(not (ipkg-checksum a)) #f]
                                     [(not (ipkg-checksum b)) #t]
                                     [else (string<? (ipkg-checksum a) (ipkg-checksum b))]))]
                           [(4) 
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
              (status-string i default-scope))
            (for/list ([i (in-list l)])
              (~a (ipkg-scope i)))
            (for/list ([i (in-list l)])
              (format "~a" (ipkg-name i)))
            (for/list ([i (in-list l)])
              (or (ipkg-checksum i) ""))
            (for/list ([i (in-list l)])
              (source->string (ipkg-source i))))
      (adjust-buttons!))))

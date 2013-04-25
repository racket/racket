#lang racket/base
(require racket/class
         racket/gui/base
         racket/port
         racket/set
         racket/format
         string-constants
         net/url
         pkg/lib
         (prefix-in pkg: pkg)
         (prefix-in db: pkg/db)
         "common.rkt")

(provide by-list-panel%)

(define sc-pkg-update-package-list (string-constant install-pkg-update-package-list))
(define sc-pkg-stop-update (string-constant install-pkg-stop-update))

(define check-mark "✓")

(define default-status
  (~a check-mark ": installed"
      "  "
      "*: auto-installed"
      "  "
      "!: not default scope"
      "  "
      "=: installed as link"
      "  "
      "@: installed from URL"))

(define by-list-panel%
  (class vertical-panel%
    (init-field [in-terminal in-terminal])

    (super-new)

    (inherit get-top-level-window)

    (define tool-panel
      (new horizontal-panel%
           [parent this]
           [alignment '(left center)]
           [stretchable-height #f]))

    (define keep-rx #rx"")
    (define/private (list-pkg-keep? a)
      (or (regexp-match? keep-rx (db:pkg-name a))
          (regexp-match? keep-rx (db:pkg-author a))
          (regexp-match? keep-rx (db:pkg-desc a))
          (regexp-match? keep-rx (format-tags (pkg-tags a)))
          (regexp-match? keep-rx (db:pkg-source a))
          (regexp-match? keep-rx (db:pkg-catalog a))))

    (define filter-text
      (new text-field%
           [label (~a (string-constant install-pkg-filter) ":")]
           [parent tool-panel]
           [font small-control-font]
           [stretchable-width #t]
           [callback (lambda (tf e)
                       (define s (send tf get-value))
                       (define terms (filter (lambda (s) (not (string=? s "")))
                                             (regexp-split #rx"[, \t\r\n]" s)))
                       (define rx
                         (regexp (apply ~a
                                        #:separator "|"
                                        (for/list ([term terms])
                                          (~a "(?i:" (regexp-quote term) ")")))))
                       (unless (equal? rx keep-rx)
                         (set! keep-rx rx)
                         (sort-pkg-list!)))]))

    (define filter-result
      (new message%
           [label "9999/9999 match"]
           [parent tool-panel]
           [font small-control-font]))
    (send filter-result set-label "")
  
    (define updating? #f)

    (define update-button
      (new button%
           [label sc-pkg-update-package-list]
           [parent tool-panel]
           [font small-control-font]
           [callback
            (lambda (b e)
              (if updating?
                  (interrupt-task!)
                  (update-db-package-list)))]))

    (define status-text
      (new message%
           [parent this]
           [label default-status]
           [font small-control-font]
           [stretchable-width #t]))

    (define sort-column -1)
    (define list-pkg<? (lambda (a b) #f))

    (define/private (sort-by! col)
      (define sel (case col
                    [(0 1) db:pkg-name]
                    [(2) db:pkg-author]
                    [(3) db:pkg-desc]
                    [(4) (lambda (p) (format-tags (pkg-tags p)))]
                    [(5) db:pkg-checksum]
                    [(6) db:pkg-source]
                    [(7) db:pkg-catalog]))
      (define switch (if (= sort-column col)
                         not
                         values))
      (set! sort-column (if (= sort-column col)
                            -1
                            col))
      (set! list-pkg<?
            (lambda (a b)
              (switch
               (cond
                [(and (not (string=? (sel a) ""))
                      (string=? (sel b) ""))
                 #t]
                [(and (string=? (sel a) "")
                      (not (string=? (sel b) "")))
                 #f]
                [(string<? (sel a) (sel b)) #t]
                [(string=? (sel a) (sel b))
                 (cond
                  [(string<? (db:pkg-name a) (db:pkg-name b)) #t]
                  [(string=? (db:pkg-name a) (db:pkg-name b))
                   (string<? (db:pkg-catalog a) (db:pkg-catalog))]
                  [else #f])]
                [else #f])))))
    (sort-by! 7)

    (define pkg-list
      (new list-box%
           [parent this]
           [label #f]
           [choices null]
           [columns (list check-mark "Package" "Author" "Description" "Tags" "Checksum" "Source" "Catalog")]
           [style '(multiple column-headers clickable-headers)]
           [callback (lambda (lb e)
                       (when (e . is-a? . column-control-event%)
                         (sort-by! (send e get-column))
                         (sort-pkg-list!))
                       (adjust-buttons!))]))
    
    (send pkg-list set-column-width 0 30 2 1000)
    (send pkg-list set-column-width 3 300 2 1000)

    (define button-line
      (new horizontal-panel%
           [parent this]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define install-button
      (new button%
           [label (pick-wider normal-control-font
			      (string-constant install-pkg-install)
			      (string-constant install-pkg-update))]
           [parent button-line]
           [style '(border)]
           [callback (lambda (b e)
                       (define pkgs (selected-pkgs))
                       (define names (map db:pkg-name pkgs))
                       (define first-inst
                         (hash-ref installed (db:pkg-name (car pkgs)) #f))
                       (define s (queue-scroll!))
                       (in-terminal
                        (if first-inst
                            (string-constant install-pkg-abort-update)
                            (string-constant install-pkg-abort-install))
                        (lambda ()
                          (apply
                           (if first-inst pkg:update pkg:install)
                           #:scope (and first-inst
                                        (car first-inst))
                           names)))
                       (set-box! s #f)
                       (refresh-installed-list!))]))
    (define remove-button
      (new button%
           [label sc-install-pkg-remove]
           [parent button-line]
           [callback (lambda (b e)
                       (define pkgs (selected-pkgs))
                       (define names (map db:pkg-name pkgs))
                       (when (really-remove? names #:parent (get-top-level-window))
                         (define scope
                           (car (hash-ref installed (db:pkg-name (car pkgs)) #f)))
                         (define s (queue-scroll!))
                         (in-terminal
                          (string-constant install-pkg-abort-remove)
                          (lambda ()
                            (apply
                             pkg:remove
                             #:scope scope
                             names)))
                         (set-box! s #f)
                         (refresh-installed-list!)))]))

    ;; When a terminal panel appears, it may shrink the
    ;; list box enough that the selected item is not visible,
    ;; so re-sort to adjust scrolling:
    (define (queue-scroll!)
      (define b (box #t))
      (queue-callback (lambda ()
                        (when (unbox b)
                          (sort-pkg-list!)))
                      #f)
      b)

    (define/private (selected-pkgs)
      (for/list ([i (in-list (send pkg-list get-selections))])
        (send pkg-list get-data i)))

    (define/private (adjust-buttons!)
      (define sels (for/list ([p (selected-pkgs)])
                     (hash-ref installed (db:pkg-name p) #f)))
      (define all-installed? (and (pair? sels) 
                                  (andmap values sels)
                                  ;; must be all in the same scope:
                                  (for/and ([p (cdr sels)])
                                    (eq? (car p) (caar sels)))))
      (define none-installed? (not (ormap values sels)))
      (send install-button enable (and (pair? sels)
                                       (or (and all-installed?
                                                (not (for/or ([p (in-list sels)])
                                                       (eq? 'link (car (pkg-info-orig-pkg (cdr p)))))))
                                           none-installed?)))
      (send install-button set-label (if all-installed?
                                         (string-constant install-pkg-update)
                                         (string-constant install-pkg-install)))
      (send remove-button enable all-installed?))

    (adjust-buttons!)

    (define/private (check-catalogs)
      (define user-catalogs (pkg-config-catalogs))
      (define db-catalogs (db:get-catalogs))
      (unless (equal? (list->set db-catalogs)
                      (list->set user-catalogs))
        (when (= 1 (message-box/custom "Package Catalogs"
                                       (~a
                                        (string-constant install-pkg-update-catalogs?)
                                        "\n\n"
                                        (string-constant install-pkg-currently-configured-are) ":\n"
                                        (apply ~a
                                               (for/list ([url user-catalogs])
                                                 (~a "  " url "\n")))
                                        "\n"
                                        (string-constant install-pkg-database-recorded-are) ":\n"
                                        (apply ~a
                                               (for/list ([url db-catalogs])
                                                 (~a "  " url "\n"))))
                                       (string-constant install-pkg-update-catalogs)
                                       (string-constant install-pkg-do-not-update-catalogs)
                                       #f
                                       this
                                       '(caution default=1)))
          (db:set-catalogs! user-catalogs)
          (update-db-package-list))))

    (define/private (check-init)
      (unless (file-exists? (db:current-pkg-catalog-file))
        (db:set-catalogs! (pkg-config-catalogs))
        (update-db-package-list)))

    (define task #f)
    (define finalize void)
    
    (define/private (task! thunk given-finalize)
      (define finalized? #f)
      (set! finalize (lambda (ok?)
                       (unless finalized?
                         (set! finalized? #t)
                         (given-finalize ok?))))
      (set! task (thread 
                  (lambda ()
                    (with-handlers ([exn:break? void])
                      (thunk)
                      (let ([f finalize])
                        (queue-callback/wait (lambda () (f #t)))))))))

    (define/private (queue-callback/wait thunk)
      (define s (make-semaphore))
      (define ok? #t)
      (queue-callback (lambda ()
                        (when ok?
                          (thunk)
                          (semaphore-post s))))
      (dynamic-wind
          void
          (lambda () (semaphore-wait s))
          (lambda () (set! ok? #f))))

    (define/private (interrupt-task!)
      (when task
        (break-thread task)
        (sync task)
        (finalize #f))
      (set! task #f)
      (send status-text set-label default-status))

    (define/private (update-db-package-list)
      (interrupt-task!)
      (set! updating? #t)
      (send update-button set-label sc-pkg-stop-update)
      (send status-text set-label "Updating package list...")
      (task!
       (lambda ()
         (define db-catalogs (db:get-catalogs))
         (for ([catalog (in-list db-catalogs)])
           (queue-callback/wait
            (lambda ()
              (send status-text set-label (format "Updating from ~a..." catalog))))
           (define details
             (parameterize ([current-pkg-catalogs (list (string->url catalog))])
               (get-all-pkg-details-from-catalogs)))
           (db:set-pkgs! catalog (for/list ([(name ht) (in-hash details)])
                                   (db:pkg name
                                           catalog
                                           (hash-ref ht 'author "")
                                           (hash-ref ht 'source "")
                                           (hash-ref ht 'checksum "")
                                           (hash-ref ht 'description ""))))
           (for/list ([(name ht) (in-hash details)])
             (db:set-pkg-tags! name catalog (hash-ref ht 'tags '())))))
       (lambda (finished?)
         (send status-text set-label default-status)
         (set! updating? #f)
         (send update-button set-label sc-pkg-update-package-list)
         (refresh-pkg-list!))))

    (define/private (background-pkg-details)
      (task!
       (lambda ()
         (define catalog-ht (make-hash))
         (for ([a-pkg (in-vector pkgs)]
               [pos (in-naturals)])
           (define name (db:pkg-name a-pkg))
           (define catalog (db:pkg-catalog a-pkg))
           (send status-text set-label 
                 (~a "Getting details for " name " from " catalog "..."))
           (define all-ht 
             (or (hash-ref catalog-ht catalog #f)
                 (let ([all-ht (parameterize ([current-pkg-catalogs (list (string->url catalog))])
                                 (get-all-pkg-details-from-catalogs))])
                   (hash-set! catalog-ht catalog all-ht)
                   all-ht)))
           (define ht (hash-ref all-ht name))
           (define author (hash-ref ht 'author ""))
           (define source (hash-ref ht 'source ""))
           (define checksum (hash-ref ht 'checksum ""))
           (define desc (hash-ref ht 'description ""))
           (define tags (hash-ref ht 'tags '()))
           (define new-pkg (db:pkg name catalog author source checksum desc))
           (unless (and (equal? new-pkg a-pkg)
                        (equal? tagss (pkg-tags a-pkg)))
             (db:set-pkg! name catalog author source checksum desc)
             (db:set-pkg-tags! name catalog tags)
             (queue-callback/wait
              (lambda ()
                (vector-set! pkgs pos new-pkg)
                (set! tagss (hash-set (hash-remove tagss a-pkg)
                                      new-pkg
                                      tags))
                (define lpos (vector-ref posns pos))
                (when lpos
                  (send pkg-list set-string lpos (->label-string author) 2)
                  (send pkg-list set-string lpos (->label-string desc) 3)
                  (send pkg-list set-string lpos (->label-string (format-tags tags)) 4)
                  (send pkg-list set-string lpos (->label-string checksum) 5)
                  (send pkg-list set-string lpos (->label-string source) 6)))))))
       (lambda (ok?)
         (send status-text set-label default-status))))

    (define/private (->label-string s)
      (let ([s (regexp-replace* #rx"[\r\n]+" s " ")])
	(substring s 0 (min 200 (string-length s)))))

    (define pkgs '#())
    (define tagss #(hash))
    (define posns '#())
    (define installed '#())
    (define default-scope 'user)
    
    (define/private (refresh-pkg-list!)
      (define pkg-list (db:get-pkgs))
      (define tags-list (map (lambda (p)
                               (db:get-pkg-tags (db:pkg-name p)
                                                (db:pkg-catalog p)))
                             pkg-list))
      (set! pkgs (list->vector pkg-list))
      (set! tagss (for/hash ([p (in-list pkg-list)]
                             [t (in-list tags-list)])
                    (values p t)))
      (set! default-scope (default-pkg-scope))
      (refresh-installed-list! #:always? #t))

    (define/private (refresh-installed-list! #:always? [always? #f])
      (define new-installed
        (for*/hash ([scope (in-list '(installation user shared))]
                    [(k v) (in-hash (installed-pkg-table #:scope scope))])
          (values k (cons scope v))))
      (when (or always?
                (not (equal? installed new-installed)))
        (set! installed new-installed)
        (sort-pkg-list!)))

    (define/private (pkg-tags p)
      (hash-ref tagss p '()))
    
    (define/private (sort-pkg-list!)
      (define sels (for/list ([i (in-list (send pkg-list get-selections))])
                     (define p (send pkg-list get-data i))
                     (cons (db:pkg-name p) (db:pkg-catalog p))))
      (set! posns (make-vector (vector-length pkgs) #f))
      (define list-pkg+poses
        (sort
         (filter
          (lambda (p) (list-pkg-keep? (car p)))
          (for/list ([p pkgs]
                     [i (in-naturals)])
            (cons p i)))
         list-pkg<?
         #:key car))
      (for ([p (in-list list-pkg+poses)]
            [j (in-naturals)])
        (vector-set! posns (cdr p) j))
      (define list-pkgs (map car list-pkg+poses))
      (send filter-result set-label (format "~a/~a match" 
                                            (length list-pkgs)
                                            (vector-length pkgs)))
      (send pkg-list set
            (for/list ([p list-pkgs])
              (define v (hash-ref installed (db:pkg-name p) #f))
              (cond
               [(not v) ""]
               [else
                (define info (cdr v))
                (~a (cond 
                     [(pkg-info-auto? info) "*"]
                     [else check-mark])
                    (cond
                     [(eq? (car v) default-scope) ""]
                     [else "!"])
                    (case (car (pkg-info-orig-pkg info))
                      [(catalog) ""]
                      [(link) "="]
                      [(url) "@"]))]))
            (for/list ([p list-pkgs]) (->label-string (db:pkg-name p)))
            (for/list ([p list-pkgs]) (->label-string (db:pkg-author p)))
            (for/list ([p list-pkgs]) (->label-string (db:pkg-desc p)))
            (for/list ([p list-pkgs]) (->label-string (format-tags (pkg-tags p))))
            (for/list ([p list-pkgs]) (->label-string (db:pkg-checksum p)))
            (for/list ([p list-pkgs]) (->label-string (db:pkg-source p)))
            (for/list ([p list-pkgs]) (->label-string (db:pkg-catalog p))))
      (let ([ht (for/hash ([p list-pkgs]
                           [i (in-naturals)])
                  (send pkg-list set-data i p)
                  (values (cons (db:pkg-name p) (db:pkg-catalog p))
                          i))])
        (for/fold ([did? #f]) ([sel (in-list sels)])
          (define i (hash-ref ht sel #f))
          (when i
            (if did?
                (send pkg-list select i)
                (begin
                  (send pkg-list set-selection i)
                  (let ([f (send pkg-list get-first-visible-item)]
                        [n (send pkg-list number-of-visible-items)])
                    (unless (<= f i (+ f n -1))
                      (send pkg-list set-first-visible-item
                            (if (i . < . f)
                                i
                                (max 0 (add1 (- i n))))))))))
          (or did? i)))
      (adjust-buttons!))

    (define/private (format-tags tags)
      (apply ~a #:separator ", " tags))

    (define prepared? #f)

    (define/override (on-superwindow-show on?)
      (if on?
          (cond
           [prepared? (refresh-installed-list!)]
           [else
            (check-init)
            (refresh-pkg-list!)
            (check-catalogs)
            (set! prepared? #t)])
          (interrupt-task!)))))

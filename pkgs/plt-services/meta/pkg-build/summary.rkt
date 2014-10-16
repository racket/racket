#lang at-exp racket/base
(require racket/format
         racket/file
         scribble/html
         (only-in plt-web site page call-with-registered-roots))

(provide summary-page
         (struct-out doc/main)
         (struct-out doc/extract)
         (struct-out doc/salvage)
         (struct-out doc/none)
         (struct-out conflicts/indirect))

(struct doc/main (name path) #:prefab)
(struct doc/extract (name path) #:prefab)
(struct doc/salvage (name path) #:prefab)
(struct doc/none (name) #:prefab)

(struct conflicts/indirect (path) #:prefab)

(define (summary-page summary-ht dest-dir)
  (define page-site (site "pkg-build"
                          #:url "http://pkg-build.racket-lang.org/"
                          #:share-from (site "www"
                                             #:url "http://racket-lang.org/"
                                             #:generate? #f)))

  (define page-title "Package Build Results")

  (define summary
    (for/list ([pkg (in-list (sort (hash-keys summary-ht) string<?))])
      (define ht (hash-ref summary-ht pkg))
      (define failed? (and (hash-ref ht 'failure-log) #t))
      (define succeeded? (and (hash-ref ht 'success-log) #t))
      (define status
        (cond
         [(and failed? (not succeeded?)) 'failure]
         [(and succeeded? (not failed?)) 'success]
         [(and succeeded? failed?) 'confusion]
         [else 'unknown]))
      (define dep-status
        (if (eq? status 'success)
            (if (hash-ref ht 'dep-failure-log)
                'failure
                'success)
            'unknown))
      (define docs (hash-ref ht 'docs))
      (define author (hash-ref ht 'author))
      (define conflicts-log (hash-ref ht 'conflicts-log))
      (tr (td pkg
              (div class: "author" author))
          (td (if (null? docs)
                  ""
                  (list
                   "Docs: "
                   (add-between
                    (for/list ([doc (in-list docs)])
                      (cond
                       [(doc/main? doc)
                        (a href: (doc/main-path doc)
                           (doc/main-name doc))]
                       [(doc/extract? doc)
                        (a href: (doc/extract-path doc)
                           (doc/extract-name doc))]
                       [(doc/salvage? doc)
                        (list (a href: (doc/salvage-path doc)
                                 (doc/salvage-name doc))
                              (span class: "annotation"
                                    nbsp
                                    "(salvaged)"))]
                       [(doc/none? doc)
                        (doc/none-name doc)]
                       [else "???"]))
                    ", "))))
          (td class: (case status
                       [(failure confusion) "stop"]
                       [(success)
                        (case dep-status
                          [(failure) "yield"]
                          [else "go"])]
                       [else "unknown"])
              (case status
                [(failure)
                 (a href: (hash-ref ht 'failure-log)
                    "install fails")]
                [(success)
                 (list
                  (a href: (hash-ref ht 'success-log)
                     "install succeeds")
                  (case dep-status
                    [(failure)
                     (list
                      " with "
                      (a href: (hash-ref ht 'dep-failure-log)
                         "dependency problems"))]))]
                [(confusion)
                 (list
                  "install both "
                  (a href: (hash-ref ht 'success-log)
                     "succeeds")
                  " and "
                  (a href: (hash-ref ht 'failure-log) "fails"))]
                [else ""]))
          (td class: (if conflicts-log "stop" "neutral")
              (if conflicts-log
                  (a href: (if (conflicts/indirect? conflicts-log)
                               (conflicts/indirect-path conflicts-log)
                               conflicts-log)
                     (if (conflicts/indirect? conflicts-log)
                         "conflicts in dependency"
                         "conflicts"))
                  "")))))

  (define page-headers
    (style/inline @~a|{
                    .go { background-color: #ccffcc }
                    .stop { background-color: #ffcccc }
                    .yield { background-color: #ffffcc }
                    .author { font-size: small; font-weight: normal; }
                    .annotation { font-size: small }
                  }|))

  (void (page #:site page-site
              #:file "index.html"
              #:title page-title
              (html (head (title page-title)
                          page-headers)
                    (body (table summary)))))

  ;; Render to "pkg-build", then move up:
  (call-with-registered-roots
   (lambda ()
     (parameterize ([current-directory dest-dir])
       (render-all))))

  (define sub-dir (build-path dest-dir "pkg-build"))
  (for ([f (in-list (directory-list sub-dir))])
    (delete-directory/files f #:must-exist? #f)
    (rename-file-or-directory (build-path sub-dir f) f))
  (delete-directory sub-dir))

#lang racket/base
(require racket/format
         racket/match
         racket/list
         racket/path
         racket/file
         racket/port
         racket/string
         setup/unpack
         setup/dirs
         net/url
         file/untgz
         file/unzip
         file/private/check-path
         openssl/sha1
         json
         net/git-checkout
         "../name.rkt"
         "../strip.rkt"
         "catalog.rkt"
         "download.rkt"
         "print.rkt"
         "path.rkt"
         "dirs.rkt"
         "desc.rkt"
         "params.rkt"
         "get-info.rkt"
         "mod-paths.rkt"
         "addl-installs.rkt"
         "repo-path.rkt"
         "orig-pkg.rkt"
         "git.rkt"
         "prefetch.rkt"
         "checkout-credentials.rkt"
         "network.rkt"
         "github-url.rkt"
         "timeout.rkt")

(provide (struct-out install-info)
         remote-package-checksum
         stage-package/info
         pkg-stage
         package-url->checksum
         github-client_secret
         github-client_id)

(struct install-info (name orig-pkg directory git-directory clean? checksum module-paths additional-installs)
  #:prefab)

(define (communication-type type)
  (if (and (eq? type 'github)
           use-git-for-github?)
      'git
      (if (eq? type 'git-url)
          'git
          type)))

(define (remote-package-checksum pkg download-printf pkg-name
                                 #:type [type #f]
                                 #:prefetch? [prefetch? #f]
                                 #:prefetch-group [prefetch-group #f]
                                 #:catalog-lookup-cache [catalog-lookup-cache #f]
                                 #:remote-checksum-cache [remote-checksum-cache #f])
  (define (lookup-normally download-printf)
    (define checksum
      (match pkg
        [`(catalog ,pkg-name . ,_)
         (define info (package-catalog-lookup pkg-name #f catalog-lookup-cache
                                              download-printf
                                              #:prefetch-group prefetch-group))
         (hash-ref info 'checksum)]
        [`(url ,pkg-url-str)
         (package-url->checksum pkg-url-str
                                #:type type
                                #:download-printf download-printf
                                #:pkg-name pkg-name)]
        [`(git ,pkg-url-str)
         (package-url->checksum pkg-url-str
                                #:type (or type 'git-url)
                                #:download-printf download-printf
                                #:pkg-name pkg-name)]
        [`(clone ,_ ,pkg-url-str)
         (package-url->checksum pkg-url-str
                                #:type 'clone
                                #:download-printf download-printf
                                #:pkg-name pkg-name)]))
    (when remote-checksum-cache
      (hash-set! remote-checksum-cache pkg checksum))
    checksum)

  (when (and prefetch? (not (and catalog-lookup-cache
                                 remote-checksum-cache
                                 prefetch-group)))
    (error "internal error: insufficient caches or group for prefetch of package checksum"))

  ;; Loop to combine cache lookup and prefetch dispatch:
  (let loop ([prefetch? prefetch?] [download-printf download-printf])
    (cond
     [(and remote-checksum-cache
           (hash-ref remote-checksum-cache pkg #f))
      => (lambda (checksum)
           (if (and (prefetch-future? checksum)
                    (not prefetch?))
               (prefetch-touch checksum prefetch-group download-printf)
               checksum))]
     [prefetch?
      (make-prefetch-future/hash remote-checksum-cache
                                 pkg
                                 lookup-normally
                                 prefetch-group
                                 download-printf)]
     [else
      (lookup-normally download-printf)])))

;; Downloads a package (if needed) and unpacks it (if needed) into a  
;; temporary directory.
(define (stage-package/info pkg
                            given-type
                            given-pkg-name
                            #:at-dir given-at-dir
                            #:given-checksum [given-checksum #f]
                            #:cached-url [cached-url #f]
                            #:use-cache? use-cache?
                            check-sums?
                            download-printf
                            metadata-ns
                            #:catalog-lookup-cache [catalog-lookup-cache #f]
                            #:remote-checksum-cache [remote-checksum-cache #f]
                            #:strip [strip-mode #f]
                            #:force-strip? [force-strip? #f]
                            #:in-place? [in-place? #f]
                            #:in-place-clean? [in-place-clean? #f]
                            #:link-dirs? [link-dirs? #f])
  (define-values (inferred-pkg-name type) 
    (if (path? pkg)
        (package-source->name+type (path->string pkg)
                                   (or given-type
                                       (if (directory-exists? pkg)
                                           (if link-dirs?
                                               'link
                                               'dir)
                                           'file))
                                   #:must-infer-name? (not given-pkg-name)
                                   #:complain (complain-about-source given-pkg-name))
        (package-source->name+type pkg given-type
                                   #:link-dirs? link-dirs?
                                   #:must-infer-name? (not given-pkg-name)
                                   #:complain (complain-about-source given-pkg-name))))
  (define pkg-name (or given-pkg-name inferred-pkg-name))
  (when (and type (not pkg-name))
    (pkg-error (~a "could not infer package name from source\n"
                   "  source: ~a")
               pkg))
  (cond
   [(and (eq? type 'github)
         (not (regexp-match? #rx"^git(?:hub)?://" pkg)))
    ;; Add "git://github.com/"
    (stage-package/info (string-append "git://github.com/" pkg) type 
                        pkg-name
                        #:at-dir given-at-dir
                        #:given-checksum given-checksum
                        #:use-cache? use-cache?
                        check-sums? download-printf
                        metadata-ns
                        #:strip strip-mode
                        #:force-strip? force-strip?)]
   [(eq? type 'clone)
    (define pkg-url (string->url pkg))
    (define-values (transport host port repo branch-or-commit path)
      (split-git-or-hub-url pkg-url))
    (define branch (if (or (eq? branch-or-commit 'head)
                           (looks-like-commit? branch-or-commit))
                       'head
                       branch-or-commit))
    (define pkg-no-query (real-git-url pkg-url host port repo))
    (define clone-dir (or given-at-dir
                          (current-directory)))
    
    (define (status s) (download-printf "~a\n" s))

    (define orig-pkg (desc->orig-pkg 'clone pkg given-at-dir))

    (define checksum
      (cond
        [(and (eq? branch-or-commit 'head)
              given-checksum)
         given-checksum]
        [else
         (define found-checksum
           (remote-package-checksum orig-pkg download-printf pkg-name
                                    #:catalog-lookup-cache catalog-lookup-cache
                                    #:remote-checksum-cache remote-checksum-cache))
         ;; If `found-checksum` matches `branch-or-commit`, then it must
         ;; be a commit, and so we're pinned to that commit
         (if (equal? found-checksum branch-or-commit)
             branch-or-commit
             (or given-checksum
                 found-checksum))]))

    ;; If the clone directory already exists, and if it already has
    ;; the target commit, then we use that directory. It may have
    ;; changes to package metadata (even uncommitted changes) that
    ;; we'd prefer to use
    (define working-dir
      (and (directory-exists? clone-dir)
           (or (not checksum)
               (let ([o (open-output-bytes)])
                 (and (parameterize ([current-directory clone-dir]
                                     [current-error-port (open-output-bytes)]
                                     [current-output-port o])
                        (git "log" "--pretty=%H" #:fail-mode 'status))
                      (regexp-match (~a "(?m:^" (regexp-quote checksum) ")")
                                    (get-output-bytes o)))))
           (if (null? path)
               clone-dir
               (apply build-path clone-dir path))))

    (define tmp-dir (and (not working-dir)
                         (make-temporary-file
                          (string-append "~a-" pkg-name)
                          'directory)))

    (define staged? #f)
    (dynamic-wind
     void
     (λ ()
       (unless (and (directory-exists? clone-dir)
                    (directory-exists? (build-path clone-dir ".git")))
         (download-printf "Cloning remote repository ~a\n to ~a\n"
                          pkg-no-query
                          clone-dir)
         (make-directory* clone-dir)
         (parameterize ([current-directory clone-dir])
           (apply git #:status status "clone"
                  (append
                   (if (eq? branch 'head) null (list "-b" branch))
                   (list pkg-no-query ".")))))

       (unless working-dir
         (parameterize ([current-directory clone-dir])
           (download-printf "Fetching from remote repository ~a\n"
                            pkg-no-query)
           (apply git #:status status "fetch"
                  (append
                   (list pkg-no-query)
                   (if (eq? branch 'head) null (list branch))))))

       (cond
        [tmp-dir
         ;; Make a clone of the [to-be-]linked checkout so that
         ;; we can check dependencies, etc., before changing
         ;; the checkout. By using `--shared` for the clone, all
         ;; the commits that we've fetched will be available to
         ;; checkout (even though they may be unreachable at this
         ;; point, since we haven't merged the fetched commits).
         (download-printf "Cloning repository locally for staging\n")
         (git #:status status "clone" "--shared" clone-dir tmp-dir)
         (parameterize ([current-directory tmp-dir])
           (apply git #:status status "checkout"
                  (cond
                    [checksum => list]
                    [(eq? branch 'head) null]
                    [else (list branch)])))
         (lift-git-directory-content tmp-dir path)]
        [else
         (download-printf "Using ~s clone directory directly for metadata\n" pkg-name)])

       (begin0
        (update-install-info-checksum
         (update-install-info-orig-pkg
          (update-install-info-git-dir
           (stage-package/info (or working-dir tmp-dir)
                               'dir
                               pkg-name
                               #:at-dir given-at-dir
                               #:given-checksum checksum
                               #:cached-url pkg-url
                               #:use-cache? use-cache?
                               check-sums?
                               download-printf
                               metadata-ns
                               #:strip strip-mode
                               #:force-strip? force-strip?
                               #:in-place? #t
                               #:in-place-clean? (not working-dir))
           (apply build-path clone-dir path))
          orig-pkg)
         checksum)
        (set! staged? #t)))
     (λ ()
       (unless staged?
         (when tmp-dir
           (delete-directory/files tmp-dir)))))]
   [(or (eq? type 'file-url)
        (eq? type 'dir-url)
        (eq? type 'github)
        (eq? type 'git)
        (eq? type 'git-url))
    (define pkg-url-str (normalize-url type pkg (string->url pkg)))
    (define pkg-url (string->url pkg-url-str))
    (define scheme (url-scheme pkg-url))

    (define orig-pkg (desc->orig-pkg type pkg-url-str #f))
    (define found-checksum
      ;; If a checksum is given, use that. In the case of a non-github
      ;; source, we could try to get the checksum from the source, and
      ;; then check whether it matches the expected one, but we choose
      ;; to avoid an extra trip to the server.
      (or given-checksum
          (remote-package-checksum orig-pkg download-printf pkg-name
                                   #:catalog-lookup-cache catalog-lookup-cache
                                   #:remote-checksum-cache remote-checksum-cache)))
    (when check-sums?
      (check-checksum given-checksum found-checksum "unexpected" pkg #f))
    (define checksum (or found-checksum given-checksum))
    (define downloaded-info
       (match (communication-type type)
         ['git
          (when (equal? checksum "")
            (pkg-error 
             (~a "cannot use empty checksum for Git repository package source\n"
                 "  source: ~a")
             pkg))
          (define-values (transport host port repo branch path)
            (split-git-or-hub-url pkg-url #:type type))
          (define tmp-dir
            (make-temporary-file
             (string-append
              "~a-"
              (regexp-replace* #rx"[:/\\.~]" (format "~a.~a" repo branch) "_"))
             'directory))
          
          (define staged? #f)
          (dynamic-wind
           void
           (λ ()
             (download-repo! pkg-url transport host port repo tmp-dir checksum 
                             #:use-cache? use-cache?
                             #:download-printf download-printf)
             (lift-git-directory-content tmp-dir path)

             (begin0
              (stage-package/info tmp-dir
                                  'dir
                                  pkg-name
                                  #:at-dir given-at-dir
                                  #:given-checksum checksum
                                  #:cached-url pkg-url
                                  #:use-cache? use-cache?
                                  check-sums?
                                  download-printf
                                  metadata-ns
                                  #:strip strip-mode
                                  #:force-strip? force-strip?
                                  #:in-place? #t
                                  #:in-place-clean? #t)
              (set! staged? #t)))
           (λ ()
             (when (and use-cache? (not staged?))
               (clean-cache pkg-url checksum))
             (unless staged?
               (delete-directory/files tmp-dir))))]
         ['github
          (unless checksum
            (pkg-error 
             (~a "could not find checksum for GitHub package source, which implies it doesn't exist\n"
                 "  source: ~a")
             pkg))
          (when (equal? checksum "")
            (pkg-error 
             (~a "cannot use empty checksum for GitHub package source\n"
                 "  source: ~a")
             pkg))
          (match-define (list* user repo branch path)
                        (split-github-url pkg-url))
          (define new-url
            (url "https" #f "github.com" #f #t
                 (map (λ (x) (path/param x empty))
                      (list user repo "legacy.tar.gz" checksum))
                 empty
                 #f))
          (define tmp.tgz
            (make-temporary-file
             (string-append
              "~a-"
              (format "~a.~a.tgz" repo branch))
             #f))
          (define tmp-dir
            (make-temporary-file
             (string-append
              "~a-"
              (format "~a.~a" repo branch))
             'directory))

          (dynamic-wind
              void
              (λ ()
                 (download-file! new-url tmp.tgz checksum
                                 #:use-cache? use-cache?
                                 #:download-printf download-printf)
                 (define staged? #f)
                 (dynamic-wind
                     void
                     (λ ()
                        (untar tmp.tgz tmp-dir #:strip-components 1)

                        (unless (null? path)
                          (unless (directory-exists? (apply build-path tmp-dir path))
                            (pkg-error
                             (~a "specified directory is not in GitHub respository archive\n"
                                 "  path: ~a")
                             (apply build-path path)))
                          (lift-directory-content tmp-dir path))

                        (begin0
                         (stage-package/info tmp-dir
                                             'dir
                                             pkg-name
                                             #:at-dir given-at-dir
                                             #:given-checksum checksum
                                             #:cached-url new-url
                                             #:use-cache? use-cache?
                                             check-sums?
                                             download-printf
                                             metadata-ns
                                             #:strip strip-mode
                                             #:force-strip? force-strip?
                                             #:in-place? #t
                                             #:in-place-clean? #t)
                         (set! staged? #t)))
                     (λ ()
                       (when (and use-cache? (not staged?))
                         (clean-cache new-url checksum))
                       (unless staged?
                         (delete-directory/files tmp-dir)))))
              (λ ()
                 (delete-directory/files tmp.tgz)))]
         [_
          (define url-last-component
            (path/param-path (last (url-path pkg-url))))
          (define url-looks-like-directory? (eq? type 'dir-url))
          (define-values
            (package-path download-type download-package!)
            (cond
             [url-looks-like-directory?
              (define package-path
                (make-temporary-file
                 (string-append
                  "~a-"
                  pkg-name)
                 'directory))
              (define (path-like f)
                (check-unpack-path 'MANIFEST f)
                (build-path package-path f))
              (define (url-like f)
                (if (and (pair? (url-path pkg-url))
                         (equal? "" (path/param-path (last (url-path pkg-url)))))
                    ;; normal relative path:
                    (combine-url/relative pkg-url f)
                    ;; we're assuming that the last path element is
                    ;; a directory, so just add f:
                    (struct-copy url pkg-url [path
                                              (append
                                               (url-path pkg-url)
                                               (list (path/param f null)))])))
              (values package-path
                      'dir
                      (λ ()
                         (download-printf "Cloning remote directory ~a\n"
                                          (url->string pkg-url))
                         (make-directory* package-path)
                         (define manifest
                           (call/input-url+200
                            #:who 'download-manifest
                            (url-like "MANIFEST")
                            port->lines))
                         (unless manifest
                           (define suggest-git (suggested-git-path pkg-url given-type))
                           (pkg-error (~a "could not find MANIFEST for package source\n"
                                          "  source: ~a~a")
                                      pkg
                                      (if suggest-git
                                          (~a "\n"
                                              "  possible solution:\n"
                                              "   If the URL is intended to refer to a Git repository, use\n"
                                              "     " suggest-git "\n"
                                              "   so that the URL ends in \".git\"")
                                          "")))
                         (for ([f (in-list manifest)])
                           (download-file! (url-like f)
                                           (path-like f)
                                           #f
                                           #:use-cache? use-cache?))))]
             [else
              (define package-path
                (make-temporary-file
                 (string-append
                  "~a-"
                  url-last-component)
                 #f))
              (delete-file package-path)
              (values package-path
                      'file
                      (λ ()
                        (log-pkg-debug "\tAssuming URL names a file")
                        (download-file! pkg-url package-path checksum
                                        #:use-cache? use-cache?
                                        #:download-printf download-printf)))]))
          (define staged? #f)
          (dynamic-wind
              void
              (λ ()
                 (download-package!)
                 (log-pkg-debug "\tDownloading done, installing ~a as ~a"
                                package-path pkg-name)
                 (begin0
                  (stage-package/info package-path
                                      download-type
                                      pkg-name
                                      #:at-dir given-at-dir
                                      #:given-checksum checksum
                                      #:cached-url pkg-url
                                      #:use-cache? use-cache?
                                      check-sums?
                                      download-printf
                                      metadata-ns
                                      #:strip strip-mode
                                      #:force-strip? force-strip?)
                  (set! staged? #t)))
              (λ ()
                 (when (or (file-exists? package-path)
                           (directory-exists? package-path))
                   (when (and use-cache? (not staged?))
                     (clean-cache pkg-url checksum))
                   (delete-directory/files package-path))))]))
    (define info (update-install-info-orig-pkg downloaded-info
                                               orig-pkg))
    (when (and check-sums?
               (install-info-checksum info)
               (not checksum))
      (pkg-error (~a "remote package had no checksum\n"
                     "  package: ~a")
                 pkg))
    (when check-sums?
      (check-checksum checksum (install-info-checksum info)
                      "mismatched"
                      pkg
                      (and use-cache? cached-url)))
    (update-install-info-checksum
     info
     checksum)]
   [(eq? type 'file)
    (define pkg-path (if (path? pkg)
                         pkg
                         (package-source->path pkg type)))
    (unless (file-exists? pkg-path)
      (pkg-error "no such file\n  path: ~a" pkg-path))
    (define checksum-pth (format "~a.CHECKSUM" pkg-path))
    (define expected-checksum
      (and (file-exists? checksum-pth)
           check-sums?
           (file->string checksum-pth)))
    (check-checksum given-checksum expected-checksum "unexpected" pkg-path #f)
    (define actual-checksum
      (with-input-from-file pkg-path
        (λ ()
          (sha1 (current-input-port)))))
    (if expected-checksum
        (check-checksum expected-checksum actual-checksum "mismatched" pkg-path
                        (and use-cache? cached-url))
        (when check-sums?
          (check-checksum given-checksum actual-checksum "unexpected" pkg-path #f)))
    (define checksum
      actual-checksum)
    (define pkg-format (filename-extension pkg-path))
    (define pkg-dir
      (make-temporary-file (string-append "~a-" pkg-name)
                           'directory))
    (define staged? #f)
    (dynamic-wind
        void
        (λ ()
           (make-directory* pkg-dir)

           (match pkg-format
             [#"tgz"
              (untar pkg-path pkg-dir)
              (remove-extra-directory-layer pkg-dir)]
             [#"tar"
              (untar pkg-path pkg-dir)
              (remove-extra-directory-layer pkg-dir)]
             [#"gz" ; assuming .tar.gz
              (untar pkg-path pkg-dir)
              (remove-extra-directory-layer pkg-dir)]
             [#"zip"
              (unzip pkg-path (make-filesystem-entry-reader #:dest pkg-dir)
                     #:preserve-attributes? #t
                     #:utc-timestamps? #t)
              (remove-extra-directory-layer pkg-dir)]
             [#"plt"
              (make-directory* pkg-dir)
              (unpack pkg-path pkg-dir
                      (lambda (x) (log-pkg-debug "~a" x))
                      (lambda () pkg-dir)
                      #f
                      (lambda (auto-dir main-dir file) pkg-dir))
              (define info-path (build-path pkg-dir "info.rkt"))
              (unless (file-exists? info-path)
                ;; Add in "info.rkt" file to make it multi-collection,
                ;; since a ".plt" archive is never single-collection. This
                ;; is needed for supporting old ".plt" archives as packages.
                (call-with-output-file info-path
                  (lambda (o)
                    (fprintf o "#lang setup/infotab\n")
                    (write '(define collection 'multi) o)
                    (newline o))))]
             [x
              (pkg-error "invalid package format\n  given: ~a" x)])

           (begin0
            (update-install-info-checksum
             (update-install-info-orig-pkg
              (stage-package/info pkg-dir
                                  'dir
                                  pkg-name
                                  #:at-dir given-at-dir
                                  #:given-checksum checksum
                                  #:cached-url cached-url
                                  #:use-cache? use-cache?
                                  check-sums?
                                  download-printf
                                  metadata-ns
                                  #:strip strip-mode
                                  #:force-strip? force-strip?
                                  #:in-place? (not strip-mode)
                                  #:in-place-clean? #t)
              `(file ,(simple-form-path* pkg-path)))
             checksum)
            (unless strip-mode
              (set! staged? #t))))
        (λ ()
           (unless staged?
             (delete-directory/files pkg-dir))))]
   [(or (eq? type 'dir)
        (eq? type 'link)
        (eq? type 'static-link))
    (define pkg-path (if (path? pkg)
                         pkg
                         (package-source->path pkg type)))
    (unless (directory-exists? pkg-path)
      (pkg-error "no such directory\n  path: ~a" pkg-path))
    (cond
     [(or (eq? type 'link)
          (eq? type 'static-link))
      (install-info pkg-name
                    (desc->orig-pkg type pkg-path #f)
                    pkg-path
                    #f ; no git-dir
                    #f ; no clean?
                    given-checksum ; if a checksum is provided, just use it
                    (directory->module-paths pkg-path pkg-name metadata-ns)
                    (directory->additional-installs pkg-path pkg-name metadata-ns))]
     [else
      (define pkg-dir
        (if in-place?
            (if strip-mode
                (cond
                  [in-place-clean?
                   (unless force-strip?
                     (check-strip-compatible strip-mode pkg-name pkg-path pkg-error))
                   (generate-stripped-directory strip-mode pkg-path pkg-path)
                   pkg-path]
                  [else
                   (pkg-error "cannot strip directory in place\n  path: ~a" pkg-path)])
                pkg-path)
            (let ([pkg-dir (make-temporary-file "pkg~a" 'directory)])
              (if strip-mode
                  (begin
                    (unless force-strip?
                      (check-strip-compatible strip-mode pkg-name pkg-path pkg-error))
                    (make-directory* pkg-dir)
                    (generate-stripped-directory strip-mode pkg-path pkg-dir))
                  (begin
                    (make-parent-directory* pkg-dir)
                    (delete-directory pkg-dir)
                    (copy-directory/files pkg-path pkg-dir #:keep-modify-seconds? #t)))
              pkg-dir)))
      (when (or (not in-place?)
                in-place-clean?)
        (drop-redundant-files pkg-dir))
      (install-info pkg-name
                    `(dir ,(simple-form-path* pkg-path))
                    pkg-dir
                    #f ; no git-dir
                    (or (not in-place?) in-place-clean?)
                    given-checksum ; if a checksum is provided, just use it
                    (directory->module-paths pkg-dir pkg-name metadata-ns)
                    (directory->additional-installs pkg-dir pkg-name metadata-ns))])]
   [(eq? type 'name)
    (define catalog-info (package-catalog-lookup pkg #f catalog-lookup-cache
                                                 download-printf))
    (log-pkg-debug "catalog response: ~s" catalog-info)
    (define source (hash-ref catalog-info 'source))
    (define checksum (hash-ref catalog-info 'checksum))
    (define info (stage-package/info source
                                     #f
                                     pkg-name
                                     #:at-dir given-at-dir
                                     #:given-checksum checksum
                                     #:use-cache? use-cache?
                                     check-sums?
                                     download-printf
                                     metadata-ns
                                     #:catalog-lookup-cache catalog-lookup-cache
                                     #:remote-checksum-cache remote-checksum-cache
                                     #:strip strip-mode
                                     #:force-strip? force-strip?))
    (when check-sums?
      (check-checksum given-checksum checksum "unexpected" pkg #f)
      (check-checksum checksum (install-info-checksum info) "incorrect" pkg #f))
    (define-values (new-name new-type) (package-source->name+type source #f))
    (define (git-type? new-type)
      (or (eq? new-type 'git)
          (eq? new-type 'git-url)
          (eq? new-type 'github)))
    (define repo-url (or (and (git-type? new-type)
                              source)
                         (let* ([i (get-pkg-info (install-info-directory info)
                                                 metadata-ns)]
                                [source (and i
                                             (i 'package-original-source (lambda () #f)))])
                           (and source
                                (let-values ([(name type) (package-source->name+type source #f)])
                                  (and (git-type? type)
                                       source))))))
    (case new-type
      [(link static-link clone)
       ;; The `source` must have been something like a `file://`
       ;; URL that embeds a special installation type. In that case,
       ;; we don't try to keep track of the catalog reference.
       info]
      [else
       (update-install-info-orig-pkg
        (update-install-info-checksum
         info
         checksum)
        (desc->orig-pkg 'name pkg #f #:repo-url repo-url))])]
   [else
    (pkg-error "cannot infer package source type\n  source: ~a" pkg)]))

(define (pkg-stage desc
                   #:namespace [metadata-ns (make-metadata-namespace)] 
                   #:in-place? [in-place? #f]
                   #:strip [strip-mode #f]
                   #:force-strip? [force-strip? #f]
                   #:use-cache? [use-cache? #f]
                   #:quiet? [quiet? #t])
  (define i (stage-package/info (pkg-desc-source desc)
                                (pkg-desc-type desc)
                                (pkg-desc-name desc)
                                #:at-dir (pkg-desc-extra-path desc)
                                #:given-checksum (pkg-desc-checksum desc)
                                #:use-cache? use-cache?
                                #t
                                (if quiet? void printf/flush)
                                metadata-ns
                                #:in-place? in-place?
                                #:strip strip-mode
                                #:force-strip? force-strip?))
  (values (install-info-name i)
          (install-info-directory i)
          (install-info-checksum i)
          (install-info-clean? i)
          (install-info-module-paths i)))

;; ----------------------------------------

(define (package-url->checksum pkg-url-str
                               [query empty]
                               #:type [given-type #f]
                               #:download-printf [download-printf void]
                               #:pkg-name [pkg-name "package"]
                               #:cache [cache #f])
  (define pkg-url
    (string->url pkg-url-str))
  (define type (if (eq? given-type 'clone)
                   (if (github-url? (string->url pkg-url-str))
                       'github
                       'git)
                   (or given-type
                       (let-values ([(name type) (package-source->name+type pkg-url-str given-type)])
                         type))))
  (case (communication-type type)
    [(git)
     (define-values (transport host port repo branch path)
       (split-git-or-hub-url pkg-url #:type type))
     (download-printf "Querying Git references for ~a at ~a\n" pkg-name pkg-url-str)
     (call-with-git-checkout-credentials
      (lambda ()
        (call-with-network-retries
         (lambda ()
           (define key (vector host port repo branch))
           (cond
             [(and cache
                   (hash-ref cache key #f))
              => (lambda (checksum)
                   checksum)]
             [else
              ;; Supplying `#:dest-dir #f` means that we just resolve `branch`
              ;; to an ID:
              (define checksum
                (call-in-pkg-timeout-sandbox
                 (lambda ()
                   (git-checkout host #:port port repo
                                 #:dest-dir #f
                                 #:ref branch
                                 #:status-printf
                                 (lambda (fmt . args)
                                   (define (strip-ending-newline s)
                                     (regexp-replace #rx"\n$" s ""))
                                   (log-pkg-debug
                                    (strip-ending-newline (apply format fmt args))))
                                 #:initial-error
                                 (lambda ()
                                   (raise
                                    ;; This is a git error so that
                                    ;; call-with-git-checkout-credentials will retry
                                    (exn:fail:git
                                     (~a "pkg: Git checkout initial protocol failed;\n"
                                         " the given URL might not refer to a Git repository\n"
                                         "  given URL: "
                                         pkg-url-str)
                                     (current-continuation-marks))))
                                 #:transport transport))))
              (when cache
                (hash-set! cache key checksum))
              checksum])))))]
    [(github)
     (match-define (list* user repo url-branch path)
       (split-github-url pkg-url))
     (define (query-json path kind)
       (define api-u
         (url "https" #f "api.github.com" #f #t
              (map (λ (x) (path/param x empty))
                   path)
              (append query
                      (if (and (github-client_id)
                               (github-client_secret))
                          (list (cons 'client_id (github-client_id))
                                (cons 'client_secret (github-client_secret)))
                          empty))
              #f))
       (download-printf "Querying GitHub ~a for ~a\n" kind pkg-name)
       (log-pkg-debug "Querying GitHub at ~a" (url->string api-u))
       (define api-bs
         (call/input-url+200
          api-u port->bytes
          #:who 'query-github
          #:headers (list (format "User-Agent: raco-pkg/~a" (version)))))
       (unless api-bs
         (error 'package-url->checksum
                "could not connect to GitHub\n URL: ~a"
                (url->string 
                 (struct-copy url api-u
                              [query query]))))
       (read-json (open-input-bytes api-bs)))
     (define branch (cond
                      [(eq? url-branch 'head)
                       (define info (query-json (list "repos" user repo) 'head))
                       (hash-ref info 'default_branch)]
         [else url-branch]))
     (or
      (for/or ([kind '("branches" "tags")])
        (define branches (query-json (list "repos" user repo kind) kind))
        (unless (and (list? branches)
                     (andmap hash? branches)
                     (andmap (λ (b) (hash-has-key? b 'name)) branches)
                     (andmap (λ (b) (hash-has-key? b 'commit)) branches))
          (error 'package-url->checksum
                 "Invalid response from Github: ~e"
                 branches))
        (for/or ([b (in-list branches)])
          (and (equal? (hash-ref b 'name) branch)
               (hash-ref (hash-ref b 'commit) 'sha))))
      ;; no matching branch/tag found, so if `branch' matches the
      ;; syntax of a commit id, then assume that it refers to a commit
      (and (regexp-match? #rx"[a-f0-9]+" branch)
           branch))]
    [else
     (define u (string-append pkg-url-str ".CHECKSUM"))
     (define key u)
     (cond
       [(and cache
             (hash-ref cache key #f))
        => (lambda (checksum)
             checksum)]
       [else
        (download-printf "Downloading checksum for ~a\n" pkg-name)
        (log-pkg-debug "Downloading checksum as ~a" u)
        (define downloaded-checksum
          (call/input-url+200 (string->url u)
                              port->string
                              #:who 'download-checksum))
        (define checksum
          (or downloaded-checksum
              (get-package-checksum-by-download 'download-checksum pkg-url)))
        (when cache (hash-set! cache key checksum))
        checksum])]))

(define (check-checksum given-checksum checksum what pkg-src cached-url)
  (when (and given-checksum
             checksum
             (not (equal? given-checksum checksum)))
    (clean-cache cached-url checksum)
    (pkg-error (~a "~a checksum on package\n"
                   "  package source: ~a\n"
                   "  expected: ~e\n"
                   "  got: ~e")
               what
               pkg-src
               given-checksum
               checksum)))

;; caches checksums based on etags server
(define (get-package-checksum-by-download who pkg-url)
  ;; Get etag associated with a cached checksum, which lets us potentially avoid downloading the package
  (define pkg-url-str (url->string pkg-url))
  (define cache-dir (find-system-path 'cache-dir))
  (define etag-checksum-cache (build-path cache-dir "pkg-etag-checksum.rktd"))
  (define (get-cache-content)
    (define ht (and (file-exists? etag-checksum-cache)
                    (call-with-default-reading-parameterization
                     (lambda ()
                       (with-handlers ([exn:fail:read? (lambda (exn) #f)])
                         (call-with-input-file etag-checksum-cache read))))))
    (or (and (hash? ht) ht)
        #hash()))
  (define cache-content (get-cache-content))
  (define old-etag+checksum (hash-ref cache-content pkg-url-str #f))
  ;; Get checksum and (maybe) etag
  (define etag+checksum
    (call/input-url+200 pkg-url
                        #:get-etag? #t
                        (lambda (in etag) (cons etag (sha1 in)))
                        #:who who
                        #:if-none-match-etag (and old-etag+checksum (car old-etag+checksum))
                        #:if-none-match-handler (lambda () old-etag+checksum)))
  ;; Myabe cache the result
  (unless (or (equal? old-etag+checksum etag+checksum)
              (not etag+checksum)
              (not (car etag+checksum)))
    (make-directory* cache-dir)
    (define cache-content (get-cache-content)) ; refetch to minimize loss from concurrent updates
    (call-with-atomic-output-file
     etag-checksum-cache
     (lambda (op path)
       (write (hash-set cache-content pkg-url-str etag+checksum) op)
       (newline op))))
  ;; Return the checksum
  (and etag+checksum
       (cdr etag+checksum)))

;; ----------------------------------------

;; Disambiguate `str` as needed to ensure that it will be parsed as
;; `type` in the future.
(define (normalize-url type str as-url)
  (case type
    [(git) ; not git-url, which should not be normalized by adding ".git"
     (cond
      [(equal? "git" (url-scheme as-url))
       str]
      [(equal? "github" (url-scheme as-url))
       str]
      [else
       (define p (reverse (url-path as-url)))
       (define skip (if (equal? "" (path/param-path (car p)))
                        cdr
                        values))
       (define e (path/param-path (car (skip p))))
       (cond
        [(not (regexp-match? #rx"[.]git$" e))
         (url->string (struct-copy url as-url
                                   [path
                                    (reverse
                                     (cons (path/param (string-append e ".git")
                                                       (path/param-param (car (skip p))))
                                           (cdr (skip p))))]))]
        [else str])])]
    [else str]))

;; ----------------------------------------

(define (update-install-info-orig-pkg if op)
  (struct-copy install-info if
               [orig-pkg op]))

(define (update-install-info-checksum if op)
  (struct-copy install-info if
               [checksum op]))

(define (update-install-info-git-dir if dir)
  (struct-copy install-info if
               [git-directory dir]))

;; ----------------------------------------

(define github-client_id (make-parameter #f))
(define github-client_secret (make-parameter #f))

(define (lift-git-directory-content tmp-dir path)
  (unless (null? path)
    (unless (directory-exists? (apply build-path tmp-dir path))
      (pkg-error
       (~a "specified directory is not in Git respository\n"
           "  path: ~a")
       (apply build-path path)))
    (lift-directory-content tmp-dir path)))

;; ----------------------------------------

(define (untar pkg pkg-dir #:strip-components [strip-components 0])
  (make-directory* pkg-dir)
  (untgz pkg #:dest pkg-dir #:strip-count strip-components))

;; ----------------------------------------

(define (drop-redundant-files pkg-dir)
  ;; Ad hoc space-saving rule: for an installation-wide package, remove
  ;; any redundant license files.
  (when (and (eq? 'installation (current-pkg-scope))
             (find-share-dir))
    (for ([i (in-list '("COPYING.txt" "COPYING_LESSER.txt" "COPYRIGHT.txt"
                                      "LICENSE-APACHE" "LICENSE-MIT"))])
      (define pkg-file (build-path pkg-dir i))
      (define share-file (build-path (find-share-dir) i))
      (when (and (file-exists? pkg-file)
                 (file-exists? share-file)
                 (equal? (file->bytes pkg-file)
                         (file->bytes share-file)))
        ;; This file would be redundant, so drop it
        (delete-file pkg-file)))))


;; ----------------------------------------

(define (suggested-git-path pkg-url given-type)
  (define p (url-path pkg-url))
  (define drop-n
    (if (and ((length p) . >= . 1)
             (equal? "" (path/param-path (last p))))
        2
        1))
  (and (not given-type)
       ((length p) . >= . drop-n)
       (let ([e (list-ref p (- (length p) drop-n))])
         (url->string
          (struct-copy url pkg-url [path (append
                                          (drop-right p drop-n)
                                          (list
                                           (path/param
                                            (string-append
                                             (path/param-path e)
                                             ".git")
                                            (path/param-param e))))])))))

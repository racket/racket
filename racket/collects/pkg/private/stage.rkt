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
         "git.rkt")

(provide (struct-out install-info)
         remote-package-checksum
         stage-package/info
         pkg-stage
         package-url->checksum
         github-client_secret
         github-client_id)

(struct install-info (name orig-pkg directory git-directory clean? checksum module-paths additional-installs))

(define (remote-package-checksum pkg download-printf pkg-name
                                 #:type [type #f]
                                 #:catalog-lookup-cache [catalog-lookup-cache #f]
                                 #:remote-checksum-cache [remote-checksum-cache #f])
  (cond
   [(and remote-checksum-cache
         (hash-ref remote-checksum-cache pkg #f))
    => (lambda (checksum) checksum)]
   [else
    (define checksum
      (match pkg
        [`(catalog ,pkg-name . ,_)
         (hash-ref (package-catalog-lookup pkg-name #f catalog-lookup-cache
                                           download-printf)
                   'checksum)]
        [`(url ,pkg-url-str)
         (package-url->checksum pkg-url-str
                                #:type type
                                #:download-printf download-printf
                                #:pkg-name pkg-name)]
        [`(clone ,_ ,pkg-url-str)
         (package-url->checksum pkg-url-str
                                #:type 'clone
                                #:download-printf download-printf
                                #:pkg-name pkg-name)]))
    (when remote-checksum-cache
      (hash-set! remote-checksum-cache pkg checksum))
    checksum]))

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
    (define-values (host port repo branch path)
      (split-git-or-hub-url pkg-url))
    (define pkg-no-query (real-git-url pkg-url host port repo))
    (define clone-dir (or given-at-dir
                          (current-directory)))
    
    (define (status s) (download-printf "~a\n" s))

    (define orig-pkg (desc->orig-pkg 'clone pkg given-at-dir))

    (define checksum
      (or given-checksum
          (remote-package-checksum orig-pkg download-printf pkg-name
                                   #:catalog-lookup-cache catalog-lookup-cache
                                   #:remote-checksum-cache remote-checksum-cache)))

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
           (git #:status status "clone" "-b" branch pkg-no-query ".")))

       (unless working-dir
         (parameterize ([current-directory clone-dir])
           (download-printf "Fetching from remote repository ~a\n"
                            pkg-no-query)
           (git #:status status "fetch" pkg-no-query branch)))

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
           (git #:status status "checkout" (or checksum branch)))
         (lift-git-directory-content tmp-dir path)]
        [else
         (download-printf "Using clone directory directly for metadata\n")])

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
        (eq? type 'git))
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
       (match type
         ['git
          (when (equal? checksum "")
            (pkg-error 
             (~a "cannot use empty checksum for Git repostory package source\n"
                 "  source: ~a")
             pkg))
          (define-values (host port repo branch path) (split-git-url pkg-url))
          (define tmp-dir
            (make-temporary-file
             (string-append
              "~a-"
              (regexp-replace* #rx"[:/\\.]" (format "~a.~a" repo branch) "_"))
             'directory))
          
          (define staged? #f)
          (dynamic-wind
           void
           (λ ()
             (download-repo! pkg-url host port repo tmp-dir checksum 
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
                      (list user repo "tarball" checksum))
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
                            (url-like "MANIFEST")
                            port->lines))
                         (unless manifest
                           (pkg-error (~a "could not find MANIFEST for package source\n"
                                          "  source: ~a")
                                      pkg))
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
    (check-checksum expected-checksum actual-checksum "mismatched" pkg-path
                    (and use-cache? cached-url))
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
                     #:preserve-timestamps? #t
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
    (let ([pkg-path (directory-path-no-slash pkg-path)])
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
                  (pkg-error "cannot strip directory in place")
                  pkg-path)
              (let ([pkg-dir (make-temporary-file "pkg~a" 'directory)])
                (delete-directory pkg-dir)
                (if strip-mode
                    (begin
                      (unless force-strip?
                        (check-strip-compatible strip-mode pkg-name pkg pkg-error))
                      (make-directory* pkg-dir)
                      (generate-stripped-directory strip-mode pkg pkg-dir))
                    (begin
                      (make-parent-directory* pkg-dir)
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
                      (directory->additional-installs pkg-dir pkg-name metadata-ns))]))]
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
    (define-values (new-name new-type)  (package-source->name+type source #f))
    (define repo-url (and (or (eq? new-type 'git)
                              (eq? new-type 'github))
                          source))
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
                                (if quiet? void printf)
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

(define (package-url->checksum pkg-url-str [query empty]
                               #:type [given-type #f]
                               #:download-printf [download-printf void]
                               #:pkg-name [pkg-name "package"])
  (define pkg-url
    (string->url pkg-url-str))
  (define type (if (eq? given-type 'clone)
                   (if (equal? "github" (url-scheme (string->url pkg-url-str)))
                       'github
                       'git)
                   (or given-type
                       (let-values ([(name type) (package-source->name+type pkg-url-str given-type)])
                         type))))
  (case type
    [(git)
     (define-values (host port repo branch path)
       (split-git-url pkg-url))
     (download-printf "Querying Git references for ~a at ~a\n" pkg-name pkg-url-str)
     ;; Supplying `#:dest-dir #f` means that we just resolve `branch`
     ;; to an ID:
     (git-checkout host #:port port repo
                   #:dest-dir #f
                   #:ref branch
                   #:status-printf (lambda (fmt . args)
                                     (define (strip-ending-newline s)
                                       (regexp-replace #rx"\n$" s ""))
                                     (log-pkg-debug (strip-ending-newline (apply format fmt args))))
                   #:transport (string->symbol (url-scheme pkg-url)))]
    [(github)
     (match-define (list* user repo branch path)
                   (split-github-url pkg-url))
     (or
      (for/or ([kind '("branches" "tags")])
        (define api-u
          (url "https" #f "api.github.com" #f #t
               (map (λ (x) (path/param x empty))
                    (list "repos" user repo kind))
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
           #:headers (list (format "User-Agent: raco-pkg/~a" (version)))))
        (unless api-bs
          (error 'package-url->checksum
                 "could not connect to GitHub\n URL: ~a"
                 (url->string 
                  (struct-copy url api-u
                               [query query]))))
        (define branches
          (read-json (open-input-bytes api-bs)))
        (unless (and (list? branches)
                     (andmap hash? branches)
                     (andmap (λ (b) (hash-has-key? b 'name)) branches)
                     (andmap (λ (b) (hash-has-key? b 'commit)) branches))
          (error 'package-url->checksum
                 "Invalid response from Github: ~e"
                 api-bs))
        (for/or ([b (in-list branches)])
          (and (equal? (hash-ref b 'name) branch)
               (hash-ref (hash-ref b 'commit) 'sha))))
      ;; no matching branch/tag found, so if `branch' matches the
      ;; syntax of a commit id, then assume that it refers to a commit
      (and (regexp-match? #rx"[a-f0-9]+" branch)
           branch))]
    [else
     (define u (string-append pkg-url-str ".CHECKSUM"))
     (download-printf "Downloading checksum for ~a\n" pkg-name)
     (log-pkg-debug "Downloading checksum as ~a" u)
     (call/input-url+200 (string->url u)
                         port->string)]))

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

;; ----------------------------------------

;; Disambiguate `str` as needed to ensure that it will be parsed as
;; `type` in the future.
(define (normalize-url type str as-url)
  (case type
    [(git)
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
  ;; any redundant "COPYING.txt" or "COPYING_LESSER.txt" files.
  (when (and (eq? 'installation (current-pkg-scope))
             (find-share-dir))
    (for ([i (in-list '("COPYING.txt" "COPYING_LESSER.txt"))])
      (define pkg-file (build-path pkg-dir i))
      (define share-file (build-path (find-share-dir) i))
      (when (and (file-exists? pkg-file)
                 (file-exists? share-file)
                 (equal? (file->bytes pkg-file)
                         (file->bytes share-file)))
        ;; This file would be redundant, so drop it
        (delete-file pkg-file)))))

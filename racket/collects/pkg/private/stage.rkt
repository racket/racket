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
         "addl-installs.rkt")

(provide (struct-out install-info)
         remote-package-checksum
         stage-package/info
         pkg-stage)

(struct install-info (name orig-pkg directory clean? checksum module-paths additional-installs))

(define (remote-package-checksum pkg download-printf pkg-name)
  (match pkg
    [`(catalog ,pkg-name)
     (hash-ref (package-catalog-lookup pkg-name #f download-printf) 'checksum)]
    [`(url ,pkg-url-str)
     (package-url->checksum pkg-url-str 
                            #:download-printf download-printf
                            #:pkg-name pkg-name)]))

;; Downloads a package (if needed) and unpacks it (if needed) into a  
;; temporary directory.
(define (stage-package/info pkg
                            given-type
                            given-pkg-name
                            #:given-checksum [given-checksum #f]
                            #:cached-url [cached-url #f]
                            #:use-cache? use-cache?
                            check-sums?
                            download-printf
                            metadata-ns
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
                                   #:complain complain-about-source)
        (package-source->name+type pkg given-type
                                   #:link-dirs? link-dirs?
                                   #:must-infer-name? (not given-pkg-name)
                                   #:complain complain-about-source)))
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
                        #:given-checksum given-checksum
                        #:use-cache? use-cache?
                        check-sums? download-printf
                        metadata-ns
                        #:strip strip-mode
                        #:force-strip? force-strip?)]
   [(or (eq? type 'file-url) (eq? type 'dir-url) (eq? type 'github))
    (define pkg-url (string->url pkg))
    (define scheme (url-scheme pkg-url))

    (define orig-pkg `(url ,pkg))
    (define found-checksum
      ;; If a checksum is given, use that. In the case of a non-github
      ;; source, we could try to get the checksum from the source, and
      ;; then check whether it matches the expected one, but we choose
      ;; to avoid an extra trip to the server.
      (or given-checksum
          (remote-package-checksum orig-pkg download-printf pkg-name)))
    (when check-sums?
      (check-checksum given-checksum found-checksum "unexpected" pkg #f))
    (define checksum (or found-checksum given-checksum))
    (define downloaded-info
       (match type
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
          (delete-file tmp.tgz)
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
                                 "  path: ~a"
                                 (apply build-path path))))
                          (lift-directory-content tmp-dir path))

                        (begin0
                         (stage-package/info tmp-dir
                                             'dir
                                             pkg-name
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
                      `(,type ,(path->string
                                (find-relative-path (pkg-installed-dir)
                                                    (simple-form-path pkg-path)
                                                    #:more-than-root? #t)))
                      pkg-path
                      #f
                      given-checksum ; if a checksum is provided, just use it
                      (directory->module-paths pkg pkg-name metadata-ns)
                      (directory->additional-installs pkg pkg-name metadata-ns))]
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
                      (or (not in-place?) in-place-clean?) 
                      given-checksum ; if a checksum is provided, just use it
                      (directory->module-paths pkg-dir pkg-name metadata-ns)
                      (directory->additional-installs pkg-dir pkg-name metadata-ns))]))]
   [(eq? type 'name)
    (define catalog-info (package-catalog-lookup pkg #f download-printf))
    (log-pkg-debug "catalog response: ~s" catalog-info)
    (define source (hash-ref catalog-info 'source))
    (define checksum (hash-ref catalog-info 'checksum))
    (define info (stage-package/info source
                                     #f
                                     pkg-name
                                     #:given-checksum checksum
                                     #:use-cache? use-cache?
                                     check-sums?
                                     download-printf
                                     metadata-ns
                                     #:strip strip-mode
                                     #:force-strip? force-strip?))
    (when check-sums?
      (check-checksum given-checksum checksum "unexpected" pkg #f)
      (check-checksum checksum (install-info-checksum info) "incorrect" pkg #f))
    (update-install-info-orig-pkg
     (update-install-info-checksum
      info
      checksum)
     `(catalog ,pkg))]
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
                               #:download-printf [download-printf void]
                               #:pkg-name [pkg-name "package"])
  (define pkg-url
    (string->url pkg-url-str))
  (match (url-scheme pkg-url)
    [(or "github" "git")
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
        (download-printf "Querying GitHub ~a\n" kind)
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
    [_
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

(define (update-install-info-orig-pkg if op)
  (struct-copy install-info if
               [orig-pkg op]))

(define (update-install-info-checksum if op)
  (struct-copy install-info if
               [checksum op]))

;; ----------------------------------------

(define github-client_id (make-parameter #f))
(define github-client_secret (make-parameter #f))

(define (split-github-url pkg-url)
  (if (equal? (url-scheme pkg-url) "github")
      ;; github://
      (map path/param-path (url-path/no-slash pkg-url))
      ;; git://
      (let* ([paths (map path/param-path (url-path/no-slash pkg-url))])
        (list* (car paths)
               (regexp-replace* #rx"[.]git$" (cadr paths) "")
               (or (url-fragment pkg-url) "master")
               (let ([a (assoc 'path (url-query pkg-url))])
                 (or (and a (cdr a) (string-split (cdr a) "/"))
                     null))))))

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

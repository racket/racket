#lang racket/base
(require racket/match
         racket/format
         racket/file
         setup/pack
         file/zip
         file/tar
         openssl/sha1
         "../strip.rkt"
         "metadata.rkt"
         "print.rkt"
         "params.rkt"
         "pkg-db.rkt"
         "lock.rkt")

(provide pkg-create)

(define (create-as-is create:format pkg-name dir orig-dir
                      #:quiet? [quiet? #f]
                      #:from-command-line? [from-command-line? #f]
                      #:hide-src? [hide-src? #f]
                      #:dest [dest-dir #f])
  (begin
    (unless (directory-exists? dir)
      (pkg-error "directory does not exist\n  path: ~a" dir))
    (match create:format
      ['MANIFEST
       (unless quiet?
         (printf/flush "creating manifest for ~a\n"
                       orig-dir))
       (with-output-to-file (build-path (or dest-dir dir) "MANIFEST")
         #:exists 'replace
         (λ ()
           (for ([f (in-list (parameterize ([current-directory dir])
                               (find-files file-exists?)))])
             (display f)
             (newline))))]
      [else
       (define pkg (format "~a.~a" pkg-name create:format))
       (define actual-dest-dir (if dest-dir
                                   (path->complete-path dest-dir)
                                   (let-values ([(base name dir?) (split-path dir)])
                                     (cond
                                      [(path? base) (path->complete-path base)]
                                      [else (current-directory)]))))
       (define pkg/complete (path->complete-path pkg actual-dest-dir))
       ;; To make checksums more consistent, set a directory's timestamp to
       ;; the latest time of any of its source files.
       (define (use-real-timestamp? p)
         (and (file-exists? p)
              (regexp-match? #rx"[.](?:rkt|ss|scrbl|txt)$" p)))
       (define latest-timestamp
         (for/fold ([ts #f]) ([f (in-directory dir)])
           (define fts (and (use-real-timestamp? f)
                            (file-or-directory-modify-seconds f)))
           (if (and fts (or (not ts) (fts . > . ts)))
               fts
               ts)))
       (define (file-or-directory-timestamp p)
         (or (and (not (use-real-timestamp? p))
                  latest-timestamp)
             (file-or-directory-modify-seconds p)))
       (unless quiet?
         (printf/flush "packing~a into ~a\n"
                       (if hide-src? "" (format " ~a" dir))
                       (if dest-dir
                           pkg/complete
                           pkg)))
       (define (add-directory-layer? content)
         ;; We need to add a layer for zip/tgz if the package content
         ;; is a single directory, which is an unlikely case.
         ;; That mode is not compatble with Racket v60.0.1.12 and earlier.
         ;; When only Racket v6.0.1.12 is later is relevant,
         ;; we might prefer to always add a layer for consistency and
         ;; because it's nicer for manual unpacking.
         (and (= 1 (length content))
              (directory-exists? (car content))))
       (match create:format
         ['tgz
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (define content (directory-list))
              (apply tar-gzip pkg/complete content
                     #:path-prefix (and (add-directory-layer? content)
                                        pkg-name)
                     #:get-timestamp file-or-directory-timestamp)))]
         ['zip
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (define content (directory-list))
              (apply zip pkg/complete content
                     #:path-prefix (and (add-directory-layer? content)
                                        pkg-name)
                     #:get-timestamp file-or-directory-timestamp
                     #:utc-timestamps? #t
                     #:round-timestamps-down? #t)))]
         ['plt
          (define dest pkg/complete)
          (when (pkg-single-collection #:name pkg-name dir)
            (pkg-error (~a "single-collection package not supported in .plt format\n"
                           "  directory: ~a")
                       dir))
          (parameterize ([current-directory dir])
            (define names (filter std-filter (directory-list)))
            (define dirs (filter directory-exists? names))
            (pack-plt dest pkg-name 
                      names
                      #:plt-relative? #t
                      #:as-paths (map (lambda (v) (build-path "collects" v)) names)
                      #:collections (map list (map path->string dirs))))]
         [x
          (pkg-error "invalid package format\n  format: ~a" x)])
       (define chk (format "~a.CHECKSUM" pkg))
       (define chk/complete (path->complete-path chk actual-dest-dir))
       (unless quiet?
         (printf/flush "writing package checksum to ~a\n"
                       (if dest-dir
                           chk/complete
                           chk)))
       (with-output-to-file chk/complete
         #:exists 'replace
         (λ () (display (call-with-input-file pkg/complete sha1))))])))

(define (stripped-create mode name dir
                        #:format [create:format 'zip]
                        #:quiet? [quiet? #f]
                        #:from-command-line? [from-command-line? #f]
                        #:dest [archive-dest-dir #f])
  (define tmp-dir (make-temporary-file "create-binary-~a" 'directory))
  (dynamic-wind
      void
      (lambda ()
        (define dest-dir (build-path tmp-dir name))
        (make-directory dest-dir)
        (generate-stripped-directory mode dir dest-dir)
        (create-as-is create:format name dest-dir dir 
                      #:hide-src? #t 
                      #:quiet? quiet?
                      #:from-command-line? from-command-line?
                      #:dest (if archive-dest-dir
                                 (path->complete-path archive-dest-dir)
                                 (current-directory))))
      (lambda ()
        (delete-directory/files tmp-dir))))

(define (pkg-create create:format dir-or-name
                    #:pkg-name [given-pkg-name #f]
                    #:dest [dest-dir #f]
                    #:source [source 'dir]
                    #:mode [mode 'as-is]
                    #:quiet? [quiet? #f]
                    #:from-command-line? [from-command-line? #f])
  (define pkg-name
    (or given-pkg-name
        (if (eq? source 'dir)
            (path->string (let-values ([(base name dir?) (split-path dir-or-name)])
                            name))
            dir-or-name)))
  (define dir
    (if (eq? source 'dir)
        dir-or-name
        (let ()
          (define (get-dir scope)
            (parameterize ([current-pkg-scope scope])
              (with-pkg-lock/read-only
               (pkg-directory* dir-or-name))))
          (define dir (get-dir 'user))
          (unless dir
            (pkg-error (~a "package not installed in user scope\n"
                           "  package name: ~a"
                           (if (get-dir 'installation)
                               "\n  installed in scope: installation"
                               ""))
                       dir-or-name))
          dir)))
  (case mode
    [(as-is)
     (create-as-is create:format pkg-name dir dir
                   #:dest dest-dir
                   #:quiet? quiet?
                   #:from-command-line? from-command-line?)]
    [else (stripped-create mode pkg-name dir
                           #:dest dest-dir
                           #:format create:format
                           #:quiet? quiet?
                           #:from-command-line? from-command-line?)]))

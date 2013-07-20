#lang racket/base
(require racket/cmdline
         racket/file
         net/url
         "download-page.rkt"
         (only-in "config.rkt" extract-options))

(define build-dir (build-path "build"))
(define installers-dir (build-path "installers"))

(define-values (config-file config-mode)
  (command-line
   #:args
   (config-file config-mode)
   (values config-file config-mode)))

(define config (extract-options config-file config-mode))

(define site-dir (hash-ref config
                           '#:site-dest
                           (build-path build-dir "site")))

(define current-snapshot
  (let-values ([(base name dir?) (split-path site-dir)])
    (path-element->string name)))

(define snapshots-dir (build-path site-dir 'up))

(define link-file (build-path snapshots-dir "current"))

(when (link-exists? link-file)
  (printf "Removing old \"current\" link\n")
  (flush-output)
  (delete-file link-file))

(define (get-snapshots)
  (for/list ([p (in-list (directory-list snapshots-dir))]
             #:when (directory-exists? (build-path snapshots-dir p)))
    (path-element->string p)))

(define n (hash-ref config '#:max-snapshots 5))

(let ([snapshots (get-snapshots)])
  (when (n . < . (length snapshots))
    (define remove-snapshots (remove
                              current-snapshot
                              (list-tail (sort snapshots string>?) n)))
    (for ([s (in-list remove-snapshots)])
      (printf "Removing snapshot ~a\n" s)
      (flush-output)
      (delete-directory/files (build-path snapshots-dir s)))))

(printf "Creating \"current\" link\n")
(flush-output)
(make-file-or-directory-link current-snapshot link-file)

(make-download-page (build-path site-dir
                                installers-dir
                                "table.rktd")
                    #:installers-url "current/installers/"
                    #:docs-url (and (directory-exists? (build-path site-dir "doc"))
                                    "current/doc/index.html")
                    #:pdf-docs-url (and (directory-exists? (build-path site-dir "pdf-doc"))
                                        "current/pdf-doc/")
                    #:dest (build-path snapshots-dir
                                       "index.html")
                    #:git-clone (current-directory)
                    #:post-content `((p "Snapshot ID: " 
                                        (a ((href ,(string-append current-snapshot
                                                                  "/index.html")))
                                           ,current-snapshot))
                                     ,@(let ([snapshots (get-snapshots)])
                                         (if ((length snapshots) . < . 2)
                                             null
                                             `((div ([class "detail"]) 
                                                  "Other available snapshots:"
                                                  ,@(for/list ([s (remove "current"
                                                                          (remove current-snapshot
                                                                                  (sort snapshots string>?)))])
                                                      `(span ([class "detail"])
                                                             nbsp
                                                             (a ([href ,(string-append s "/index.html")])
                                                                ,s)))))))))

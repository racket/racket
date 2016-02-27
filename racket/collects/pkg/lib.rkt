#lang racket/base
(require racket/contract/base
         net/url
         "path.rkt"
         "private/desc.rkt"
         "private/dirs.rkt"
         "private/params.rkt"
         "private/lock.rkt"
         "private/pkg-db.rkt"
         "private/metadata.rkt"
         "private/mod-paths.rkt"
         "private/addl-installs.rkt"
         "private/catalog.rkt"
         "private/remove.rkt"
         "private/install.rkt"
         "private/new.rkt"
         "private/stage.rkt"
         "private/show.rkt"
         "private/config.rkt"
         "private/create.rkt"
         "private/migrate.rkt"
         "private/catalog-copy.rkt"
         "private/catalog-show.rkt"
         "private/content.rkt"
         "private/catalog-update.rkt"
         "private/catalog-archive.rkt"
         "private/suggestions.rkt"
         "private/archive.rkt"
         "private/trash.rkt")
  
(define dep-behavior/c
  (or/c #f 'fail 'force 'search-ask 'search-auto))

(define package-scope/c
  (or/c 'installation 'user
        (and/c path? complete-path?)))

(define pkg-desc/opt
  (let ([pkg-desc (lambda (source type name checksum auto?
                             #:path [path #f])
                    (pkg-desc source type name checksum auto? path))])
    pkg-desc))

(provide
 (all-from-out "path.rkt")
 with-pkg-lock
 with-pkg-lock/read-only
 pkg-desc?
 (contract-out
  [current-pkg-scope
   (parameter/c package-scope/c)]
  [current-pkg-scope-version
   (parameter/c string?)]
  [current-pkg-lookup-version
   (parameter/c string?)]
  [current-pkg-error 
   (parameter/c procedure?)]
  [current-pkg-catalogs
   (parameter/c (or/c #f (listof url?)))]
  [current-pkg-download-cache-dir
   (parameter/c (or/c #f (and path-string? complete-path?)))]
  [current-pkg-download-cache-max-files
   (parameter/c (or/c #f real?))]
  [current-pkg-download-cache-max-bytes
   (parameter/c (or/c #f real?))]
  [current-pkg-trash-max-packages
   (parameter/c (or/c #f real?))]
  [current-pkg-trash-max-seconds
   (parameter/c (or/c #f real?))]
  [current-pkg-network-retries
   (parameter/c (or/c #f real?))]
  [pkg-directory
   (->* (string?)
        (#:cache (or/c #f (and/c hash? (not/c immutable?))))
        (or/c path-string? #f))]
  [rename
   pkg-desc/opt pkg-desc
   (->* (string?
         (or/c #f 'file 'dir 'link 'static-link 'file-url 'dir-url 'git 'github 'clone 'name)
         (or/c string? #f)
         (or/c string? #f)
         boolean?)
        (#:path (or/c #f path-string?))
        pkg-desc?)]
  [pkg-config
   (->* (boolean? (listof string?))
        (#:from-command-line? boolean?)
        void?)]
  [pkg-new
   (-> path-string? void?)]
  [pkg-create
   (->* ((or/c 'zip 'tgz 'plt 'MANIFEST)
         path-string?)
        (#:source (or/c 'dir 'name)
                  #:pkg-name (or/c #f string?)
                  #:mode (or/c 'as-is 'source 'binary 'binary-lib 'built)
                  #:quiet? boolean?
                  #:from-command-line? boolean?
                  #:dest (or/c (and/c path-string? complete-path?) #f))
        void?)]
  [pkg-update
   (->* ((listof (or/c string? pkg-desc?)))
        (#:dep-behavior dep-behavior/c
                        #:all? boolean?
                        #:update-deps? boolean?
                        #:update-implies? boolean?
                        #:quiet? boolean?
                        #:use-trash? boolean?
                        #:from-command-line? boolean?
                        #:all-platforms? boolean?
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:strict-doc-conflicts? boolean?
                        #:skip-uninstalled? boolean?
                        #:use-cache? boolean?
                        #:strip (or/c #f 'source 'binary 'binary-lib)
                        #:force-strip? boolean?
                        #:link-dirs? boolean?
                        #:infer-clone-from-dir? boolean?
                        #:lookup-for-clone? boolean?
                        #:multi-clone-behavior (or/c 'fail 'force 'convert 'ask)
                        #:pull-behavior (or/c 'ff-only 'rebase 'try))
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-remove
   (->* ((listof string?))
        (#:auto? boolean?
                 #:force? boolean?
                 #:quiet? boolean?
                 #:use-trash? boolean?
                 #:from-command-line? boolean?
                 #:demote? boolean?)
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-show
   (->* (string? (or/c #f (listof string?)))
        (#:directory? boolean?
                      #:long? boolean?
                      #:auto? boolean?
                      #:full-checksum? boolean?
                      #:rx? boolean?)
        void?)]
  [pkg-install
   (->* ((listof pkg-desc?))
        (#:dep-behavior dep-behavior/c
                        #:update-deps? boolean?
                        #:update-implies? boolean?
                        #:all-platforms? boolean?
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:strict-doc-conflicts? boolean?
                        #:use-cache? boolean?
                        #:skip-installed? boolean?
                        #:quiet? boolean?
                        #:use-trash? boolean?
                        #:from-command-line? boolean?
                        #:strip (or/c #f 'source 'binary 'binary-lib)
                        #:force-strip? boolean?
                        #:link-dirs? boolean?
                        #:multi-clone-behavior (or/c 'fail 'force 'convert 'ask)
                        #:pull-behavior (or/c 'ff-only 'rebase 'try))
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-migrate
   (->* (string?)
        (#:dep-behavior dep-behavior/c
                        #:all-platforms? boolean?
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:strict-doc-conflicts? boolean?
                        #:use-cache? boolean?
                        #:quiet? boolean?
                        #:from-command-line? boolean?
                        #:strip (or/c #f 'source 'binary 'binary-lib)
                        #:force-strip? boolean?)
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-catalog-show
   (->* ((listof string?))
        (#:all? boolean?
                #:only-names? boolean?
                #:modules? boolean?)
        void?)]
  [pkg-catalog-copy
   (->* ((listof path-string?) path-string?)
        (#:from-config? any/c
                        #:merge? boolean?
                        #:force? boolean?
                        #:override? boolean?
                        #:relative-sources? boolean?)
        void?)]
  [pkg-catalog-archive
   (->* (path-string? (listof string?))
        (#:from-config? boolean?
                        #:state-catalog (or/c path-string? #f)
                        #:relative-sources? boolean?
                        #:quiet? boolean?
                        #:package-exn-handler (string? exn:fail? . -> . any))
        void?)]
  [pkg-archive-pkgs
   (->* (path-string? (listof string?))
        (#:include-deps? boolean?
                         #:exclude (listof string?)
                         #:relative-sources? boolean?
                         #:quiet? boolean?
                         #:package-exn-handler (string? exn:fail? . -> . any))
        void?)]
  [pkg-empty-trash
   (->* ()
        (#:list? boolean?
                 #:quiet? boolean?)
        void)]
  [default-pkg-scope
   (-> package-scope/c)]
  [installed-pkg-names
   (->* ()
        (#:scope (or/c #f package-scope/c))
        (listof string?))]
  [installed-pkg-table
   (->* ()
        (#:scope (or/c #f package-scope/c))
        (hash/c string? pkg-info?))]
  [pkg-stage (->* (pkg-desc?)
                  (#:namespace namespace?
                               #:in-place? boolean?
                               #:strip (or/c #f 'source 'binary 'binary-lib)
                               #:force-strip? boolean?
                               #:use-cache? boolean?
                               #:quiet? boolean?)
                  (values string?
                          path?
                          (or/c #f string?)
                          boolean?
                          (listof module-path?)))]
  [pkg-config-catalogs
   (-> (listof string?))]
  [pkg-catalog-update-local
   (->* ()
        (#:catalogs (listof string?)
         #:set-catalogs? boolean?
         #:catalog-file path-string?
         #:quiet? boolean?
         #:consult-packages? boolean?)
        void?)]
  [pkg-catalog-suggestions-for-module
   (->* (module-path?)
        (#:catalog-file path-string?)
        (listof string?))]
  [get-all-pkg-scopes
   (-> (listof package-scope/c))]
  [get-all-pkg-names-from-catalogs
   (-> (listof string?))]
  [get-all-pkg-details-from-catalogs
   (-> (hash/c string? (hash/c symbol? any/c)))]
  [get-pkg-details-from-catalogs
   (-> string?
       (or/c #f (hash/c symbol? any/c)))]
  [get-pkg-content
   (->* (pkg-desc?)
        (#:extract-info (-> (or/c #f
                                  ((symbol?) ((-> any)) . ->* . any))
                            any/c)
                        #:namespace namespace?
                        #:use-cache? boolean?
                        #:quiet? boolean?)
        (values (or/c #f string?)
                (listof module-path?)
                any/c))]
  [extract-pkg-dependencies
   (->* ((symbol? (-> any/c) . -> . any/c))
        (#:build-deps? boolean?
                       #:filter? boolean?
                       #:versions? boolean?)
        (listof (or/c string? (cons/c string? list?))))]
  [pkg-single-collection
   (->* (path-string?)
        (#:name string?
                #:namespace namespace?)
        (or/c #f string?))]
  [find-pkg-installation-scope (->* (string?)
                                    (#:next? boolean?)
                                    (or/c #f package-scope/c))]
  [pkg-directory->module-paths (->* (path-string? string?)
                                    (#:namespace namespace?)
                                    (listof module-path?))]
  [pkg-directory->additional-installs (->* (path-string? string?)
                                           (#:namespace namespace?
                                                        #:system-type (or/c #f symbol?)
                                                        #:system-library-subpath (or/c #f path-for-some-system?))
                                           (listof (cons/c symbol? string?)))]))

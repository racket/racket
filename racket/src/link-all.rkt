#lang racket/base
(require racket/cmdline
         racket/file
         racket/list
         racket/format
         racket/string
         racket/set
         racket/path
         setup/getinfo
         pkg/lib
         pkg/path)

;; Find packages in a directory tree ("info.rkt" indicates a package)
;; and link the packages into an installation. The packages are linked
;; in a "devel-pkgs" scope to isolate them from packages at
;; "installation" scope, so that the set of linked packages can be
;; updated when the directory content changes.

;; Used by the top-level Makefile in the main Racket repository.

(define config-dir-path (build-path "racket" "etc"))
(define config-file-path (build-path config-dir-path "config.rktd"))
(define devel-pkgs-rel-dir (build-path "devel-pkgs"))
(define devel-pkgs-dir (build-path "racket" "share" devel-pkgs-rel-dir))
(define cache-file-path (build-path "racket" "share" "info-cache.rktd"))

(define only-platform? #f)
(define save? #f)
(define restore? #f)

(define dirs null)

(define cmdline-pkgs
  (command-line
   #:once-each
   [("--platform") "Only packages whose names match the platform name"
    (set! only-platform? #t)]
   #:once-any
   [("--save") "Save package choices"
    (set! save? #t)]
   [("--restore") "Use saved package choices, if any"
    (set! restore? #t)]
   #:multi
   [("++dir") dir "Use packages in <dir>"
    (set! dirs (cons dir dirs))]
   #:args
   pkg
   (list->set pkg)))

(define pkgs-choice-path (build-path config-dir-path "link-pkgs.rktd"))

(define-values (pkgs keeping?)
  (if (and restore?
           (file-exists? pkgs-choice-path))
      (values
       (list->set
        (call-with-input-file* pkgs-choice-path read))
       #t)
      (values cmdline-pkgs #f)))

(printf "Linking packages~a:\n"
        (if keeping?
            (format " (using packages choice from ~a)" pkgs-choice-path)
            ""))
(for ([p (in-set pkgs)])
  (printf "  ~a\n" p))
(when save?
  (unless keeping?
    (printf "Recording packages choice in ~a\n" pkgs-choice-path)
    (call-with-output-file*
     pkgs-choice-path
     #:exists 'truncate/replace
     (lambda (o)
       (write (set->list pkgs) o)
       (newline o)))))

(define devel-pkgs-bytes
  (path->bytes (build-path 'up "share" devel-pkgs-rel-dir)))
(define devel-links-bytes
  (path->bytes (build-path 'up "share" devel-pkgs-rel-dir "links.rktd")))

(when (file-exists? config-file-path)
  (call-with-input-file*
   config-file-path
   (lambda (i)
     (define r (read i))
     (define (check what id bytes alt-path)
       (define l (hash-ref r id #f))
       (unless (and (list? l)
                    (or (member bytes l)
                        (member (path->bytes (path->complete-path alt-path)) l)))
         (error 'link-all
                (~a "config file exists, but does not have a definition of `~a' that includes development ~a\n"
                    "  config file: ~a\n"
                    "  development packages: ~s\n"
                    "  possible solution: delete the config file")
                id
                what
                config-file-path
                bytes)))
     (check "packages"
            'pkgs-search-dirs
            devel-pkgs-bytes
            devel-pkgs-dir)
     (check "links"
            'links-search-files
            devel-links-bytes
            (build-path devel-pkgs-dir "links.rktd")))))

;; found: maps each available package name to a directory
(define found (make-hash))

(define rx:platform (regexp
                     (regexp-quote
                      (apply
                       ~a
                       #:separator "-"
                       (map path->string
                            (explode-path (system-library-subpath #f)))))))

(printf "Finding packages\n")

;; Recur through directory tree, and treat each directory
;; that has an "info.rkt" file as a package (and don't recur
;; further into the package)
(for ([src-dir (in-list dirs)])
  (when (directory-exists? src-dir)
    (let loop ([src-dir src-dir])
      (for ([f (in-list (directory-list src-dir))])
        (define src-f (build-path src-dir f))
        (cond
         [(file-exists? (build-path src-f "info.rkt"))
          (when (or (not only-platform?)
                    (regexp-match? rx:platform f))
            (define f-name (path->string f))
            (when (hash-ref found f-name #f)
              (error 'pack-local 
                     "found package ~a multiple times: ~a and ~a"
                     f-name
                     (hash-ref found f-name)
                     src-f))
            (hash-set! found f-name src-f))]
         [(directory-exists? src-f)
          (loop src-f)])))))

(define metadata-ns (make-base-namespace))
(parameterize ([current-namespace metadata-ns])
  ;; with compiled files on:
  (dynamic-require '(submod info reader) #f)
  (dynamic-require 'info 0))
(define (get-pkg-info pkg-dir)
  ;; without compiled files:
  (parameterize ([use-compiled-file-paths '()])
    (get-info/full pkg-dir #:namespace metadata-ns)))

(define missing-desc null)
(define missing-authors null)

(define single-collection-pkgs (make-hash))

(define all-pkgs
  (let loop ([all-pkgs pkgs] [pkgs pkgs])
    (define new-pkgs
      (for/fold ([new-pkgs (set)]) ([pkg-name (in-set pkgs)]
                                    #:unless (equal? pkg-name "racket"))
        (define dir (hash-ref found pkg-name #f))
        (unless dir
          (error 'link-all "requested package not available: ~s" pkg-name))
        (define i (get-pkg-info dir))
        (define sc-name (i 'collection (lambda _ pkg-name)))
        (when (string? sc-name)
          (hash-set! single-collection-pkgs pkg-name sc-name))
        (define deps
          (extract-pkg-dependencies i #:filter? #t))
        (unless (string? (i 'pkg-desc (lambda _ #f)))
          (set! missing-desc (cons pkg-name missing-desc)))
        (unless (list? (i 'pkg-authors (lambda _ #f)))
          (set! missing-authors (cons pkg-name missing-authors)))
        (set-union
         new-pkgs
         (for/set ([dep (in-list deps)]
                   #:unless (or (set-member? all-pkgs dep)
                                (set-member? pkgs dep)
                                (equal? dep "racket")))
           dep))))
    (if (set-empty? new-pkgs)
        all-pkgs
        (loop (set-union new-pkgs all-pkgs) new-pkgs))))

(define (is-auto? name) (not (set-member? pkgs name)))

;; Exit if we detect no change:
(when (and (null? missing-desc)
           (null? missing-authors))
  (with-handlers ([exn:fail? (lambda (exn)
                               (printf "shortcut failed: ~s" (exn-message exn)))])
    (define devel-pkgs-file (build-path devel-pkgs-dir "pkgs.rktd"))
    (define expected-link-results
      (for/hash ([name (in-set all-pkgs)])
        (define dir (hash-ref found name))
        (define rel-dir (path->string (find-relative-path (path->complete-path devel-pkgs-dir)
                                                          (path->complete-path dir))))
        (define sc-name (hash-ref single-collection-pkgs name #f))
        (define auto? (is-auto? name))
        (values name
                (if sc-name
                    (sc-pkg-info `(static-link ,rel-dir) #f auto? sc-name)
                    (pkg-info `(static-link ,rel-dir) #f auto?)))))
    (when (and (file-exists? devel-pkgs-file)
               (equal? (call-with-input-file* devel-pkgs-file read)
                       expected-link-results))
      (printf "No changes to links\n")
      (exit 0))))

;; flush old configuration
(when (directory-exists? devel-pkgs-dir)
  (printf "Erasing previous development package configuration\n")
  (delete-directory/files devel-pkgs-dir))

(define orig-info-cache
  (and (file-exists? cache-file-path)
       (let ()
         (printf "Saving previous info cache\n")
         (begin0
          (call-with-input-file* cache-file-path read)
          (delete-file cache-file-path)))))

(void
 (parameterize ([current-pkg-scope (path->complete-path devel-pkgs-dir)])
   (with-pkg-lock
    (pkg-install (for/list ([name (in-list (sort (set->list all-pkgs)
                                                 ;; Non-auto before auto:
                                                 (lambda (a b)
                                                   (cond
                                                    [(is-auto? a)
                                                     (and (is-auto? b)
                                                          (string<? a b))]
                                                    [(is-auto? b) #t]
                                                    [else (string<? a b)]))))])
                   (define dir (hash-ref found name))
                   (define auto? (is-auto? name))
                   (printf "Adding ~a~a as ~a\n" name (if auto? "*" "") dir)
                   (pkg-desc (path->string dir)
                             'static-link
                             #f
                             #f
                             auto?))))))

(when orig-info-cache
  (printf "Restoring previous info cache\n")
  (call-with-output-file* cache-file-path (lambda (o)
                                            (write orig-info-cache o)
                                            (newline o))))

(for ([p (in-list missing-desc)])
  (printf "Missing package description for ~a\n" p))
(for ([p (in-list missing-authors)])
  (printf "Missing package authors for ~a\n" p))

(unless (and (null? missing-authors) (null? missing-desc))
   (error 'link-all "not all packages have description and authors."))

;; link configuration
(unless (file-exists? config-file-path)
  (printf "Writing ~a\n" config-file-path)
  (call-with-output-file*
   config-file-path
   (lambda (o)
     (write (hash 'pkgs-search-dirs
                  (list #f devel-pkgs-bytes)
                  'links-search-files
                  (list #f devel-links-bytes)
                  'installation-name
                  "development"
                  'default-scope
                  "installation")
            o)
     (newline o))))

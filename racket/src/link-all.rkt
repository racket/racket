#lang racket/base
(require racket/cmdline
         racket/file
         racket/list
         racket/format
         racket/string
         racket/set
         setup/getinfo
         pkg/lib)

(define config-file-path (build-path "racket" "etc" "config.rktd"))
(define devel-pkgs-dir (build-path "racket" "lib" "devel-pkgs"))

(define only-platform? #f)

(define dirs null)

(define pkgs
  (command-line
   #:once-each
   [("--platform") "Only packages whose names match the platform name"
    (set! only-platform? #t)]
   #:multi
   [("++dir") dir "Use packages in <dir>"
    (set! dirs (cons dir dirs))]
   #:args
   pkg
   (list->set pkg)))

(define devel-pkgs-bytes
  (path->bytes (path->complete-path devel-pkgs-dir)))
(define devel-links-bytes
  (path->bytes (path->complete-path (build-path devel-pkgs-dir "links.rktd"))))

(when (file-exists? config-file-path)
  (call-with-input-file*
   config-file-path
   (lambda (i)
     (define r (read i))
     (define (check what id bytes)
       (define l (hash-ref r id #f))
       (unless (and (list? l)
                  (member bytes l))
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
            devel-pkgs-bytes)
     (check "links"
            'links-search-files
            devel-links-bytes))))

;; found: maps each available package name to a directory
(define found (make-hash))

(define rx:platform (regexp
                     (regexp-quote
                      (apply
                       ~a
                       #:separator "-"
                       (map path->string
                            (explode-path (system-library-subpath #f)))))))

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
                     "found packages multiple times: ~a and ~a"
                     (hash-ref found f)
                     src-f))
            (hash-set! found f-name src-f))]
         [(directory-exists? src-f)
          (loop src-f)])))))

;; Like `found', but just the packages we want
(define wanted (make-hash))

(define all-pkgs
  (let loop ([all-pkgs pkgs] [pkgs pkgs])
    (define new-pkgs
      (for/fold ([new-pkgs (set)]) ([pkg-name (in-set pkgs)])
        (define dir (hash-ref found pkg-name #f))
        (unless dir
          (error 'link-all "requested package not available: ~s" pkg-name))
        (define i (get-info/full dir))
        (define deps
          (for/list ([dep (in-list (append (i 'deps (lambda () null))
                                           (i 'build-deps (lambda () null))))]
                     #:when
                     (let ([platform (and (list? dep)
                                          (member '#:platform dep))])
                       (or (not platform)
                           (let ([p (cadr platform)])
                             (if (symbol? p)
                                 (eq? p (system-type))
                                 (let ([s (path->string (system-library-subpath #f))])
                                   (if (regexp? p)
                                       (regexp-match? p s)
                                       (equal? p s))))))))
            (if (pair? dep)
                (car dep)
                dep)))
        (set-union
         new-pkgs
         (for/set ([dep (in-list deps)]
                   #:unless (or (set-member? all-pkgs dep)
                                (set-member? pkgs dep)))
           dep))))
    (if (set-empty? new-pkgs)
        all-pkgs
        (loop (set-union new-pkgs all-pkgs) new-pkgs))))

;; flush old configuration
(when (directory-exists? devel-pkgs-dir)
  (printf "Erasing previous development package configuration\n")
  (delete-directory/files devel-pkgs-dir))

(void
 (parameterize ([current-pkg-scope (path->complete-path devel-pkgs-dir)])
   (define (is-auto? name) (not (set-member? pkgs name)))
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
                            'link
                            #f
                            auto?)))))

;; link configuration
(unless (file-exists? config-file-path)
  (printf "Writing ~a\n" config-file-path)
  (call-with-output-file*
   config-file-path
   (lambda (o)
     (write (hash 'pkgs-search-dirs
                  (list #f devel-pkgs-bytes)
                  'links-search-files
                  (list #f devel-links-bytes))
            o)
     (newline o))))

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

;; Find packages in a directory tree ("info.rkt" indicates a package),
;; create a catalog that points to those packages to be installed as
;; links, and adjust the configuration to consult that catalog first.

;; Used by the top-level Makefile in the main Racket repository.

(define config-dir-path (build-path "racket" "etc"))
(define config-file-path (build-path config-dir-path "config.rktd"))
(define catalog-relative-path (build-path 'up "share" "pkgs-catalog"))
(define catalog-relative-path-str (path->string catalog-relative-path))
(define catalog-path (build-path config-dir-path catalog-relative-path))

(define dirs
  (command-line
   #:args
   dir
   dir))

(when (file-exists? config-file-path)
  (call-with-input-file*
   config-file-path
   (lambda (i)
     (define r (read i))
     (define l (hash-ref r 'catalogs #f))
     (unless (and (list? l)
                  ((length l) . >= . 1)
                  (equal? (car l) catalog-relative-path-str))
       (error 'pkgs-catalog
              (~a "config file exists, but does not have a definition of `catalogs' that starts as expected\n"
                  "  config file: ~a\n"
                  "  expected initial element: ~s\n"
                  "  possible solution: delete the config file")
                config-file-path
                catalog-relative-path-str)))))

;; found: maps each available package name to a directory
(define found (make-hash))

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
          (define f-name (path->string f))
          (when (hash-ref found f-name #f)
            (error 'pack-local 
                   "found package ~a multiple times: ~a and ~a"
                   f-name
                   (hash-ref found f-name)
                   src-f))
          (hash-set! found f-name src-f)]
         [(directory-exists? src-f)
          (loop src-f)])))))

(for ([l (directory-list (build-path catalog-path "pkg"))])
  (unless (hash-ref found (path->string l) #f)
    (printf " Uncataloging package ~a\n" (path->string l))
    (delete-directory/files (build-path catalog-path "pkg" l))))

(define metadata-ns (make-base-namespace))
(define (get-pkg-info pkg-dir)
  (get-info/full pkg-dir
                 #:namespace metadata-ns
                 #:bootstrap? #t))

(define missing-desc null)
(define missing-authors null)

(define (relative-path->relative-url p)
  (apply ~a #:separator "/"
         (map (lambda (e)
                (case e
                  [(up) ".."]
                  [(same) "."]
                  [else (path-element->string e) e]))
              (explode-path p))))

(for ([(pkg-name dir) (in-hash found)])
  (define i (get-pkg-info dir))
  (define deps
    (extract-pkg-dependencies i))
  (define desc (i 'pkg-desc (lambda _ #f)))
  (unless (string? desc)
    (set! missing-desc (cons pkg-name missing-desc)))
  (define authors (i 'pkg-authors (lambda _ null)))
  (unless (and (list? authors)
               ((length authors) . >= . 1))
    (set! missing-authors (cons pkg-name missing-authors)))
  (define pkg
    `#hash((name . ,pkg-name)
           (source . ,(string-append
                       (relative-path->relative-url
                        (find-relative-path (simple-form-path
                                             (path->complete-path catalog-path))
                                            (simple-form-path
                                             (path->complete-path dir))))
                       "?type=static-link"))
           (author . ,(string-join (for/list ([r authors])
                                     (if (symbol? r)
                                         (format "~a@racket-lang.org" r)
                                         r))
                                   " "))
           (checksum . "")
           (description . ,(or desc "???"))
           (tags . ())
           (dependencies . ,deps)
           (modules . ,(pkg-directory->module-paths
                        dir
                        pkg-name
                        #:namespace metadata-ns))))
  (define pkg-file (build-path catalog-path "pkg" pkg-name))
  (define exists? (file-exists? pkg-file))
  (cond
   [(and exists?
         (equal? (with-handlers ([exn:fail:read? void])
                   (call-with-input-file* pkg-file read))
                 pkg))
    ;; No change
    (void)]
   [else
    (printf " ~aataloging package ~a\n"
            (if exists? "Rec" "C")
            pkg-name)
    (make-directory* (build-path catalog-path "pkg"))
    (call-with-output-file*
     pkg-file
     #:exists 'truncate/replace
     (lambda (o)
       (write pkg o)
       (newline o)))]))

(for ([p (in-list missing-desc)])
  (printf "Missing package description for ~a\n" p))
(for ([p (in-list missing-authors)])
  (printf "Missing package authors for ~a\n" p))

(unless (and (null? missing-authors) (null? missing-desc))
  (error 'link-all "not all packages have description and authors."))

(unless (file-exists? config-file-path)
  (printf "Writing ~a\n" config-file-path)
  (call-with-output-file*
   config-file-path
   (lambda (o)
     (write (hash 'catalogs
                  (list catalog-relative-path-str #f)
                  'installation-name
                  "development"
                  'default-scope
                  "installation")
            o)
     (newline o))))

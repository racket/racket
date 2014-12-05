#lang racket/base
(require racket/cmdline
         racket/file
         racket/format
         racket/string
         racket/path
         setup/getinfo
         pkg/lib)

;; Find packages in a directory tree ("info.rkt" indicates a package),
;; create a catalog that points to those packages --- to be installed as
;; links if `--link` is specified.

;; Used by the top-level Makefile in the main Racket repository.

(define link? #f)

(define-values (catalog-path dirs)
  (command-line
   #:once-each
   ["--link" "Install packages as links"
    (set! link? #t)]
   #:args
   (catalog-path . dir)
   (values catalog-path dir)))

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

(when (directory-exists? (build-path catalog-path "pkg"))
  (for ([l (directory-list (build-path catalog-path "pkg"))])
    (unless (hash-ref found (path->string l) #f)
      (printf " Uncataloging package ~a\n" (path->string l))
      (delete-directory/files (build-path catalog-path "pkg" l)))))

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
                       (if link?
                           "?type=static-link"
                           "")))
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

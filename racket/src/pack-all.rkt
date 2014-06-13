#lang racket/base
(require racket/cmdline
         racket/file
         racket/port
         racket/string
         racket/list
         racket/path
         file/zip
         openssl/sha1
         net/url
         pkg/strip
         pkg/lib
         setup/getinfo)

;; Find packages in the same way as "link-all.rkt", but packs
;; them into ".zip" files and adds them to a catalog.

;; Used by the top-level Makefile in the main Racket repository.

(define pack-dest-dir #f)
(define catalog-dirs null)
(define native? #f)
(define relative? #t)
(define get-modules? #f)
(define checksum-dir #f)
(define source-checksums? #f)

(define src-dirs
  (command-line
   #:once-each
   [("--pack") dest-dir "Pack to <dest-dir>"
    (set! pack-dest-dir dest-dir)]
   [("--native") "Pack as native"
    (set! native? #t)]
   [("--absolute") "Record paths as absolute"
    (set! relative? #f)]
   [("--source-checksum") "Compute checksum from source when not packing"
    (set! source-checksums? #t)]
   [("--at-checksum") dir "Copy each to to <dir>/<checksum>"
    (set! checksum-dir dir)]
   [("--mods") "Include modules and dependencies in catalog"
    (set! get-modules? #t)]
   #:multi
   [("++catalog") catalog-dir "Write catalog entry to <catalog-dir>"
    (set! catalog-dirs (cons catalog-dir catalog-dirs))]
   #:args
   pkgs-dir
   pkgs-dir))

(when pack-dest-dir
  (make-directory* pack-dest-dir))
(for ([catalog-dir (in-list catalog-dirs)])
  (make-directory* catalog-dir))

(define metadata-ns (make-base-namespace))

(define (stream-directory d)
  (define-values (i o) (make-pipe (* 100 4096)))
  (define (skip-path? p)
    (let-values ([(base name dir?) (split-path p)])
      (define s (path->string name))
      (or (member s '("compiled"))
          (regexp-match? #rx#"^(?:[.]git.*|[.]svn|.*~|#.*#)$" s))))
  (thread (lambda ()
            (let loop ([d d])
              (for ([f (directory-list d #:build? #t)])
                (cond
                 [(skip-path? f) (void)]
                 [(directory-exists? f)
                  (write (filter-not skip-path? (directory-list f)) o)
                  (loop f)]
                 [(file-exists? f)
                  (call-with-input-file* 
                   f
                   (lambda (i) (copy-port i o)))])))
            (close-output-port o)))
  i)

(define (do-package src-dir pkg-name)
  (define zip-file (path-add-suffix pkg-name #".zip"))
  (define dest-zip (and pack-dest-dir
                        (build-path (path->complete-path pack-dest-dir) 
                                    zip-file)))

  (define pkg-src-dir (build-path src-dir pkg-name))

  (when pack-dest-dir
    (define sum-file (path-add-suffix pkg-name #".srcsum"))
    (printf "summing ~a\n" pkg-src-dir)
    (define src-sha1 (sha1 (stream-directory pkg-src-dir)))
    (define dest-sum (build-path (path->complete-path pack-dest-dir) sum-file))
    (unless (and (file-exists? dest-zip)
                 (file-exists? dest-sum)
                 (equal? (list (version) src-sha1)
                         (call-with-input-file* dest-sum read)))
      (printf "packing ~a\n" zip-file)
      (define tmp-dir (make-temporary-file "~a-pkg" 'directory))
      (parameterize ([strip-binary-compile-info #f]) ; for deterministic checksum
        (generate-stripped-directory (if native? 'binary 'source)
                                     pkg-src-dir 
                                     tmp-dir))
      (parameterize ([current-directory tmp-dir])
        (when (file-exists? dest-zip) (delete-file dest-zip))
        (apply zip dest-zip (directory-list)
               ;; Use a constant timestamp so that the checksum does
               ;; not depend on timestamps:
               #:timestamp 1359788400
               #:utc-timestamps? #t
               #:system-type 'unix))
      (delete-directory/files tmp-dir)
      (call-with-output-file*
       dest-sum
       #:exists 'truncate/replace
       (lambda (o)
         (write (list (version) src-sha1) o)
         (newline o)))))

  (define info-path (build-path src-dir pkg-name))
  (define i (get-info/full info-path))
  (define (get key)
    (i key (lambda ()
             (error 'catalog-local
                    "missing `~a'\n  path: ~a"
                    key
                    (build-path info-path "info.rkt")))))
  
  (define (write-catalog-entry catalog-dir)
    (define catalog-dir/normal (simplify-path (path->complete-path catalog-dir)))
    (define catalog-pkg-dir (build-path catalog-dir "pkg"))
    (define checksum (if dest-zip
                         (call-with-input-file* dest-zip sha1)
                         (if source-checksums?
                             (begin
                               (printf "summing ~a\n" pkg-src-dir)
                               (sha1 (stream-directory pkg-src-dir)))
                             "0")))
    (define orig-dest (if dest-zip
                          (build-path pack-dest-dir zip-file)
                          #f))
    (define checksum-dest (if checksum-dir
                              (build-path checksum-dir checksum zip-file)
                              orig-dest))
    (define pkg-dir (build-path src-dir pkg-name))
    (define info (and get-modules?
                      (get-info/full pkg-dir 
                                     #:namespace metadata-ns 
                                     #:bootstrap? #t)))
    (when dest-zip
      (when checksum-dir
        (make-directory* (build-path checksum-dir checksum))
        (copy-file orig-dest checksum-dest #t))
      (call-with-output-file*
       (build-path (path-replace-suffix checksum-dest #".zip.CHECKSUM"))
       #:exists 'truncate/replace
       (lambda (o)
         (display checksum o))))
    (make-directory* catalog-pkg-dir)
    (call-with-output-file*
     (build-path catalog-pkg-dir pkg-name)
     #:exists 'truncate
     (lambda (o)
       (write (hash 'source (path->string
                             (let ([p (path->complete-path
                                       (if dest-zip
                                           checksum-dest
                                           (path->directory-path pkg-dir)))])
                               (if relative?
                                   (find-relative-path catalog-dir/normal
                                                       (simplify-path p))
                                   p)))
                    'checksum checksum
                    'name (path->string pkg-name)
                    'author (string-join (for/list ([r (get 'pkg-authors)])
                                           (if (symbol? r)
                                               (format "~a@racket-lang.org" r)
                                               r))
                                         " ")
                    'description (get 'pkg-desc)
                    'tags '()
                    'dependencies (if get-modules?
                                      (append
                                       (info 'deps (lambda () null))
                                       (info 'build-deps (lambda () null)))
                                      '())
                    'modules (if get-modules?
                                 (pkg-directory->module-paths
                                  pkg-dir
                                  (path->string pkg-name)
                                  #:namespace metadata-ns)
                                 '()))
              o)
       (newline o))))
  (for ([catalog-dir (in-list catalog-dirs)])
    (write-catalog-entry catalog-dir)))

(define found (make-hash))

;; Recur through directory tree, and treat each directory
;; that has an "info.rkt" file as a package (and don't recur
;; further into the package)
(for ([src-dir (in-list src-dirs)])
  (let loop ([src-dir src-dir])
    (for ([f (in-list (directory-list src-dir))])
      (define src-f (build-path src-dir f))
      (cond
       [(file-exists? (build-path src-f "info.rkt"))
        (when (hash-ref found f #f)
          (error 'pack-local 
                 "found packages multiple times: ~a and ~a"
                 (hash-ref found f)
                 src-f))
        (hash-set! found f src-f)
        (do-package src-dir f)]
       [(directory-exists? src-f)
        (loop src-f)]))))

#lang racket/base

(require "config.rkt"
         "planet-archives.rkt"
         
         "private/planet-shared.rkt"
         "private/linkage.rkt"
         
         "private/resolver.rkt"
         "private/version.rkt"
         
         net/url
         xml/xml
         
         racket/file
         racket/list
         
         racket/contract/base
         racket/port
         racket/path
         racket/class
         racket/match
         
         setup/pack
         setup/plt-single-installer 
         setup/getinfo
         setup/unpack

         (for-syntax racket/base)
         (prefix-in srfi1: srfi/1)
         )

#| The util collection provides a number of useful functions for interacting with the PLaneT system. |#

(provide 
 
 current-cache-contents
 current-linkage
 make-planet-archive
 unpack-planet-archive
 force-package-building?
 build-scribble-docs?
 get-installed-planet-archives
 get-hard-linked-packages
 unlink-all
 lookup-package-by-keys
 resolve-planet-path
 display-plt-file-structure
 display-plt-archived-file
 get-package-from-cache
 pkg->download-url
 (struct-out exn:fail:planet)
 pkg-spec?
 pkg?)

(provide/contract
 [get-package-spec
  (->* (string? string?) (natural-number/c any/c) pkg-spec?)]
 [download-package 
  (-> pkg-spec? 
      (or/c string?
            (list/c #t path? natural-number/c natural-number/c)
            (list/c #f string?)))]
 [download/install-pkg
  (-> string? (and/c string? #rx"[.]plt$") natural-number/c any/c (or/c pkg? #f))]
 [install-pkg
  (-> pkg-spec? path-string? natural-number/c any/c (or/c pkg? #f))]
 [add-hard-link 
  (-> string? (and/c string? #rx"[.]plt$") natural-number/c natural-number/c path? void?)]
 [remove-hard-link 
  (->* (string? (and/c string? #rx"[.]plt$") natural-number/c natural-number/c)
       (#:quiet? boolean?)
      void?)]
 [remove-pkg
  (-> string? (and/c string? #rx"[.]plt$") natural-number/c natural-number/c void?)]
 [erase-pkg
  (-> string? (and/c string? #rx"[.]plt$") natural-number/c natural-number/c void?)])


;; get-package-spec : string string [nat | #f] [min-ver-spec | #f] -> pkg?
;; gets the package that corresponds to the given arguments, which are
;; values corresponding to the four parts of a package specifier in require syntax
(define (get-package-spec owner pkg [maj #f] [min #f])
  (define arg
    (cond
      [(not maj) (list owner pkg)]
      [(not min) (list owner pkg maj)]
      [else      (list owner pkg maj min)]))
  (pkg-spec->full-pkg-spec arg #f))

;; download/install-pkg : string string nat nat -> pkg | #f
(define (download/install-pkg owner name maj min)
  (let* ([pspec (pkg-spec->full-pkg-spec (list owner name maj min) #f)]
         [upkg (get-package-from-server pspec)])
    (cond
      [(uninstalled-pkg? upkg)
       (pkg-promise->pkg upkg)]
      [else #f])))

;; current-cache-contents : -> ((string ((string ((nat (nat ...)) ...)) ...)) ...)
;; returns the packages installed in the local PLaneT cache
(define (current-cache-contents)
  (cdr (tree->list (repository-tree))))

;; just so it will be provided
(define unlink-all remove-all-linkage!)

;; to remove:
;;   -- setup-plt -c the package
;;   -- remove relevant infodomain cache entries
;;   -- delete files from cache directory
;;   -- remove any existing linkage for package
;; returns void if the removal worked; raises an exception if no package existed.

(define (remove-pkg owner name maj min)
  (let ((p (get-installed-package owner name maj min)))
    (unless p
      (raise (make-exn:fail:planet "Could not find package" (current-continuation-marks))))
    (unless (normally-installed-pkg? p)
      (raise (make-exn:fail:planet "Not a normally-installed package, can't remove" (current-continuation-marks))))
    
    (let ((path (pkg-path p)))
      (with-logging 
       (LOG-FILE)
       (lambda () 
         (printf "\n============= Removing ~a =============\n" (list owner name maj min))
         (clean-planet-package path (list owner name '() maj min))))
      (planet-log "Erasing metadata")
      (erase-metadata p)
      (planet-log "Deleting metadata and files in ~a" (path->string path))
      (for ([file (in-list (dir->metadata-files path))])
        (with-handlers ((exn:fail:filesystem? void))
          (delete-file file)))
      (delete-directory/files path)
      (planet-log "Trimming empty directories")
      (trim-directory (CACHE-DIR) path)
      (planet-log "Rebuilding documentation index")
      (reindex-user-documentation)
      (void))))

;; erase-metadata : pkg -> void
;; clears out any references to the given package in planet's metadata files
;; (i.e., linkage and info.rkt cache; not hard links which are not considered metadata)
(define (erase-metadata p)
  (remove-infodomain-entries (pkg-path p))
  (remove-linkage-to! p))

;; this really should go somewhere else. But what should setup's behavior be
;; when a package is cleaned? should it clear info-domain entries out? I think
;; no; an uncompiled package isn't necessarily not to be indexed and so on.
;; remove-infodomain-entries : path -> void
(define (remove-infodomain-entries path)
  (let* ([pathbytes (path->bytes path)]
         [cache-file (build-path (PLANET-DIR) "cache.rktd")])
    (when (file-exists? cache-file)
      (let ([cache-lines (with-input-from-file cache-file read)])
        (call-with-output-file cache-file
          (λ (op)
            (if (pair? cache-lines)
                (write (filter
                        (λ (line) 
                          (not 
                           (and
                            (pair? line)
                            (or (not (directory-exists? (bytes->path (car line))))
                                (subpath? path (bytes->path (car line)))))))                              
                        cache-lines)
                       op)
                (fprintf op "\n")))
          #:exists 'truncate/replace)))))

;; subpath? : path path -> boolean
;; determines if p1 is a subpath of p2.
(define (subpath? p1 p2)
  (let ([full-p1 (explode-path (simple-form-path p1))]
        [full-p2 (explode-path (simple-form-path p2))])
    (sublist? full-p1 full-p2 (o2 bytes=? path->bytes))))

;; o2 : (X X -> Y) (Z -> X) -> (Z Z -> Y)
;; "compose-two"
(define (o2 a b) (λ (x y) (a (b x) (b y))))

;; sublist? : (listof X) (listof X) (X X -> boolean) -> boolean
;; determine if l1 is a sublist of l2, using = as the comparison operator for elements
(define (sublist? l1 l2 =)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(= (car l1) (car l2)) (sublist? (cdr l1) (cdr l2) =)]
    [else #f]))

(define (erase-pkg owner name maj min)
  (let* ([uninstalled-pkg-dir
          (build-path (UNINSTALLED-PACKAGE-CACHE) owner name (number->string maj) (number->string min))]
         [uninstalled-pkg-file (build-path uninstalled-pkg-dir name)]
         [uninstalled-file-exists? (file-exists? uninstalled-pkg-file)])
    (when uninstalled-file-exists?
      (delete-file uninstalled-pkg-file)
      (trim-directory (UNINSTALLED-PACKAGE-CACHE) uninstalled-pkg-dir))
    (with-handlers ([exn:fail:planet? 
                     (λ (e) (if uninstalled-file-exists? 
                                ;; not really a failure, just return
                                (void)
                                (raise e)))])
      (remove-pkg owner name maj min))))

;; listof X * listof X -> nonempty listof X
;; returns de-prefixed version of l2 if l1 is a proper prefix of l2; 
;; signals an error otherwise.
(define (drop-common-base list1 list2)
  (let loop ((l1 list1) (l2 list2))
    (cond
      [(null? l2)
       (error 'drop-common-base "root ~s is not a prefix of stem ~s" list1 list2)]
      [(null? l1) l2]
      [(not (equal? (car l1) (car l2)))
       (error 'drop-common-base "root ~s is not a prefix of stem ~s" list1 list2)]
      [else (loop (cdr l1) (cdr l2))])))

;; pathify-list : path (listof path) -> listof path
;; given a base and a list of names, interprets each name as a subdirectory
;; of the previous, starting with base, and returns a list. (This list
;; is in reverse order, so the deepest subdirectory is returned first)
(define (pathify-list root dirs)
  (let loop ((base root) (dirs dirs) (acc '()))
    (cond
      [(null? dirs) acc]
      [else
       (let ((new (build-path base (car dirs))))
         (loop new (cdr dirs) (cons new acc)))])))

;; directory-empty? path -> bool
;; #t iff the given directory contains no subdirectories of files
(define (directory-empty? dir)
  (null? (directory-list dir)))

;; trim-directory: path path -> void
;; deletes empty directories starting with stem and working down to root
(define (trim-directory root stem)
  (let* ([rootl (explode-path root)]
         [steml (explode-path stem)]
         [extras (cdr (pathify-list root (drop-common-base rootl steml)))])
    (let loop ((dirs extras))
      (cond
        [(null? dirs) (void)]
        [(directory-empty? (car dirs))
         (delete-directory (car dirs))
         (loop (cdr dirs))]
        [else (void)]))))

;; regexp->filter : (string | regexp) -> (path -> bool)
;; computes a filter that accepts paths that match the given regexps and rejects other paths
(define (regexp->filter re-s)
  (let ([re (cond
              [(string? re-s) (regexp re-s)]
              [(regexp? re-s) re-s]
              [else (error 'regexp->filter "not a regular expression")])])
    (lambda (p) (regexp-match re (path->bytes p)))))

(define force-package-building? (make-parameter #f))
(define build-scribble-docs? (make-parameter #t))

;; ---
;; documentation stuff --- loaded on demand so that setup/scribble can be
;; omitted in the Racket distribution
(define-namespace-anchor anchor)

;; render : path[fully-expanded scribble file path] path[fully expanded directory] boolean? -> xref?
;; renders the given scribble doc file (in src dir) into the directory
;; dir as a real scribble document
(define (render src-file dest-dir multi-page?)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    (make-directory* dest-dir)
    (let* ([index-dir (if multi-page?
                          (let-values ([(base name dir?) (split-path dest-dir)]) base)
                          dest-dir)]
           [renderer (new ((if multi-page?
                               (dynamic-require 'scribble/html-render 'render-multi-mixin)
                               values)
                           ((dynamic-require 'scribble/html-render 'render-mixin) 
                            (dynamic-require 'scribble/base-render 'render%)))
                          [dest-dir index-dir]
                          [root-path dest-dir])]
           [doc (dynamic-require `(file ,(path->string src-file)) 'doc)]
           [fp (send renderer traverse (list doc) (list dest-dir))]
           [ci (send renderer collect (list doc) (list dest-dir) fp)]
           [xref ((dynamic-require 'setup/xref 'load-collections-xref))]
           [_ ((dynamic-require 'scribble/xref 'xref-transfer-info) renderer ci xref)]
           [ri (send renderer resolve (list doc) (list dest-dir) ci)])
      (send renderer set-external-tag-path
            "/servlets/doc-search.rkt")
      (send renderer render 
            (list doc) 
            (list (if multi-page? 
                      dest-dir
                      (build-path dest-dir "index.html")))
            ri)
      ;; return cross-reference info:
      (send renderer serialize-info ri))))

;; this MUST BE a syntactic directory (with the trailing slash)
;; or the scribble renderer gets very confused
(define SCRIBBLE-DOCUMENT-DIR "planet-docs/")

;; scribble-entry? : Any -> Boolean
;; Recognizes valid list entries in info.rkt's scribblings field.
(define scribble-entry?
  (match-lambda
    [(or (list (? string?))
         (list (? string?) (? scribble-flags?))
         (list (? string?) (? scribble-flags?) (? scribble-category?))
         (list (? string?) (? scribble-flags?) (? scribble-category?) (? string?)))
     #t]
    [_ #f]))

;; scribble-flags? : Any -> Boolean
;; Recognizes a list of flags from an info.rkt scribblings entry.
(define scribble-flags?
  (match-lambda
    [(list (? symbol?) ...) #t]
    [_ #f]))

;; scribble-category : Any -> Boolean
;; Recognizes a category descriptor from an info.rkt scribblings entry.
(define scribble-category?
  (match-lambda
    [(or (list (? symbol?))
         (list (? symbol?) (? real?))) #t]
    [_ #f]))

;; scribble-entry-file : ScribbleEntry -> String
;; Produces the filename of an info.rkt scribblings entry.
(define scribble-entry-file
  (match-lambda [(list file _ ...) file]))

;; scribble-entry-flags : ScribbleEntry -> (Listof Symbol)
;; Produces the list of flags from an info.rkt scribblings entry.
(define scribble-entry-flags
  (match-lambda
    [(list _) null]
    [(list _ flags _ ...) flags]))

;; make-planet-archive: path<directory> [path<file>] -> path<file>
;; Makes a .plt archive file suitable for PLaneT whose contents are
;; all files in the given directory and returns that file's name.
;; If the optional filename argument is provided, that filename will 
;; be used as the output file's name.
(define make-planet-archive
  (case-lambda
    [(dir) 
     (let-values ([(path name must-be-dir?) (split-path dir)])
       (make-planet-archive 
        dir 
        (build-path (current-directory)
                    (string-append (path->string name) ".plt"))))]
    [(dir archive-name)
     (let ([abs-dir (simple-form-path dir)])
       (parameterize ((current-directory abs-dir))
         (let ([announcements '()]
               [warnings '()]
               [critical-errors '()])
           
           (define info.rkt
             (let ([real-info
                    (check-info.rkt-sanity 
                     dir
                     (λ (msg . args) (set! announcements (cons (apply format msg args) announcements)))
                     (λ (bad) (set! warnings (cons bad warnings)))
                     (λ (err) (set! critical-errors (cons err critical-errors))))])
               (or real-info (λ (x [y (λ () (error 'info.rkt (format "undefined field: ~a" x)))]) (y)))))

           (let ([scribble-files (info.rkt 'scribblings (λ () '()))])
             
             (define (outdir file-str)
               (let* ([filename (file-name-from-path file-str)]
                      [pathname (regexp-match #rx"(.*)\\.scrbl$" (path->bytes filename))])
                 (build-path SCRIBBLE-DOCUMENT-DIR (bytes->path (cadr pathname)))))
             
             (when (and (build-scribble-docs?)
                        (file-exists? (build-path (collection-path "setup") "scribble.rkt")))
               (with-handlers ([exn:fail? 
                                (lambda (e)
                                  (set! critical-errors
                                        (cons (format "Error generating scribble documentation: ~a" (render-exn e))
                                              critical-errors)))])
                 (unless (list? scribble-files)
                   (error (format (string-append
                                   "malformed scribblings field; expected"
                                   " (listof (list string (listof symbol))), received ~e") 
                                  scribble-files)))
                 (for ([entry scribble-files])
                   (unless (scribble-entry? entry)
                     (error "malformed scribblings entry"))
                   (let* ([filename (scribble-entry-file entry)]
                          [flags (scribble-entry-flags entry)])
                     (unless (and (relative-path? filename) 
                                  (subpath? abs-dir filename)
                                  (bytes=? (filename-extension filename) #"scrbl"))
                       (error (string-append
                               "illegal scribblings file ~a (must be a file with"
                               " extension .scrbl in the package directory or a subdirectory")))
                     (unless (file-exists? (build-path abs-dir filename))
                       (error (format "scribblings file ~a not found" filename)))
                     (printf "Building: ~a\n" filename)
                     (let* ([name.scrbl (file-name-from-path filename)]
                            [name       (path-replace-suffix name.scrbl #"")])
                       (render (build-path filename)
                               (build-path SCRIBBLE-DOCUMENT-DIR name)
                               (memq 'multi-page flags))))))))
           
           (unless (or (null? critical-errors)
                       (force-package-building?))
             (raise-user-error '|PLaneT packager| "~a\nRefusing to continue packaging." 
                               (if (pair? critical-errors)
                                   (car critical-errors)
                                   "")))

           (pack archive-name
                 "archive" 
                 (list ".") ;; if this changes, the filter (just below) must also change
                 null
                 (let ([p-a-f (PLANET-ARCHIVE-FILTER)])
                   (if p-a-f
                       (regexp->filter p-a-f)
                       (λ (p)
                         (or (for/and ([always-in (list 'same (string->path "planet-docs"))]
                                       [this-one (explode-path p)])
                               (equal? always-in this-one))
                             (std-filter p)))))
                 #t
                 'file
                 #f
                 #f)
           
           (for-each display (reverse announcements))
           (newline)
           (for-each (λ (s) (eprintf "WARNING:\n\t~a\n" s))
                     (reverse warnings))))
       
       (simple-form-path archive-name))]))

(define (unpack-planet-archive plt-file target)
  (parameterize ([current-directory target])
    (unpack plt-file)))

(define (foreach-planet-archive plt-file on-dir on-file)
  (fold-plt-archive plt-file
                    void
                    void 
                    (λ (l _)     (on-dir l))
                    (λ (l fip _) (on-file l fip))
                    (void)))

;; hash-tree ::= (hash-table [string -o> (union string hash-tree)])

;; chop-path : path -> (listof (union path symbol))
;; fully chops up the given path into directory list, without
;; accessing the filesystem
(define (chop-path path)
  (let loop ([p path] [acc '()])
    (cond
      [(not (path? p)) acc]
      [else
       (let-values ([(base name _) (split-path p)])
         (loop base (cons name acc)))])))

;; ============================================================
;; hash trees

(define (new-hash-tree)
  (make-hash))

(define (hash-tree-get htree pth)
  (let loop ([pth pth]
             [htree htree]
             [route '()])
    (cond
      [(null? pth) htree]
      [(not (hash? htree))
       (error (format "subpath ~s maps to a value" (reverse route)))]
      [else 
       (let* ([head (car pth)]
              [next (hash-ref htree
                              head
                              (λ ()
                                (let ([extension (new-hash-tree)])
                                  (hash-set! htree head extension)
                                  extension)))])
         (loop (cdr pth) next (cons (car pth) route)))])))

(define (hash-tree-put-value htree pth val)
  (let-values ([(where key) (split-last pth)])
    (let ([ht (hash-tree-get htree where)])
      (unless (hash? ht)
        (error "subpath ~s maps to a value" where))
      (hash-set! ht key val))))

(define (split-last l)
  (let loop ([l l]
             [front '()])
    (cond
      [(null? (cdr l)) (values (reverse front) (car l))]
      [else
       (loop (cdr l) 
             (cons (car l) front))])))

(define (hash-tree->list ht)
  (let ([lst (hash-map ht 
                       (λ (k v)
                         (cons k 
                               (if (hash? v)
                                   (hash-tree->list v)
                                   (list v)))))])
    (sort lst (λ (a b) (string<? (car a) (car b))))))
              
;; a 'a treelist is ::= (list string 'a) | (list string ('a treelist) ...)

;; ============================================================

;; print out file treelists (treelists where 'file is the only non-structure
;; element)
(define (print-tree t depth)
  (cond
    [(and (not (null? (cdr t)))
          (not (pair? (cadr t))))
     (printf "~a~a\n" (padding depth) (car t))]
    [else
     (printf "~a~a:\n" (padding depth) (car t))
     (print-tree-list (cdr t) (add1 depth))]))

(define (print-tree-list ts depth)
  (for-each (λ (t) (print-tree t depth)) ts))
       
(define (padding n)
  (apply string-append (build-list n (λ (_) "  "))))

;; list-plt-file-contents : path-string[.plt-file] -> void
;; prints out a manifest of the given plt file
(define (display-plt-file-structure plt-file)
  
  (define root (new-hash-tree))

  (define (gen-put f)
    (λ (path) (f (chop-path (simplify-path path #f)))))
  
  (define put-directory
    (gen-put
     (λ (ps)
       (cond
         [(equal? ps '(same)) (void)]
         [else (hash-tree-get root (map path->string ps))]))))
  
  (define put-file 
    (gen-put
     (λ (ps)
       (hash-tree-put-value root (map path->string ps) 'file))))
  
  (foreach-planet-archive
   plt-file
   put-directory
   (λ (p _) (put-file p)))
  
  (print-tree-list (hash-tree->list root) 0))

;; display-plt-archived-file : path-string[.plt-file] string -> void
(define (display-plt-archived-file plt-file file-to-print)
  (let/ec finished
    (let ([target (simplify-path file-to-print #f)])
      (foreach-planet-archive
       plt-file
       void
       (λ (path fip)
         (when (equal? (simplify-path path #f) target)
           (copy-port fip (current-output-port))
           (finished (void))))))
    (error 'display-archived-plt-file "The given file was not found in the given package")))

;; check-info.rkt-sanity : path (string -> void) (string -> void) (string -> void) -> info.rkt-fn | #f
;; gets all the info.rkt fields that planet will use (using the info.rkt file
;; from the current directory) and calls the announce, warn, and fail functions with strings
;; that describe how PLaneT sees the info.rkt file. NOTA BENE: if this function calls fail, it may 
;; also warn on the same field, and the warning may not make sense. This is based on the
;; assumption that errors will be turned into some kind of critical failure that obliterates
;; all the other information produced.
(define (check-info.rkt-sanity dir announce warn fail)
  (with-handlers ([exn:fail:read? 
                   (λ (e) 
                     (fail (format "Package has an unreadable info.rkt file. ~a" (render-exn e))) 
                     #f)]
                  [exn:fail:syntax? 
                   (λ (e) 
                     (fail (format "Package's info.rkt has an syntactically ill-formed info.rkt file: ~a" (render-exn e)))
                     #f)])
    (let ([i* (get-info/full dir)])
      (cond
        [(not i*) 
         (warn (string-append
                "Package has no info.rkt file. This means it will not have"
                " a description or documentation on the PLaneT web site."))]
        [else
         (let ([i (λ (field) (i* field (λ () #f)))])
           (checkinfo i fail
                      [name     ; field name 
                       string?  ; check
                       (announce "Name: ~a\n" name)  ; success action
                       (warn "Package's info.rkt file has no name field.") ;failure action
                       ]
                      [blurb 
                       (λ (b) (and (list? b) (andmap xexpr? b)))
                       (announce "Package blurb: ~s\n" blurb)
                       (unless blurb
                         (warn 
                          (string-append
                           "Package's info.rkt does not contain a blurb field."
                           " Without a blurb field, the package will have no description on planet.racket-lang.org.")))]
                      [release-notes 
                       (λ (b) (and (list? b) (andmap xexpr? b)))
                       (announce "Release notes: ~s\n" release-notes)
                       (unless release-notes
                         (warn
                          (string-append
                           "Package's info.rkt does not contain a release-notes field. Without a release-notes"
                           " field, the package will not have any listed release information on"
                           " planet.racket-lang.org beyond the contents of the blurb field.")))]
                      [categories
                       (λ (s) (and (list? s) (andmap symbol? s)))
                       (cond
                         [(ormap illegal-category categories)
                          =>
                          (λ (bad-cat)
                            (fail (format (string-append
                                           "Package's info.rkt file contains illegal category \"~a\"."
                                           " The legal categories are: ~a\n") 
                                          bad-cat
                                          legal-categories)))]
                         [else (announce "Categories: ~a\n" categories)])
                       (unless categories
                         (warn (string-append
                                "Package's info.rkt file does not contain a category listing."
                                " It will be placed in the Miscellaneous category.")))]
                      [doc.txt
                       string?
                       (announce "doc.txt file: ~a\n" doc.txt)
                       (when doc.txt
                         (warn
                          (string-append
                           "Package's info.rkt contains a doc.txt entry, which is now considered deprecated."
                           " The preferred method of documentation for PLaneT packages is now Scribble"
                           " (see the Scribble documentation included in the Racket distribution for"
                           " more information).")))]
                      [html-docs
                       (lambda (s) (and (list? s) (andmap string? s)))
                       (warn (string-append
                              "Package specifies an html-docs entry. The preferred method of documentation"
                              " for PLaneT packages is now Scribble (see the Scribble documentation included"
                              " in the Racket distribution for more information)."))]
                      [scribblings
                       (lambda (s) 
                         (and (list? s) 
                              (andmap scribble-entry? s)))
                       (void)
                       (unless scribblings
                         (warn (string-append
                                "Package does not specify a scribblings field. Without a scribblings field,"
                                " the package will not have browsable online documentation.")))]
                      [homepage 
                       string?
                       (cond
                         [(url-string? homepage)
                          (announce "Home page: ~a\n" homepage)]
                         [else
                          (fail (format (string-append
                                         "The value of the package's info.rkt homepage field, ~s, "
                                         "does not appear to be a legal URL.")
                                        homepage))])]
                      [primary-file
                       (λ (x) (or (string? x) (and (list? x) (andmap string? x))))
                       (begin
                         (cond
                           [(string? primary-file) 
                            (unless (file-in-current-directory? primary-file)
                              (warn (format (string-append
                                             "Package's info.rkt primary-file field is ~s, a file that"
                                             " does not exist in the package.") 
                                            primary-file)))]
                           [(pair? primary-file)
                            (let ([bad-files (filter (λ (f) (not (file-in-current-directory? f))) primary-file)])
                              (unless (null? bad-files)
                                (warn (format (string-append
                                               "Package's info.rkt primary-file field is ~s, which contains"
                                               " non-existant files ~s.")
                                              primary-file bad-files))))])
                         (announce "Primary file: ~a\n" primary-file))
                       (unless primary-file
                         (warn 
                          (string-append
                           "Package's info.rkt does not contain a primary-file field."
                           " The package's listing on planet.racket-lang.org will not have a"
                           " valid require line for your package.")))]
                      [required-core-version 
                       core-version?
                       (announce "Required racket version: ~a\n" required-core-version)]
                      [repositories
                       (λ (x) (and (list? x) 
                                   (srfi1:lset<= equal? x '("3xx" "4.x"))))
                       (announce "Repositories: ~s\n" repositories)
                       (unless repositories
                         (warn (string-append
                                "Package's info.rkt does not contain a repositories field."
                                " The package will be listed in all repositories by default.")))]
                      [version
                       string?
                       (announce "Version description: ~a\n" version)]))])
      i*)))


(define (render-exn e)
  (let ([sp (open-output-string)])
    (parameterize ([current-output-port sp]
                   [current-error-port sp])
      ((error-display-handler) (exn-message e) e))
    (get-output-string sp)))

;; legal-categories : (listof symbol)
(define legal-categories
  '(devtools net media xml datastructures io scientific
             system ui metaprogramming planet misc))

;; legal-category : symbol -> boolean
;; determine if the given symbol is a legal category
(define (legal-category? x) (memq x legal-categories))

;; illegal-category : symbol -> (union symbol false)
;; returns #f if the symbol is a legal category, or the symbol itself if it isn't
(define (illegal-category s) (if (legal-category? s) #f s)) 

;; url-string? : string -> boolean
;; determines if the given string is a reasonable homepage url
(define (url-string? s)
  (and (string? s)
       (let ([u (string->url s)])
         (and (url-scheme u)
              (url-host u)))))

;; file-in-current-directory? : string -> boolean
;; determines if the given string represents a file in the current directory
(define (file-in-current-directory? f)
  (and (string? f) (file-exists? f)))

;; core-version : string -> boolean
;; determines if the given string is something that (version) could've produced
(define (core-version? s)
  (and (string? s)
       (string->mz-version s)))

;; checkinfo: syntax
;; given an info.rkt function, a failure function, and a bunch of fields to check,
;; goes through the checklist calling either the success or the failure branch
;; of each check as appropriate
(define-syntax checkinfo
  (syntax-rules ()
    [(checkinfo fn fail clauses ...)
     (let ([fn* fn] [fail* fail])
       (checkinfo* () fn* fail* clauses ...))]))

(define-syntax checkinfo*
  (syntax-rules ()
    [(checkinfo* () fn fail) (void)]
    [(checkinfo* (handler1 handler ...) fn fail) (begin handler1 handler ...)]
    [(checkinfo* (handler ...) fn fail [id check on-success] clauses ...)
     (checkinfo* (handler ...) fn fail [id check on-success void] clauses ...)]
    [(checkinfo* (handler ...) fn fail [id check on-success on-fail] clauses ...)
     (checkinfo*
      (handler ...
       (let ([id (fn 'id)])
         (cond
           [id
            (let ([checked (check id)])
              (unless checked
                on-fail
                (fail (format "Package's info.rkt contained a malformed ~a field." 'id)))
              on-success)]
           [else on-fail])))
      fn fail clauses ...)]))

;; ============================================================
;; HARD LINKS (aka development links)

;; add-hard-link : string string num num path -> void
;; adds an entry in the hard-links table associating the given
;; require spec to the given path
(define (add-hard-link owner pkg-name maj min path)
  (unless (directory-exists? path)
    (if (file-exists? path)
        (error 'add-hard-link "Hard links must point to directories, not files")
        (eprintf "Warning: directory ~a does not exist\n"
                 (path->string path))))
  (add-hard-link! pkg-name (list owner) maj min path))

;; remove-hard-link : string string num num (#:quiet boolean) -> void
;; removes any development association from the given package spec
(define (remove-hard-link owner pkg-name maj min #:quiet? [quiet? #false])
  (define (matching-link? row)
    (points-to? row pkg-name (list owner) maj min))
  (when (and (empty? (filter matching-link? (get-hard-link-table)))
             (not quiet?))
    (error "no existing links match the given specification"))
  (filter-link-table!
   (lambda (row) (not (matching-link? row)))
   (lambda (row) 
     (let ([p (row->package row)])
       (when p
         (erase-metadata p))))))

;; ============================================================
;; VERSION INFO
;;  re-provided here for backwards compatibility (no idea 
;;  why it was here in the first place, actually)
(provide this-package-version
         this-package-version-name
         this-package-version-owner
         this-package-version-maj
         this-package-version-min
         this-package-version-symbol
         package-version->symbol
         make-planet-symbol
         path->package-version)

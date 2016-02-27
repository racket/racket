#lang racket/base
(require racket/match
         racket/format
         racket/path
         net/url
         "../name.rkt"
         "../path.rkt"
         "stage.rkt"
         "pkg-db.rkt"
         "catalog.rkt"
         "repo-path.rkt"
         "desc.rkt"
         "dirs.rkt"
         "print.rkt")

(provide initial-repo-descs
         adjust-to-normalize-repos
         convert-clone-name-to-clone-repo/update
         convert-clone-name-to-clone-repo/install
         convert-directory-to-installed-clone
         desc->name
         desc->repo)

;; Get repo info for installed packages (i.e., packages
;; that are installed with repo-related sources); a
;; `repo-descs` has the form:
;;   (hash repo (hash pkg-name desc) ...)
(define (initial-repo-descs db download-printf)
  (for/fold ([ht (hash)]) ([(name info) (in-hash db)])
    (define d (pkg-info->clone-desc name info
                                    #:checksum (pkg-info-checksum info)
                                    #:auto? (pkg-info-auto? info)))
    (cond
     [(not d) ht]
     [else
      (define repo (desc->repo d #f download-printf))
      (define descs (hash-ref ht repo (hash)))
      (hash-set ht repo (hash-set descs name d))])))

;; Adjusts `descs` through `done-infos` to keep packages
;; in repo clones together (as much as possible and allowed by the user).
;; Also produces an updated `clone-behavior` to keep track of a
;; user's preferences, and produces a set of packages that are
;; effectively being updated in install mode.
(define (adjust-to-normalize-repos descs           ; new additions to consider
                                   done-descs      ; descs already staged; we may drop some
                                   done-infos      ; infos for descs already staged; we may drop some
                                   clone-behavior  ; how we're currently handling handle mismatches
                                   old-repo-descs  ; accumulated repo mappings
                                   updating?       ; update vs. install mode
                                   catalog-lookup-cache
                                   download-printf
                                   from-command-line?
                                   convert-to-non-clone?
                                   prefetch-group)
  ;; A `repo-descs` is (hash repo (hash pkg-name desc) ...)
  (define (add-repo repo-descs repo name desc)
    (hash-set repo-descs repo
              (hash-set (hash-ref repo-descs repo (hash))
                        name
                        desc)))
  
  ;; Filter `descs` to get get repo mappings
  (define (add-repo-desc desc ht #:prefetch? [prefetch? #f])
    (cond
     [(desc->name desc)
      => (lambda (name)
           (cond
            [(desc->repo desc catalog-lookup-cache download-printf
                         #:prefetch? prefetch?
                         #:prefetch-group prefetch-group)
             => (lambda (repo)
                  (if prefetch?
                      ht
                      (add-repo ht repo name desc)))]
            [else ht]))]
     [else ht]))
  (when prefetch-group
    (for ([desc (in-list descs)])
      (add-repo-desc desc (hash) #:prefetch? #t)))
  (define new-repo-descs
    (for/fold ([ht (hash)]) ([desc (in-list descs)])
      (add-repo-desc desc ht)))
  
  ;; If updating, we don't want to complain about repos
  ;; whose repo status isn't changing.
  (define check-repo-descs
    (if updating?
        (for*/fold ([check-repo-descs (hash)]) ([(repo ht) (in-hash new-repo-descs)]
                                                [(name desc) (in-hash ht)])
          (define old-ht (hash-ref old-repo-descs repo #f))
          (define old-desc (and old-ht (hash-ref old-ht name #f)))
          (cond
           [(and old-desc
                 (equal? (desc-clone desc) (desc-clone old-desc)))
            ;; skip it
            check-repo-descs]
           [else
            (add-repo check-repo-descs repo name desc)]))
        new-repo-descs))

  ;; Combine old and new:
  (define repo-descs
    (for*/fold ([repo-descs old-repo-descs]) ([(repo ht) (in-hash new-repo-descs)]
                                              [(name desc) (in-hash ht)])
      (add-repo repo-descs repo name desc)))
  
  ;; Check each new repo, revising various mappings
  (for/fold ([descs descs]
             [done-descs done-descs]
             [done-infos done-infos]
             [clone-behavior clone-behavior]
             [repo-descs repo-descs]
             [extra-updates #hash()])
            ([(repo ht) (in-hash check-repo-descs)])
    (define (continue)
      (values descs done-descs done-infos clone-behavior repo-descs extra-updates))
    
    (define (clone-summary ht)
      (for/fold ([clones (hash)] [non-clones null]) ([(name desc) (in-hash ht)])
        (define c (desc-clone desc))
        (if c
            (values (hash-set clones c (cons name (hash-ref clones c null)))
                    non-clones)
            (values clones
                    (cons name non-clones)))))
    (define-values (clones non-clones) (clone-summary (hash-ref repo-descs repo)))
    (cond
     [(zero? (hash-count clones))
      ;; No clones for `repo`, so everything is consistent
      (continue)]
     [(and (= 1 (hash-count clones))
           (null? non-clones))
      ;; All clones the same for `repo`, so that's consistent
      (continue)]
     [else
      (define (msg #:would [would "would"]
                   #:convert [convert ""])
        (apply
         string-append 
         "packages from a Git repository " would " not share a local clone"
         convert "\n"
         (~a "  repository: " repo)
         (append
          (for/list ([(clone names) (in-hash clones)])
            (~a "\n"
                "  local clone: " clone "\n"
                "  packages for local clone:"
                (format-list names)))
          (list
           (if (null? non-clones)
               ""
               (~a "\n"
                   "  non-clone packages:"
                   (format-list non-clones)))))))
      
      ;; Determine a direction of conversion; we consider converting from
      ;; clones only for `raco pkg update --lookup`:
      (define convert-direction
        (cond
         [(not (= (hash-count clones) 1)) #f]
         [convert-to-non-clone? 'non-clone]
         [else 'clone]))

      (define (convert-to/from-clones new-clone-behavior)
        ;; Change `descs` to include each currently non-clone item as a clone
        (define clone (car (hash-keys clones)))
        (define ht (hash-ref repo-descs repo))
        (for/fold ([descs descs]
                   [done-descs done-descs]
                   [done-infos done-infos]
                   [clone-behavior new-clone-behavior]
                   [repo-descs repo-descs]
                   [extra-updates extra-updates])
                  ([name (in-list (case convert-direction
                                    [(clone) non-clones]
                                    [(non-clone) (car (hash-values clones))]))])
          (define desc (hash-ref ht name))
          (define converted-desc
            (case convert-direction
              [(clone)
               (convert-desc-to-clone desc clone
                                      catalog-lookup-cache
                                      download-printf)]
              [(non-clone)
               (convert-desc-to-lookup desc name)]))
          (values (cons converted-desc
                        (remove-desc-by-name name descs))
                  (remove-desc-by-name name done-descs)
                  (remove-info-by-name name done-infos)
                  clone-behavior
                  (hash-set repo-descs repo
                            (let ([ht (hash-ref repo-descs repo)])
                              (case convert-direction
                                [(clone)
                                 (hash-set ht name converted-desc)]
                                [(non-clone)
                                 (hash-remove ht name)])))
                  (if (not (hash-ref (hash-ref new-repo-descs repo) name #f))
                      ;; Count the conversion as an update, not an install,
                      ;; and make sure it's removed before the re-install:
                      (hash-set extra-updates name #t)
                      extra-updates))))
      
      (cond
       [(eq? clone-behavior 'force)
        (download-printf "~a\n" (msg #:would "will"))
        (continue)]
       [(or (eq? clone-behavior 'fail)
            (not convert-direction))
        (pkg-error "~a" (msg #:convert (if (and from-command-line?
                                                convert-direction)
                                           ";\n use `--multi-clone ask' for automated help"
                                           "")))]
       [(eq? clone-behavior 'convert)
        (download-printf "~a\n" (msg #:convert (format ";\n CONVERTING the ~aclone packages to ~aclones"
                                                       (if (eq? convert-direction 'clone) "non-" "")
                                                       (if (eq? convert-direction 'clone) "" "NON-"))))
        (convert-to/from-clones 'convert)]
       [else
        (displayln (msg))
        (case (ask (format "Convert the ~aclone packages to ~aclones, too?"
                           (if (eq? convert-direction 'clone) "non-" "")
                           (if (eq? convert-direction 'clone) "" "NON-")))
          [(no) (continue)]
          [(yes)
           (convert-to/from-clones 'ask)]
          [(always-yes)
           (convert-to/from-clones 'convert)]
          [(cancel)
           (pkg-error "canceled")])])])))

(define (remove-desc-by-name name descs)
  (for/list ([d (in-list descs)]
             #:unless (equal? name (desc->name d)))
    d))

(define (remove-info-by-name name infos)
  (for/list ([i (in-list infos)]
             #:unless (equal? name (install-info-name i)))
    i))

;; ----------------------------------------

;; If `desc` is a description with the type 'clone, but its syntax
;; matches a package name, then consult the catalog to determine whether
;; 'clone mode makes sense, and complain if not
(define ((convert-clone-name-to-clone-repo/install catalog-lookup-cache
                                                   download-printf) 
         desc)
  (cond
   [(and (eq? 'clone (pkg-desc-type desc))
         (let-values ([(name type) (package-source->name+type (pkg-desc-source desc) 'name)])
           name))
    => (lambda (name)
         (define src (package-catalog-lookup-source name 
                                                    catalog-lookup-cache
                                                    download-printf))
         (define-values (new-name new-type)
            (package-source->name+type src #f))
         (case new-type
           [(git github)
            (pkg-desc src 'clone name
                      (pkg-desc-checksum desc)
                      (pkg-desc-auto? desc)
                      (pkg-desc-extra-path desc))]
           [else
            (pkg-error (~a "catalog mapping for package name is not a Git repository\n"
                           "  package name: ~a\n"
                           "  catalog mapping: ~a")
                       name
                       src)]))]
   [else desc]))

;; If `pkg-name` is a description with the type 'clone, but its syntax
;; matches a package name, then infer a repo from the current package
;; installation and return an alternate description.
(define ((convert-clone-name-to-clone-repo/update db
                                                  skip-uninstalled?
                                                  from-command-line?)
         pkg-name)
  (cond
   [(and (pkg-desc? pkg-name)
         (eq? 'clone (pkg-desc-type pkg-name))
         (let-values ([(name type) (package-source->name+type (pkg-desc-source pkg-name) 'name)])
           name))
    => (lambda (name)
         ;; Infer or complain
         (define info (package-info name #:db db (not skip-uninstalled?)))
         (cond
          [(not info)
           ;; Skipping uninstalled packages
           #f]
          [else
           (define new-pkg-name
             (pkg-info->clone-desc name info
                                   #:checksum (pkg-desc-checksum pkg-name)
                                   #:auto? (pkg-desc-auto? pkg-name)
                                   #:extra-path (pkg-desc-extra-path pkg-name)
                                   #:reject-existing-clone? #t))
           (define current-orig-pkg (pkg-info-orig-pkg info))
           (unless new-pkg-name
             (pkg-error (~a "package is not currently installed from a repository\n"
                            "  package: ~a\n"
                            "  current installation: ~a"
                            (cond
                             [from-command-line?
                              (case (car current-orig-pkg)
                                [(link static-link)
                                 (~a "\n  extra advice:\n"
                                     "   Your current installation is a directory link, and the directory might\n"
                                     "   be a Git repostory checkout, but the package system doesn't know that.\n"
                                     "   If so, try\n"
                                     "    cd " (simplify-path
                                                (path->complete-path (cadr current-orig-pkg) (pkg-installed-dir)))
                                     "\n"
                                     "    raco pkg update --clone . <repository-URL>")]
                                [else ""])]
                             [else ""]))
                        name
                        current-orig-pkg))
           new-pkg-name]))]
   [else pkg-name]))

(define ((convert-directory-to-installed-clone db) d)
  (cond
   [(pkg-desc? d)
    (define-values (name type)
      (package-source->name+type (pkg-desc-source d)
                                 (pkg-desc-type d)
                                 #:must-infer-name? (not (pkg-desc-name d))))
    (case type
     [(dir)
      (define pkg-name (or (pkg-desc-name d) name))
      (define info (package-info pkg-name #:db db #f))
      (case (and info
                 (car (pkg-info-orig-pkg info)))
        [(clone)
         (cond
          [(equal? (path->directory-path
                    (simple-form-path (pkg-desc-source d)))
                   (path->directory-path
                    (simplify-path
                     (path->complete-path (cadr (pkg-info-orig-pkg info))
                                          (pkg-installed-dir)))))
           ;; Directory refers to a clone-linked package; preserve the
           ;; link form:
           (pkg-info->clone-desc pkg-name info
                                 #:checksum #f
                                 #:auto? (pkg-info-auto? info))]
          [else d])]
        [else d])]
     [else d])]
   [else d]))

;; ----------------------------------------

(define (desc->name d)
  (or (pkg-desc-name d)
      (package-source->name (pkg-desc-source d) 
                            (pkg-desc-type d))))

;; If `catalog-lookup-cache` is given, then check the catalog
;; if necessary to see whether the name resolves to a repository
;; (where the catalog will be used, anyway, so it's fine to
;; lookup now and cache the result).
;; In prefetch mode, the result is not useful (even as a prefetch
;; future), because no prefetch is set up for a recursive
;; resolution.
(define (desc->repo d catalog-lookup-cache download-printf
                    #:prefetch? [prefetch? #f]
                    #:prefetch-group [prefetch-group #f])
  (define-values (name type) (package-source->name+type 
                              (pkg-desc-source d) 
                              (pkg-desc-type d)))
  (and name
       (case type
         [(name)
          (cond
           [catalog-lookup-cache
            (define src (package-catalog-lookup-source name 
                                                       catalog-lookup-cache
                                                       download-printf
                                                       #:prefetch? prefetch?
                                                       #:prefetch-group prefetch-group))
            ;; Might be a prefetch future in prefetch mode, so continue
            ;; only if possible:
            (and (string? src)
                 (desc->repo (pkg-desc src #f name #f #f #f)
                             catalog-lookup-cache
                             download-printf
                             #:prefetch? prefetch?
                             #:prefetch-group prefetch-group))]
           [else #f])]
         [(git github clone)
          (define pkg-url (string->url (pkg-desc-source d)))
          (define-values (transport host port repo branch path)
            (split-git-or-hub-url #:type type pkg-url))
          (real-git-url pkg-url #:type type host port repo)]
         [else #f])))

(define (pkg-info->clone-desc name info
                              #:checksum [checksum #f]
                              #:auto? [auto? #f]
                              #:extra-path [extra-path #f]
                              #:reject-existing-clone? [reject-existing? #f])
  (match (pkg-info-orig-pkg info)
    [`(clone ,path ,url-str)
     (if reject-existing?
         (pkg-error (~a "package is already a linked repository clone\n"
                        "  package: ~a")
                    name)
         (pkg-desc url-str 'clone name
                   checksum auto?
                   (enclosing-path-for-repo url-str
                                            (path->complete-path path
                                                                 (pkg-installed-dir)))))]
    [`(catalog ,lookup-name ,url-str)
     (pkg-desc url-str (if reject-existing?
                           'clone
                           (if (equal? "github" (url-scheme (string->url url-str)))
                               'github
                               'git))
               name
               checksum auto? extra-path)]
    [`(url ,url-str)
     (define-values (current-name current-type)
       (package-source->name+type url-str #f))
     (case current-type
       [(git github)
        ;; found a repo URL
        (pkg-desc url-str (if reject-existing? 'clone current-type) name
                  checksum auto? extra-path)]
       [else #f])]
    [else #f]))

;; For a `desc`, extract it's clone location, if it's a clone
(define (desc-clone desc)
  (and (eq? (pkg-desc-type desc) 'clone)
       (path->directory-path
        (simple-form-path 
         (or (pkg-desc-extra-path desc)
             (current-directory))))))

;; Change a clone-compatible desc into a clone desc:
(define (convert-desc-to-clone d clone catalog-lookup-cache download-printf)
  (struct-copy pkg-desc d
               [source
                (let ()
                  (define-values (name type) (package-source->name+type 
                                              (pkg-desc-source d) 
                                              (pkg-desc-type d)))
                  (if (eq? type 'name)
                      ;; Since we got here, it must be that we have a
                      ;; Git repo source cached:
                      (package-catalog-lookup-source name
                                                     catalog-lookup-cache
                                                     download-printf)
                      (pkg-desc-source d)))]
               [type 'clone]
               [extra-path clone]))

(define (convert-desc-to-lookup d name)
  (struct-copy pkg-desc d
               [source name]
               [type 'name]
               [checksum #f]))

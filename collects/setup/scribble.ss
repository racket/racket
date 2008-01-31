#lang scheme/base

(require "getinfo.ss"
         "dirs.ss"
         "private/doc-path.ss"
         scheme/class
         scheme/file
         setup/main-collects
         scribble/base-render
         scribble/struct
         scribble/manual ; really shouldn't be here... see dynamic-require-doc
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render))

(provide setup-scribblings
         verbose)

(define verbose (make-parameter #t))

(define-struct doc (src-dir src-file dest-dir flags under-main? category))
(define-struct info (doc sci provides undef searches deps 
                     build? time out-time need-run? 
                     need-in-write? need-out-write?
                     vers rendered?)
  #:mutable)

(define (user-doc? doc)
  (or (memq 'user-doc-root (doc-flags doc))
      (memq 'user-doc (doc-flags doc))))

(define (filter-user-start docs)
  ;; If we've built user-specific before...
  (if (file-exists? (build-path (find-user-doc-dir) "index.html"))
      ;; Keep building:
      docs
      ;; Otherwise, see if we need it:
      (let ([cnt-not-main (apply +
                                 (map (lambda (doc)
                                        (if (or (doc-under-main? doc)
                                                (memq 'no-depend-on (doc-flags doc)))
                                            0
                                            1))
                                      docs))])
        (let ([any-not-main? (positive? cnt-not-main)])
          (cond
           [any-not-main? 
            ;; Need user-specific:
            docs]
           [else
            ;; Don't need them, so drop them:
            (filter (lambda (doc) (not (user-doc? doc)))
                    docs)])))))

(define (setup-scribblings only-dirs        ; limits doc builds
                           latex-dest       ; if not #f, generate Latex output
                           auto-start-doc?) ; if #t, expands `only-dir' with [user-]start to catch new docs
  (let* ([dirs (find-relevant-directories '(scribblings))]
         [infos (map get-info/full dirs)]
         [docs (map (lambda (i dir)
                      (let ([s (i 'scribblings)])
                        (if (and (list? s)
                                 (andmap (lambda (v)
                                           (and (list? v)
                                                (<= 1 (length v) 3)
                                                (string? (car v))
                                                (relative-path? (car v))
                                                (or (null? (cdr v))
                                                    (and (list? (cadr v))
                                                         (andmap (lambda (i)
                                                                   (member i '(main-doc
                                                                               main-doc-root
                                                                               user-doc-root
                                                                               user-doc
                                                                               multi-page
                                                                               depends-all
                                                                               depends-all-main
                                                                               no-depend-on
                                                                               always-run)))
                                                                 (cadr v))
                                                         (or (null? (cddr v))
                                                             (and (path-string? (caddr v))
                                                                  (relative-path? (caddr v))))))))
                                         s))
                          (map (lambda (d cat)
                                 (let* ([flags (if (pair? (cdr d)) (cadr d) null)]
                                        [under-main? (and (not (memq 'main-doc-root flags))
                                                          (not (memq 'user-doc-root flags))
                                                          (not (memq 'user-doc flags))
                                                          (or (memq 'main-doc flags)
                                                              (pair? (path->main-collects-relative dir))))])
                                   (make-doc dir
                                             (build-path dir (car d))
                                             (let ([name (if (and (pair? (cdr d))
                                                                  (pair? (cddr d))
                                                                  (caddr d))
                                                           (cadr d)
                                                           (let-values ([(base name dir?) (split-path (car d))])
                                                             (path-replace-suffix name #"")))])
                                               (doc-path dir name flags))
                                             flags
                                             under-main?
                                             cat)))
                               s
                               (i 'doc-categories (lambda () (map (lambda (a) 'library) s))))
                          (begin
                            (fprintf (current-error-port)
                                     " bad 'scribblings info: ~e from: ~e\n"
                                     s
                                     dir)
                            null))))
                    infos dirs)]
         [docs (filter-user-start (apply append docs))])
    (when (ormap (can-build? only-dirs) docs)
      (let* ([auto-main? (and auto-start-doc?
                              (ormap (can-build? only-dirs) 
                                     (filter doc-under-main? docs)))]
             [auto-user? (and auto-start-doc?
                              (ormap (can-build? only-dirs) 
                                     (filter (lambda (doc) (not (doc-under-main? doc)))
                                             docs)))]
             [infos (filter values (map (get-doc-info only-dirs latex-dest auto-main? auto-user?) 
                                        docs))])
        (let loop ([first? #t] [iter 0])
          (let ([ht (make-hash-table 'equal)])
            ;; Collect definitions
            (for* ([info infos]
                   [k (info-provides info)])
              (let ([prev (hash-table-get ht k #f)])
                (when (and first? prev)
                  (fprintf (current-error-port)
                           "DUPLICATE tag: ~s\n  in: ~a\n and: ~a\n"
                           k
                           (doc-src-file (info-doc prev))
                           (doc-src-file (info-doc info))))
                (hash-table-put! ht k info)))
            ;; Build deps:
            (let ([src->info (make-hash-table 'equal)])
              (for ([i infos])
                (hash-table-put! src->info (doc-src-file (info-doc i)) i))
              (for ([info infos]
                    #:when (info-build? info))
                (let ([one? #f]
                      [added? #f]
                      [deps (make-hash-table)])
                  (set-info-deps!
                   info
                   (map (lambda (d)
                          (if (info? d)
                            d
                            (or (hash-table-get src->info d #f)
                                d)))
                        (info-deps info)))
                  (for ([d (info-deps info)])
                    (let ([i (if (info? d)
                                 d
                                 (hash-table-get src->info d #f))])
                      (if i
                          (hash-table-put! deps i #t)
                          (unless (or (memq 'depends-all (doc-flags (info-doc info)))
                                      (and (doc-under-main? (info-doc i))
                                           (memq 'depends-all-main (doc-flags (info-doc info)))))
                            (set! added? #t)
                            (when (verbose)
                              (printf " [Removed Dependency: ~a]\n"
                                      (doc-src-file (info-doc info))))))))
                  (let ([all-main? (memq 'depends-all-main (doc-flags (info-doc info)))])
                    (when (or (memq 'depends-all (doc-flags (info-doc info)))
                              all-main?)
                      ;; Add all:
                      (when (verbose)
                        (printf " [Adding all~a as dependencies: ~a]\n"
                                (if all-main? " main" "")
                                (doc-src-file (info-doc info))))
                      (for ([i infos])
                        (unless (eq? i info)
                          (when (not (hash-table-get deps i #f))
                            (when (and (or (not all-main?)
                                           (doc-under-main? (info-doc i)))
                                       (not (memq 'no-depend-on (doc-flags (info-doc i)))))
                              (set! added? #t)
                              (hash-table-put! deps i #t)))))))
                  (let ([not-found
                         (lambda (k)
                           (unless one?
                             (fprintf (current-error-port)
                                      "In ~a:\n"
                                      (doc-src-file (info-doc info)))
                             (set! one? #t))
                           (fprintf (current-error-port)
                                    "  undefined tag: ~s\n"
                                    k))])
                    (for ([k (info-undef info)])
                      (let ([i (hash-table-get ht k #f)])
                        (if i
                          (when (not (hash-table-get deps i #f))
                            (set! added? #t)
                            (hash-table-put! deps i #t))
                          (when first?
                            (unless (eq? (car k) 'dep) (not-found k))))))
                    (when first?
                      (for ([(s-key s-ht) (info-searches info)])
                        (unless (ormap (lambda (k) (hash-table-get ht k #f))
                                       (hash-table-map s-ht (lambda (k v) k)))
                          (not-found s-key)))))
                  (when added?
                    (when (verbose)
                      (printf " [Added Dependency: ~a]\n"
                              (doc-src-file (info-doc info))))
                    (set-info-deps! info (hash-table-map deps (lambda (k v) k)))
                    (set-info-need-in-write?! info #t)
                    (set-info-need-run?! info #t)))))
            ;; If a dependency changed, then we need a re-run:
            (for ([i infos]
                  #:when (not (or (info-need-run? i) (not (info-build? i)))))
              (let ([ch (ormap (lambda (i2)
                                 (and (>= (info-out-time i2) (info-time i)) i2))
                               (info-deps i))])
                (when ch
                  (when (verbose)
                    (printf " [Dependency: ~a\n  <- ~a]\n"
                            (doc-src-file (info-doc i))
                            (doc-src-file (info-doc ch))))
                  (set-info-need-run?! i #t))))
            ;; Iterate, if any need to run:
            (when (and (ormap info-need-run? infos) (iter . < . 30))
              ;; Build again, using dependencies
              (for ([i infos] #:when (info-need-run? i))
                (set-info-need-run?! i #f)
                (build-again! latex-dest i))
              (loop #f (add1 iter)))))
        ;; cache info to disk
        (unless latex-dest
          (for ([i infos] #:when (info-need-in-write? i))
            (write-in i)))))))

(define (make-renderer latex-dest doc)
  (if latex-dest
    (new (latex:render-mixin render%)
         [dest-dir latex-dest])
    (new ((if (memq 'multi-page (doc-flags doc)) html:render-multi-mixin values)
          (html:render-mixin render%))
         [dest-dir (if (memq 'multi-page (doc-flags doc))
                     (let-values ([(base name dir?) (split-path (doc-dest-dir doc))])
                       base)
                     (doc-dest-dir doc))]
         [css-path (if (doc-under-main? doc)
                       "../scribble.css"
                       #f)]
         [up-path (if (doc-under-main? doc)
                      "../index.html"
                      #f)])))

(define (pick-dest latex-dest doc)
  (if latex-dest
    (build-path latex-dest (let-values ([(base name dir?) (split-path (doc-src-file doc))])
                             (path-replace-suffix name #".tex")))
    (if (memq 'multi-page (doc-flags doc))
      (doc-dest-dir doc)
      (build-path (doc-dest-dir doc) "index.html"))))

(define ((can-build? only-dirs) doc)
  (or (not only-dirs)
      (ormap (lambda (d)
               (let ([d (path->directory-path d)])
                 (let loop ([dir (path->directory-path (doc-src-dir doc))])
                   (or (equal? dir d)
                       (let-values ([(base name dir?) (split-path dir)])
                         (and (path? base)
                              (loop base)))))))
             only-dirs)))

(define (ensure-doc-prefix v src-file)
  (let ([p (format "~a" (path->main-collects-relative src-file))])
    (when (and (part-tag-prefix v)
               (not (equal? p (part-tag-prefix v))))
      (error 'setup
             "bad tag prefix: ~e for: ~a expected: ~e"
             (part-tag-prefix v)
             src-file
             p))
    (let ([tag-prefix p]
          [tags (if (member '(part "top") (part-tags v))
                  (part-tags v)
                  (cons '(part "top") (part-tags v)))])
      (make-versioned-part tag-prefix
                           tags
                           (part-title-content v)
                           (part-style v)
                           (part-to-collect v)
                           (part-flow v)
                           (part-parts v)
                           (and (versioned-part? v) (versioned-part-version v))))))

(define (omit? cat)
  (or (eq? cat 'omit)
      (and (pair? cat)
           (eq? (car cat) 'omit))))

(define ((get-doc-info only-dirs latex-dest auto-main? auto-user?) doc)
  (let* ([info-out-file (build-path (or latex-dest (doc-dest-dir doc)) "out.sxref")]
         [info-in-file  (build-path (or latex-dest (doc-dest-dir doc)) "in.sxref")]
         [out-file (build-path (doc-dest-dir doc) "index.html")]
         [src-zo (let-values ([(base name dir?) (split-path (doc-src-file doc))])
                   (build-path base "compiled" (path-add-suffix name ".zo")))]
         [renderer (make-renderer latex-dest doc)]
         [can-run? ((can-build? only-dirs) doc)]
         [aux-time (max (file-or-directory-modify-seconds
                         (build-path (collection-path "scribble")
                                     "compiled"
                                     (path-add-suffix
                                      (if latex-dest
                                        "latex-render.ss"
                                        "html-render.ss")
                                      ".zo"))
                         #f (lambda () -inf.0))
                        (file-or-directory-modify-seconds
                         (build-path (collection-path "scribble")
                                     "scribble.css")
                         #f (lambda () +inf.0)))]
         [my-time (file-or-directory-modify-seconds out-file #f (lambda () -inf.0))]
         [info-out-time (file-or-directory-modify-seconds info-out-file #f (lambda () #f))]
         [info-in-time (file-or-directory-modify-seconds info-in-file #f (lambda () #f))]
         [vers (send renderer get-serialize-version)]
         [up-to-date?
          (and info-out-time
               info-in-time
               (or (not can-run?)
                   (my-time . >= . (max aux-time
                                        (file-or-directory-modify-seconds
                                         src-zo #f (lambda () +inf.0))))))]
         [can-run? (and (or (not latex-dest)
                            (not (omit? (doc-category doc))))
                        (or can-run?
                            (and auto-main?
                                 (memq 'depends-all-main (doc-flags doc)))
                            (and auto-user?
                                 (memq 'depends-all (doc-flags doc)))))])
    (printf " [~a ~a]\n"
            (if up-to-date? "Using" (if can-run? "Running" "Skipping"))
            (doc-src-file doc))
    (if up-to-date?
      ;; Load previously calculated info:
      (with-handlers ([exn? (lambda (exn)
                              (fprintf (current-error-port) "~a\n" (exn-message exn))
                              (delete-file info-out-file)
                              (delete-file info-in-file)
                              ((get-doc-info only-dirs latex-dest auto-main? auto-user?) doc))])
        (let* ([v-in (with-input-from-file info-in-file read)]
               [v-out (with-input-from-file info-out-file read)])
          (unless (and (equal? (car v-in) (list vers (doc-flags doc)))
                       (equal? (car v-out) (list vers (doc-flags doc))))
            (error "old info has wrong version or flags"))
          (make-info doc
                     (list-ref v-out 1) ; sci
                     (list-ref v-out 2) ; provides
                     (list-ref v-in 1)  ; undef
                     (list-ref v-in 3)  ; searches
                     (map rel->path (list-ref v-in 2)) ; deps, in case we don't need to build...
                     can-run?
                     my-time info-out-time
                     (and can-run?
                          (memq 'always-run (doc-flags doc)))
                     #f #f
                     vers
                     #f)))
      (if can-run?
        ;; Run the doc once:
        (parameterize ([current-directory (doc-src-dir doc)])
          (let* ([v (ensure-doc-prefix (dynamic-require-doc (doc-src-file doc))
                                       (doc-src-file doc))]
                 [dest-dir (pick-dest latex-dest doc)]
                 [ci (send renderer collect (list v) (list dest-dir))]
                 [ri (send renderer resolve (list v) (list dest-dir) ci)]
                 [out-v (and info-out-time
                             (with-handlers ([exn? (lambda (exn) #f)])
                               (let ([v (with-input-from-file info-out-file read)])
                                 (unless (equal? (car v) (list vers (doc-flags doc)))
                                   (error "old info has wrong version or flags"))
                                 v)))]
                 [sci (send renderer serialize-info ri)]
                 [defs (send renderer get-defined ci)]
                 [searches (resolve-info-searches ri)]
                 [need-out-write?
                  (or (not (equal? (list (list vers (doc-flags doc)) sci defs)
                                   out-v))
                      (info-out-time . > . (current-seconds)))])
            (when (and (verbose) need-out-write?)
              (fprintf (current-error-port) " [New out ~a]\n" (doc-src-file doc)))
            (gc-point)
            (make-info doc
                       sci
                       defs
                       (send renderer get-undefined ri)
                       searches
                       null ; no deps, yet
                       can-run?
                       -inf.0
                       (if need-out-write?
                         (/ (current-inexact-milliseconds) 1000)
                         info-out-time)
                       #t
                       can-run? need-out-write?
                       vers
                       #f)))
        #f))))

(define (build-again! latex-dest info)
  (let* ([doc (info-doc info)]
         [renderer (make-renderer latex-dest doc)])
    (printf " [R~aendering ~a]\n" 
            (if (info-rendered? info) "e-r" "")
            (doc-src-file doc))
    (set-info-rendered?! info #t)
    (parameterize ([current-directory (doc-src-dir doc)])
      (let* ([v (ensure-doc-prefix (dynamic-require-doc (doc-src-file doc))
                                   (doc-src-file doc))]
             [dest-dir (pick-dest latex-dest doc)]
             [ci (send renderer collect (list v) (list dest-dir))])
        (for ([i (info-deps info)])
             (send renderer deserialize-info (info-sci i) ci))
        (let* ([ri (send renderer resolve (list v) (list dest-dir) ci)]
               [sci (send renderer serialize-info ri)]
               [defs (send renderer get-defined ci)]
               [undef (send renderer get-undefined ri)]
               [in-delta? (not (equal? undef (info-undef info)))]
               [out-delta? (not (equal? (list sci defs)
                                        (list (info-sci info)
                                              (info-provides info))))])
          (when (verbose)
            (printf " [~a~afor ~a]\n"
                    (if in-delta? "New in " "")
                    (cond [out-delta? "New out "]
                          [in-delta? ""]
                          [else "No change "])
                    (doc-src-file doc)))
          (when out-delta?
            (set-info-out-time! info (/ (current-inexact-milliseconds) 1000)))
          (set-info-sci! info sci)
          (set-info-provides! info defs)
          (set-info-undef! info undef)
          (when in-delta? (set-info-deps! info null)) ; recompute deps outside
          (when (or out-delta? (info-need-out-write? info))
            (unless latex-dest (write-out info))
            (set-info-need-out-write?! info #f))
          (when in-delta? (set-info-need-in-write?! info #t))
          (unless latex-dest
            (let ([dir (doc-dest-dir doc)])
              (unless (directory-exists? dir) (make-directory dir))
              (for ([f (directory-list dir)]
                    #:when (regexp-match? #"[.]html$" (path-element->bytes f)))
                (delete-file (build-path dir f)))))
          (send renderer render (list v) (list dest-dir) ri)
          (set-info-time! info (/ (current-inexact-milliseconds) 1000))
          (gc-point)
          (void))))))

(define (gc-point)
  ;; Forcing a GC on document boundaries helps keep peak memory use down.
  (collect-garbage))

(define-namespace-anchor anchor)

(define (dynamic-require-doc path)
  ;; Use a separate namespace so that we don't end up with all the documentation
  ;;  loaded at once.
  ;; Use a custodian to compensate for examples executed during the build
  ;;  that may not be entirely clean (e.g., leaves a stuck thread).
  (let ([p (make-empty-namespace)]
        [c (make-custodian)]
        [ch (make-channel)]
        [ns (namespace-anchor->empty-namespace anchor)])
    (parameterize ([current-custodian c])
      (namespace-attach-module ns 'scribble/base-render p)
      (namespace-attach-module ns 'scribble/html-render p)
      ;; This is here for de-serialization; we need a better repair than
      ;;  hard-wiring the "manual.ss" library:
      (namespace-attach-module ns 'scribble/manual p)
      (parameterize ([current-namespace p])
        (call-in-nested-thread (lambda () (dynamic-require path 'doc)))))))

(define (write- info name sel)
  (let* ([doc (info-doc info)]
         [info-file (build-path (doc-dest-dir doc) name)])
    (when (verbose) (printf " [Caching ~a]\n" info-file))
    (with-output-to-file info-file #:exists 'truncate/replace
      (lambda ()
        (write ((sel (lambda ()
                       (list (list (info-vers info) (doc-flags doc))
                             (info-sci info)
                             (info-provides info)))
                     (lambda ()
                       (list (list (info-vers info) (doc-flags doc))
                             (info-undef info)
                             (map (lambda (i)
                                    (path->rel (doc-src-file (info-doc i))))
                                  (info-deps info))
                             (info-searches info))))))))))

(define (write-out info)
  (make-directory* (doc-dest-dir (info-doc info)))
  (write- info "out.sxref" (lambda (o i) o)))
(define (write-in info)
  (make-directory* (doc-dest-dir (info-doc info)))
  (write- info "in.sxref" (lambda (o i) i)))

(define (rel->path r)
  (if (bytes? r)
      (bytes->path r)
      (main-collects-relative->path r)))

(define (path->rel r)
  (let ([r (path->main-collects-relative r)])
    (if (path? r)
        (path->bytes r)
        r)))


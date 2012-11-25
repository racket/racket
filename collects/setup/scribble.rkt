#lang scheme/base

(require "getinfo.rkt"
         "dirs.rkt"
         "path-to-relative.rkt"
         "private/path-utils.rkt"
         "main-collects.rkt"
         "main-doc.rkt"
         "parallel-do.rkt"
         "doc-db.rkt"
         scheme/class
         scheme/list
         scheme/file
         scheme/fasl
         scheme/match
         scheme/serialize
         compiler/cm
         syntax/modread
         scribble/base-render
         scribble/core
         scribble/html-properties
         scribble/manual ; really shouldn't be here... see dynamic-require-doc
         scribble/private/run-pdflatex
         setup/xref
         scribble/xref
         unstable/file
         racket/place
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render)
         (prefix-in contract: scribble/contract-render))

(provide setup-scribblings
         verbose
         run-pdflatex)

(define verbose (make-parameter #t))

(define-logger setup)

(define-serializable-struct doc (src-dir src-spec src-file dest-dir flags under-main? category out-count)
  #:transparent)
(define-serializable-struct info (doc       ; doc structure above
                                  undef     ; unresolved requires
                                  searches 
                                  deps 
                                  known-deps
                                  build? time out-time need-run?
                                  need-in-write? need-out-write?
                                  vers rendered? failed?)
  #:transparent
  #:mutable)

(define (main-doc? doc)
  (pair? (path->main-doc-relative (doc-dest-dir doc))))

(define (filter-user-docs docs make-user?)
  (cond ;; Specifically disabled user stuff, filter
   [(not make-user?) (filter main-doc? docs)]
   ;; If we've built user-specific before, keep building
   [(file-exists? (build-path (find-user-doc-dir) "index.html")) docs]
   ;; Otherwise, see if we need it:
   [(ormap (lambda (doc)
             (not (or (doc-under-main? doc)
                      (memq 'no-depend-on (doc-flags doc)))))
           docs)
    docs]
   [else (filter main-doc? docs)])) ; Don't need them, so drop them

(define (parallel-do-error-handler setup-printf doc errmsg outstr errstr)
  (setup-printf "error running" (module-path-prefix->string (doc-src-spec doc)))
  (eprintf errstr))

;; We use a lock to control writing to the database, because
;; the database or binding doesn't seem to deal well with concurrent
;; writers within a process.
(define no-lock void)
(define (lock-via-channel lock-ch)
  (let ([saved-ch #f])
    (lambda (mode)
      (case mode
        [(lock)
         (define ch (sync lock-ch))
         (place-channel-put ch 'lock)
         (set! saved-ch ch)]
        [(unlock)
         (place-channel-put saved-ch 'done)
         (set! saved-ch #f)]))))
(define lock-ch #f)
(define lock-ch-in #f)
(define (init-lock-ch!)
  (unless lock-ch
    (set!-values (lock-ch lock-ch-in) (place-channel))
    (thread (lambda ()
              (define-values (ch ch-in) (place-channel))
              (let loop ()
                (place-channel-put lock-ch-in ch)
                (place-channel-get ch-in)
                (place-channel-get ch-in)
                (loop))))))
(define (call-with-lock lock thunk)
  (lock 'lock)
  (dynamic-wind
   void
   thunk
   (lambda () (lock 'unlock))))

(define (setup-scribblings
         worker-count       ; number of cores to use to create documentation
         program-name       ; name of program that calls setup-scribblings
         only-dirs          ; limits doc builds
         latex-dest         ; if not #f, generate Latex output
         auto-start-doc?    ; if #t, expands `only-dir' with [user-]start to
                                        ;  catch new docs
         make-user?         ; are we making user stuff?
         with-record-error  ; catch & record exceptions
         setup-printf)
  (unless (doc-db-available?)
    (error 'setup "install SQLite to build documentation"))
  (when latex-dest
    (log-setup-info "latex working directory: ~a" latex-dest))
  (define (scribblings-flag? sym)
    (memq sym '(main-doc main-doc-root user-doc-root user-doc multi-page
                         depends-all depends-all-main no-depend-on always-run)))
  (define (validate-scribblings-infos infos)
    (define (validate path [flags '()] [cat '(library)] [name #f] [out-count 1])
      (and (string? path) (relative-path? path)
           (list? flags) (andmap scribblings-flag? flags)
           (or (not name) (and (path-string? name) (relative-path? name) name))
           (and (list? cat)
                (<= 1 (length cat) 2)
                (symbol? (car cat))
                (or (null? (cdr cat))
                    (real? (cadr cat))))
           (and (exact-positive-integer? out-count))
           (list path flags cat
                 (or name (let-values ([(_1 name _2) (split-path path)])
                            (path-replace-suffix name #"")))
                 out-count)))
    (and (list? infos)
         (let ([infos (map (lambda (i)
                             (and (list? i) (<= 1 (length i) 5)
                                  (apply validate i)))
                           infos)])
           (and (not (memq #f infos)) infos))))
  (define ((get-docs main-dirs) i rec)
    (let ([s (validate-scribblings-infos (i 'scribblings))]
          [dir (directory-record-path rec)])
      (if s
          (map (lambda (d)
                 (let* ([flags (cadr d)]
                        [under-main?
                         (and (not (memq 'main-doc-root flags))
                              (not (memq 'user-doc-root flags))
                              (not (memq 'user-doc flags))
                              (or (memq 'main-doc flags)
                                  (hash-ref main-dirs dir #f)
                                  (pair? (path->main-collects-relative dir))))])
                   (make-doc dir
                             (let ([spec (directory-record-spec rec)])
                               (list* (car spec)
                                      (car d)
                                      (if (eq? 'planet (car spec))
                                          (list (append (cdr spec)
                                                        (list (directory-record-maj rec)
                                                              (list '= (directory-record-min rec)))))
                                          (cdr spec))))
                             (simplify-path (build-path dir (car d)) #f)
                             (doc-path dir (cadddr d) flags under-main?)
                             flags under-main? (caddr d)
                             (list-ref d 4))))
               s)
          (begin (setup-printf
                  "WARNING"
                  "bad 'scribblings info: ~e from: ~e" (i 'scribblings) dir)
                 null))))
  (log-setup-info "getting documents")
  (define docs
    (let* ([recs (find-relevant-directory-records '(scribblings) 'all-available)]
           [main-dirs (parameterize ([current-library-collection-paths
                                      (list (find-collects-dir))])
                        (for/hash ([k (in-list (find-relevant-directories '(scribblings) 'no-planet))])
                          (values k #t)))]
           [infos (map get-info/full (map directory-record-path recs))])
      (filter-user-docs (append-map (get-docs main-dirs) infos recs) make-user?)))
  (define-values (main-docs user-docs) (partition doc-under-main? docs))
  (define (can-build*? docs) (can-build? only-dirs docs))
  (define auto-main? (and auto-start-doc? (ormap can-build*? main-docs)))
  (define auto-user? (and auto-start-doc? (ormap can-build*? user-docs)))
  (define force-out-of-date? (not (file-exists? (find-doc-db-path latex-dest #f))))
  (log-setup-info "getting document information")
  (define infos
    (and (ormap can-build*? docs)
         (filter 
          values
          (if ((min worker-count (length docs)) . < . 2)
              ;; non-parallel version:
              (map (get-doc-info only-dirs latex-dest auto-main? auto-user? 
                                 with-record-error setup-printf #f
                                 #f force-out-of-date?
                                 no-lock)
                   docs)
              ;; maybe parallel...
              (or
               (let ([infos (map (get-doc-info only-dirs latex-dest auto-main? auto-user? 
                                               with-record-error setup-printf #f 
                                               ;; only-fast:
                                               #t
                                               force-out-of-date?
                                               no-lock)
                                 docs)])
                 ;; check fast result
                 (and (andmap values infos)
                      infos))
               ;; parallel:
               (parallel-do 
                (min worker-count (length docs))
                (lambda (workerid)
                  (init-lock-ch!)
                  (list workerid program-name (verbose) only-dirs latex-dest auto-main? auto-user?
                        force-out-of-date? lock-ch))
                (list-queue
                 docs
                 (lambda (x workerid) (s-exp->fasl (serialize x)))
                 (lambda (work r outstr errstr) 
                   (printf "~a" outstr)
                   (printf "~a" errstr)
                   (deserialize (fasl->s-exp r)))
                 (lambda (work errmsg outstr errstr) 
                   (parallel-do-error-handler setup-printf work errmsg outstr errstr)))
                (define-worker (get-doc-info-worker workerid program-name verbosev only-dirs latex-dest 
                                                    auto-main? auto-user? force-out-of-date? lock-ch)
                  (define ((get-doc-info-local program-name only-dirs latex-dest auto-main? auto-user? 
                                               force-out-of-date? lock
                                               send/report) 
                           doc)
                    (define (setup-printf subpart formatstr . rest)
                      (let ([task (if subpart
                                      (format "~a: " subpart)
                                      "")])
                        (send/report
                         (format "~a: ~a~a\n" program-name task (apply format formatstr rest)))))
                    (define (with-record-error cc go fail-k)
                      (with-handlers ([exn:fail?
                                       (lambda (exn)
                                         ((error-display-handler) (exn-message exn) exn)
                                         (raise exn))])
                        (go)))
                    (s-exp->fasl (serialize 
                                  ((get-doc-info only-dirs latex-dest auto-main? auto-user?  
                                                 with-record-error setup-printf workerid
                                                 #f force-out-of-date? lock)
                                   (deserialize (fasl->s-exp doc))))))
                  
                  (verbose verbosev)
                  (match-message-loop
                   [doc (send/success 
                         ((get-doc-info-local program-name only-dirs latex-dest auto-main? auto-user? 
                                              force-out-of-date? (lock-via-channel lock-ch)
                                              send/report) 
                          doc))]))))))))

  (define (out-path->info path infos out-path->info-cache)
    (or (hash-ref out-path->info-cache path #f)
        (let ([filename (main-doc-relative->path path)])
          (for*/or ([i (in-list infos)]
                    [c (in-range (add1 (doc-out-count (info-doc i))))])
            (and (equal? (sxref-path latex-dest (info-doc i) (format "out~a.sxref" c))
                         filename)
                 (hash-set! out-path->info-cache path i)
                 i)))))

  (define (make-loop first? iter)
    (let ([infos (filter-not info-failed? infos)]
          [src->info (make-hash)]
          [out-path->info-cache (make-hash)]
          [main-db (find-doc-db-path latex-dest #f)]
          [user-db (find-doc-db-path latex-dest #t)])
      (unless only-dirs
        (log-setup-info "cleaning database")
        (define files (make-hash))
        (define (get-files! main?)
          (for ([i (in-list infos)]
                #:when (eq? main? (main-doc? (info-doc i))))
            (define doc (info-doc i))
            (hash-set! files (sxref-path latex-dest doc "in.sxref") #t)
            (for ([c (in-range (add1 (doc-out-count doc)))])
              (hash-set! files (sxref-path latex-dest doc (format "out~a.sxref" c)) #t))))
        (get-files! #t)
        (doc-db-clean-files main-db files)
        (when (and (file-exists? user-db)
                   (not (equal? main-db user-db)))
          (get-files! #f)
          (doc-db-clean-files user-db files)))
      ;; Check for duplicate definitions
      (when first?
        (log-setup-info "checking for duplicates")
        (let ([dups (append
                     (doc-db-check-duplicates main-db #:main-doc-relative-ok? #t)
                     (if (and make-user?
                              (file-exists? user-db)
                              (not (equal? main-db user-db)))
                         (doc-db-check-duplicates user-db #:attach main-db #:main-doc-relative-ok? #t)
                         null))])
          (for ([dup dups])
            (let ([k (car dup)]
                  [paths (cdr dup)])
              (setup-printf "WARNING" "duplicate tag: ~s" k)
              (for ([path paths])
                (define i (out-path->info path infos out-path->info-cache))
                (setup-printf #f " in: ~a" (if i
                                               (doc-src-file (info-doc i))
                                               "<unknown>")))))))
      ;; Build deps:
      (log-setup-info "determining dependencies")
      (for ([i infos])
        (hash-set! src->info (doc-src-file (info-doc i)) i))
      (for ([info infos] #:when (info-build? info))
        (let ([one? #f]
              [added? #f]
              [deps (make-hasheq)]
              [known-deps (make-hasheq)]
              [all-main? (memq 'depends-all-main (doc-flags (info-doc info)))])
          ;; Convert current deps from paths to infos, keeping paths that have no info
          (set-info-deps!
           info
           (map (lambda (d)
                  (if (info? d) d (or (hash-ref src->info d #f) d)))
                (info-deps info)))
          (unless (andmap info? (info-deps info))
            (set-info-need-in-write?! info #t))
          ;; Propagate existing dependencies as expected dependencies:
          (for ([d (info-deps info)])
            (let ([i (if (info? d) d (hash-ref src->info d #f))])
              (if i
                  ;; Normal case:
                  (hash-set! deps i #t)
                  ;; Path has no info; normally keep it as expected, and it gets
                  ;; removed later.
                  (unless (or (memq 'depends-all (doc-flags (info-doc info)))
                              (and (if (info? d)
                                       (doc-under-main? (info-doc d))
                                       (not (path? (path->main-collects-relative d))))
                                   all-main?))
                    (set! added? #t)
                    (when (verbose)
                      (printf " [Removed Dependency: ~a]\n"
                              (doc-src-file (info-doc info))))))))
          (when (or (memq 'depends-all (doc-flags (info-doc info))) all-main?)
            ;; Add all as expected dependency:
            (when (verbose)
              (printf " [Adding all~a as dependencies: ~a]\n"
                      (if all-main? " main" "")
                      (doc-src-file (info-doc info))))
            (for ([i infos])
              (when (and (not (eq? i info))
                         (not (hash-ref deps i #f))
                         (or (not all-main?) (doc-under-main? (info-doc i)))
                         (not (memq 'no-depend-on (doc-flags (info-doc i)))))
                (set! added? #t)
                (hash-set! deps i #t))))
          ;; Add definite dependencies based on referenced keys
          (let ([not-found
                 (lambda (k)
                   (unless (or (memq 'depends-all (doc-flags (info-doc info)))
                               (memq 'depends-all-main (doc-flags (info-doc info))))
                     (unless one?
                       (setup-printf
                        "WARNING" "undefined tag in ~a:"
                        (path->relative-string/setup
                         (doc-src-file (info-doc info))))
                       (set! one? #t))
                     (setup-printf #f " ~s" k)))])
            (let* ([filename (sxref-path latex-dest (info-doc info) "in.sxref")]
                   [as-user? (and (not (main-doc? (info-doc info)))
                                  (not (equal? main-db user-db)))]
                   [found-deps (doc-db-get-dependencies filename
                                                        (if as-user? user-db main-db)
                                                        #:attach (if as-user? main-db #f)
                                                        #:main-doc-relative-ok? #t)]
                   [missing (if first?
                                (doc-db-check-unsatisfied filename
                                                          (if as-user? user-db main-db)
                                                          #:attach (if as-user? main-db #f))
                                null)])
              (for ([found-dep (in-list found-deps)])
                ;; Record a definite dependency:
                (define i (out-path->info found-dep infos out-path->info-cache))
                (when (not (hash-ref known-deps i #f))
                  (hash-set! known-deps i #t))
                      ;; Record also in the expected-dependency list:
                      (when (not (hash-ref deps i #f))
                        (set! added? #t)
                        (when (verbose)
                          (printf " [Adding... ~a]\n"
                                  (doc-src-file (info-doc i))))
                        (hash-set! deps i #t)))
              (for ([s-key (in-list missing)])
                (not-found s-key))))
          ;; If we added anything (expected or known), then mark as needed to run
          (when added?
            (when (verbose)
              (printf " [Added Dependency: ~a]\n"
                      (doc-src-file (info-doc info))))
            (set-info-deps! info (hash-map deps (lambda (k v) k)))
            (set-info-known-deps! info (hash-map known-deps (lambda (k v) k)))
            (set-info-need-in-write?! info #t)
            (set-info-need-run?! info #t))))
      ;; If any expected dependency changed, then we need a re-run:
      (for ([i infos]
            #:unless (or (info-need-run? i) (not (info-build? i))))
        (let ([ch (ormap (lambda (i2)
                           (or (and (not (info? i2))
                                    i2)
                               (and (>= (info-out-time i2) (info-time i)) i2)))
                         (info-deps i))])
          (when ch
            (when (verbose)
              (printf " [Dependency: ~a\n  <- ~a]\n"
                      (doc-src-file (info-doc i))
                      (if (info? ch)
                          (doc-src-file (info-doc ch))
                          ch)))
            (set-info-need-run?! i #t))))
      ;; Iterate, if any need to run:
      (when (and (ormap info-need-run? infos) (iter . < . 30))
        (log-setup-info "building")
        ;; Build again, using dependencies
        (let ([need-rerun (filter-map (lambda (i) 
                                        (and (info-need-run? i)
                                             (begin
                                               (when (info-need-in-write? i) 
                                                 (write-in/info latex-dest i no-lock)
                                                 (set-info-need-in-write?! i #f))
                                               (set-info-deps! i (filter info? (info-deps i)))
                                               (set-info-need-run?! i #f)
                                               i)))
                                      infos)])
          (define (say-rendering i workerid)
            (setup-printf (string-append
                           (if workerid (format "~a " workerid) "")
                           (if (info-rendered? i) "re-rendering" "rendering") )
                          "~a"
                          (path->relative-string/setup (doc-src-file (info-doc i)))))
          (define (update-info info response)
            (match response 
              [#f (set-info-failed?! info #t)]
              [(list in-delta? out-delta? undef searches)
               (set-info-rendered?! info #t)
               (set-info-undef! info undef)
               (set-info-searches! info searches)
               (when out-delta?
                 (set-info-out-time! info (/ (current-inexact-milliseconds) 1000)))
               (when in-delta? 
                 ;; Reset expected dependencies to known dependencies, and recompute later:
                 (set-info-deps! info (info-known-deps info))
                 (set-info-need-in-write?! info #t))
               (set-info-time! info (/ (current-inexact-milliseconds) 1000))]))
          (if ((min worker-count (length need-rerun)) . < . 2)
              (map (lambda (i) 
                     (say-rendering i #f)
                     (update-info i (build-again! latex-dest i with-record-error no-lock))) 
                   need-rerun)
              (parallel-do 
               (min worker-count (length need-rerun))
               (lambda (workerid)
                 (init-lock-ch!)
                 (list workerid (verbose) latex-dest lock-ch))
               (list-queue
                need-rerun
                (lambda (i workerid) 
                  (say-rendering i workerid)
                  (s-exp->fasl (serialize (info-doc i))))
                (lambda (i r outstr errstr) 
                  (printf "~a" outstr) 
                  (printf "~a" errstr)
                  (update-info i (deserialize (fasl->s-exp r))))
                (lambda (i errmsg outstr errstr) 
                  (parallel-do-error-handler setup-printf (info-doc i) errmsg outstr errstr)))
               (define-worker (build-again!-worker2 workerid verbosev latex-dest lock-ch)
                 (define (with-record-error cc go fail-k)
                   (with-handlers ([exn:fail?
                                    (lambda (x)
                                      ((error-display-handler) (exn-message x) x)
                                      (raise x))])
                     (go)))
                 (verbose verbosev)
                 (match-message-loop
                  [info 
                   (send/success 
                    (s-exp->fasl (serialize (build-again! latex-dest
                                                          (deserialize (fasl->s-exp info))
                                                          with-record-error
                                                          (lock-via-channel lock-ch)))))])))))
        ;; If we only build 1, then it reaches it own fixpoint
        ;; even if the info doesn't seem to converge immediately.
        ;; This is a useful shortcut when re-building a single
        ;; document.
        (unless (= 1 (for/fold ([count 0])
                         ([i infos]
                          #:when (info-build? i))
                       (add1 count)))
          (make-loop #f (add1 iter))))))
  (when infos
    (make-loop #t 0)
    ;; cache info to disk
    (for ([i infos] #:when (info-need-in-write? i)) (write-in/info latex-dest i no-lock))))

(define (make-renderer latex-dest doc)
  (if latex-dest
      (new (latex:render-mixin render%)
           [dest-dir latex-dest]
           ;; Use PLT manual style:
           [prefix-file (collection-file-path "manual-prefix.tex" "scribble")]
           [style-file (collection-file-path "manual-style.tex" "scribble")]
           ;; All .tex files go to the same directory, so prefix
           ;; generated/copied file names to keep them separate:
           [helper-file-prefix (let-values ([(base name dir?) (split-path
                                                               (doc-dest-dir doc))])
                                 (path-element->string name))])
      (let* ([flags (doc-flags doc)]
             [multi? (memq 'multi-page flags)]
             [main?  (doc-under-main? doc)]
             [ddir   (doc-dest-dir doc)]
             [root?  (or (memq 'main-doc-root flags)
                         (memq 'user-doc-root flags))]
             [contract-override-mixin
              (if multi?
                  contract:override-render-mixin-multi 
                  contract:override-render-mixin-single)])
        (new (contract-override-mixin
              ((if multi? html:render-multi-mixin values)
               (html:render-mixin render%)))
             [dest-dir (if multi?
                           (let-values ([(base name dir?) (split-path ddir)]) base)
                           ddir)]
             [alt-paths   (if main?
                              (let ([std-path (lambda (s)
                                                (cons (collection-file-path s "scribble")
                                                      (format "../~a" s)))])
                                (list (std-path "scribble.css")
                                      (std-path "scribble-style.css")
                                      (std-path "racket.css")
                                      (std-path "scribble-common.js")))
                              null)]
             ;; For main-directory, non-start files, up-path is #t, which makes the
             ;; "up" link go to the (user's) start page using cookies. For other files,
             ;; 
             [up-path     (and (not root?)
                               (if main?
                                   #t
                                   (build-path (find-user-doc-dir) "index.html")))]
             [search-box? #t]))))

(define (pick-dest latex-dest doc)
  (cond [(path? latex-dest)
         (let-values ([(base name dir?) (split-path (doc-src-file doc))])
           (build-path latex-dest (path-replace-suffix name #".tex")))]
        [(not latex-dest)
         (cond
           [(memq 'multi-page (doc-flags doc)) (doc-dest-dir doc)]
           [else (build-path (doc-dest-dir doc) "index.html")])]))

(define (sxref-path latex-dest doc file)
  (cond [(path? latex-dest)
         (let-values ([(base name dir?) (split-path (doc-src-file doc))])
           (build-path latex-dest (path-replace-suffix name (string-append "." file))))]
        [(not latex-dest) (build-path (doc-dest-dir doc) file)]))

(define (find-doc-db-path latex-dest user?)
  (cond
   [latex-dest
    (build-path latex-dest "docindex.sqlite")]
   [else
    (build-path (if user? (find-user-doc-dir) (find-doc-dir)) "docindex.sqlite")]))

(define (can-build? only-dirs doc)
  (or (not only-dirs)
      (ormap (lambda (d)
               (let ([d (path->directory-path d)])
                 (let loop ([dir (path->directory-path (doc-src-dir doc))])
                   (or (equal? dir d)
                       (let-values ([(base name dir?) (split-path dir)])
                         (and (path? base) (loop base)))))))
             only-dirs)))

(define (load-doc/ensure-prefix doc)
  (define (ensure-doc-prefix v src-spec)
    (let ([p (module-path-prefix->string src-spec)])
      (when (and (part-tag-prefix v)
                 (not (equal? p (part-tag-prefix v))))
        (error 'setup
               "bad tag prefix: ~e for: ~a expected: ~e"
               (part-tag-prefix v)
               src-spec
               p))
      (let ([tag-prefix p]
            [tags (if (member '(part "top") (part-tags v))
                      (part-tags v)
                      (cons '(part "top") (part-tags v)))]
            [style (part-style v)])
        (make-part
         tag-prefix
         tags
         (part-title-content v)
         (let* ([v (style-properties style)]
                [v (if (ormap body-id? v)
                       v
                       (cons (make-body-id "doc-racket-lang-org")
                             v))]
                [v (if (ormap document-version? v)
                       v
                       (cons (make-document-version (version))
                             v))])
           (make-style (style-name style) v))
         (part-to-collect v)
         (part-blocks v)
         (part-parts v)))))
  (ensure-doc-prefix
   (dynamic-require-doc (doc-src-spec doc))
   (doc-src-spec doc)))

(define (omit? cat)
  (or (eq? cat 'omit)
      (and (pair? cat)
           (eq? (car cat) 'omit))))

(define (any-order keys)
  (let ([ht (make-hash)])
    (for-each (lambda (k) (hash-set! ht k #t)) keys)
    ht))

(define (load-sxref filename #:skip [skip 0])
  (call-with-input-file* filename 
    (lambda (x) 
      (for ([i skip]) (fasl->s-exp x))
      (fasl->s-exp x))))

(define (file-or-directory-modify-seconds/stamp file
                                                stamp-time stamp-data pos
                                                get-sha1)
  (let ([t (file-or-directory-modify-seconds file #f (lambda () +inf.0))])
    (cond
     [(t . <= . stamp-time) stamp-time]
     [(equal? (list-ref stamp-data pos) (get-sha1 file)) stamp-time]
     [else t])))

(define (find-db-file doc latex-dest)
  (define p (find-doc-db-path latex-dest (not (main-doc? doc))))
  (define-values (base name dir?) (split-path p))
  (unless (directory-exists? base)
    (make-directory* base))
  p)

(define ((get-doc-info only-dirs latex-dest auto-main? auto-user?
                       with-record-error setup-printf workerid 
                       only-fast? force-out-of-date? lock)
         doc)
  (let* ([info-out-files (for/list ([i (add1 (doc-out-count doc))])
                           (sxref-path latex-dest doc (format "out~a.sxref" i)))]
         [info-in-file  (sxref-path latex-dest doc "in.sxref")]
         [db-file (find-db-file doc latex-dest)]
         [stamp-file  (sxref-path latex-dest doc "stamp.sxref")]
         [out-file (build-path (doc-dest-dir doc) "index.html")]
         [src-zo (let-values ([(base name dir?) (split-path (doc-src-file doc))])
                   (build-path base "compiled" (path-add-suffix name ".zo")))]
         [renderer (make-renderer latex-dest doc)]
         [can-run? (can-build? only-dirs doc)]
         [stamp-time (file-or-directory-modify-seconds stamp-file #f (lambda () -inf.0))]
         [stamp-data (with-handlers ([exn:fail:filesystem? (lambda (exn) (list "" "" ""))])
                       (let ([v (call-with-input-file* stamp-file read)])
                         (if (and (list? v)
                                  (= 3 (length v))
                                  (andmap string? v))
                             v
                             (list "" "" ""))))]
         [renderer-path (build-path (collection-path "scribble")
                                    "compiled"
                                    (path-add-suffix
                                     (cond
                                       [(path? latex-dest) "latex-render.rkt"]
                                       [(not latex-dest) "html-render.rkt"])
                                     ".zo"))]
         [css-path (collection-file-path "scribble.css" "scribble")]
         [aux-time (max (file-or-directory-modify-seconds/stamp
                         renderer-path
                         stamp-time stamp-data 1
                         get-compiled-file-sha1)
                        (file-or-directory-modify-seconds/stamp
                         css-path
                         stamp-time stamp-data 2
                         get-file-sha1))]
         [my-time (file-or-directory-modify-seconds out-file #f (lambda () -inf.0))]
         [info-out-time (for/fold ([t +inf.0]) ([info-out-file info-out-files])
                          (and t
                               (let ([t2 (file-or-directory-modify-seconds info-out-file #f (lambda () #f))])
                                 (and t2 (min t t2)))))]
         [info-in-time (file-or-directory-modify-seconds info-in-file #f (lambda () #f))]
         [info-time (min (or info-out-time -inf.0) (or info-in-time -inf.0))]
         [vers (send renderer get-serialize-version)]
         [src-time (file-or-directory-modify-seconds/stamp
                    src-zo
                    stamp-time stamp-data 0
                    get-compiled-file-sha1)]
         [up-to-date?
          (and (not force-out-of-date?)
               info-out-time
               info-in-time
               (or (not can-run?)
                   ;; Need to rebuild if output file is older than input:
                   (my-time . >= . src-time)
                   ;; But we can use in/out information if they're already built;
                   ;; this is mostly useful if we interrupt setup-plt after
                   ;; it runs some documents without rendering them:
                   (info-time . >= . src-time)))]
         [can-run? (and (or (not latex-dest)
                            (not (omit? (doc-category doc))))
                        (or can-run?
                            (and auto-main?
                                 (memq 'depends-all-main (doc-flags doc)))
                            (and auto-user?
                                 (memq 'depends-all (doc-flags doc)))))])
    (when (or (and (not up-to-date?) (not only-fast?))
              (verbose))
      (setup-printf
       (string-append
        (if workerid (format "~a " workerid) "")
        (cond 
         [up-to-date? "using"] 
         [can-run? (if only-fast?
                       "checking"
                       "running")]
         [else "skipping"]))
       "~a"
       (path->relative-string/setup (doc-src-file doc))))
    
    (when force-out-of-date?
      (for ([p (in-list info-out-files)])
        (when (file-exists? p)
          (delete-file p))))

    (if up-to-date?
        ;; Load previously calculated info:
        (render-time
         "use"
         (with-handlers ([exn:fail? (lambda (exn)
                                      (log-error (format "get-doc-info error: ~a"
                                                         (exn-message exn)))
                                      (for-each delete-file info-out-files)
                                      (delete-file info-in-file)
                                      ((get-doc-info only-dirs latex-dest auto-main?
                                                     auto-user? with-record-error
                                                     setup-printf workerid #f #f lock)
                                       doc))])
           (let ([v-in  (load-sxref info-in-file)])
             (unless (equal? (car v-in) (list vers (doc-flags doc)))
               (error "old info has wrong version or flags"))
             (make-info
              doc
              'delayed
              'delayed
              (map rel->path (list-ref v-in 1)) ; expected deps, in case we don't need to build...
              null ; known deps (none at this point)
              can-run?
              my-time info-out-time
              (and can-run? (memq 'always-run (doc-flags doc)))
              #f 
              #f
              vers
              #f
              #f))))
        (if (and can-run? 
                 (not only-fast?))
            ;; Run the doc once:
            (with-record-error
             (doc-src-file doc)
             (lambda ()
               (parameterize ([current-directory (doc-src-dir doc)])
                 (let* ([v (load-doc/ensure-prefix doc)]
                        [dest-dir (pick-dest latex-dest doc)]
                        [fp (send renderer traverse (list v) (list dest-dir))]
                        [ci (send renderer collect (list v) (list dest-dir) fp)]
                        [ri (send renderer resolve (list v) (list dest-dir) ci)]
                        [out-vs (and info-out-time
                                     (info-out-time . >= . src-time)
                                     (with-handlers ([exn:fail? (lambda (exn) #f)])
                                       (for/list ([info-out-file info-out-files])
                                         (let ([v (load-sxref info-out-file)])
                                           (unless (equal? (car v) (list vers (doc-flags doc)))
                                             (error "old info has wrong version or flags"))
                                           v))))]
                        [scis (send renderer serialize-infos ri (add1 (doc-out-count doc)) v)]
                        [defss (send renderer get-defineds ci (add1 (doc-out-count doc)) v)]
                        [undef (send renderer get-external ri)]
                        [searches (resolve-info-searches ri)]
                        [need-out-write?
                         (or force-out-of-date?
                             (not out-vs)
                             (not (for/and ([out-v out-vs])
                                    (equal? (list vers (doc-flags doc))
                                            (car out-v))))
                             (not (for/and ([sci scis]
                                            [out-v out-vs])
                                    (serialized=? sci (cadr out-v))))
                             (info-out-time . > . (current-seconds)))])
                   (when (and (verbose) need-out-write?)
                     (eprintf " [New out ~a]\n" (doc-src-file doc)))
                   (gc-point)
                   (let ([info
                          (make-info doc
                                     undef
                                     searches
                                     null ; no deps, yet
                                     null ; no known deps, yet
                                     can-run?
                                     -inf.0
                                     (if need-out-write?
                                         (/ (current-inexact-milliseconds) 1000)
                                         info-out-time)
                                     #t
                                     can-run?
                                     need-out-write?
                                     vers
                                     #f
                                     #f)])
                     (when need-out-write?
                       (render-time "xref-out" (write-out/info latex-dest info scis defss db-file lock))
                       (set-info-need-out-write?! info #f))
                     (when (info-need-in-write? info)
                       (render-time "xref-in" (write-in/info latex-dest info lock))
                       (set-info-need-in-write?! info #f))

                     (when (or (stamp-time . < . aux-time)
                               (stamp-time . < . src-time))
                       (let ([data (list (get-compiled-file-sha1 src-zo)
                                         (get-compiled-file-sha1 renderer-path)
                                         (get-file-sha1 css-path))])
                         (with-compile-output stamp-file (lambda (out tmp-filename) (write data out)))
                         (let ([m (max aux-time src-time)])
                           (unless (equal? m +inf.0)
                             (file-or-directory-modify-seconds stamp-file m)))))
                     info))))
             (lambda () #f))
            #f))))

(define (read-delayed-in! info latex-dest)
  (let* ([doc (info-doc info)]
         [info-in-file (sxref-path latex-dest doc "in.sxref")]
         [v-in (load-sxref info-in-file #:skip 1)])
    (if (and (equal? (car v-in) (list (info-vers info) (doc-flags doc))))
        ;; version is ok:
        (let ([undef+searches
               (let ([v (list-ref v-in 1)])
                 (with-my-namespace
                  (lambda ()
                    (deserialize v))))])
          (set-info-undef! info (car undef+searches))
          (set-info-searches! info (cadr undef+searches)))
        ;; version was bad:
        (begin
          (set-info-undef! info null)
          (set-info-searches! info #hash())))))

(define (make-prod-thread)
  ;; periodically dumps a stack trace, which can give us some idea of
  ;; what the main thread is doing; usually used in `render-time'.
  (let ([t (current-thread)])
    (thread (lambda ()
              (let loop ()
                (sleep 0.05)
                (for-each (lambda (i)
                            (printf "~s\n" i))
                          (continuation-mark-set->context (continuation-marks t)))
                (newline)
                (loop))))))

(define-syntax-rule (render-time what expr)
  (do-render-time 
   what 
   (lambda () expr)))

(define (do-render-time what thunk)
  (define start (current-process-milliseconds))
  (begin0
   (thunk)
   (let ([end (current-process-milliseconds)])
     (log-setup-debug "~a: ~a msec" what (- end start)))))

(define (load-sxrefs latex-dest doc vers)
  (define in-filename (sxref-path latex-dest doc "in.sxref"))
  (match (list (load-sxref in-filename)
               (load-sxref in-filename #:skip 1)
               (for/list ([i (add1 (doc-out-count doc))])
                 (load-sxref (sxref-path latex-dest doc (format "out~a.sxref" i)))))
    [(list (list in-version deps-rel)
           (list in-version2 undef+searches) 
           (list (list out-versions scis) ...))
     (define expected (list vers (doc-flags doc)))
     (unless (and (equal? in-version expected)
                  (equal? in-version2 expected)
                  (for/and ([out-version out-versions])
                    (equal? out-version expected)))
       (error "old info has wrong version or flags"))
     (match (with-my-namespace
             (lambda ()
               (deserialize undef+searches)))
       [(list undef searches)
        (with-my-namespace*
         (values undef
                 deps-rel
                 searches
                 scis))])]))

(define (build-again! latex-dest info with-record-error lock)
  (define (cleanup-dest-dir doc)
    (unless latex-dest
      (let ([dir (doc-dest-dir doc)])
        (if (not (directory-exists? dir))
            (make-directory*/ignore-exists-exn dir)
            (for ([f (directory-list dir)]
                  #:when
                  (and (file-exists? f)
                       (not (regexp-match? #"[.]sxref$"
                                           (path-element->bytes f)))))
              (delete-file (build-path dir f)))))))
  (define (load-doc-scis doc)
    (map cadr (for/list ([i (add1 (doc-out-count doc))])
                (load-sxref (sxref-path latex-dest doc (format "out~a.sxref" i))))))
  (define doc (if (info? info) (info-doc info) info))
  (define renderer (make-renderer latex-dest doc))
  (with-record-error
   (doc-src-file doc)
   (lambda ()
     (define vers (send renderer get-serialize-version))
     (define-values (ff-undef ff-deps-rel ff-searches ff-scis)
       (if (info? info)
           (begin
             (when (eq? 'delayed (info-undef info))
               (read-delayed-in! info latex-dest))
             (values (info-undef info) 
                     (info-deps->rel-doc-src-file info)
                     (info-searches info)
                     (load-doc-scis doc)))
           (load-sxrefs latex-dest doc vers)))
     
     (parameterize ([current-directory (doc-src-dir doc)])
       (let* ([v (render-time "load" (load-doc/ensure-prefix doc))]
              [dest-dir (pick-dest latex-dest doc)]
              [fp (render-time "traverse" (send renderer traverse (list v) (list dest-dir)))]
              [ci (render-time "collect" (send renderer collect (list v) (list dest-dir) fp))]
              [ri (begin 
                    (xref-transfer-info renderer ci (make-collections-xref 
                                                     #:no-user? (main-doc? doc)
                                                     #:doc-db (and latex-dest
                                                                   (find-doc-db-path latex-dest #t))))
                    (render-time "resolve" (send renderer resolve (list v) (list dest-dir) ci)))]
              [scis (render-time "serialize" (send renderer serialize-infos ri (add1 (doc-out-count doc)) v))]
              [defss (render-time "defined" (send renderer get-defineds ci (add1 (doc-out-count doc)) v))]
              [undef (render-time "undefined" (send renderer get-external ri))]
              [searches (render-time "searches" (resolve-info-searches ri))]
              [in-delta? (not (and (equal? (any-order undef) (any-order ff-undef))
                                   (equal? searches ff-searches)))]
              [out-delta? (not (for/and ([sci scis]
                                         [ff-sci ff-scis])
                                 (serialized=? sci ff-sci)))]
              [db-file (find-db-file doc latex-dest)])
         (when (verbose)
           (printf " [~a~afor ~a]\n"
                   (if in-delta? "New in " "")
                   (cond [out-delta? "New out "]
                         [in-delta? ""]
                         [else "No change "])
                   (doc-src-file doc)))

         (when in-delta?
           (render-time "xref-in" (write-in latex-dest vers doc undef ff-deps-rel searches db-file lock)))
         (when out-delta?
           (render-time "xref-out" (write-out latex-dest vers doc scis defss db-file lock)))

         (cleanup-dest-dir doc)
         (render-time
          "render"
          (with-record-error
           (doc-src-file doc)
           (lambda () (send renderer render (list v) (list dest-dir) ri))
           void))
         (gc-point)
         (list in-delta? out-delta? undef searches))))
   (lambda () #f)))

(define (gc-point)
  ;; Forcing a GC on document boundaries helps keep peak memory use down.
  (collect-garbage))

(define-namespace-anchor anchor)

(define (with-my-namespace thunk)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    (thunk)))

(define-syntax-rule (with-my-namespace* body ...)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    body ...))

(define (dynamic-require-doc mod-path)
  ;; Use a separate namespace so that we don't end up with all the
  ;;  documentation loaded at once.
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
      ;;  hard-wiring the "manual.rkt" library:
      (namespace-attach-module ns 'scribble/manual p)
      (parameterize ([current-namespace p])
        (call-in-nested-thread (lambda () (dynamic-require mod-path 'doc)))))))

(define (write- latex-dest vers doc name datas prep!)
  (let* ([filename (sxref-path latex-dest doc name)])
    (prep! filename)
    (when (verbose) (printf " [Caching to disk ~a]\n" filename))
    (make-directory*/ignore-exists-exn (doc-dest-dir doc))
    (with-compile-output 
     filename 
     (lambda (out tmp-filename)
       (for ([data (in-list datas)])
         (write-bytes (s-exp->fasl (append (list (list vers (doc-flags doc))) 
                                           data)) 
                      out))))))

(define (write-out latex-dest vers doc scis providess db-file lock)
  (for ([i (add1 (doc-out-count doc))]
        [sci scis]
        [provides providess])
    (write- latex-dest vers doc (format "out~a.sxref" i)
            (list (list sci))
            (lambda (filename)
              (call-with-lock
               lock
               (lambda ()
                 (doc-db-clear-provides db-file filename)
                 (doc-db-add-provides db-file provides filename)))))))

(define (write-out/info latex-dest info scis providess db-file lock)
  (write-out latex-dest (info-vers info) (info-doc info) scis providess db-file lock))

(define (write-in latex-dest vers doc undef rels searches db-file lock)
  (write- latex-dest vers doc "in.sxref" 
          (list (list rels)
                (list (serialize (list undef
                                       searches))))
          (lambda (filename)
            (call-with-lock
             lock
             (lambda ()
               (doc-db-clear-dependencies db-file filename)
               (doc-db-clear-searches db-file filename)
               (doc-db-add-dependencies db-file undef filename)
               (doc-db-add-searches db-file searches filename))))))

(define (write-in/info latex-dest info lock)
  (when (eq? 'delayed (info-undef info))
    (read-delayed-in! info latex-dest))
  (write-in latex-dest
            (info-vers info)
            (info-doc info)
            (info-undef info)
            (info-deps->rel-doc-src-file info)
            (info-searches info)
            (find-db-file (info-doc info) latex-dest)
            lock))

(define (rel->path r)
  (if (bytes? r)
      (bytes->path r)
      (main-collects-relative->path r)))

(define (path->rel r)
  (let ([r (path->main-collects-relative r)])
    (if (path? r)
        (path->bytes r)
        r)))

(define (doc->rel-doc d)
  (struct-copy doc 
               d
               [src-dir (path->main-collects-relative (doc-src-dir d))]
               [src-file (path->main-collects-relative (doc-src-file d))]
               [dest-dir (path->main-doc-relative (doc-dest-dir d))]))

(define (rel-doc->doc d)
  (struct-copy doc 
               d
               [src-dir (main-collects-relative->path (doc-src-dir d))]
               [src-file (main-collects-relative->path (doc-src-file d))]
               [dest-dir (main-doc-relative->path (doc-dest-dir d))]))

(define (info-deps->rel-doc-src-file info)
  (filter-map (lambda (i) (and (info? i) 
                               (path->rel (doc-src-file (info-doc i))))) 
              (info-deps info)))

(define (info-deps->doc info)
  (filter-map (lambda (i) (and (info? i) (info-doc i))) (info-deps info)))

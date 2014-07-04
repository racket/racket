#lang racket/base

(require setup/getinfo
         setup/dirs
         setup/path-to-relative
         "private/doc-path.rkt"
         setup/collects
         setup/collection-name
         setup/main-doc
         setup/parallel-do
         setup/doc-db
         racket/class
         racket/list
         racket/file
         racket/fasl
         racket/match
         racket/serialize
         racket/set
         compiler/cm
         scribble/base-render
         scribble/core
         scribble/html-properties
         scribble/manual ; really shouldn't be here... see dynamic-require-doc
         scribble/private/run-pdflatex
         setup/xref
         scribble/xref
         syntax/modcollapse
         racket/place
         pkg/lib
         pkg/strip
         openssl/sha1
         compiler/compilation-path
         (prefix-in u: net/url)
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render)
         (prefix-in contract: scribble/contract-render))

(provide setup-scribblings
         verbose
         run-pdflatex)

(define verbose (make-parameter #t))

(define-logger setup)

(define-syntax-rule (verbose/log format-str arg ...)
  (begin
    (when (verbose)
      (printf (string-append " [" format-str "]\n")
              arg ...)
      (flush-output))
    (log-setup-debug format-str arg ...)))

(define-serializable-struct doc (src-dir 
                                 src-spec
                                 src-file
                                 dest-dir
                                 flags
                                 under-main?
                                 pkg?
                                 category
                                 out-count
                                 name
                                 order-hint)
  #:transparent)
(define-serializable-struct info (doc       ; doc structure above
                                  undef     ; unresolved requires
                                  searches 
                                  deps       ; (listof (cons <path-or-info> hash))
                                  build?
                                  out-hash
                                  start-time done-time
                                  need-run?
                                  need-in-write? need-out-write?
                                  vers rendered? failed?)
  #:transparent
  #:mutable)

(define (main-doc? doc)
  (pair? (path->main-doc-relative (doc-dest-dir doc))))

(define (doc<? a b)
  (or (< (doc-order-hint a)
         (doc-order-hint b))
      (and (= (doc-order-hint a)
              (doc-order-hint b))
           (string<? (doc-name a)
                     (doc-name b)))))
(define (info<? a b)
  (doc<? (info-doc a) (info-doc b)))

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

(define (parallel-do-error-handler with-record-error doc errmsg outstr errstr)
  (with-record-error
   (doc-src-file doc)
   (lambda () (error errmsg))
   void))

;; We use a lock to control writing to the database. It's not
;; strictly necessary, but place channels can deal with blocking
;; more efficiently than the database connection.
(define no-lock void)
(define (lock-via-channel lock-ch)
  (if lock-ch
      (let ([saved-ch #f])
        (lambda (mode)
          (case mode
            [(lock)
             (define ch (sync lock-ch))
             (place-channel-put ch 'lock)
             (set! saved-ch ch)]
            [(unlock)
             (place-channel-put saved-ch 'done)
             (set! saved-ch #f)])))
      void))
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
         tidy?              ; clean up, even beyond `only-dirs'
         avoid-main?        ; avoid main collection, even for `tidy?'
         with-record-error  ; catch & record exceptions
         setup-printf)
  (unless (doc-db-available?)
    (error 'setup "install SQLite to build documentation"))
  (when latex-dest
    (log-setup-info "latex working directory: ~a" latex-dest))
  (define (scribblings-flag? sym)
    (memq sym '(main-doc main-doc-root user-doc-root user-doc multi-page
                         depends-all depends-all-main depends-all-user
                         no-depend-on always-run keep-style no-search)))
  (define (validate-scribblings-infos infos)
    (define (validate path [flags '()] [cat '(library)] [name #f] [out-count 1] [order-hint 0])
      (and (string? path) (relative-path? path)
           (list? flags) (andmap scribblings-flag? flags)
           (or (not name) (collection-name-element? name))
           (and (list? cat)
                (<= 1 (length cat) 2)
                (symbol? (car cat))
                (or (null? (cdr cat))
                    (real? (cadr cat))))
           (and (exact-positive-integer? out-count))
           (and (real? order-hint))
           (list path flags cat
                 (or name (let-values ([(_1 name _2) (split-path path)])
                            (path-replace-suffix name #"")))
                 out-count
                 order-hint)))
    (and (list? infos)
         (let ([infos (map (lambda (i)
                             (and (list? i) (<= 1 (length i) 6)
                                  (apply validate i)))
                           infos)])
           (and (not (memq #f infos)) infos))))
  (define ((get-docs main-dirs) i rec)
    (let* ([pre-s (and i (i 'scribblings))]
           [s (validate-scribblings-infos pre-s)]
           [dir (directory-record-path rec)])
      (if s
          (map (lambda (d)
                 (let* ([flags (cadr d)]
                        [under-main?
                         (and (not (memq 'user-doc-root flags))
                              (not (memq 'user-doc flags))
                              (or (memq 'main-doc flags)
                                  (hash-ref main-dirs dir #f)))])
                   (define src (simplify-path (build-path dir (car d)) #f))
                   (define name (cadddr d))
                   (define dest (doc-path dir name flags under-main?))
                   (make-doc dir
                             (let ([spec (directory-record-spec rec)])
                               (list* (car spec)
                                      (car d)
                                      (if (eq? 'planet (car spec))
                                          (list (append (cdr spec)
                                                        (list (directory-record-maj rec)
                                                              (list '= (directory-record-min rec)))))
                                          (cdr spec))))
                             src
                             dest
                             flags under-main? (and (path->pkg src) #t)
                             (caddr d)
                             (list-ref d 4)
                             (if (path? name) (path-element->string name) name)
                             (list-ref d 5))))
               s)
          (begin (setup-printf
                  "WARNING"
                  "bad 'scribblings info: ~e from: ~e" 
                  pre-s dir)
                 null))))
  (log-setup-info "getting documents")
  (define docs
    (sort
     (let* ([recs (find-relevant-directory-records '(scribblings) 'all-available)]
            [main-dirs (for/hash ([k (in-list
                                      (find-relevant-directories '(scribblings) 'no-user))])
                         (values k #t))]
            [infos (map get-info/full (map directory-record-path recs))])
       (filter-user-docs (append-map (get-docs main-dirs) infos recs)
                         make-user?))
     doc<?))
  (define-values (main-docs user-docs) (partition doc-under-main? docs))
  (define main-doc-exists? (ormap (lambda (d) (member 'main-doc-root (doc-flags d))) 
                                  main-docs))

  (define (can-build*? docs) (can-build? only-dirs docs))
  
  (define main-db (find-doc-db-path latex-dest #f main-doc-exists?))
  (define user-db (find-doc-db-path latex-dest #t main-doc-exists?))

  ;; Ensure that databases are created:
  (define (touch-db db-file)
    (unless (file-exists? db-file)
      (define-values (base name dir?) (split-path db-file))
      (make-directory* base)
      (doc-db-disconnect
       (doc-db-file->connection db-file #t))))
  (when (or (ormap can-build*? main-docs)
            (and tidy? (not avoid-main?)))
    (touch-db main-db))
  (when (or (ormap can-build*? user-docs)
            (and tidy? make-user?))
    (touch-db user-db))

  (when (and (or (not only-dirs) tidy?)
             (not avoid-main?)
             (not latex-dest))
    ;; Check for extra document directories that we should remove
    ;; in the main installation:
    (log-setup-info "checking installation document directories")
    (define main-doc-dir (find-doc-dir))
    (define expected (for/set ([doc (in-list main-docs)])
                       (doc-dest-dir doc)))
    (for ([i (in-list (if (directory-exists? main-doc-dir)
                          (directory-list main-doc-dir)
                          null))])
      (define p (build-path main-doc-dir i))
      (when (directory-exists? p)
        (unless (set-member? expected (build-path p))
          (setup-printf
           "removing"
           "~a (documentation directory)"
           (path->relative-string/setup p))
          (delete-directory/files p)))))

  (define auto-main? (and auto-start-doc? 
                          (or (ormap can-build*? main-docs)
                              (and tidy? (not avoid-main?)))))
  (define auto-user? (and auto-start-doc? 
                          (or (ormap can-build*? user-docs)
                              (and tidy? make-user?))))
  (define (can-build**? doc) (can-build? only-dirs doc auto-main? auto-user?))
  
  (unless latex-dest
    ;; Make sure "scribble.css", etc., is in place:
    (let ([ht (make-hash)])
      (for ([doc (in-list docs)])
        (when (can-build**? doc)
          (check-shared-files (doc-dest-dir doc)
                              (or (memq 'main-doc-root (doc-flags doc))
                                  (memq 'user-doc-root (doc-flags doc)))
                              (doc-under-main? doc)
                              ht
                              setup-printf)))))

  (define force-out-of-date? #f)
  
  (define lock-ch #f)
  (define lock-ch-in #f)
  (define (init-lock-ch!)
    (unless lock-ch
      ;; If places are not available, then tasks will be run
      ;; in separate OS processes, and we can do without an
      ;; extra lock.
      (when (place-enabled?)
        (set!-values (lock-ch lock-ch-in) (place-channel))
        (thread (lambda ()
                  (define-values (ch ch-in) (place-channel))
                  (let loop ()
                    (place-channel-put lock-ch-in ch)
                    (place-channel-get ch-in)
                    (place-channel-get ch-in)
                    (loop)))))))

  (log-setup-info "getting document information")
  (define (make-sequential-get-info only-fast?)
    (get-doc-info only-dirs latex-dest
                  auto-main? auto-user? main-doc-exists?
                  with-record-error setup-printf #f 
                  only-fast? force-out-of-date?
                  no-lock))
  (define num-sequential (let loop ([docs docs])
                           (cond
                            [(null? docs) 0]
                            [((doc-order-hint (car docs)) . > . -10) 0]
                            [else
                             (add1 (loop (cdr docs)))])))
  (define infos
    (and (ormap can-build**? docs)
         (filter 
          values
          (if ((min worker-count (length docs)) . < . 2)
              ;; non-parallel version:
              (map (make-sequential-get-info #f) docs)
              ;; maybe parallel...
              (or
               (let ([infos (map (make-sequential-get-info #t)
                                 docs)])
                 ;; check fast result
                 (and (andmap values infos)
                      infos))
               ;; parallel:
               (append
                (map (make-sequential-get-info #f) 
                     (take docs num-sequential))
                (parallel-do 
                 (min worker-count (length (list-tail docs num-sequential)))
                 (lambda (workerid)
                   (init-lock-ch!)
                   (list workerid program-name (verbose) only-dirs latex-dest
                         auto-main? auto-user? main-doc-exists?
                         force-out-of-date? lock-ch))
                 (list-queue
                  (list-tail docs num-sequential)
                  (lambda (x workerid) (s-exp->fasl (serialize x)))
                  (lambda (work r outstr errstr) 
                    (printf "~a" outstr)
                    (printf "~a" errstr)
                    (deserialize (fasl->s-exp r)))
                  (lambda (work errmsg outstr errstr) 
                    (parallel-do-error-handler with-record-error work errmsg outstr errstr)))
                 (define-worker (get-doc-info-worker workerid program-name verbosev only-dirs latex-dest 
                                                     auto-main? auto-user? main-doc-exists?
                                                     force-out-of-date? lock-ch)
                   (define ((get-doc-info-local program-name only-dirs latex-dest
                                                auto-main? auto-user? main-doc-exists?
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
                                   ((get-doc-info only-dirs latex-dest
                                                  auto-main? auto-user? main-doc-exists?
                                                  with-record-error setup-printf workerid
                                                  #f force-out-of-date? lock)
                                    (deserialize (fasl->s-exp doc))))))
                   
                   (verbose verbosev)
                   (match-message-loop
                    [doc (send/success 
                          ((get-doc-info-local program-name only-dirs latex-dest
                                               auto-main? auto-user? main-doc-exists?
                                               force-out-of-date? (lock-via-channel lock-ch)
                                               send/report) 
                           doc))])))))))))

  (define (out-path->info path infos out-path->info-cache)
    (or (hash-ref out-path->info-cache path #f)
        (let ([filename (main-doc-relative->path path)])
          (for*/or ([i (in-list infos)]
                    [c (in-range (add1 (doc-out-count (info-doc i))))])
            (and (equal? (sxref-path latex-dest (info-doc i) (format "out~a.sxref" c))
                         filename)
                 (hash-set! out-path->info-cache path i)
                 i)))))

  (define (tidy-database)
    (when (and (or (not only-dirs) tidy?)
               (not latex-dest)
               infos)
      (log-setup-info "tidying database")
      (define files (make-hash))
      (define tidy-docs (if tidy?
                            docs
                            (map info-doc infos)))
      (define (get-files! main?)
        (for ([doc (in-list tidy-docs)]
              #:when (eq? main? (main-doc? doc)))
          (hash-set! files (sxref-path latex-dest doc "in.sxref") #t)
          (for ([c (in-range (add1 (doc-out-count doc)))])
            (hash-set! files (sxref-path latex-dest doc (format "out~a.sxref" c)) #t))))
      (unless avoid-main?
        (get-files! #t)
        (doc-db-clean-files main-db files))
      (when (and (file-exists? user-db)
                 (not (equal? main-db user-db)))
        (get-files! #f)
        (doc-db-clean-files user-db files))))

  (define (make-loop first? iter)
    (let ([infos (filter-not info-failed? infos)]
          [src->info (make-hash)]
          [out-path->info-cache (make-hash)])
      (tidy-database)
      ;; Check for duplicate definitions
      (when first?
        (log-setup-info "checking for duplicates")
        (let ([dups (append
                     (if (file-exists? main-db)
                         (doc-db-check-duplicates main-db #:main-doc-relative-ok? #t)
                         null)
                     (if (and make-user?
                              (file-exists? user-db)
                              (not (equal? main-db user-db)))
                         (doc-db-check-duplicates user-db
                                                  #:attach (and (file-exists? main-db) main-db)
                                                  #:main-doc-relative-ok? #t)
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
      (define quick-fix? #f)
      (for ([info infos] #:when (info-build? info))
        (let ([one? #f]
              [added? #f]
              [deps (make-hasheq)]
              [known-deps (make-hasheq)]
              [all-main? (memq 'depends-all-main (doc-flags (info-doc info)))]
              [all-user? (memq 'depends-all-user (doc-flags (info-doc info)))]
              [all? (memq 'depends-all (doc-flags (info-doc info)))])
          ;; Convert current deps from paths to infos, keeping paths that have no info
          (set-info-deps!
           info
           (map (lambda (d)
                  (cons (if (info? (car d))
                            (car d)
                            (or (hash-ref src->info (car d) #f) 
                                (car d)))
                        (cdr d)))
                (info-deps info)))
          (unless (andmap (lambda (d) (info? (car d)))
                          (info-deps info))
            (set-info-need-in-write?! info #t))
          ;; Propagate existing dependencies as expected dependencies:
          (for ([dd (info-deps info)])
            (define d (car dd))
            (let ([i (if (info? d) d (hash-ref src->info d #f))])
              (if i
                  ;; Normal case:
                  (hash-set! deps i #t)
                  ;; Path has no info; normally keep it as expected, and it gets
                  ;; removed later.
                  (unless (or all?
                              (and (info? d)
                                   (cond
                                    [all-main?
                                     (doc-under-main? (info-doc d))]
                                    [all-user?
                                     (not (doc-under-main? (info-doc d)))]
                                    [else #f])))
                    (set! added? #t)
                    (verbose/log "Removed Dependency for ~a: ~a"
                                 (doc-name (info-doc info))
                                 (if i
                                     (doc-name (info-doc i))
                                     d))))))
          (define (add-dependency info i)
            (cond
             [((info-start-time info) . < . (info-done-time info))
              ;; Although this dependency wasn't in the list, yet,
              ;; the build actually happened after the dependency's "out.sxref"
              ;; files were written, so they would have been used.
              ;; Fix up the dependency list.
              (verbose/log "Quick-add for ~a: ~a"
                           (doc-name (info-doc info))
                           (doc-name (info-doc i)))
              (hash-set! deps i #t)
              (set-info-deps! info (cons (cons i (info-out-hash i))
                                         (info-deps info)))
              (set-info-need-in-write?! info #t)]
             [else
              (verbose/log "Adding for ~a: ~a"
                           (doc-name (info-doc info))
                           (doc-name (info-doc i)))
              (set! added? #t)
              (hash-set! deps i #t)]))
          ;; Add expected dependencies for an "all dependencies" doc:
          (when (or all? all-main? all-user?)
            (verbose/log "Adding all~a as dependencies for ~a"
                         (cond
                          [all-main? " main"]
                          [all-user? " user"]
                          [else ""])
                         (doc-name (info-doc info)))
            (for ([i infos])
              (hash-set! known-deps i #t)
              (when (and (not (eq? i info))
                         (not (hash-ref deps i #f))
                         (cond
                          [all-main? (doc-under-main? (info-doc i))]
                          [all-user? (not (doc-under-main? (info-doc i)))]
                          [else #t])
                         (not (memq 'no-depend-on (doc-flags (info-doc i)))))
                (add-dependency info i))))
          ;; Determine definite dependencies based on referenced keys, and also
          ;; report missing links.
          (let ([not-found
                 (lambda (k)
                   (unless (or all? all-main? all-user?)
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
                (unless i
                  (setup-printf
                   "WARNING" "failed to find info for path: ~a"
                   found-dep))
                (when i
                  ;; Record this known dependency:
                  (when (not (hash-ref known-deps i #f))
                    (hash-set! known-deps i #t))
                  (when (not (hash-ref deps i #f))
                    ;; Record dependency in "expected", too, which triggers
                    ;; a re-run if needed:
                    (add-dependency info i))))
              (for ([s-key (in-list missing)])
                (not-found s-key))))
          ;; Check whether this document needs a re-run:
          (when (or 
                 ;; If we added anything (expected or known), then mark as needed to run:
                 (and added?
                      (verbose/log "Rerun, since added dependencies for ~a"
                                   (doc-name (info-doc info))))
                 ;; If any dependency change, then mark as needed to run:
                 (and (let ([ch (ormap (lambda (p)
                                         (define i2 (car p))
                                         (or (and (not (info? i2))
                                                  i2)
                                             (and (not (equal? (info-out-hash i2) (cdr p)))
                                                  (if ((info-start-time info) . < . (info-done-time info))
                                                      (begin
                                                        ;; Actually used more recent:
                                                        (set! quick-fix? #t)
                                                        #f)
                                                      #t)
                                                  i2)))
                                       (info-deps info))])
                        (and ch
                             (verbose/log "Rerun, since dependency changed for ~a: ~a"
                                          (doc-name (info-doc info))
                                          (if (info? ch)
                                              (doc-name (info-doc ch))
                                              ch))))))
            (define (key->dep i v) (cons i (info-out-hash i)))
            (set-info-deps! info (hash-map known-deps key->dep))
            (set-info-need-in-write?! info #t)
            (set-info-need-run?! info #t))
          (when (and quick-fix? (not (info-need-run? info)))
            ;; Because the run was later enough, it actually used the latest
            ;; "out.sxref" for all dependencies.
            (set-info-deps! info (for/list ([dep (in-list (info-deps info))])
                                   (cons (car dep)
                                         (info-out-hash (car dep)))))
            (set-info-need-in-write?! info #t))))
      ;; Write out any "in.sxref" files that have been updated with dependency
      ;; information, and where another run isn't needed:
      (for ([info (in-list infos)])
        (when (and (info-need-in-write? info)
                   (not (info-need-run? info)))
          (write-in/info latex-dest info no-lock main-doc-exists?)
          (set-info-need-in-write?! info #f)))
      ;; Iterate, if any need to run:
      (when (and (ormap info-need-run? infos) (iter . < . 30))
        (log-setup-info "building")
        ;; Build again, using dependencies
        (let ([need-rerun (sort (filter-map (lambda (i) 
                                              (and (info-need-run? i)
                                                   (begin
                                                     (set-info-need-run?! i #f)
                                                     i)))
                                            infos)
                                info<?)])
          (define (say-rendering i workerid)
            (setup-printf (string-append
                           (if workerid (format "~a " workerid) "")
                           (if (info-rendered? i) "re-rendering" "rendering") )
                          "~a"
                          (path->relative-string/setup (doc-src-file (info-doc i)))))
          (define (prep-info! i)
            (set-info-start-time! i (current-inexact-milliseconds)))
          (define (update-info! info response)
            (match response 
              [#f (set-info-failed?! info #t)]
              [(list undef searches out-delta?)
               (set-info-rendered?! info #t)
               (set-info-undef! info undef)
               (set-info-searches! info searches)
               (set-info-need-in-write?! info #f)
               (when out-delta?
                 (set-info-out-hash! info (get-info-out-hash (info-doc info) latex-dest))
                 (set-info-done-time! info (current-inexact-milliseconds)))]))
          (if ((min worker-count (length need-rerun)) . < . 2)
              (for ([i (in-list need-rerun)])
                (say-rendering i #f)
                (prep-info! i)
                (update-info! i (build-again! latex-dest i with-record-error no-lock 
                                              main-doc-exists?)))
              (parallel-do 
               (min worker-count (length need-rerun))
               (lambda (workerid)
                 (init-lock-ch!)
                 (list workerid (verbose) latex-dest lock-ch main-doc-exists?))
               (list-queue
                need-rerun
                (lambda (i workerid) 
                  (say-rendering i workerid)
                  (prep-info! i)
                  (s-exp->fasl (serialize (list (info-doc i)
                                                ;; Other content of `info' can be re-read from
                                                ;; "in.sxref", but not the dependencies and the
                                                ;; fact that we need to write the dependencies,
                                                ;; because those should only be written after
                                                ;; a successful render. So, we pass them along
                                                ;; in this list:
                                                (info-deps->rel-doc-src-file i)
                                                (info-need-in-write? i)))))
                (lambda (i r outstr errstr) 
                  (printf "~a" outstr) 
                  (printf "~a" errstr)
                  (update-info! i (deserialize (fasl->s-exp r))))
                (lambda (i errmsg outstr errstr) 
                  (parallel-do-error-handler with-record-error (info-doc i) errmsg outstr errstr)))
               (define-worker (build-again!-worker2 workerid verbosev latex-dest lock-ch
                                                    main-doc-exists?)
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
                                                          (lock-via-channel lock-ch)
                                                          main-doc-exists?))))])))))
        ;; If we only build 1, then it reaches it own fixpoint
        ;; even if the info doesn't seem to converge immediately.
        ;; This is a useful shortcut when re-building a single
        ;; document.
        (unless (= 1 (for/fold ([count 0])
                         ([i infos]
                          #:when (info-build? i))
                       (add1 count)))
          (make-loop #f (add1 iter))))))

  (unless infos
    ;; since we won't use `make-loop':
    (tidy-database))

  (when infos
    (make-loop #t 0)
    ;; cache info to disk
    (for ([i infos] #:when (info-need-in-write? i))
      (write-in/info latex-dest i no-lock main-doc-exists?))))

(define shared-style-files
  (list "scribble.css"
        "scribble-style.css"
        "racket.css"
        "manual-style.css"
        "manual-racket.css"
        "manual-racket.js"
        "manual-fonts.css"
        "scribble-common.js"))
(define shared-empty-style-files
  (list "doc-site.css"))
(define shared-empty-script-files
  (list "doc-site.js"))

(define (make-renderer latex-dest doc main-doc-exists?)
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
                  contract:override-render-mixin-single)]
             [allow-indirect? (and (doc-pkg? doc)
                                   ;; (not main?)
                                   (not (memq 'no-depend-on (doc-flags doc))))]
             [local-redirect-file (build-path (if main-doc-exists?
                                                  (find-doc-dir)
                                                  (find-user-doc-dir))
                                              "local-redirect"
                                              "local-redirect.js")]
             [local-user-redirect-file (build-path (if main?
                                                       (find-doc-dir)
                                                       (find-user-doc-dir))
                                                   "local-redirect"
                                                   "local-user-redirect.js")])
        (define r
          (new (contract-override-mixin
                ((if multi? html:render-multi-mixin values)
                 (html:render-mixin render%)))
               ;; Use PLT manual style:
               [style-file (if (memq 'keep-style flags)
                               #f
                               (collection-file-path "manual-style.css" "scribble"))]
               [extra-files (if (memq 'keep-style flags)
                                null
                                (list (collection-file-path "manual-fonts.css" "scribble")))]
               ;; See also `style-extra-files`, below
               [dest-dir (if multi?
                             (let-values ([(base name dir?) (split-path ddir)]) base)
                             ddir)]
               [alt-paths   (let ([std-path (lambda (s)
                                              (cons (collection-file-path s "scribble")
                                                    (if root?
                                                        s
                                                        (format "../~a" s))))])
                              (list* (cons local-redirect-file 
                                           (if main?
                                               "../local-redirect/local-redirect.js"
                                               (u:url->string (u:path->url local-redirect-file))))
                                     (cons local-user-redirect-file
                                           (if main?
                                               "../local-redirect/local-user-redirect.js"
                                               (u:url->string (u:path->url local-user-redirect-file))))
                                     (map std-path (append
                                                    shared-style-files
                                                    shared-empty-style-files
                                                    shared-empty-script-files))))]
               [up-path (cond
                         [root? #f] ; no up from root
                         [main?
                          ;; #t make the "up" link go to the (user's) start page
                          ;; using cookies:
                          #t]
                         [allow-indirect?
                          ;; building a package, so also rely on cookies in this
                          ;; case:
                          #t]
                         [else
                          ;; user-installed and not a package, so hard link is ok:
                          (build-path (find-user-doc-dir) "index.html")])]

               ;; In cross-reference information, use paths that are relative
               ;; to the target rendering directory for documentation that might
               ;; be moved into a binary package:
               [root-path (and allow-indirect? ddir)]

               [style-extra-files (if (memq 'keep-style flags)
                                      null
                                      (cons
                                       (collection-file-path "manual-racket.css" "scribble")
                                       (map (lambda (s)
                                              (collection-file-path s "scribble"))
                                            shared-empty-style-files)))]

               [search-box? (not (memq 'no-search flags))]))
        (unless (memq 'keep-style flags)
          (send r add-extra-script-file (collection-file-path "manual-racket.js" "scribble"))
          (for ([s (in-list shared-empty-script-files)])
            (send r add-extra-script-file (collection-file-path s "scribble"))))
        (when allow-indirect?
          ;; For documentation that might be moved into a binary package
          ;; or that can contain an indirect reference, use a server indirection
          ;; for all links external to the document, but also install the
          ;; "local-[user-]redirect.js" hooks:
          (send r set-external-tag-path 
                (u:url->string
                 (let ([u (u:string->url (get-doc-search-url))])
                   (struct-copy
                    u:url
                    u
                    [query
                     (cons (cons 'version (version))
                           (u:url-query u))]))))
          (send r add-extra-script-file local-redirect-file)
          (send r add-extra-script-file local-user-redirect-file))
        ;; Result is the renderer:
        r)))

(define (pick-dest latex-dest doc)
  (cond [(path? latex-dest)
         (let-values ([(base name dir?) (split-path (doc-dest-dir doc))])
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

(define (find-doc-db-path latex-dest user? main-doc-exists?)
  (cond
   [latex-dest
    (build-path latex-dest "docindex.sqlite")]
   [else
    (build-path (if (or user? (not main-doc-exists?))
                    (find-user-doc-dir)
                    (find-doc-dir))
                "docindex.sqlite")]))

(define (can-build? only-dirs doc [auto-main? #f] [auto-user? #f])
  (or (not only-dirs)
      (and auto-main?
           (memq 'depends-all-main (doc-flags doc)))
      (and auto-user?
           (or (memq 'depends-all (doc-flags doc))
               (memq 'depends-all-user (doc-flags doc))))
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
                             v))]
                [v (cons (document-source
                          (collapse-module-path src-spec
                                                'scribble))
                         v)])
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

(define (find-db-file doc latex-dest main-doc-exists?)
  (define p (find-doc-db-path latex-dest (not (main-doc? doc)) main-doc-exists?))
  (define-values (base name dir?) (split-path p))
  (unless (directory-exists? base)
    (make-directory* base))
  p)

(define (xref-transfer-db renderer ci doc latex-dest
                          main-doc-exists?
                          #:quiet-fail? [quiet-fail? #f])
  (define shutdown void)
  (define xref (make-collections-xref
                #:quiet-fail? quiet-fail?
                #:no-user? (main-doc? doc)
                #:no-main? (or (not main-doc-exists?)
                               (memq 'depends-all-user (doc-flags doc)))
                #:doc-db (and latex-dest
                              (find-doc-db-path latex-dest #t main-doc-exists?))
                #:register-shutdown! (lambda (s)
                                       (set! shutdown s))))
  (xref-transfer-info renderer ci xref)
  shutdown)

(define (get-info-out-hash doc latex-dest)
  (define info-out-files (for/list ([i (add1 (doc-out-count doc))])
                           (sxref-path latex-dest doc (format "out~a.sxref" i))))
  (define-values (i o) (make-pipe))
  (for/list ([info-out-file info-out-files])
    (display (get-file-sha1 info-out-file) o))
  (close-output-port o)
  (sha1 i))

(define ((get-doc-info only-dirs latex-dest
                       auto-main? auto-user? main-doc-exists?
                       with-record-error setup-printf workerid 
                       only-fast? force-out-of-date? lock)
         doc)

  ;; First, move pre-rendered documentation, if any, into place
  (let ([rendered-dir (let-values ([(base name dir?) (split-path (doc-dest-dir doc))])
                        (build-path (doc-src-dir doc) "doc" name))])
    (when (and (can-build? only-dirs doc)
               (directory-exists? rendered-dir)
               (not (file-exists? (build-path rendered-dir "synced.rktd")))
               (or (not (directory-exists? (doc-dest-dir doc)))
                   force-out-of-date?
                   (not (file-exists? (build-path (doc-dest-dir doc) "synced.rktd")))))
      (move-documentation-into-place doc rendered-dir setup-printf workerid lock
                                     main-doc-exists?)))

  (let* ([info-out-files (for/list ([i (add1 (doc-out-count doc))])
                           (sxref-path latex-dest doc (format "out~a.sxref" i)))]
         [info-in-file  (sxref-path latex-dest doc "in.sxref")]
         [db-file (find-db-file doc latex-dest main-doc-exists?)]
         [stamp-file  (sxref-path latex-dest doc "stamp.sxref")]
         [out-file (build-path (doc-dest-dir doc) "index.html")]
         [src-zo (let ([path (get-compilation-bytecode-file (doc-src-file doc))])
                   (or (and (file-exists? path)
                            path)
                       (if (file-exists? out-file)
                           ;; assume installed as pre-rendered:
                           #f
                           ;; need to render, so complain if no source is available:
                           path)))]
         [src-sha1 (and src-zo (get-compiled-file-sha1 src-zo))]
         [renderer (make-renderer latex-dest doc main-doc-exists?)]
         [can-run? (can-build? only-dirs doc)]
         [stamp-data (with-handlers ([exn:fail:filesystem? (lambda (exn) (list "" "" ""))])
                       (let ([v (call-with-input-file* stamp-file read)])
                         (if (and (list? v)
                                  (= 3 (length v))
                                  (andmap string? v))
                             v
                             (list "" "" ""))))]
         [renderer-path (let ([p (collection-file-path
                                  #:check-compiled? #t
                                  (cond
                                   [(path? latex-dest) "latex-render.rkt"]
                                   [(not latex-dest) "html-render.rkt"])
                                  "scribble")])
                          (get-compilation-bytecode-file p))]
         [css-path (collection-file-path "scribble.css" "scribble")]
         [aux-sha1s (list (get-compiled-file-sha1 renderer-path)
                          (get-file-sha1 css-path))]
         [out-exists? (file-exists? out-file)]
         [info-out-time (for/fold ([t +inf.0]) ([info-out-file info-out-files])
                          (and t
                               (let ([t2 (file-or-directory-modify-seconds info-out-file #f (lambda () #f))])
                                 (and t2 (min t t2)))))]
         [provides-time (for/fold ([t +inf.0]) ([info-out-file info-out-files])
                          (and t
                               (let ([t2 (and (file-exists? db-file)
                                              (doc-db-get-provides-timestamp db-file info-out-file))])
                                 (and t2 (min t t2)))))]
         [info-in-exists? (file-exists? info-in-file)]
         [vers (send renderer get-serialize-version)]
         [use-built? (and (not src-zo)
                          info-in-exists?
                          info-out-time)]
         [out-of-date (and (not use-built?)
                           (or (and force-out-of-date?
                                    'force)
                               (and (not info-out-time)
                                    'missing-out)
                               (and (not info-in-exists?)
                                    'missing-in)
                               (and can-run?
                                    (not (equal? (car stamp-data)
                                                 src-sha1))
                                    'newer)))]
         [up-to-date? (not out-of-date)]
         [can-run? (and src-zo
                        (or (not latex-dest)
                            (not (omit? (doc-category doc))))
                        (or can-run?
                            (and auto-main?
                                 (memq 'depends-all-main (doc-flags doc)))
                            (and auto-user?
                                 (or (memq 'depends-all (doc-flags doc))
                                     (memq 'depends-all-user (doc-flags doc))))))])

    (when (or (and (not up-to-date?) (not only-fast?))
              (verbose))
      (when out-of-date
        (verbose/log "Need run (~a) ~a" out-of-date (doc-name doc)))
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
                                      ((get-doc-info only-dirs latex-dest
                                                     auto-main? auto-user? main-doc-exists?
                                                     with-record-error setup-printf workerid 
                                                     #f #f lock)
                                       doc))])
           (let ([v-in  (load-sxref info-in-file)])
             (unless (equal? (car v-in) (list vers (doc-flags doc)))
               (error "old info has wrong version or flags"))
             (when (and (or (not provides-time)
                            (provides-time . < . info-out-time))
                        (can-build? only-dirs doc))
               ;; Database is out of sync, and we don't need to build
               ;; this document, so update databse now. Note that a
               ;; timestamp is good enough for determing a sync,
               ;; instead of sha1s, because a database is never moved
               ;; across installations.
               (move-documentation-into-place doc #f
                                              setup-printf workerid lock
                                              main-doc-exists?))
             (define out-hash (get-info-out-hash doc latex-dest))
             (make-info
              doc
              'delayed
              'delayed
              ;; expected deps, in case we don't need to build:
              (map (lambda (p) (cons (rel->path (car p)) (cdr p)))
                   (list-ref v-in 1)) 
              can-run?
              out-hash
              -inf.0 -inf.0
              (and can-run?
                   (or (memq 'always-run (doc-flags doc))
                       ;; maybe info is up-to-date but not rendered doc:
                       (not out-exists?)))
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
                 (let* ([start-time (current-inexact-milliseconds)]
                        [v (load-doc/ensure-prefix doc)]
                        [dest-dir (pick-dest latex-dest doc)]
                        [fp (send renderer traverse (list v) (list dest-dir))]
                        [ci (send renderer collect (list v) (list dest-dir) fp)]
                        ;; It's ok if cross-reference information isn't available
                        ;; at this point, but we can sometimes save another iteration
                        ;; if the information is available at this pass.
                        [db-shutdown (xref-transfer-db renderer ci doc latex-dest
                                                       main-doc-exists?
                                                       #:quiet-fail? #t)]
                        [ri (send renderer resolve (list v) (list dest-dir) ci)]
                        [out-vs (and info-out-time
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
                        [need-out-write
                         (or (and force-out-of-date? 'forced)
                             (and (not out-vs) 'missing)
                             (and (not (for/and ([out-v out-vs])
                                         (equal? (list vers (doc-flags doc))
                                                 (car out-v))))
                                  'version/flags)
                             (and (not (for/and ([sci scis]
                                                 [out-v out-vs])
                                         (serialized=? sci (cadr out-v))))
                                  'content)
                             (and (not provides-time) 'db-missing)
                             (and (info-out-time . > . provides-time) 'db-older))])
                   (when need-out-write
                     (verbose/log "New out (~a) ~a" need-out-write (doc-name doc)))
                   (gc-point)
                   (let ([info
                          (make-info doc
                                     undef
                                     searches
                                     null ; haven't figured out deps, yet
                                     can-run?
                                     (and (not need-out-write)
                                          (get-info-out-hash doc latex-dest))
                                     start-time +inf.0
                                     #t
                                     can-run?
                                     (and need-out-write #t)
                                     vers
                                     #f
                                     #f)])
                     (when need-out-write
                       (render-time "xref-out" (write-out/info latex-dest info scis defss db-file lock))
                       (set-info-out-hash! info (get-info-out-hash doc latex-dest))
                       (set-info-need-out-write?! info #f)
                       (set-info-done-time! info (current-inexact-milliseconds)))
                     (when (info-need-in-write? info)
                       (render-time "xref-in" (write-in/info latex-dest info lock main-doc-exists?))
                       (set-info-need-in-write?! info #f))

                     (let ([data (cons src-sha1 aux-sha1s)])
                       (unless (equal? data stamp-data)
                         (with-compile-output 
                          stamp-file 
                          (lambda (out tmp-filename) (write data out)))))
                     (db-shutdown)
                     info))))
             (lambda () #f))
            #f))))

(define (check-shared-files dir root? main? done setup-printf)
  (define dest-dir (simplify-path (if root?
                                      dir
                                      (build-path dir 'up))))
  (unless (hash-ref done dir #f)
    (for ([f (in-list shared-style-files)])
      (define src (collection-file-path f "scribble"))
      (define dest (build-path dest-dir f))
      (unless (and (file-exists? dest)
                   (equal? (file->bytes src)
                           (file->bytes dest)))
        (when (or (verbose) main?)
          (setup-printf "installing" "~a" dest))
        (make-directory* dest-dir)
        (copy-file src dest #t)))
    (for ([f (in-list (append shared-empty-style-files
                              shared-empty-script-files))])
      (define dest (build-path dest-dir f))
      (unless (file-exists? dest)
        (when (or (verbose) main?)
          (setup-printf "installing" "~a" dest))
        (make-directory* dest-dir)
        (call-with-output-file* dest void)))
    (hash-set! done dir #t)))

(define (move-documentation-into-place doc src-dir setup-printf workerid lock
                                       main-doc-exists?)
  (with-handlers ([exn:fail? (lambda (exn)
                               ;; On any failure, log the error and give up.
                               ;; Maybe further actions are appropriate, but
                               ;; overall clean-up and repair is intended to be
                               ;; the job of the regular documentation builder.
                               (log-error (exn-message exn)))])
    (define dest-dir (doc-dest-dir doc))
    (define move? (and src-dir
                       (not (equal? (file-or-directory-identity src-dir)
                                    (and (directory-exists? dest-dir)
                                         (file-or-directory-identity dest-dir))))))
    (setup-printf (string-append
                   (if workerid (format "~a " workerid) "")
                   (if move? "moving" "syncing"))
                  "~a"
                  (path->relative-string/setup (or src-dir dest-dir)))
    (when move?
      (when (directory-exists? dest-dir)
        (delete-directory/files dest-dir))
      (let-values ([(base name dir?) (split-path dest-dir)])
        (when (and (path? base) (not (directory-exists? base)))
          (make-directory* base)))
      (copy-directory/files src-dir dest-dir)
      (delete-directory/files src-dir))
    ;; Register provided-tag information with the database:
    (let ([provides-path (build-path dest-dir "provides.sxref")])
      (when (file-exists? provides-path)
        ;; register keys provided in "out<n>.sxref" with
        ;; the database
        (define providess (call-with-input-file*
                           provides-path
                           (lambda (in) (fasl->s-exp in))))
        (define db-file (find-db-file doc #f main-doc-exists?))
        (for ([provides (in-list providess)]
              [n (in-naturals)])
          (define filename (sxref-path #f doc (format "out~a.sxref" n)))
          (call-with-lock
           lock
           (lambda ()
             (doc-db-clear-provides db-file filename)
             (doc-db-add-provides db-file provides filename)
             (doc-db-set-provides-timestamp db-file filename 
                                            (file-or-directory-modify-seconds filename)))))))
    ;; For each ".html" file, check for a reference to "local-redirect.js",
    ;; and fix up the path if there is a reference:
    (define js-path (if (doc-under-main? doc)
                        "../local-redirect"
                        (u:url->string 
                         (u:path->url 
                          (build-path (if main-doc-exists?
                                          (find-doc-dir)
                                          (find-user-doc-dir))
                                      "local-redirect")))))
    (define user-js-path (if (doc-under-main? doc)
                             "../local-redirect"
                             (u:url->string
                              (u:path->url
                               (build-path (find-user-doc-dir) "local-redirect")))))
    (for ([p (in-directory dest-dir)])
      (when (regexp-match? #rx#"[.]html$" (path->bytes p))
        (fixup-local-redirect-reference p js-path #:user user-js-path)))
    ;; The existence of "synced.rktd" means that the db is in sync
    ;; with "provides.sxref" and ".html" files have been updated.
    (let ([provided-path (build-path dest-dir "synced.rktd")])
      (unless (file-exists? provided-path)
        (call-with-output-file provided-path (lambda (o) (write '#t o)))))))

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

(define (load-sxrefs latex-dest doc vers new-deps-rel)
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
                 new-deps-rel
                 searches
                 scis))])]))

(define (build-again! latex-dest info-or-list with-record-error lock
                      main-doc-exists?)
  ;; If `info-or-list' is a list, then we're in a parallel build, and
  ;; it provides just enough of `info' from the main place to re-build
  ;; in this place along with the content of "in.sxref".
  (define (cleanup-dest-dir doc)
    (unless latex-dest
      (let ([dir (doc-dest-dir doc)])
        (if (not (directory-exists? dir))
            (make-directory* dir)
            (for ([f (directory-list dir)]
                  #:when
                  (and (file-exists? f)
                       (not (regexp-match? #"[.]sxref$"
                                           (path-element->bytes f)))))
              (delete-file (build-path dir f)))))))
  (define (load-doc-scis doc)
    (map cadr (for/list ([i (add1 (doc-out-count doc))])
                (load-sxref (sxref-path latex-dest doc (format "out~a.sxref" i))))))
  (define info (and (info? info-or-list) info-or-list))
  (define doc (if info (info-doc info) (car info-or-list)))
  (define renderer (make-renderer latex-dest doc main-doc-exists?))
  (with-record-error
   (doc-src-file doc)
   (lambda ()
     (define vers (send renderer get-serialize-version))
     (define-values (ff-undef ff-deps-rel ff-searches ff-scis)
       (if info
           (begin
             (when (eq? 'delayed (info-undef info))
               (read-delayed-in! info latex-dest))
             (values (info-undef info)
                     (info-deps->rel-doc-src-file info)
                     (info-searches info)
                     (load-doc-scis doc)))
           (load-sxrefs latex-dest doc vers (cadr info-or-list))))
     
     (parameterize ([current-directory (doc-src-dir doc)])
       (let* ([v (render-time "load" (load-doc/ensure-prefix doc))]
              [dest-dir (pick-dest latex-dest doc)]
              [fp (render-time "traverse" (send renderer traverse (list v) (list dest-dir)))]
              [ci (render-time "collect" (send renderer collect (list v) (list dest-dir) fp))]
              [db-shutdown (xref-transfer-db renderer ci doc latex-dest main-doc-exists?)]
              [ri (render-time "resolve" (send renderer resolve (list v) (list dest-dir) ci))]
              [scis (render-time "serialize" (send renderer serialize-infos ri (add1 (doc-out-count doc)) v))]
              [defss (render-time "defined" (send renderer get-defineds ci (add1 (doc-out-count doc)) v))]
              [undef (render-time "undefined" (send renderer get-external ri))]
              [searches (render-time "searches" (resolve-info-searches ri))]
              [in-delta? (not (and (equal? (any-order undef) (any-order ff-undef))
                                   (equal? searches ff-searches)))]
              [out-delta? (not (for/and ([sci scis]
                                         [ff-sci ff-scis])
                                 (serialized=? sci ff-sci)))]
              [db-file (find-db-file doc latex-dest main-doc-exists?)])
         (verbose/log "~a~afor ~a"
                      (if in-delta? "New in " "")
                      (cond [out-delta? "New out "]
                            [in-delta? ""]
                            [else "No change "])
                      (doc-name doc))

         (when (or in-delta?
                   (and info (info-need-in-write? info))
                   (and (not info) (caddr info-or-list)))
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
         (unless (or latex-dest (main-doc? doc))
           ;; Since dest dir is the same place as pre-built documentation,
           ;; mark it so that it is not treated as needing an install:
           (let ([synced (build-path (doc-dest-dir doc) "synced.rktd")])
             (unless (file-exists? synced)
               (close-output-port (open-output-file synced)))))
         (db-shutdown)
         (gc-point)
         (list undef searches out-delta?))))
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
        (call-in-nested-thread (lambda ()
                                 (define sub
                                   (if (and (pair? mod-path)
                                            (eq? (car mod-path) 'submod))
                                       (append mod-path '(doc))
                                       `(submod ,mod-path doc)))
                                 (if (module-declared? sub #t)
                                     (dynamic-require sub 'doc)
                                     (dynamic-require mod-path 'doc))))))))

(define (write- latex-dest vers doc name datas prep! final!)
  (let* ([filename (sxref-path latex-dest doc name)])
    (prep! filename)
    (verbose/log "Caching to disk ~a" filename)
    (make-directory* (doc-dest-dir doc))
    (with-compile-output 
     filename 
     (lambda (out tmp-filename)
       (for ([data (in-list datas)])
         (write-bytes (s-exp->fasl (append (list (list vers (doc-flags doc))) 
                                           data)) 
                      out))))
    (final! filename)))

(define (write-out latex-dest vers doc scis providess db-file lock)
  ;; A "provides.sxref" file is used when a package is converted to binary
  ;; form, in which case cross-reference information needs to be loaded
  ;; into the database at install time:
  (when (and (doc-pkg? doc)
             (not (doc-under-main? doc))
             (not latex-dest))
    (make-directory* (doc-dest-dir doc))
    (with-compile-output
     (sxref-path latex-dest doc "provides.sxref")
     (lambda (out tmp-filename)
       (s-exp->fasl providess out))))
  ;; Write each "out.sxref" file and install the corresponding provides
  ;; into the database:
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
                 (doc-db-add-provides db-file provides filename))))
            (lambda (filename)
              (call-with-lock
               lock
               (lambda ()
                 (doc-db-set-provides-timestamp
                  db-file filename 
                  (file-or-directory-modify-seconds filename))))))))

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
               (doc-db-add-searches db-file searches filename))))
          void))

(define (write-in/info latex-dest info lock main-doc-exists?)
  (when (eq? 'delayed (info-undef info))
    (read-delayed-in! info latex-dest))
  (write-in latex-dest
            (info-vers info)
            (info-doc info)
            (info-undef info)
            (info-deps->rel-doc-src-file info)
            (info-searches info)
            (find-db-file (info-doc info) latex-dest main-doc-exists?)
            lock))

(define (rel->path r)
  (if (bytes? r)
      (bytes->path r)
      (collects-relative->path r)))

(define (path->rel r)
  (let ([r (path->collects-relative r)])
    (if (path? r)
        (path->bytes r)
        r)))

(define (doc->rel-doc d)
  (struct-copy doc 
               d
               [src-dir (path->collects-relative (doc-src-dir d))]
               [src-file (path->collects-relative (doc-src-file d))]
               [dest-dir (path->main-doc-relative (doc-dest-dir d))]))

(define (rel-doc->doc d)
  (struct-copy doc 
               d
               [src-dir (collects-relative->path (doc-src-dir d))]
               [src-file (collects-relative->path (doc-src-file d))]
               [dest-dir (main-doc-relative->path (doc-dest-dir d))]))

(define (info-deps->rel-doc-src-file info)
  (filter-map (lambda (ii) 
                (define i (car ii))
                (and (info? i)
                     (cons (path->rel (doc-src-file (info-doc i)))
                           (cdr ii))))
              (info-deps info)))

(define (info-deps->doc info)
  (filter-map (lambda (i) (and (info? i) (info-doc i))) (info-deps info)))

(define (reroot-path* base root)
  (cond
   [(eq? root 'same) base]
   [(relative-path? root)
    (build-path base root)]
   [else
    (reroot-path base root)]))

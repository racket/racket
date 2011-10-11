#lang scheme/base

(require "getinfo.rkt"
         "dirs.rkt"
         "path-to-relative.rkt"
         "private/path-utils.rkt"
         "main-collects.rkt"
         "main-doc.rkt"
         "parallel-do.rkt"
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
         unstable/file
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render))

(provide setup-scribblings
         verbose
         run-pdflatex)

(define verbose (make-parameter #t))

(define-serializable-struct doc (src-dir src-spec src-file dest-dir flags under-main? category) #:transparent)
(define-serializable-struct info (doc      ; doc structure above
                                  provides ; provides
                                  undef    ; unresolved requires
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
  (define (scribblings-flag? sym)
    (memq sym '(main-doc main-doc-root user-doc-root user-doc multi-page
                         depends-all depends-all-main no-depend-on always-run)))
  (define (validate-scribblings-infos infos)
    (define (validate path [flags '()] [cat '(library)] [name #f])
      (and (string? path) (relative-path? path)
           (list? flags) (andmap scribblings-flag? flags)
           (or (not name) (and (path-string? name) (relative-path? name) name))
           (and (list? cat)
                (<= 1 (length cat) 2)
                (symbol? (car cat))
                (or (null? (cdr cat))
                    (real? (cadr cat))))
           (list path flags cat
                 (or name (let-values ([(_1 name _2) (split-path path)])
                            (path-replace-suffix name #""))))))
    (and (list? infos)
         (let ([infos (map (lambda (i)
                             (and (list? i) (<= 1 (length i) 4)
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
                             flags under-main? (caddr d))))
               s)
          (begin (setup-printf
                  "WARNING"
                  "bad 'scribblings info: ~e from: ~e" (i 'scribblings) dir)
                 null))))
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
  (define infos
    (and (ormap can-build*? docs)
         (filter values
                 (if (not (worker-count . > . 1))
                     (map (get-doc-info only-dirs latex-dest auto-main? auto-user? 
                                        with-record-error setup-printf #f) 
                          docs)
                     (parallel-do 
                      worker-count
                      (lambda (workerid)
                        (list workerid program-name (verbose) only-dirs latex-dest auto-main? auto-user?))
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
                                                          auto-main? auto-user?) 
                        (define ((get-doc-info-local program-name only-dirs latex-dest auto-main? auto-user? send/report) 
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
                                               (eprintf "~a\n" (exn-message exn))
                                               (raise exn))])
                              (go)))
                          (s-exp->fasl (serialize 
                                        ((get-doc-info only-dirs latex-dest auto-main? auto-user?  
                                                       with-record-error setup-printf workerid)
                                         (deserialize (fasl->s-exp doc))))))
                        
                        (verbose verbosev)
                        (match-message-loop
                         [doc (send/success 
                               ((get-doc-info-local program-name only-dirs latex-dest auto-main? auto-user? send/report) 
                                doc))])))))))

  (define (make-loop first? iter)
    (let ([ht (make-hash)]
          [infos (filter-not info-failed? infos)]
          [src->info (make-hash)])
      ;; Collect definitions
      (for* ([info infos]
             [k (info-provides info)])
        (let ([prev (hash-ref ht k #f)])
          (when (and first? prev)
            (setup-printf "WARNING" "duplicate tag: ~s" k)
            (setup-printf #f " in: ~a" (doc-src-file (info-doc prev)))
            (setup-printf #f " and: ~a" (doc-src-file (info-doc info))))
          (hash-set! ht k info)))
      ;; Build deps:
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
          ;; Add defeinite dependencies based on referenced keys
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
            (for ([k (info-undef info)])
              (let ([i (hash-ref ht k #f)])
                (if i
                    (begin
                      ;; Record a definite dependency:
                      (when (not (hash-ref known-deps i #f))
                        (hash-set! known-deps i #t))
                      ;; Record also in the expected-dependency list:
                      (when (not (hash-ref deps i #f))
                        (set! added? #t)
                        (when (verbose)
                          (printf " [Adding... ~a]\n"
                                  (doc-src-file (info-doc i))))
                        (hash-set! deps i #t)))
                    (when first? 
                      ;; FIXME: instead of special-casing 'dep, we should
                      ;; skip any key that is covered by `(info-searches info)'.
                      (unless (eq? (car k) 'dep) 
                        (not-found k))))))
            (when first?
              (for ([(s-key s-ht) (info-searches info)])
                (unless (ormap (lambda (k) (hash-ref ht k #f))
                               (hash-map s-ht (lambda (k v) k)))
                  (not-found s-key)))))
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
        ;; Build again, using dependencies
        (let ([need-rerun (filter-map (lambda (i) 
                                        (and (info-need-run? i)
                                             (begin
                                               (when (info-need-in-write? i) 
                                                 (write-in/info latex-dest i)
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
              [(list in-delta? out-delta? defs undef)
               (set-info-rendered?! info #t)
               (set-info-provides! info defs)
               (set-info-undef! info undef)
               (when out-delta?
                 (set-info-out-time! info (/ (current-inexact-milliseconds) 1000)))
               (when in-delta? 
                 ;; Reset expected dependencies to known dependencies, and recompute later:
                 (set-info-deps! info (info-known-deps info))
                 (set-info-need-in-write?! info #t))
               (set-info-time! info (/ (current-inexact-milliseconds) 1000))]))
          (if (not (worker-count . > . 1))
              (map (lambda (i) 
                     (say-rendering i #f)
                     (update-info i (build-again! latex-dest i with-record-error))) need-rerun)
              (parallel-do 
               worker-count
               (lambda (workerid) (list workerid (verbose) latex-dest))
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
               (define-worker (build-again!-worker2 workerid verbosev latex-dest)
                 (define (with-record-error cc go fail-k)
                   (with-handlers ([exn:fail?
                                    (lambda (x)
                                      (eprintf "~a\n" (exn-message x))
                                      (raise x))])
                     (go)))
                 (verbose verbosev)
                 (match-message-loop
                  [info 
                   (send/success 
                    (s-exp->fasl (serialize (build-again! latex-dest (deserialize (fasl->s-exp info)) with-record-error))))])))))
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
    (for ([i infos] #:when (info-need-in-write? i)) (write-in/info latex-dest i))))

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
                         (memq 'user-doc-root flags))])
        (new ((if multi? html:render-multi-mixin values)
              (html:render-mixin render%))
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
  (cond [latex-dest
         (let-values ([(base name dir?) (split-path (doc-src-file doc))])
           (build-path latex-dest (path-replace-suffix name #".tex")))]
        [(memq 'multi-page (doc-flags doc)) (doc-dest-dir doc)]
        [else (build-path (doc-dest-dir doc) "index.html")]))

(define (sxref-path latex-dest doc file)
  (cond [latex-dest
         (let-values ([(base name dir?) (split-path (doc-src-file doc))])
           (build-path latex-dest (path-replace-suffix name (string-append "." file))))]
        [else (build-path (doc-dest-dir doc) file)]))

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

(define (load-sxref filename)
  (call-with-input-file filename (lambda (x) (fasl->s-exp x))))

(define (file-or-directory-modify-seconds/stamp file
                                                stamp-time stamp-data pos
                                                get-sha1)
  (let ([t (file-or-directory-modify-seconds file #f (lambda () +inf.0))])
    (cond
     [(t . <= . stamp-time) stamp-time]
     [(equal? (list-ref stamp-data pos) (get-sha1 file)) stamp-time]
     [else t])))

(define ((get-doc-info only-dirs latex-dest auto-main? auto-user?
                       with-record-error setup-printf workerid)
         doc)
  (let* ([info-out-file (sxref-path latex-dest doc "out.sxref")]
         [info-in-file  (sxref-path latex-dest doc "in.sxref")]
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
                                     (if latex-dest
                                         "latex-render.rkt"
                                         "html-render.rkt")
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
         [info-out-time (file-or-directory-modify-seconds info-out-file #f (lambda () #f))]
         [info-in-time (file-or-directory-modify-seconds info-in-file #f (lambda () #f))]
         [info-time (min (or info-out-time -inf.0) (or info-in-time -inf.0))]
         [vers (send renderer get-serialize-version)]
         [src-time (file-or-directory-modify-seconds/stamp
                    src-zo
                    stamp-time stamp-data 0
                    get-compiled-file-sha1)]
         [up-to-date?
          (and info-out-time
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
    (when (or (not up-to-date?) (verbose))
      (setup-printf
       (string-append
        (if workerid (format "~a " workerid) "")
        (cond [up-to-date? "using"] [can-run? "running"] [else "skipping"]))
       "~a"
       (path->relative-string/setup (doc-src-file doc))))

    (if up-to-date?
        ;; Load previously calculated info:
        (render-time
         "use"
         (with-handlers ([exn:fail? (lambda (exn)
                                      (log-error (format "get-doc-info error: ~a"
                                                         (exn-message exn)))
                                      (delete-file info-out-file)
                                      (delete-file info-in-file)
                                      ((get-doc-info only-dirs latex-dest auto-main?
                                                     auto-user? with-record-error
                                                     setup-printf workerid)
                                       doc))])
           (let* ([v-in  (load-sxref info-in-file)]
                  [v-out (load-sxref info-out-file)])
             (unless (and (equal? (car v-in) (list vers (doc-flags doc)))
                          (equal? (car v-out) (list vers (doc-flags doc))))
               (error "old info has wrong version or flags"))
             (make-info
              doc
              (let ([v (list-ref v-out 2)]) ; provides
                (with-my-namespace
                 (lambda ()
                   (deserialize v))))
              (let ([v (list-ref v-in 1)])  ; undef
                (with-my-namespace
                 (lambda ()
                   (deserialize v))))
              (let ([v (list-ref v-in 3)])  ; searches
                (with-my-namespace
                 (lambda ()
                   (deserialize v))))
              (map rel->path (list-ref v-in 2)) ; expected deps, in case we don't need to build...
              null ; known deps (none at this point)
              can-run?
              my-time info-out-time
              (and can-run? (memq 'always-run (doc-flags doc)))
              #f 
              #f
              vers
              #f
              #f))))
        (if can-run?
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
                        [out-v (and info-out-time
                                    (info-out-time . >= . src-time)
                                    (with-handlers ([exn:fail? (lambda (exn) #f)])
                                      (let ([v (load-sxref info-out-file)])
                                        (unless (equal? (car v) (list vers (doc-flags doc)))
                                          (error "old info has wrong version or flags"))
                                        v)))]
                        [sci (send renderer serialize-info ri)]
                        [defs (send renderer get-defined ci)]
                        [undef (send renderer get-external ri)]
                        [searches (resolve-info-searches ri)]
                        [need-out-write?
                         (or (not out-v)
                             (not (equal? (list vers (doc-flags doc))
                                          (car out-v)))
                             (not (serialized=? sci (cadr out-v)))
                             (not (equal? (any-order defs) (any-order (deserialize (caddr out-v)))))
                             (info-out-time . > . (current-seconds)))])
                   (when (and (verbose) need-out-write?)
                     (fprintf (current-error-port) " [New out ~a]\n" (doc-src-file doc)))
                   (gc-point)
                   (let ([info
                          (make-info doc
                                     defs     ; provides
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
                       (render-time "xref-out" (write-out/info latex-dest info sci))
                       (set-info-need-out-write?! info #f))
                     (when (info-need-in-write? info)
                       (render-time "xref-in" (write-in/info latex-dest info))
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
  expr
  #;
  (begin
  (printf "For ~a\n" what)
  (time expr))
  #;
  (begin
  (collect-garbage) (collect-garbage) (printf "pre: ~a ~s\n" what (current-memory-use))
  (begin0
  (time expr)
  (collect-garbage) (collect-garbage) (printf "post ~a ~s\n" what (current-memory-use)))))

(define (load-sxrefs latex-dest doc vers)
  (match (list (load-sxref (sxref-path latex-dest doc "in.sxref")) (load-sxref (sxref-path latex-dest doc "out.sxref")))
    [(list (list in-version undef deps-rel searches dep-docs) (list out-version sci provides))
     (unless (and (equal? in-version  (list vers (doc-flags doc)))
                  (equal? out-version (list vers (doc-flags doc))))
       (error "old info has wrong version or flags"))
     (with-my-namespace*
      (values (deserialize undef) 
              deps-rel
              (deserialize searches)
              (map rel-doc->doc (deserialize dep-docs))
              sci
              (deserialize provides)))]))

(define (build-again! latex-dest info with-record-error)
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
  (define (load-doc-sci doc)
    (cadr (load-sxref (sxref-path latex-dest doc "out.sxref"))))
  (define doc (if (info? info ) (info-doc info) info))
  (define renderer (make-renderer latex-dest doc))
  (with-record-error
   (doc-src-file doc)
   (lambda ()
     (define vers (send renderer get-serialize-version))
     (define-values (ff-undef ff-deps-rel ff-searches ff-dep-docs ff-sci ff-provides)
       (if (info? info)
           (values (info-undef info) 
                   (info-deps->rel-doc-src-file info)
                   (info-searches info)
                   (info-deps->doc info)
                   (load-doc-sci doc)
                   (info-provides info))
           (load-sxrefs latex-dest doc vers)))
     
     (parameterize ([current-directory (doc-src-dir doc)])
       (let* ([v (render-time "load" (load-doc/ensure-prefix doc))]
              [dest-dir (pick-dest latex-dest doc)]
              [fp (render-time "traverse" (send renderer traverse (list v) (list dest-dir)))]
              [ci (render-time "collect" (send renderer collect (list v) (list dest-dir) fp))]
              [ri (begin 
                    (render-time "deserialize" 
                                 (with-my-namespace* 
                                  (for ([dep-doc ff-dep-docs]) 
                                    (send renderer deserialize-info (load-doc-sci dep-doc) ci))))
                    (render-time "resolve" (send renderer resolve (list v) (list dest-dir) ci)))]
              [sci (render-time "serialize" (send renderer serialize-info ri))]
              [defs (render-time "defined" (send renderer get-defined ci))]
              [undef (render-time "undefined" (send renderer get-external ri))]
              [in-delta? (not (equal? (any-order undef) (any-order ff-undef)))]
              [out-delta? (or (not (serialized=? sci ff-sci))
                              (not (equal? (any-order defs) (any-order ff-provides))))])
         (when (verbose)
           (printf " [~a~afor ~a]\n"
                   (if in-delta? "New in " "")
                   (cond [out-delta? "New out "]
                         [in-delta? ""]
                         [else "No change "])
                   (doc-src-file doc)))

         (when in-delta?
           (render-time "xref-in" (write-in latex-dest vers doc undef ff-deps-rel ff-searches ff-dep-docs)))
         (when out-delta?
           (render-time "xref-out" (write-out latex-dest vers doc sci defs)))

         (cleanup-dest-dir doc)
         (render-time
          "render"
          (with-record-error
           (doc-src-file doc)
           (lambda () (send renderer render (list v) (list dest-dir) ri))
           void))
         (gc-point)
         (list in-delta? out-delta? defs undef))))
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

(define (write- latex-dest vers doc name data)
  (let* ([filename (sxref-path latex-dest doc name)])
    (when (verbose) (printf " [Caching to disk ~a]\n" filename))
    (make-directory*/ignore-exists-exn (doc-dest-dir doc))
    (with-compile-output filename 
                         (lambda (out tmp-filename)
                           (write-bytes (s-exp->fasl (append (list (list vers (doc-flags doc))) data)) out)))))

(define (write-out latex-dest vers doc sci provides)
  (write- latex-dest vers doc "out.sxref" 
          (list sci
                (serialize provides))))

(define (write-out/info latex-dest info sci)
  (write-out latex-dest (info-vers info) (info-doc info) sci (info-provides info)))

(define (write-in latex-dest vers doc undef rels searches dep-docs)
  (write- latex-dest vers doc "in.sxref" 
          (list (serialize undef)
                rels
                (serialize searches)
                ;; The following last element is used only by the parallel build.
                ;; It's redundant in the sense that the same information
                ;; is in `rels' --- the docs that this one depends on ---
                ;; but putting the whole `doc' record here makes it easier
                ;; for a place to reconstruct a suitable `doc' record.
                ;; It probably would be better to reconstruct the `doc'
                ;; record in a place from the path.
                (serialize (map doc->rel-doc dep-docs)))))

(define (write-in/info latex-dest info)
  (write-in latex-dest
            (info-vers info)
            (info-doc info)
            (info-undef info)
            (info-deps->rel-doc-src-file info)
            (info-searches info)
            (info-deps->doc info)))

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

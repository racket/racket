#lang scheme/base

(require "getinfo.ss"
         "dirs.ss"
         "private/path-utils.ss"
         "main-collects.ss"
         "main-doc.ss"
         scheme/class
         scheme/list
         scheme/file
         scheme/fasl
         scheme/serialize
         compiler/cm
         syntax/modread
         scribble/base-render
         scribble/core
         scribble/html-properties
         scribble/manual ; really shouldn't be here... see dynamic-require-doc
         scribble/private/run-pdflatex
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render))

(provide setup-scribblings
         verbose
         run-pdflatex)

(define verbose (make-parameter #t))

(define-struct doc (src-dir src-spec src-file dest-dir flags under-main? category))
(define-struct info (doc get-sci provides undef searches deps known-deps
                     build? time out-time need-run?
                     need-in-write? need-out-write?
                     vers rendered? failed?)
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

(define (setup-scribblings
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
  (define (get-docs i rec)
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
                           (build-path dir (car d))
                           (doc-path dir (cadddr d) flags)
                           flags under-main? (caddr d))))
             s)
        (begin (setup-printf
                "WARNING"
                "bad 'scribblings info: ~e from: ~e" (i 'scribblings) dir)
               null))))
  (define docs
    (let* ([recs (find-relevant-directory-records '(scribblings) 'all-available)]
           [infos (map get-info/full (map directory-record-path recs))])
      (filter-user-docs (append-map get-docs infos recs) make-user?)))
  (define-values (main-docs user-docs) (partition doc-under-main? docs))
  (define (can-build*? docs) (can-build? only-dirs docs))
  (define auto-main? (and auto-start-doc? (ormap can-build*? main-docs)))
  (define auto-user? (and auto-start-doc? (ormap can-build*? user-docs)))
  (define infos
    (and (ormap can-build*? docs)
         (filter values
                 (map (get-doc-info only-dirs latex-dest auto-main? auto-user?
                                    with-record-error setup-printf)
                      docs))))
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
                       (setup-printf "WARNING"
                                     "undefined tag in ~a:" 
                                     (path->name (doc-src-file
                                                  (info-doc info))))
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
            #:when (not (or (info-need-run? i) (not (info-build? i)))))
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
        (for ([i infos] #:when (info-need-run? i))
          (set-info-deps! i (filter info? (info-deps i)))
          (set-info-need-run?! i #f)
          (build-again! latex-dest i with-record-error setup-printf))
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
    (unless latex-dest
      (for ([i infos] #:when (info-need-in-write? i)) (write-in i)))))

(define (make-renderer latex-dest doc)
  (if latex-dest
    (new (latex:render-mixin render%)
         [dest-dir latex-dest]
         ;; Use PLT manual style:
         [prefix-file (build-path (collection-path "scribble") "manual-prefix.tex")]
         [style-file (build-path (collection-path "scribble") "manual-style.tex")])
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
                                           (cons (build-path (collection-path "scribble") s)
                                                 (format "../~a" s)))])
                           (list (std-path "scribble.css")
                                 (std-path "scribble-style.css")
                                 (std-path "scheme.css")
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

(define (can-build? only-dirs doc)
  (or (not only-dirs)
      (ormap (lambda (d)
               (let ([d (path->directory-path d)])
                 (let loop ([dir (path->directory-path (doc-src-dir doc))])
                   (or (equal? dir d)
                       (let-values ([(base name dir?) (split-path dir)])
                         (and (path? base) (loop base)))))))
             only-dirs)))

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

(define (omit? cat)
  (or (eq? cat 'omit)
      (and (pair? cat)
           (eq? (car cat) 'omit))))

(define (any-order keys)
  (let ([ht (make-hash)])
    (for-each (lambda (k) (hash-set! ht k #t)) keys)
    ht))

(define (read-sxref)
  (fasl->s-exp (current-input-port)))

(define (make-sci-cached sci info-out-file setup-printf)
  (when (verbose)
    (fprintf (current-error-port) " [Lazy ~a]\n" info-out-file))
  (let ([b (make-weak-box sci)])
    (lambda ()
      (let ([v (weak-box-value b)])
        (or v
            (begin
              (when (verbose)
                (void)
                #;
                (fprintf (current-error-port) " [Re-load ~a]\n" info-out-file))
              (let ([v (cadr (with-input-from-file info-out-file read-sxref))])
                (set! b (make-weak-box v))
                v)))))))

(define (make-sci-computed sci)
  (lambda () sci))

(define (file-or-directory-modify-seconds/stamp file
                                                stamp-time stamp-data pos
                                                get-sha1)
  (let ([t (file-or-directory-modify-seconds file #f (lambda () +inf.0))])
    (cond
     [(t . <= . stamp-time) stamp-time]
     [(equal? (list-ref stamp-data pos) (get-sha1 file)) stamp-time]
     [else t])))

(define ((get-doc-info only-dirs latex-dest auto-main? auto-user?
                       with-record-error setup-printf)
         doc)
  (let* ([info-out-file (build-path (or latex-dest (doc-dest-dir doc)) "out.sxref")]
         [info-in-file  (build-path (or latex-dest (doc-dest-dir doc)) "in.sxref")]
         [stamp-file  (build-path (or latex-dest (doc-dest-dir doc)) "stamp.sxref")]
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
         [css-path (build-path (collection-path "scribble")
                               "scribble.css")]
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
       (cond [up-to-date? "using"] [can-run? "running"] [else "skipping"])
       "~a"
       (path->name (doc-src-file doc))))
    (if up-to-date?
      ;; Load previously calculated info:
      (render-time
       "use"
       (with-handlers ([exn:fail? (lambda (exn)
                                    (fprintf (current-error-port) "~a\n" (exn-message exn))
                                    (delete-file info-out-file)
                                    (delete-file info-in-file)
                                    ((get-doc-info only-dirs latex-dest auto-main?
                                                   auto-user? with-record-error
                                                   setup-printf)
                                     doc))])
         (let* ([v-in (with-input-from-file info-in-file read-sxref)]
                [v-out (with-input-from-file info-out-file read-sxref)])
           (unless (and (equal? (car v-in) (list vers (doc-flags doc)))
                        (equal? (car v-out) (list vers (doc-flags doc))))
             (error "old info has wrong version or flags"))
           (make-info
            doc
            (make-sci-cached
             (list-ref v-out 1) ; sci (leave serialized)
             info-out-file
             setup-printf)
            (let ([v (list-ref v-out 2)])  ; provides
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
            #f #f
            vers
            #f
            #f))))
      (if can-run?
        ;; Run the doc once:
        (with-record-error
         (doc-src-file doc)
         (lambda ()
           (parameterize ([current-directory (doc-src-dir doc)])
             (let* ([v (ensure-doc-prefix
                        (dynamic-require-doc (doc-src-spec doc))
                        (doc-src-spec doc))]
                    [dest-dir (pick-dest latex-dest doc)]
                    [ci (send renderer collect (list v) (list dest-dir))]
                    [ri (send renderer resolve (list v) (list dest-dir) ci)]
                    [out-v (and info-out-time
                                (with-handlers ([exn:fail? (lambda (exn) #f)])
                                  (let ([v (with-input-from-file info-out-file read-sxref)])
                                    (unless (equal? (car v) (list vers (doc-flags doc)))
                                      (error "old info has wrong version or flags"))
                                    v)))]
                    [sci (send renderer serialize-info ri)]
                    [defs (send renderer get-defined ci)]
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
                                 (if need-out-write?
                                     (make-sci-computed sci)
                                     (make-sci-cached sci info-out-file setup-printf))
                                 defs
                                 (send renderer get-undefined ri)
                                 searches
                                 null ; no deps, yet
                                 null ; no known deps, yet
                                 can-run?
                                 -inf.0
                                 (if need-out-write?
                                     (/ (current-inexact-milliseconds) 1000)
                                     info-out-time)
                                 #t
                                 can-run? need-out-write?
                                 vers
                                 #f
                                 #f)])
                 (when need-out-write?
                   (unless latex-dest 
                     (render-time "xref-out" (write-out info setup-printf)))
                   (set-info-need-out-write?! info #f))
                 (when (info-need-in-write? info)
                   (unless latex-dest 
                     (render-time "xref-in" (write-in info)))
                   (set-info-need-in-write?! info #f))
                 (when (or (stamp-time . < . aux-time)
                           (stamp-time . < . src-time))
                   (let ([data (list (get-compiled-file-sha1 src-zo)
                                     (get-compiled-file-sha1 renderer-path)
                                     (get-file-sha1 css-path))])
                     (with-output-to-file stamp-file #:exists 'truncate/replace (lambda () (write data)))
                     (file-or-directory-modify-seconds stamp-file (max aux-time src-time))))
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

(define (build-again! latex-dest info with-record-error setup-printf)
  (define doc (info-doc info))
  (define renderer (make-renderer latex-dest doc))
  (setup-printf (format "~arendering"
                        (if (info-rendered? info) "re-" ""))
                "~a"
                (path->name (doc-src-file doc)))
  (set-info-rendered?! info #t)
  (with-record-error
   (doc-src-file doc)
   (lambda ()
     (parameterize ([current-directory (doc-src-dir doc)])
       (let* ([v (ensure-doc-prefix (render-time 
                                     "load"
                                     (dynamic-require-doc (doc-src-spec doc)))
                                    (doc-src-spec doc))]
              [dest-dir (pick-dest latex-dest doc)]
              [ci (render-time "collect" 
                               (send renderer collect (list v) (list dest-dir)))])
         (render-time
          "deserialize"
          (for ([i (info-deps info)])
            (when (info? i)
              (with-my-namespace
               (lambda ()
                 (send renderer deserialize-info ((info-get-sci i)) ci))))))
         (let* ([ri (render-time "resolve" (send renderer resolve (list v) (list dest-dir) ci))]
                [sci (render-time "serialize" (send renderer serialize-info ri))]
                [defs (render-time "defined" (send renderer get-defined ci))]
                [undef (render-time "undefined" (send renderer get-undefined ri))]
                [in-delta? (not (equal? (any-order undef)
                                        (any-order (info-undef info))))]
                [out-delta? (or (not (serialized=? sci ((info-get-sci info))))
                                (not (equal? (any-order defs)
                                             (any-order (info-provides info)))))])
           (when (verbose)
             (printf " [~a~afor ~a]\n"
                     (if in-delta? "New in " "")
                     (cond [out-delta? "New out "]
                           [in-delta? ""]
                           [else "No change "])
                     (doc-src-file doc)))
           (when out-delta?
             (set-info-out-time! info (/ (current-inexact-milliseconds) 1000)))
           (set-info-provides! info defs)
           (set-info-undef! info undef)
           (when in-delta? 
             ;; Reset expected dependencies to known dependencies, and recompute later:
             (set-info-deps! info (info-known-deps info)))
           (when (or out-delta? (info-need-out-write? info))
             (set-info-get-sci! info (make-sci-computed sci))
             (unless latex-dest 
               (render-time "xref-out" (write-out info setup-printf)))
             (set-info-need-out-write?! info #f))
           (when in-delta? (set-info-need-in-write?! info #t))
           (unless latex-dest
             (let ([dir (doc-dest-dir doc)])
               (if (not (directory-exists? dir))
                 (make-directory dir)
                 (for ([f (directory-list dir)]
                       #:when
                       (and (file-exists? f)
                            (not (regexp-match? #"[.]sxref$"
                                                (path-element->bytes f)))))
                   (delete-file (build-path dir f))))))
           (render-time
            "render"
            (with-record-error
             (doc-src-file doc)
             (lambda () (send renderer render (list v) (list dest-dir) ri))
             void))
           (set-info-time! info (/ (current-inexact-milliseconds) 1000))
           (gc-point)
           (void)))))
   (lambda () (set-info-failed?! info #t))))

(define (gc-point)
  ;; Forcing a GC on document boundaries helps keep peak memory use down.
  (collect-garbage))

(define-namespace-anchor anchor)

(define (with-my-namespace thunk)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    (thunk)))

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
      ;;  hard-wiring the "manual.ss" library:
      (namespace-attach-module ns 'scribble/manual p)
      (parameterize ([current-namespace p])
        (call-in-nested-thread (lambda () (dynamic-require mod-path 'doc)))))))

(define (write- info name sel)
  (let* ([doc (info-doc info)]
         [info-file (build-path (doc-dest-dir doc) name)])
    (when (verbose) (printf " [Caching ~a]\n" info-file))
    (with-output-to-file info-file #:exists 'truncate/replace
      (lambda ()
        (sel (lambda ()
               (list (list (info-vers info) (doc-flags doc))
                     ((info-get-sci info))
                     (serialize (info-provides info))))
             (lambda ()
               (list (list (info-vers info) (doc-flags doc))
                     (serialize (info-undef info))
                     (convert-deps (info-deps info))
                     (serialize (info-searches info)))))))))

(define (write-out info setup-printf)
  (make-directory* (doc-dest-dir (info-doc info)))
  (write- info "out.sxref" (lambda (o i) (write-bytes (s-exp->fasl (o)))))
  (set-info-get-sci! info
                     (make-sci-cached ((info-get-sci info))
                                      (build-path (doc-dest-dir (info-doc info)) "out.sxref")
                                      setup-printf)))
(define (write-in info)
  (make-directory* (doc-dest-dir (info-doc info)))
  (write- info "in.sxref" (lambda (o i) (write-bytes (s-exp->fasl (i))))))

(define (rel->path r)
  (if (bytes? r)
      (bytes->path r)
      (main-collects-relative->path r)))

(define (path->rel r)
  (let ([r (path->main-collects-relative r)])
    (if (path? r)
        (path->bytes r)
        r)))

(define (convert-deps deps)
  (filter
   values
   (map (lambda (i)
          (and (info? i)
               (path->rel (doc-src-file (info-doc i)))))
        deps)))

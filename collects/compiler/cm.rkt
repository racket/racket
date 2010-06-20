#lang scheme/base
(require syntax/modcode
         syntax/modresolve
         setup/main-collects
	 unstable/file
         scheme/file
         scheme/list
         scheme/path
         racket/promise
         openssl/sha1)

(provide make-compilation-manager-load/use-compiled-handler
         managed-compile-zo
         make-caching-managed-compile-zo
         trust-existing-zos
         manager-compile-notify-handler
         manager-skip-file-handler
         file-stamp-in-collection
         file-stamp-in-paths
         (rename-out [trace manager-trace-handler])
         get-file-sha1
         get-compiled-file-sha1)

(define manager-compile-notify-handler (make-parameter void))
(define trace (make-parameter void))
(define indent (make-parameter ""))
(define trust-existing-zos (make-parameter #f))
(define manager-skip-file-handler (make-parameter (λ (x) #f)))

(define (file-stamp-in-collection p)
  (file-stamp-in-paths p (current-library-collection-paths)))

(define (file-stamp-in-paths p paths)
  (let ([p-eles (explode-path (simple-form-path p))])
    (let c-loop ([paths paths])
      (cond
        [(null? paths) #f]
        [else
         (let i-loop ([collects-eles (explode-path (simple-form-path (car paths)))]
                      [p-eles p-eles])
           (cond
             [(null? collects-eles)
              ;; we're inside the collection hierarchy, so we just 
              ;; use the date of the original file (or the zo, whichever
              ;; is newer).
              (let-values ([(base name dir) (split-path p)])
                (let* ([p-date (file-or-directory-modify-seconds p #f (lambda () #f))]
                       [alt-date (and (not p-date)
                                      (file-or-directory-modify-seconds 
                                       (rkt->ss p) 
                                       #f 
                                       (lambda () #f)))]
                       [date (or p-date alt-date)]
                       [get-path (lambda ()
                                   (if p-date
                                       p
                                       (rkt->ss p)))]
                       [mode (car (use-compiled-file-paths))]
                       [get-zo-date (lambda (name)
                                      (file-or-directory-modify-seconds
                                       (build-path 
                                        base
                                        mode
                                        (path-add-suffix name #".zo"))
                                       #f
                                       (lambda () #f)))]
                       [main-zo-date (and (or p-date (not alt-date))
                                          (get-zo-date name))]
                       [alt-zo-date (and (or alt-date
                                             (and (not p-date) 
                                                  (not alt-date)
                                                  (not main-zo-date)))
                                         (get-zo-date (rkt->ss name)))]
                       [zo-date (or main-zo-date alt-zo-date)]
                       [get-zo-path (lambda ()
                                      (if main-zo-date
                                          (path-add-suffix name #".zo")
                                          (path-add-suffix (rkt->ss name) #".zo")))])
                  (cond
                   [(and zo-date
                         (or (not date)
                             (zo-date . > . date)))
                    (cons zo-date
                          (delay (get-compiled-file-sha1 (get-zo-path))))]
                   [date
                    (cons date
                          (delay (get-source-sha1 (get-path))))]
                   [else #f])))]
             [(null? p-eles) 
              ;; this case shouldn't happen... I think.
              (c-loop (cdr paths))]
             [else
              (cond
                [(equal? (car p-eles) (car collects-eles))
                 (i-loop (cdr collects-eles) (cdr p-eles))]
                [else 
                 (c-loop (cdr paths))])]))]))))

(define (trace-printf fmt . args)
  (let ([t (trace)])
    (unless (eq? t void)
      (t (string-append (indent) (apply format fmt args))))))

(define (get-deps code path)
  (filter-map (lambda (x)
                (let ([r (resolve-module-path-index x path)])
                  (and (path? r) 
                       (path->bytes r))))
              (append-map cdr (module-compiled-imports code))))

(define (get-compilation-dir+name mode path)
  (let-values ([(base name must-be-dir?) (split-path path)])
    (values 
     (cond
       [(eq? 'relative base) mode]
       [else (build-path base mode)])
     name)))

(define (get-compilation-path mode path)
  (let-values ([(dir name) (get-compilation-dir+name mode path)])
    (build-path dir name)))

(define (get-compilation-dir mode path)
  (let-values ([(dir name) (get-compilation-dir+name mode path)])
    dir))

(define (touch path)
  (file-or-directory-modify-seconds 
   path
   (current-seconds)
   (lambda ()
     (close-output-port (open-output-file path #:exists 'append)))))

(define (try-file-time path)
  (file-or-directory-modify-seconds path #f (lambda () #f)))

(define (try-delete-file path)
  ;; Attempt to delete, but give up if it doesn't work:
  (with-handlers ([exn:fail:filesystem? void])
    (trace-printf "deleting: ~a" path)
    (delete-file path)))

(define (compilation-failure mode path zo-name date-path reason)
  (try-delete-file zo-name)
  (trace-printf "failure"))

;; with-compile-output : path (output-port -> alpha) -> alpha
;;  Open a temporary path for writing, automatically renames after,
;;  and arranges to delete path if there's
;;  an exception. Breaks are managed so that the port is reliably
;;  closed and the file is reliably deleted if there's a break
(define (with-compile-output path proc)
  (let ([bp (current-break-parameterization)]
        [tmp-path (make-temporary-file "tmp~a" #f (path-only path))]
        [ok? #f])
    (dynamic-wind
     void
     (lambda ()
       (begin0
         (let ([out (open-output-file tmp-path #:exists 'truncate/replace)])
           (dynamic-wind
            void
            (lambda ()
              (call-with-break-parameterization bp (lambda () (proc out tmp-path))))
            (lambda ()
              (close-output-port out))))
         (set! ok? #t)))
     (lambda ()
       (if ok?
           (rename-file-or-directory tmp-path path #t)
           (try-delete-file tmp-path))))))

(define (get-source-sha1 p)
  (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
    (call-with-input-file* p sha1)))

(define (get-dep-sha1s deps up-to-date read-src-syntax mode must-exist?)
  (let ([l (for/fold ([l null]) ([dep (in-list deps)])
             (and l
                  ;; (cons 'ext rel-path) => a non-module file, check source
                  ;; rel-path => a module file name, check cache
                  (let* ([ext? (and (pair? dep) (eq? 'ext (car dep)))]
                         [p (main-collects-relative->path (if ext? (cdr dep) dep))])
                    (cond
                     [ext? (let ([v (get-source-sha1 p)])
                             (cond
                              [v (cons (cons (delay v) dep) l)]
                              [must-exist? (error 'cm "cannot find external-dependency file: ~v" p)]
                              [else #f]))]
                     [(or (hash-ref up-to-date (simple-form-path p) #f)
                          ;; Use `compiler-root' with `sha1-only?' as #t:
                          (compile-root mode p up-to-date read-src-syntax #t))
                      => (lambda (sh)
                           (cons (cons (cdr sh) dep) l))]
                     [must-exist?
                      ;; apparently, we're forced to use the source of the module,
                      ;; so compute a sha1 from it instead of the bytecode
                      (cons (cons (get-source-sha1 p) dep) l)]
                     [else #f]))))])
    (and l
         (let ([p (open-output-string)]
               [l (map (lambda (v) (cons (force (car v)) (cdr v))) l)])
           ;; sort by sha1s so that order doesn't matter
           (write (sort l string<? #:key car) p)
           ;; compute one hash from all hashes
           (sha1 (open-input-bytes (get-output-bytes p)))))))

(define (write-deps code mode path src-sha1 external-deps reader-deps up-to-date read-src-syntax)
  (let ([dep-path (path-add-suffix (get-compilation-path mode path) #".dep")]
        [deps (remove-duplicates (append (get-deps code path)
                                         reader-deps))]
        [external-deps (remove-duplicates external-deps)])
    (with-compile-output dep-path
      (lambda (op tmp-path)
        (let ([deps (append
                     (map path->main-collects-relative deps)
                     (map (lambda (x)
                            (cons 'ext (path->main-collects-relative x)))
                          external-deps))])
        (write (list* (version)
                      (cons (or src-sha1 (get-source-sha1 path))
                            (get-dep-sha1s deps up-to-date read-src-syntax mode #t))
                      deps)
               op)
        (newline op))))))

(define (format-time sec)
  (let ([d (seconds->date sec)])
    (format "~a-~a-~a ~a:~a:~a"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))

(define (verify-times ss-name zo-name)
  (define ss-sec (try-file-time ss-name))
  (define zo-sec (try-file-time zo-name))
  (cond [(not ss-sec) (error 'compile-zo "internal error")]
        [(not zo-sec) (error 'compile-zo "failed to create .zo file (~a) for ~a"
                             zo-name ss-name)]
        [(< zo-sec ss-sec) (error 'compile-zo
                                  "date for newly created .zo file (~a @ ~a) ~
                                   is before source-file date (~a @ ~a)~a"
                                  zo-name (format-time zo-sec)
                                  ss-name (format-time ss-sec)
                                  (if (> ss-sec (current-seconds))
                                    ", which appears to be in the future"
                                    ""))]))

(define-struct ext-reader-guard (proc top)
  #:property prop:procedure (struct-field-index proc))
(define-struct file-dependency (path) #:prefab)

(define (compile-zo* mode path src-sha1 read-src-syntax zo-name up-to-date)
  ;; The `path' argument has been converted to .rkt or .ss form,
  ;;  as appropriate.
  ;; External dependencies registered through reader guard and
  ;; accomplice-logged events:
  (define external-deps null)
  (define reader-deps null)
  (define deps-sema (make-semaphore 1))
  (define done-key (gensym))
  (define (external-dep! p)
    (call-with-semaphore
     deps-sema
     (lambda ()
       (set! external-deps (cons (path->bytes p) external-deps)))))
  (define (reader-dep! p)
    (call-with-semaphore
     deps-sema
     (lambda ()
       (set! reader-deps (cons (path->bytes p) reader-deps)))))

  ;; Set up a logger to receive and filter accomplice events:
  (define accomplice-logger (make-logger))
  (define log-th
    (let ([orig-log (current-logger)]
          [receiver (make-log-receiver accomplice-logger 'info)])
      (thread (lambda ()
                (let loop ()
                  (let ([l (sync receiver)])
                    (unless (eq? (vector-ref l 2) done-key)
                      (if (and (eq? (vector-ref l 0) 'info)
                               (file-dependency? (vector-ref l 2))
                               (path? (file-dependency-path (vector-ref l 2))))
                        (external-dep! (file-dependency-path (vector-ref l 2)))
                        (log-message orig-log (vector-ref l 0) (vector-ref l 1)
                                     (vector-ref l 2)))
                      (loop))))))))

  ;; Compile the code:
  (define code
    (parameterize ([current-reader-guard
                    (let* ([rg (current-reader-guard)]
                           [rg (if (ext-reader-guard? rg)
                                 (ext-reader-guard-top rg)
                                 rg)])
                      (make-ext-reader-guard
                       (lambda (d)
                         ;; Start by calling the top installed guard to
                         ;; transform the module path, avoiding redundant
                         ;; dependencies by avoiding accumulation of these
                         ;; guards.
                         (let ([d (rg d)])
                           (when (module-path? d)
                             (let ([p (resolved-module-path-name
                                       (module-path-index-resolve
                                        (module-path-index-join d #f)))])
                               (when (path? p) (reader-dep! p))))
                           d))
                       rg))]
                   [current-logger accomplice-logger])
      (get-module-code path mode compile
                       (lambda (a b) #f) ; extension handler
                       #:source-reader read-src-syntax)))
  (define code-dir (get-compilation-dir mode path))

  ;; Wait for accomplice logging to finish:
  (log-message accomplice-logger 'info "stop" done-key)
  (sync log-th)

  ;; Write the code and dependencies:
  (when code
    (make-directory*/ignore-exists-exn code-dir)
    (with-compile-output zo-name
      (lambda (out tmp-name)
        (with-handlers ([exn:fail?
                         (lambda (ex)
                           (close-output-port out)
                           (compilation-failure mode path zo-name #f
                                                (exn-message ex))
                           (raise ex))])
          (parameterize ([current-write-relative-directory
                          (let-values ([(base name dir?) (split-path path)])
                            (if (eq? base 'relative)
                              (current-directory)
                              (path->complete-path base (current-directory))))])
            (write code out)))
        ;; redundant, but close as early as possible:
        (close-output-port out)
        ;; Note that we check time and write .deps before returning from
        ;; with-compile-output...
        (verify-times path tmp-name)
        (write-deps code mode path src-sha1 external-deps reader-deps up-to-date read-src-syntax)))))

(define depth (make-parameter 0))

(define (actual-source-path path)
  (if (file-exists? path) 
      path
      (let ([alt-path (rkt->ss path)])
        (if (file-exists? alt-path)
            alt-path
            path))))

(define (maybe-compile-zo sha1-only? deps mode path orig-path read-src-syntax up-to-date)
  (let ([actual-path (actual-source-path orig-path)])
    (unless sha1-only?
      ((manager-compile-notify-handler) actual-path)
      (trace-printf "compiling: ~a" actual-path))
    (begin0
     (parameterize ([indent (string-append "  " (indent))])
       (let* ([zo-name (path-add-suffix (get-compilation-path mode path) #".zo")]
              [zo-exists? (file-exists? zo-name)])
         (if (and zo-exists? (trust-existing-zos))
             (begin
               (log-info (format "cm: ~atrusting ~a" 
                                 (build-string 
                                  (depth)
                                  (λ (x) (if (= 2 (modulo x 3)) #\| #\space)))
                                 zo-name))
               (touch zo-name)
               #f)
             (let ([src-sha1 (and zo-exists?
                                  deps
                                  (cadr deps)
                                  (get-source-sha1 path))])
               (if (and zo-exists?
                        src-sha1
                        (equal? src-sha1 (caadr deps))
                        (equal? (get-dep-sha1s (cddr deps) up-to-date read-src-syntax mode #f)
                                (cdadr deps)))
                   (begin
                     (log-info (format "cm: ~ahash-equivalent ~a" 
                                       (build-string 
                                        (depth)
                                        (λ (x) (if (= 2 (modulo x 3)) #\| #\space)))
                                       zo-name))
                     (touch zo-name)
                     #f)
                   ((if sha1-only? values (lambda (build) (build) #f))
                    (lambda ()
                      (when zo-exists? (delete-file zo-name))
                      (log-info (format "cm: ~acompiling ~a" 
                                        (build-string 
                                         (depth)
                                         (λ (x) (if (= 2 (modulo x 3)) #\| #\space)))
                                        actual-path))
                      (parameterize ([depth (+ (depth) 1)])
                        (with-handlers
                            ([exn:get-module-code?
                              (lambda (ex)
                                (compilation-failure mode path zo-name
                                                     (exn:get-module-code-path ex)
                                                     (exn-message ex))
                                (raise ex))])
                          (compile-zo* mode path src-sha1 read-src-syntax zo-name up-to-date))))))))))
     (unless sha1-only?
       (trace-printf "end compile: ~a" actual-path)))))

(define (get-compiled-time mode path)
  (define-values (dir name) (get-compilation-dir+name mode path))
  (or (try-file-time (build-path dir "native" (system-library-subpath)
                                 (path-add-suffix name (system-type
                                                        'so-suffix))))
      (try-file-time (build-path dir (path-add-suffix name #".zo")))
      -inf.0))

(define (try-file-sha1 path dep-path)
  (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
    (string-append
     (call-with-input-file* path sha1)
     (with-handlers ([exn:fail:filesystem? (lambda (exn) "")])
       (call-with-input-file* dep-path (lambda (p) (cdadr (read p))))))))

(define (get-compiled-sha1 mode path)
  (define-values (dir name) (get-compilation-dir+name mode path))
  (let ([dep-path (build-path dir (path-add-suffix name #".dep"))])
    (or (try-file-sha1 (build-path dir "native" (system-library-subpath)
                                   (path-add-suffix name (system-type
                                                          'so-suffix)))
                       dep-path)
        (try-file-sha1 (build-path dir (path-add-suffix name #".zo"))
                       dep-path)
        "")))

(define (rkt->ss p)
  (let ([b (path->bytes p)])
    (if (regexp-match? #rx#"[.]rkt$" b)
        (path-replace-suffix p #".ss")
        p)))

(define (compile-root mode path0 up-to-date read-src-syntax sha1-only?)
  (define orig-path (simple-form-path path0))
  (define (read-deps path)
    (with-handlers ([exn:fail:filesystem? (lambda (ex) (list (version) '#f))])
      (call-with-input-file
          (path-add-suffix (get-compilation-path mode path) #".dep")
        read)))
  (define (do-check)
    (let* ([main-path orig-path]
           [alt-path (rkt->ss orig-path)]
           [main-path-time (try-file-time main-path)]
           [alt-path-time (and (not main-path-time)
                               (not (eq? alt-path main-path))
                               (try-file-time alt-path))]
           [path (if alt-path-time alt-path main-path)]
           [path-time (or main-path-time alt-path-time)]
           [path-zo-time (get-compiled-time mode path)])
      (cond
       [(not path-time)
        (trace-printf "~a does not exist" orig-path)
        (or (and up-to-date (hash-ref up-to-date orig-path #f))
            (let ([stamp (cons path-zo-time
                               (delay (get-compiled-sha1 mode path)))])
              (hash-set! up-to-date main-path stamp)
              (unless (eq? main-path alt-path)
                (hash-set! up-to-date alt-path stamp))
              stamp))]
       [else
        (let ([deps (read-deps path)])
          (define build
            (cond
             [(not (and (pair? deps) (equal? (version) (car deps))))
              (lambda ()
                (trace-printf "newer version...")
                (maybe-compile-zo #f #f mode path orig-path read-src-syntax up-to-date))]
             [(> path-time path-zo-time)
              (trace-printf "newer src...")
              ;; If `sha1-only?', then `maybe-compile-zo' returns a #f or thunk:
              (maybe-compile-zo sha1-only? deps mode path orig-path read-src-syntax up-to-date)]
             [(ormap
               (lambda (p)
                 ;; (cons 'ext rel-path) => a non-module file (check date)
                 ;; rel-path => a module file name (check transitive dates)
                 (define ext? (and (pair? p) (eq? 'ext (car p))))
                 (define d (main-collects-relative->path (if ext? (cdr p) p)))
                 (define t
                   (if ext?
                       (cons (try-file-time d) #f)
                       (compile-root mode d up-to-date read-src-syntax #f)))
                 (and (car t)
                      (> (car t) path-zo-time)
                      (begin (trace-printf "newer: ~a (~a > ~a)..."
                                           d (car t) path-zo-time)
                             #t)))
               (cddr deps))
              ;; If `sha1-only?', then `maybe-compile-zo' returns a #f or thunk:
              (maybe-compile-zo sha1-only? deps mode path orig-path read-src-syntax up-to-date)]
             [else #f]))
          (cond
           [(and build sha1-only?) #f]
           [else
            (when build (build))
            (let ([stamp (cons (get-compiled-time mode path)
                               (delay (get-compiled-sha1 mode path)))])
              (hash-set! up-to-date main-path stamp)
              (unless (eq? main-path alt-path)
                (hash-set! up-to-date alt-path stamp))
              stamp)]))])))
  (or (and up-to-date (hash-ref up-to-date orig-path #f))
      (let ([v ((manager-skip-file-handler) orig-path)])
        (and v
             (hash-set! up-to-date orig-path v)
             v))
      (begin (trace-printf "checking: ~a" orig-path)
             (do-check))))

(define (managed-compile-zo zo [read-src-syntax read-syntax])
  ((make-caching-managed-compile-zo read-src-syntax) zo))

(define (make-caching-managed-compile-zo [read-src-syntax read-syntax])
  (let ([cache (make-hash)])
    (lambda (src)
      (parameterize ([current-load/use-compiled
                      (make-compilation-manager-load/use-compiled-handler/table
                       cache)])
        (compile-root (car (use-compiled-file-paths))
                      (path->complete-path src)
                      cache
                      read-src-syntax
                      #f)
        (void)))))

(define (make-compilation-manager-load/use-compiled-handler)
  (make-compilation-manager-load/use-compiled-handler/table (make-hash)))

(define (make-compilation-manager-load/use-compiled-handler/table cache)
  (let ([orig-eval (current-eval)]
        [orig-load (current-load)]
        [orig-registry (namespace-module-registry (current-namespace))]
        [default-handler (current-load/use-compiled)]
        [modes (use-compiled-file-paths)])
    (define (compilation-manager-load-handler path mod-name)
      (cond [(not mod-name)
             (trace-printf "skipping:  ~a mod-name ~s" path mod-name)]
            [(not (or (file-exists? path)
                      (let ([p2 (rkt->ss path)])
                        (and (not (eq? path p2))
                             (file-exists? p2)))))
             (trace-printf "skipping:  ~a file does not exist" path)]
            [(or (null? (use-compiled-file-paths))
                 (not (equal? (car modes)
                              (car (use-compiled-file-paths)))))
             (trace-printf "skipping:  ~a compiled-paths's first element changed; current value ~s, first element was ~s"
                           path 
                           (use-compiled-file-paths)
                           (car modes))]
            [(not (eq? compilation-manager-load-handler
                       (current-load/use-compiled)))
             (trace-printf "skipping:  ~a current-load/use-compiled changed ~s"
                           path (current-load/use-compiled))]
            [(not (eq? orig-eval (current-eval)))
             (trace-printf "skipping:  ~a orig-eval ~s current-eval ~s"
                           path orig-eval (current-eval))]
            [(not (eq? orig-load (current-load)))
             (trace-printf "skipping:  ~a orig-load ~s current-load ~s"
                           path orig-load (current-load))]
            [(not (eq? orig-registry
                       (namespace-module-registry (current-namespace))))
             (trace-printf "skipping:  ~a orig-registry ~s current-registry ~s"
                           path orig-registry
                           (namespace-module-registry (current-namespace)))]
            [else
             (trace-printf "processing: ~a" path)
             (compile-root (car modes) path cache read-syntax #f)
             (trace-printf "done: ~a" path)])
      (default-handler path mod-name))
    (when (null? modes)
      (raise-mismatch-error 'make-compilation-manager-...
                            "empty use-compiled-file-paths list: "
                            modes))
    compilation-manager-load-handler))


;; Exported:
(define (get-compiled-file-sha1 path)
  (try-file-sha1 path (path-replace-suffix path #".dep")))

(define (get-file-sha1 path)
  (get-source-sha1 path))

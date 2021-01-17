#lang racket/base
(require syntax/private/modcode-noctc
         syntax/modread
         setup/private/dirs
         racket/file
         racket/list
         racket/path
         racket/promise
         file/sha1
         setup/cross-system
         compiler/compilation-path
         compiler/private/dep
         "cm-util.rkt"
         "cm-dep.rkt"
         "cm-path.rkt"
         "cm-security.rkt"
         "cm-log.rkt"
         "cm-file.rkt"
         "cm-hash.rkt")

;; The compilation manager (CM) is responsible for rebuilding Racket
;; ".zo" files when module sources or dependencies change. It
;; generates and consults ".dep" files to manage dependencies. Two
;; design choices/constraints complicate its job:
;;
;;  - Dependencies are recorded as unordered. Consequenlt, CM can't
;;    recur in a simple way on dependencies, because a change in one
;;    dependency may cause another former dependency to be removed.
;;
;;    As a result, CM has two paths to explore a dependency tree: an
;;    "optimistic" mode that doesn't build anytihing but just checks
;;    whether everything has stayed the same, and a general mode that
;;    rebuilds.
;;
;;  - Concurrency is managed through a lock server, but the lock
;;    server's API equates taking a lock with rebuilding.
;;
;;    As a result, the "optimistic" pass can't take a lock while
;;    checking a module's build state. To make that work, ".zo" and
;;    ".dep" files have to be detected, read, and written in just the
;;    right way to avoid races. This approach to concurrency is
;;    fragile and difficult to reason about.
;;
;; Other complications:
;;
;;  - The collection sinstallation, `use-compile-file-paths`, and
;;    `current-compiled-file-roots` create search paths that multiply.
;;    Generally, CM checks at all roots but writes only at the first
;;    root.
;;
;;  - CM may need to promote machine-independent files to
;;    machine-dependent files, or it may need to write both
;;    machine-independent and -dependent files (see
;;    `cross-multi-compile?`).

(provide make-compilation-manager-load/use-compiled-handler
         managed-compile-zo
         make-caching-managed-compile-zo
         trust-existing-zos
         managed-recompile-only
         manager-compile-notify-handler
         manager-skip-file-handler
         manager-trace-handler
         get-file-sha1
         get-compiled-file-sha1
         with-compile-output

         managed-compiled-context-key
         make-compilation-context-error-display-handler

         parallel-lock-client

         install-module-hashes!

         current-path->mode

         cross-multi-compile?)

(module+ cm-internal
  (provide try-file-time
           rkt->ss
           get-source-sha1))

;; Parameters for external configuration:
(define current-path->mode (make-parameter #f))
(define trust-existing-zos (make-parameter #f))
(define managed-recompile-only (make-parameter #f))
(define manager-skip-file-handler (make-parameter (λ (x) #f)))
(define parallel-lock-client (make-parameter #f))

(define (make-compilation-manager-load/use-compiled-handler [delete-zos-when-rkt-file-does-not-exist? #f]
                                                            #:security-guard 
                                                            [security-guard #f])
  (make-compilation-manager-load/use-compiled-handler/table (make-hash) (make-hash)
                                                            delete-zos-when-rkt-file-does-not-exist?
                                                            #:security-guard security-guard))

(define (managed-compile-zo zo [read-src-syntax read-syntax] #:security-guard [security-guard #f])
  ((make-caching-managed-compile-zo read-src-syntax #:security-guard security-guard) zo))

(define (make-caching-managed-compile-zo [read-src-syntax read-syntax] #:security-guard [security-guard #f])
  (let ([cache (make-hash)]
        [collection-cache (make-hash)])
    (lambda (src)
      (parameterize ([current-load/use-compiled
                      (make-compilation-manager-load/use-compiled-handler/table
                       cache
                       collection-cache
                       #f
                       #:security-guard security-guard)]
                     [error-display-handler
                      (make-compilation-context-error-display-handler
                       (error-display-handler))])
        (compile-root (or (current-path->mode)
                          (let ([mode (car (use-compiled-file-paths))])
                            (λ (pth) mode)))
                      (current-compiled-file-roots)
                      (path->complete-path src)
                      cache
                      collection-cache
                      read-src-syntax
                      #hash())
        (void)))))

;; Make the entry point as a module name resolver. In the non-"optimistic" case,
;; going thorough the handler is the way CM ends up calling itself for dependencies.
;; But only treat this re-entry like a recursive calls if the compilation environment
;; hasn't changed (since a change suggests that a macro is trying to do some other
;; sort of evaluation or compilation).
(define (make-compilation-manager-load/use-compiled-handler/table cache collection-cache
                                                                  delete-zos-when-rkt-file-does-not-exist?
                                                                  #:security-guard [security-guard #f])
  (define cp->m (current-path->mode))
  (define modes (use-compiled-file-paths))
  (when (and (not cp->m) (null? modes))
    (raise-arguments-error 'make-compilation-manager-...
                           "use-compiled-file-paths is '() and current-path->mode is #f"))
  (define path->mode (or cp->m (λ (p) (car modes))))
  (let ([orig-eval (current-eval)]
        [orig-load (current-load)]
        [orig-registry (namespace-module-registry (current-namespace))]
        [default-handler (current-load/use-compiled)]
        [roots (current-compiled-file-roots)])
    (define (compilation-manager-load-handler path mod-name)
      (cond [(or (not mod-name)
                 ;; Don't trigger compilation if we're not supposed to work with source:
                 (and (pair? mod-name)
                      (not (car mod-name))))
             (trace-printf "skipping:  ~a mod-name ~s" path mod-name)]
            [(not (or (file-exists? path)
                      (let ([p2 (rkt->ss path)])
                        (and (not (eq? path p2))
                             (file-exists? p2)))))
             (trace-printf "skipping:  ~a file does not exist" path)
             (when delete-zos-when-rkt-file-does-not-exist?
               (define to-delete (path-add-extension (get-compilation-path path->mode roots path) #".zo"))
               (when (file-exists? to-delete)
                 (trace-printf "deleting:  ~s" to-delete)
                 (with-compiler-security-guard (delete-file* to-delete))))]
            [(if cp->m
                 (not (equal? (current-path->mode) cp->m))
                 (let ([current-cfp (use-compiled-file-paths)])
                   (or (null? current-cfp)
                       (not (equal? (car current-cfp) (car modes))))))
             (if cp->m
                 (trace-printf "skipping:  ~a current-path->mode changed; current value ~s, original value was ~s"
                               path (current-path->mode) cp->m)
                 (trace-printf "skipping:  ~a use-compiled-file-paths's first element changed; current value ~s, first element was ~s"
                               path
                               (use-compiled-file-paths)
                               (car modes)))]
            [(not (equal? roots (current-compiled-file-roots)))
             (trace-printf "skipping:  ~a current-compiled-file-roots changed; current value ~s, original was ~s"
                           path 
                           (current-compiled-file-roots)
                           roots)]
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
             (parameterize ([compiler-security-guard security-guard])
               (compile-root path->mode roots path cache collection-cache read-syntax #hash()))
             (trace-printf "done: ~a" path)])
      (default-handler path mod-name))
    (when (null? roots)
      (raise-arguments-error 'make-compilation-manager-...
                             "empty current-compiled-file-roots list"))
    compilation-manager-load-handler))

;; The `compile-root` function implements the "optimistic" path. It's
;; a wrapper on `maybe-compile-zo` that tries to take shortcuts based
;; on file timestamps and the supplied `update-to-date` cache. If the
;; answer is not in timestamps or the cache, it has to defer to
;; `maybe-compile-zo` to decide whether a file has to be built.
(define (compile-root path->mode roots path0 up-to-date collection-cache read-src-syntax seen
                      #:sha1-only? [sha1-only? #f])
  (define orig-path (simple-form-path path0))
  (define (read-deps path)
    (read-deps-file
     (path-add-extension (get-compilation-path path->mode roots path) #".dep")))
  (define (do-check)
    (let* ([main-path orig-path]
           [alt-path (rkt->ss orig-path)]
           [main-path-time (try-file-time main-path)]
           [alt-path-time (and (not main-path-time)
                               (not (eq? alt-path main-path))
                               (try-file-time alt-path))]
           [path (if alt-path-time alt-path main-path)]
           [path-time (or main-path-time alt-path-time)]
           [path-zo-time (get-compiled-time path->mode roots path)])
      (cond
       [(hash-ref seen path #f)
        (error 'compile-zo 
               "dependency cycle\n  involves module: ~a"
               path)
        #f]
       [(and (not path-time)
             ;; Even though the source doesn't exist, maybe
             ;; platform-independent bytecode needs to be recompiled,
             ;; so check that:
             (or (not (current-compile-target-machine))
                 (deps-machine (read-deps path))))
        (trace-printf "~a does not exist" orig-path)
        (or (hash-ref up-to-date orig-path #f)
            (let ([stamp (cons (or path-zo-time +inf.0)
                               (delay (get-compiled-sha1 path->mode roots path)))])
              (hash-set! up-to-date main-path stamp)
              (unless (eq? main-path alt-path)
                (hash-set! up-to-date alt-path stamp))
              stamp))]
       [else
        (let ([deps (read-deps path)]
              [new-seen (hash-set seen path #t)])
          (define needs-build?
            (cond
              [(not (and (deps-has-version? deps)
                         (equal? (version) (deps-version deps))))
               (trace-printf "old version ~a for ~a..."
                             (and (deps-has-version? deps)
                                  (deps-version deps))
                             path)
               #t]
              [(not (and (deps-has-machine? deps)
                         (or (eq? (current-compile-target-machine) (deps-machine deps))
                             (and sha1-only? (not (deps-machine deps)))
                             (and (eq? (system-type 'target-machine) (deps-machine deps))
                                  (cross-multi-compile? roots)))
                         (or sha1-only?
                             (deps-machine deps)
                             (not (cross-multi-compile? roots)))))
               (trace-printf "different machine ~a for ~a..."
                             (and (deps-has-machine? deps)
                                  (deps-machine deps))
                             path)
               #t]
              [(> path-time (or path-zo-time -inf.0))
               (trace-printf "newer src... ~a > ~a" path-time path-zo-time)
               (maybe-compile-zo deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen
                                 #:trying-sha1? sha1-only?)]
              [(different-source-sha1-and-dep-recorded path deps)
               => (lambda (difference)
                    (trace-printf "different src hash ~a for ~a..." difference path)
                    (maybe-compile-zo deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen
                                      #:trying-sha1? sha1-only?))]
              [((if sha1-only? ormap ormap-strict)
                (lambda (p)
                  (define ext? (external-dep? p))
                  (define d (collects-relative*->path (dep->encoded-path p) collection-cache))
                  (define t
                    (if ext?
                        (cons (or (try-file-time d) +inf.0) #f)
                        (compile-root path->mode roots d up-to-date collection-cache read-src-syntax new-seen
                                      #:sha1-only? sha1-only?)))
                  (cond
                    [(not t) #t]
                    [else (and (car t)
                               (> (car t) (or path-zo-time -inf.0))
                               (begin (trace-printf "newer for ~a: ~a (~a > ~a)..."
                                                    path d (car t) path-zo-time)
                                      #t))]))
                (deps-imports deps))
               (maybe-compile-zo deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen
                                 #:trying-sha1? sha1-only?)]
              [else #f]))
          (cond
           [(and needs-build? sha1-only?)
            (hash-set! up-to-date (cons 'needs-build? main-path) #t)
            (unless (eq? main-path alt-path)
              (hash-set! up-to-date (cons 'needs-build? alt-path) #t))
            #f]
           [else
            (when needs-build?
              (maybe-compile-zo deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen))
            (let ([stamp (cons (or (get-compiled-time path->mode roots path) +inf.0)
                               (delay (get-compiled-sha1 path->mode roots path)))])
              (define (make-key p)
                (if (or needs-build?
                        ;; If `(deps-machine deps)` is #f and doesn't match the current machine,
                        ;; then we still need to build.
                        (and (or (eq? (current-compile-target-machine) (deps-machine deps))
                                 (and (eq? (system-type 'target-machine) (deps-machine deps))
                                      (cross-multi-compile? roots)))
                             (or (deps-machine deps)
                                 (not (cross-multi-compile? roots)))))
                    p
                    ;; We didn't actually recompile, yet, so don't record the path
                    ;; as done. But record an "assume" sha1-stamp, so we don't keep
                    ;; computing it.
                    (cons 'assume p)))
              (hash-set! up-to-date (make-key main-path) stamp)
              (unless (eq? main-path alt-path)
                (hash-set! up-to-date (make-key alt-path) stamp))
              stamp)]))])))
  (or (hash-ref up-to-date orig-path #f)
      (and sha1-only?
           (hash-ref up-to-date (cons 'assume orig-path) #f))
      (let ([v ((manager-skip-file-handler) orig-path)])
        (and v
             (hash-set! up-to-date orig-path v)
             v))
      (cond
        [(and sha1-only?
              (hash-ref up-to-date (cons 'needs-build? orig-path) #f))
         #f]
        [else
         (trace-printf "checking: ~a" orig-path)
         (do-check)])))

;; The `maybe-compile-zo` check is the ultimate word on whether a file
;; needs to be recompiled. It is called through the `compile-root`
;; layer, which tries to take shortcuts based on file timestamps and a
;; cached decisions.
;;
;; There's a catch here: If `trying-sha1?` is #t, then the question is
;; "must a recorded SHA-1 be disbelieved?", and it must be answered
;; without committing to compiling the file right now. Crucially,
;; calling the lock manager would mean committing to compiling, so
;; the lock manager can't be used in that case. Also, the existence
;; of the ".zo" file cannot be part of the answer if the ".dep" file
;; provides a SHA-1 to assume, since that's related to recompilation,
;; except in the special case when `(trust-existing-zos)` is #t.
;;
;; If `trying-sha1?` is #f, then actually build if the compiled form
;; is out of date, and return #f to report that no further build is
;; needed. Since there may be concurrent building processes, even if
;; this process isn't the one to build a file, don't return until any
;; concurrent builder is defintely done building; in other words,
;; never return a #f unless the lock manager is consulted (or unless
;; `trying-sha1?` is #t).
;;
;; Beware that if a ".dep" file provides a SHA-1 for the generated
;; bytecode (because the bytecode was once recompiled from
;; machine-independent bytecode) but the bytecode file isn't present,
;; then dependent files will assume that compiling will produce the
;; same SHA-1. That limitation is necessary to avoid recompilation
;; when one concurrent processes is recompiling and other processes
;; are checking whether they can use or merely recompile existing
;; dependent files, where that checking is not allowed to test for the
;; bytecode file's existence.
;;
(define (maybe-compile-zo deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache seen
                          #:trying-sha1? [trying-sha1? #f])
  (let ([actual-path (actual-source-path orig-path)])
    (unless trying-sha1?
      ((manager-compile-notify-handler) actual-path)
      (trace-printf "maybe-compile-zo starting ~a" actual-path))
    (begin0
     (parameterize ([indent (+ 2 (indent))])
       (let* ([zo-name (path-add-extension (get-compilation-path path->mode roots path) #".zo")])
         (cond
           [(and (trust-existing-zos)
                 (file-exists? zo-name))
            (trace-printf "trusting: ~a" zo-name)
            (touch zo-name)
            #f]
           [else
            (define lock-zo-name (if (cross-multi-compile? roots)
                                     ;; Make sure we use a file path for the lock that is consistent
                                     ;; with being in a phase of compiling for the current machine:
                                     (path-add-extension (get-compilation-path path->mode roots path #:for-lock? #t)
                                                         #".zo")
                                     zo-name))
            ;; Called when `tryng-sha1?` is #f and this process (or some process)
            ;; needs to compile, recompile, or touch:
            (define (build #:just-touch? [just-touch? #f]
                           #:recompile-from [recompile-from #f]
                           #:recompile-from-machine [recompile-from-machine #f]
                           #:assume-compiled-sha1 [assume-compiled-sha1 #f]
                           #:use-existing-deps [use-existing-deps #f])
              (define lc (parallel-lock-client))
              (when lc (log-compile-event path 'locking))
              (define locked? (and lc (lc 'lock lock-zo-name)))
              (define ok-to-compile? (or (not lc) locked?))
              (dynamic-wind
               (lambda () (void))
               (lambda ()
                 (when ok-to-compile?
                   (cond
                     [(and just-touch? (file-exists? zo-name))
                      (log-compile-event path 'start-touch)
                      (touch zo-name)]
                     [else
                      (when just-touch? (set! just-touch? #f))
                      (define mi-recompile-from (select-machine-independent recompile-from
                                                                            recompile-from-machine
                                                                            path
                                                                            roots
                                                                            path->mode))
                      (define recompile-from-exists? (and mi-recompile-from
                                                          ;; Checking existence now after taking lock:
                                                          (file-exists? mi-recompile-from)))
                      (trace-printf "~acompiling ~a" (if recompile-from-exists? "re" "") actual-path)
                      (log-compile-event path (if recompile-from-exists? 'start-recompile 'start-compile))
                      (with-handlers ([exn:get-module-code?
                                       (lambda (ex)
                                         (compilation-failure zo-name recompile-from)
                                         (raise ex))])
                        (compile-zo*/cross-compile path->mode roots path src-sha1 read-src-syntax zo-name up-to-date collection-cache
                                                   #:recompile-from (and recompile-from-exists?
                                                                         mi-recompile-from)
                                                   #:assume-compiled-sha1 (and recompile-from-exists?
                                                                               (force assume-compiled-sha1))
                                                   #:use-existing-deps (and recompile-from-exists?
                                                                            use-existing-deps)
                                                   #:compile-dependency
                                                   (lambda (path)
                                                     (compile-root path->mode roots path up-to-date collection-cache read-src-syntax seen))))
                      (trace-printf "~acompiled ~a" (if recompile-from-exists? "re" "") actual-path)])))
               (lambda ()
                 (log-compile-event path (if (or (not lc) locked?)
                                             (cond
                                               [just-touch? 'finish-touch]
                                               [recompile-from 'finish-recompile]
                                               [else 'finish-compile])
                                             'already-done))
                 (when locked?
                   (lc 'unlock lock-zo-name))))
              #f)
            ;; Called to recompile bytecode that is currently in
            ;; machine-independent form:
            (define (build/recompile zo-name-machine)
              (build #:recompile-from zo-name
                     #:recompile-from-machine zo-name-machine
                     #:assume-compiled-sha1 (or (deps-assume-compiled-sha1 deps)
                                                ;; delay until lock is held:
                                                (delay (call-with-input-file* zo-name sha1)))
                     #:use-existing-deps deps))
            ;; Called to "build" the file by just updating its timestamp
            ;; -- unless it doesn't exist, in which case really build:
            (define (build/touch)
              (build #:just-touch? #t))
            ;; Called when there's no need for this process to build, but make sure the
            ;; bytecode is there, in case a concurrent process is building it:
            (define (build/sync)
              (define lc (parallel-lock-client))
              (when lc
                (when (lc 'lock lock-zo-name)
                  (lc 'unlock lock-zo-name)))
              #f)
            ;; ----------------------------------------
            ;; Determine whether and how to rebuild the file:
            (define src-sha1 (and deps
                                  (equal? (version) (deps-version deps))
                                  (deps-src-sha1 deps)
                                  (get-source-sha1 path)))
            (define-syntax-rule (explain v e) (or v (and e #f)))
            (cond
              [(and (not src-sha1)
                    (not (file-exists? actual-path)))
               ;; If we have bytecode but not source, then we can't compile,
               ;; but maybe we need to recompile
               (cond
                 [(or (not (eq? (deps-machine deps) (current-compile-target-machine)))
                      (and (not (deps-machine deps))
                           (cross-multi-compile? roots)))
                  ;; We'd like to recompile, but that should end up with the same reported hash,
                  ;; so we don't need to rebuild if just looking for the hash.
                  (cond
                    [trying-sha1? #f]
                    [else (build/recompile (deps-machine deps))])]
                 [else
                  ;; No need to build
                  (cond
                    [trying-sha1? #f]
                    [else (build/sync)])])]
              [(and (explain src-sha1
                             (trace-printf "no source hash: ~a" path))
                    (explain (equal? (version) (deps-version deps))
                             (trace-printf "different version: ~a" path))
                    (explain (equal? src-sha1 (and (pair? (deps-sha1s deps))
                                                   (deps-src-sha1 deps)))
                             (trace-printf "source hash changed: ~a" path))
                    (explain (or (eq? (deps-machine deps) (current-compile-target-machine))
                                 (not (deps-machine deps))
                                 (and (cross-multi-compile? roots)
                                      (eq? (system-type 'target-machine) (deps-machine deps))))
                             (trace-printf "wrong machine: ~a" path))
                    (let ([imports-sha1
                           (get-dep-sha1s path
                                          (deps-imports deps) up-to-date collection-cache read-src-syntax path->mode roots seen
                                          #:must-exist? #f)])
                      (explain (equal? imports-sha1 (deps-imports-sha1 deps))
                               (trace-printf "different dependency deps for ~a: ~a ~a"
                                             zo-name
                                             imports-sha1
                                             (deps-imports-sha1 deps)))))
               ;; We need to recompile the file from machine-independent bytecode,
               ;; or maybe just update the file's modification date
               (trace-printf "hash-equivalent: ~a" zo-name)
               (cond
                 [(and (eq? (deps-machine deps) (current-compile-target-machine))
                       (or (deps-machine deps)
                           (not (cross-multi-compile? roots))))
                  (cond
                    [trying-sha1? #f]
                    [else (build/touch)])]
                 [else
                  ;; (deps-machine deps) is #f, so we can recompile machine-independent
                  ;; bytecode to this machine's format
                  (cond
                    [trying-sha1?
                     ;; We're not supposed to build now, so claim that it's already built.
                     ;; If we claimed that it needed to be built, then a dependent module
                     ;; would start compiling from scratch. But either recompiling or compiling
                     ;; that module will cause this one to be recompiled (i.e., back here
                     ;; with `trying-sha1?` as #f)
                     #f]
                    [else (build/recompile (deps-machine deps))])])]
              [trying-sha1?
               ;; Needs to be built, but we can't build now
               #t]
              [else
               ;; Build
               (build)])])))
     (unless trying-sha1?
       (trace-printf "maybe-compile-zo finished ~a" actual-path)))))

;; Handle cross-multi-compile mode, or just continue on to `compile-zo*`
(define (compile-zo*/cross-compile path->mode roots path src-sha1 read-src-syntax orig-zo-name
                                   up-to-date collection-cache
                                   #:recompile-from recompile-from
                                   #:assume-compiled-sha1 assume-compiled-sha1
                                   #:use-existing-deps use-existing-deps
                                   #:compile-dependency compile-dependency)
  (when (and (not recompile-from)
             (managed-recompile-only))
    (error 'compile-zo 
           "compile from source disallowed\n  module: ~a"
           path))
  (cond
    [(cross-multi-compile? roots)
     (define running-root (car roots))
     (define target-root (cadr roots))
     ;; First, generate machine-independent form at the second root:
     (define mi-zo-name
       (compile-zo* path->mode (list target-root) path src-sha1 read-src-syntax #f up-to-date collection-cache
                    #:recompile-from recompile-from
                    #:assume-compiled-sha1 assume-compiled-sha1
                    #:use-existing-deps use-existing-deps
                    #:compile-dependency compile-dependency))
     (define mi-dep-path (path-replace-extension mi-zo-name #".dep"))
     (define mi-deps (call-with-input-file* mi-dep-path read))
     (define mi-sha1 (or (deps-assume-compiled-sha1 mi-deps)
                         (call-with-input-file* mi-zo-name sha1)))
     ;; Recompile to running-Racket form:
     (define running-zo
       (parameterize ([current-compile-target-machine (system-type 'target-machine)])
         (compile-zo* path->mode (list running-root) path src-sha1 read-src-syntax #f up-to-date collection-cache
                      #:recompile-from mi-zo-name
                      #:assume-compiled-sha1 mi-sha1
                      #:use-existing-deps mi-deps
                      #:compile-dependency compile-dependency)))
     (when (cross-system-type 'target-machine)
       ;; Recompile to cross-compile target form:
       (parameterize ([current-compile-target-machine (cross-system-type 'target-machine)])
         (compile-zo* path->mode (list target-root) path src-sha1 read-src-syntax #f up-to-date collection-cache
                      #:recompile-from mi-zo-name
                      #:assume-compiled-sha1 mi-sha1
                      #:use-existing-deps mi-deps
                      #:compile-dependency compile-dependency)))
     running-zo]
    [else
     ;; Regular mode, just [re]compile:
     (compile-zo* path->mode roots path src-sha1 read-src-syntax orig-zo-name up-to-date collection-cache
                  #:recompile-from recompile-from
                  #:assume-compiled-sha1 assume-compiled-sha1
                  #:use-existing-deps use-existing-deps
                  #:compile-dependency compile-dependency)]))

;; For communication within `compile-zo*`:
(define-struct ext-reader-guard (proc top)
  #:property prop:procedure (struct-field-index proc))
(define-struct file-dependency (path module?) #:prefab)
(define-struct (file-dependency/options file-dependency) (table) #:prefab)

;; Compiles one file in one compilation mode
(define (compile-zo* path->mode roots path src-sha1 read-src-syntax orig-zo-name up-to-date collection-cache
                     #:recompile-from recompile-from
                     #:assume-compiled-sha1 assume-compiled-sha1
                     #:use-existing-deps use-existing-deps
                     #:compile-dependency compile-dependency)
  ;; The `path' argument has been converted to .rkt or .ss form,
  ;;  as appropriate.
  ;; External dependencies registered through reader guard and
  ;; accomplice-logged events:
  (define external-deps null)
  (define external-module-deps null)
  (define reader-deps null)
  (define deps-sema (make-semaphore 1))
  (define done-key (gensym))
  (define (external-dep! p module? indirect?)
    (define bstr (path->bytes p))
    (define dep (if indirect?
                    (cons 'indirect bstr)
                    bstr))
    (if module?
        (set! external-module-deps (cons dep external-module-deps))
        (set! external-deps (cons dep external-deps))))
  (define (reader-dep! p)
    (call-with-semaphore
     deps-sema
     (lambda ()
       (set! reader-deps (cons (path->bytes p) reader-deps)))))

  ;; Set up a logger to receive and filter accomplice events:
  (define accomplice-logger (make-logger #f (current-logger)
                                         ;; Don't propoagate 'cm-accomplice events, so that
                                         ;; enclosing compilations don't see events intended
                                         ;; for this one:
                                         'none 'cm-accomplice
                                         ;; Propagate everything else:
                                         'debug))
  (define receiver (make-log-receiver accomplice-logger 'info 'cm-accomplice))

  (define dest-roots (list (car roots)))
  (define-values (code-dir code-name)
    (get-compilation-dir+name path #:modes (list (path->mode path)) #:roots dest-roots))
  (define zo-name
    ;; If we have multiple roots, make sure that compilation uses the first one
    (if (or (pair? (cdr roots)) (not orig-zo-name))
        (build-path code-dir (path-add-suffix code-name #".zo"))
        orig-zo-name))

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
                             (let* ([p (resolved-module-path-name
                                        (module-path-index-resolve
                                         (module-path-index-join d #f)))]
                                    [p (if (pair? p)
                                           ;; Create a dependency only if 
                                           ;; the corresponding submodule is
                                           ;; declared:
                                           (if (module-declared? d #t)
                                               (car p)
                                               #f)
                                           p)])
                               (when (path? p) (reader-dep! p))))
                           d))
                       rg))]
                   [current-logger accomplice-logger])
      (with-continuation-mark
        managed-compiled-context-key
        path
        (cond
          [(and (equal? recompile-from zo-name)
                (not (current-compile-target-machine)))
           ;; We don't actually need to do anything, so
           ;; avoid updating the file.
           (check-recompile-module-dependencies use-existing-deps
                                                collection-cache
                                                compile-dependency)
           #f]
          [recompile-from
           (recompile-module-code recompile-from
                                  path
                                  use-existing-deps
                                  collection-cache
                                  compile-dependency)]
          [else
           (get-module-code path (path->mode path) compile
                            #:choose (lambda (src zo so) 'src)
                            #:extension-handler (lambda (a b) #f)
                            #:roots (list (car roots))
                            #:source-reader read-src-syntax)]))))

  ;; Get all accomplice data:
  (when code
    (let loop ()
      (let ([l (sync/timeout 0 receiver)])
        (when l
          (when (and (eq? (vector-ref l 0) 'info)
                     (file-dependency? (vector-ref l 2))
                     (path? (file-dependency-path (vector-ref l 2))))
            (external-dep! (file-dependency-path (vector-ref l 2))
                           (file-dependency-module? (vector-ref l 2))
                           (and (file-dependency/options? (vector-ref l 2))
                                (hash-ref (file-dependency/options-table (vector-ref l 2))
                                          'indirect
                                          #f))))
          (loop)))))

  ;; Write the code and dependencies:
  (when code
    (with-compiler-security-guard (make-directory* code-dir))
    (with-compile-output zo-name
      (lambda (out tmp-name)
        (with-handlers ([exn:fail?
                         (lambda (ex)
                           (close-output-port out)
                           (compilation-failure zo-name recompile-from)
                           (raise ex))])
          (parameterize ([current-write-relative-directory
                          (let* ([dir
                                  (let-values ([(base name dir?) (split-path path)])
                                    (if (eq? base 'relative)
                                        (current-directory)
                                        (path->complete-path base (current-directory))))]
                                 [collects-dir (find-collects-dir)]
                                 [e-dir (explode-path dir)]
                                 [e-collects-dir (explode-path collects-dir)])
                            (if (and ((length e-dir) . > . (length e-collects-dir))
                                     (for/and ([a (in-list e-dir)]
                                               [b (in-list e-collects-dir)])
                                       (equal? a b)))
                                ;; `dir' extends `collects-dir':
                                (cons dir collects-dir)
                                ;; `dir' doesn't extend `collects-dir':
                                dir))])
            (let ([b (open-output-bytes)])
              ;; Write bytecode into string
              (write code b)
              ;; Compute SHA1 over modules within bytecode
              (let* ([s (get-output-bytes b)])
                (install-module-hashes! s)
                ;; Write out the bytecode with module hash
                (write-bytes s out)))))
        ;; redundant, but close as early as possible:
        (close-output-port out)
        ;; Note that we check time and write ".dep" before returning from
        ;; with-compile-output...
        (verify-times path tmp-name)
        (when (equal? recompile-from zo-name)
          (trace-printf "recompiling in-place: ~a" zo-name)
          ;; In the case of recompiling, make sure that any concurrent
          ;; process always sees recompile possibilities by writing
          ;; the expected sha1 into ".dep" before deleting the ".zo"
          (write-updated-deps use-existing-deps assume-compiled-sha1 zo-name
                              #:target-machine #f))
        ;; Explicitly delete target file before writing ".dep", just so
        ;; ".dep" is doesn't claim a description of the wrong file
        (when (file-exists? zo-name)
          (try-delete-file zo-name #f))
        (cond
          [use-existing-deps
           (write-updated-deps use-existing-deps assume-compiled-sha1 zo-name)]
          [else
           (write-deps code zo-name path->mode dest-roots path src-sha1
                       external-deps external-module-deps reader-deps 
                       up-to-date collection-cache read-src-syntax)])))
    (trace-printf "wrote zo file: ~a" zo-name))

  ;; Return generated ".zo" path:
  zo-name)

;; Recompile an individual file
(define (recompile-module-code recompile-from src-path deps collection-cache compile-dependency)
  (check-recompile-module-dependencies deps collection-cache compile-dependency)
  ;; Recompile the module:
  (define-values (base name dir?) (split-path src-path))
  (parameterize ([current-load-relative-directory
                  (if (path? base) base (current-directory))])
    (define code (parameterize ([read-accept-compiled #t])
                   (call-with-input-file* recompile-from read)))
    (compiled-expression-recompile code)))

;; Returns a single hash for all dependencies --- or `#f` if it can't be computed
;; because things are not up-to-date, but `#:must-exist? #t` insists that things
;; are as up-to-date as they can be.
(define (get-dep-sha1s for-path deps up-to-date collection-cache read-src-syntax path->mode roots seen
                       #:must-exist? must-exist?)
  (let ([l (for/fold ([l null]) ([dep (in-list deps)])
             (and l
                  (let* ([ext? (external-dep? dep)]
                         [p (collects-relative*->path (dep->encoded-path dep) collection-cache)])
                    (cond
                     [ext? (let ([v (get-source-sha1 p)])
                             (cond
                              [v (cons (cons (delay v) dep) l)]
                              [must-exist? (error 'cm "cannot find external-dependency file: ~v" p)]
                              [else #f]))]
                     [(or (let ([p (simple-form-path p)])
                            (or (hash-ref up-to-date p #f)
                                (hash-ref up-to-date (cons 'assume p) #f)))
                          (compile-root #:sha1-only? #t
                                        path->mode roots p up-to-date collection-cache read-src-syntax seen))
                      => (lambda (sh)
                           (cons (cons (cdr sh) dep) l))]
                     [must-exist?
                      ;; apparently, we're forced to use the source of the module,
                      ;; so compute a sha1 from it instead of the bytecode
                      (cons (cons (get-source-sha1 p) dep) l)]
                     [else
                      (trace-printf "no hash available toward ~a: ~a" for-path p)
                      #f]))))])
    (and l
         (let ([p (open-output-string)]
               [l (map (lambda (v) 
                         (let ([sha1 (force (car v))]
                               [dep (cdr v)])
                           (unless sha1
                             (error 'cm "no SHA-1 for dependency: ~s" dep))
                           (cons sha1 dep)))
                       l)])
           ;; sort by sha1s so that order doesn't matter
           (write (sort l string<? #:key car) p)
           ;; compute one hash from all hashes
           (sha1 (get-output-bytes p))))))

;; ----------------------------------------

(define (cross-multi-compile? roots)
  ;; Combination of cross-installation mode, compiling to machine-independent form,
  ;; and multiple compiled-file roots triggers a special multi-target compilation mode.
  ;; Write code compiled for the running Racket to the first root, and write code for
  ;; the cross-compile target to the second root --- but count the cross-compile target
  ;; as machine-independent if it would be the same as the current target.
  (and ((length roots) . > . 1)
       (cross-installation?)
       (not (current-compile-target-machine))))

;; Find a path with a suitable search, depending on the compilation mode
;; and whether we use the path for a lock
(define (get-compilation-path path->mode roots path #:for-lock? [for-lock? #f])
  (let-values ([(dir name) (get-compilation-dir+name path
                                                     #:modes (list (path->mode path))
                                                     #:roots (if for-lock?
                                                                 ;; When taking a lock, we need to consistently
                                                                 ;; use the main destination (where we'll write)
                                                                 ;; and not find a ".zo" file that exists in an
                                                                 ;; earlier root:
                                                                 (list (car roots))
                                                                 roots)
                                                     ;; In cross-multi mode, we need to default to the
                                                     ;; ".zo" file that is written first, otherwise we
                                                     ;; may pick the first root where there's no ".dep"
                                                     ;; written yet when the second root on has a ".dep"
                                                     ;; and the ".zo" is not yet in place
                                                     #:default-root (if (and (not for-lock?)
                                                                             (cross-multi-compile? roots))
                                                                        (cadr roots)
                                                                        (car roots)))])
    (build-path dir name)))

;; Returns timestamp of the target file, but return the timestamp on an extension if present
(define (get-compiled-time path->mode roots path)
  (define-values (dir name) (get-compilation-dir+name path #:modes (list (path->mode path)) #:roots roots))
  (or (and (eq? 'racket (system-type 'vm))
           (try-file-time (build-path dir "native" (system-library-subpath)
                                      (path-add-extension name (system-type
                                                                'so-suffix)))))
      (try-file-time (build-path dir (path-add-extension name #".zo")))))

;; Fill out and write deps with atomic update of the ".dep" file
(define (write-deps code zo-name path->mode roots path src-sha1
                    external-deps external-module-deps reader-deps 
                    up-to-date collection-cache read-src-syntax)
  (let ([dep-path (path-replace-extension zo-name #".dep")]
        [deps (remove-duplicates (append (get-deps code path)
                                         external-module-deps ; can create cycles if misused!
                                         reader-deps))]
        [external-deps (remove-duplicates external-deps)])
    (define (path*->collects-relative/maybe-indirect dep)
      (if (and (pair? dep) (eq? 'indirect (car dep)))
          (cons 'indirect (path*->collects-relative (cdr dep)))
          (path*->collects-relative dep)))
    (with-compile-output dep-path
      (lambda (op tmp-path)
        (let ([deps (append
                     (map path*->collects-relative/maybe-indirect deps)
                     (map (lambda (x)
                            (define d (path*->collects-relative/maybe-indirect x))
                            (if (and (pair? d) (eq? 'indirect d))
                                (cons 'indirect (cons 'ext (cdr d)))
                                (cons 'ext d)))
                          external-deps))])
          (write (list* (version)
                        (current-compile-target-machine)
                        (cons (or src-sha1 (get-source-sha1 path))
                              (get-dep-sha1s path
                                             deps up-to-date collection-cache read-src-syntax path->mode roots #hash()
                                             #:must-exist? #t))
                        (sort deps s-exp<?))
                 op)
          (newline op))))))

;; Write revised deps with atomic update of the ".dep" file
(define (write-updated-deps deps assume-compiled-sha1 zo-name
                            #:target-machine [target-machine (current-compile-target-machine)])
  (let ([dep-path (path-replace-extension zo-name #".dep")])
    (with-compile-output dep-path
      (lambda (op tmp-path)
        (write (list* (version)
                      target-machine
                      (cons (deps-src-sha1 deps)
                            (cons (deps-imports-sha1 deps)
                                  assume-compiled-sha1))
                      (deps-imports deps))
               op)
        (newline op)))))

;; Force potential recompilation of dependencies. Otherwise, we
;; end up relying on cross-module optimization demands, which might
;; not happen and are unlikely to cover everything.
(define (check-recompile-module-dependencies deps collection-cache compile-dependency)
  (for ([d (in-list (deps-imports deps))]
        #:unless (external-dep? d))
    (define path (collects-relative*->path (dep->encoded-path d) collection-cache))
    (compile-dependency path)))

;; Gets a multi-sha1 string that represents the compiled code
;; as well as its dependencies:
(define (try-file-sha1 path dep-path roots)
  (with-module-reading-parameterization
   (lambda ()
     ;; First, try SHA-1 of file; we need to try this first to be
     ;; consistent with the order that `compile-zo*` writes and
     ;; deletes files:
     (define path-sha1
       (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
         (call-with-input-file* path sha1)))
     ;; Extract sha1s from ".dep", if possible, including a sha1
     ;; that we should assume for the compiled form:
     (define-values (imports-sha1 assume-compiled-sha1)
       (with-handlers ([exn:fail:filesystem? (lambda (exn)
                                               (values "" #f))])
         (call-with-input-file*
          dep-path
          (lambda (p)
            (define deps (read p))
            (define ok-machine? (and (equal? (version) (deps-version deps))
                                     (or (eq? (current-compile-target-machine) (deps-machine deps))
                                         (not (deps-machine deps))
                                         (and (cross-multi-compile? roots)
                                              (eq? (system-type 'target-machine) (deps-machine deps))))))
            (values (or (and ok-machine?
                             (deps-imports-sha1 deps))
                        "")
                    (and ok-machine?
                         (deps-assume-compiled-sha1 deps)))))))
     ;; Combine the sha1 for the compiled form with the sha1 of imports;
     ;; if we have to read the compiled form and that failed (e.g., because
     ;; the file's not there), then return #f overall:
     (let ([sha-1 (or assume-compiled-sha1 path-sha1)])
       (trace-printf "compiled hash for ~a: ~a ~a ~a" path sha-1 (and assume-compiled-sha1 #t) imports-sha1)
       (and sha-1
            (string-append sha-1 imports-sha1))))))

;; Gets a multi-sha1 string that represents the compiled code
;; (plus dependencies), checking for a native library before
;; falling back normally to bytecode, and returning "" insteda of
;; #f if compiled code is not available:
(define (get-compiled-sha1 path->mode roots path)
  (define-values (dir name) (get-compilation-dir+name path
                                                      #:modes (list (path->mode path))
                                                      #:roots roots
                                                      #:default-root (if (cross-multi-compile? roots)
                                                                         (cadr roots)
                                                                         (car roots))))
  (let ([dep-path (build-path dir (path-add-extension name #".dep"))])
    (or (and (eq? 'racket (system-type 'vm))
             (try-file-sha1 (build-path dir "native" (system-library-subpath)
                                        (path-add-extension name (system-type
                                                                  'so-suffix)))
                            dep-path
                            roots))
        (try-file-sha1 (build-path dir (path-add-extension name #".zo"))
                       dep-path
                       roots)
        "")))

;; Checks to see whether things are the same, and returns the other
;; values if they's different
(define (different-source-sha1-and-dep-recorded path deps)
  (define src-hash (get-source-sha1 path))
  (define recorded-hash (and (pair? (deps-sha1s deps))
                             (deps-src-sha1 deps)))
  (if (equal? src-hash recorded-hash)
      #f
      (list src-hash recorded-hash)))

;; Make sure `recompile-from` is machine-independent so that
;; recompilation makes sense.
;; The compilation lock must is held for the source of `recompile-from`.
(define (select-machine-independent recompile-from
                                    recompile-from-machine
                                    path
                                    roots
                                    path->mode)
  (cond
    [(not recompile-from) #f]
    [(not recompile-from-machine) recompile-from]
    [(and (pair? roots) (pair? (cdr roots)))
     ;; We have a machine-dependent ".zo" file. Maybe we'll
     ;; fine a machine-independent version by checking the
     ;; last compilation path
     (define-values (code-dir code-name)
       (get-compilation-dir+name path #:modes (list (path->mode path)) #:roots (list (last roots))))
     (define alt-recompile-from
       (build-path code-dir (path-add-suffix code-name #".zo")))
     (define deps (read-deps-file (path-replace-suffix alt-recompile-from #".dep")))
     (and (not (deps-machine deps))
          alt-recompile-from)]
    [else #f]))

(define (compilation-failure zo-name keep-zo-name)
  (unless (equal? zo-name keep-zo-name)
    (try-delete-file zo-name))
  (trace-printf "failure"))

;; Exported:
(define (get-compiled-file-sha1 path)
  (try-file-sha1 path
                 (path-replace-extension path #".dep")
                 (current-compiled-file-roots)))

(define (get-file-sha1 path)
  (get-source-sha1 path))

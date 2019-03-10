#lang racket/base
(require (for-syntax (only-in racket/base ~@))
         racket/contract/base
         racket/match
         syntax/modresolve
         setup/collection-name
         setup/collects
         setup/dirs
         raco/all-tools
         setup/collection-paths
         pkg/lib)
(provide
 (contract-out
  [whereis-module
   (-> (or/c module-path? module-path-index?) path?)]
  [whereis-pkg
   (-> string? path?)]
  [whereis-collection
   (-> collection-name? (listof path?))]
  [whereis-raco
   (-> string? path?)]
  [whereis-system
   (-> symbol? (or/c path? (listof path?)))]
  [whereis-binding
   (->* [identifier?] [(or/c exact-integer? #f)] path?)]
  [whereis-binding/symbol
   (-> module-path? symbol? path?)]))

;; whereis-module : ModulePath -> Path
;; What path contains given module-path?
;; Error if no corresponding path (eg, '#%kernel, not found).
(define (whereis-module modpath)
  (define resolved
    (with-handlers ([exn:fail? (mk-cannot-resolve 'whereis-module modpath)])
      (cond [(module-path-index? modpath)
             (resolve-module-path-index modpath (current-directory))]
            [(module-path? modpath)
             (resolve-module-path modpath (current-directory))])))
  (resolved->path 'whereis-module resolved))

(define (resolved->path who p)
  (let loop ([p p])
    (cond [(and (pair? p) (eq? (car p) 'submod)) (loop (cadr p))]
          [(path? p)
           (cond [(file-exists? p) p]
                 [else (error who "module path does not exist: ~e" (path->string p))])]
          [(symbol? p) (error who "built-in module has no path: ~e" p)]
          [else (error 'resolved->path "unexpected value: ~e" p)])))

(define ((mk-cannot-resolve who modpath) e)
  (error who "failed to resolve module path: ~s" modpath))

;; whereis-pkg : String -> Path
;; What is the directory of given package?
(define (whereis-pkg pkg-name)
  (cond [(pkg-directory pkg-name) => simplify-path]
        [else (error 'whereis-pkg "failed to find package: ~s" pkg-name)]))

;; whereis-collection : String -> (Listof Path)
;; What directories correspond to given collection?
(define (whereis-collection collect)
  (define dirs (collection-paths collect))
  (unless (pair? dirs)
    (error 'whereis-collection "failed to find collection: ~s" collect))
  dirs)

;; whereis-raco : String -> Path
;; Where is the implementation of the given raco subcommand?
(define (whereis-raco command)
  (cond [(hash-ref (all-tools) command #f)
         => (lambda (info) (whereis-module (cadr info)))]
        [else (error 'whereis-raco "failed to find raco command: ~s" command)]))

;; whereis-system : Symbol -> (U Path (Listof Path))
(define (whereis-system name)
  (case name

    ;; == find-system-path ==
    ;; omit run-file, orig-dir -- these only make sense for programs
    [(home-dir
      pref-dir pref-file temp-dir init-dir init-file
      addon-dir doc-dir desk-dir sys-dir)
     (find-system-path name)]
    [(exec-file config-dir host-config-dir collects-dir host-collects-dir)
     ;; may be relative to racket executable
     (define path (find-system-path name))
     (cond [(and (path? path) (relative-path? path))
            (find-executable-path (find-system-path 'exec-file) path #t)]
           [else path])]

    ;; == others ==
    [else
     (cond [(hash-ref whereis-system-procs name #f)
            => (lambda (proc)
                 (or (proc)
                     (error 'whereis-system "no path for location: (~s) returned #f"
                            name)))]
           [else (error 'whereis-system "unknown system location: ~e" name)])]))

;; whereis-system-procs : Hash[Symbol => (-> (U Path (Listof Path) #f))
(define whereis-system-procs
  (let ()
    (define-syntax-rule (hq name ...) (hasheq (~@ 'name name) ...))
    (hq
     ;; == setup/dirs ==
     find-collects-dir
     find-user-collects-dir
     get-collects-search-dirs
     get-main-collects-search-dirs
     find-config-dir
     find-pkgs-dir
     find-user-pkgs-dir
     get-pkgs-search-dirs
     find-doc-dir
     find-user-doc-dir
     get-doc-search-dirs
     find-lib-dir
     find-user-lib-dir
     get-lib-search-dirs
     get-cross-lib-search-dirs
     find-dll-dir
     find-cross-dll-dir
     find-share-dir
     find-user-share-dir
     find-include-dir
     get-include-search-dirs
     find-console-bin-dir
     find-gui-bin-dir
     find-user-console-bin-dir
     find-user-gui-bin-dir
     find-apps-dir
     find-user-apps-dir
     find-man-dir
     find-user-man-dir
     find-addon-tethered-console-bin-dir
     find-addon-tethered-gui-bin-dir
     find-config-tethered-console-bin-dir
     find-config-tethered-gui-bin-dir
     )))

;; whereis-binding : Identifier [Nat/#f] -> Path
(define (whereis-binding id [phase (syntax-local-phase-level)])
  (define b (identifier-binding id phase))
  (cond [(list? b) (whereis-module (car b))]
        [else (error 'whereis-binding "not bound to module export (at phase ~s): ~e"
                     phase id)]))

;; whereis-binding/symbol : ModulePath Symbol -> Path
;; FIXME: add option to search through provide/contract?
(define (whereis-binding/symbol providing-mod name)
  ;; Avoid instantiating any modules at phase 0, but may have to
  ;; compile providing-mod!
  (parameterize ((current-namespace (make-base-empty-namespace)))
    (namespace-require `(for-label ,providing-mod))
    (define id (namespace-symbol->identifier name))
    (define b (identifier-binding id #f))
    (cond [(list? b) (whereis-module (car b))]
          [else (error 'whereis-binding/symbol
                       "no binding provided by module ~s named ~s"
                       providing-mod name)])))


;; FIXME: add "Where is the documentation for the given module/binding/etc?"
;; This information does not seem to be available from scribble/xref etc.
;; Two levels of granularity; for example, given "lambda from racket":
;; - root: => path of (lib "scribblings/reference/reference.scrbl")
;;   (This is what the rendered "Link to this section" contains.)
;; - fine: => path of (lib "scribblings/reference/syntax.scrbl")
;;   (This is where the defform actually occurs.)

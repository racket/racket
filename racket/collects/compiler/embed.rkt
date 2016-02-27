#lang racket/base
(require racket/path
         racket/file
         racket/port
         racket/promise
         racket/list
	 racket/contract
         syntax/moddep
         syntax/modcollapse
         xml/plist
         setup/dirs
         setup/variant
         file/ico
         racket/private/so-search
         setup/cross-system
         "private/winsubsys.rkt"
         "private/macfw.rkt"
         "private/mach-o.rkt"
         "private/elf.rkt"
         "private/windlldir.rkt"
         "private/pe-rsrc.rkt"
         "private/collects-path.rkt"
         "private/configdir.rkt"
         "private/write-perm.rkt"
         "find-exe.rkt")


(provide/contract [make-embedding-executable
                   (->* (path-string?
                         any/c
                         any/c
                         (listof (or/c (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?))
                                       (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?)
                                               (listof symbol?))))
                         (listof path-string?)
                         any/c
                         (listof string?))
                        ((listof (cons/c symbol? any/c))
                         any/c
                         symbol?
                         (or/c #f
                               path-string?
                               (listof path-string?)))
                        void?)]
                  [create-embedding-executable
                   (->* (path-string?)
                        (#:modules 
                         (listof (or/c (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?))
                                       (list/c (or/c symbol? #f #t)
                                               (or/c path? module-path?)
                                               (listof symbol?))))
                         #:configure-via-first-module? any/c
                         #:early-literal-expressions (listof any/c)
                         #:literal-files (listof path-string?)
                         #:literal-expression any/c
                         #:literal-expressions (listof any/c)
                         #:cmdline (listof string?)
                         #:gracket? any/c
                         #:mred? any/c
                         #:variant (or/c '3m 'cgc)
                         #:aux (listof (cons/c symbol? any/c))
                         #:collects-path (or/c #f
                                               path-string?
                                               (listof path-string?))
                         #:collects-dest (or/c #f path-string?)
                         #:launcher? any/c
                         #:verbose? any/c
                         #:compiler (-> any/c compiled-expression?)
                         #:expand-namespace namespace?
                         #:src-filter (-> path? any)
                         #:on-extension (or/c #f (-> path-string? boolean? any))
                         #:get-extra-imports (-> path? compiled-module-expression? (listof module-path?)))
                        void?)])

(provide write-module-bundle
         embedding-executable-is-directory?
         embedding-executable-is-actually-directory?
         embedding-executable-put-file-extension+style+filters
         embedding-executable-add-suffix)

  
(define (embedding-executable-is-directory? mred?)
  #f)

(define (embedding-executable-is-actually-directory? mred?)
  (and mred? (eq? 'macosx (cross-system-type))))

(define (embedding-executable-put-file-extension+style+filters mred?)
  (case (cross-system-type)
    [(windows) (values "exe" null '(("Executable" "*.exe")))]
    [(macosx) (if mred?
                  (values "app" '(enter-packages) '(("App" "*.app")))
                  (values #f null null))]
    [else (values #f null null)]))

(define (embedding-executable-add-suffix path mred?)
  (let* ([path (if (string? path)
                   (string->path path)
                   path)]
         [fixup (lambda (re sfx)
                  (if (regexp-match re (path->bytes path))
                      path
                      (path-replace-suffix path sfx)))])
    (case (cross-system-type)
      [(windows) (fixup #rx#"[.][eE][xX][eE]$" #".exe")]
      [(macosx) (if mred?
                    (fixup #rx#"[.][aA][pP][pP]$" #".app")
                    path)]
      [else path])))

(define (mac-dest->executable dest mred?)
  (if mred?
      (let-values ([(base name dir?) (split-path dest)])
        (build-path dest
                    "Contents" "MacOS"
                    (path-replace-suffix name #"")))
      dest))

(define exe-suffix?
  (delay (equal? #"i386-cygwin" (path->bytes (cross-system-library-subpath)))))

;; Find the magic point in the binary:
(define (find-cmdline what rx)
  (let ([m (regexp-match-positions rx (current-input-port))])
    (if m
        (caar m)
        (error 
         'create-embedding-executable
         (format
          "can't find ~a position in executable"
          what)))))


(define (relativize exec-name dest adjust)
  (let ([p (find-relative-path
            (let-values ([(dir name dir?) (split-path 
                                           (normal-case-path
                                            (normalize-path dest)))])
              dir)
            (normal-case-path (normalize-path exec-name)))])
    (if (relative-path? p)
        (adjust p)
        p)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prepare-macosx-mred exec-name dest aux variant)
  (let* ([name (let-values ([(base name dir?) (split-path dest)])
                 (path-replace-suffix name #""))]
         [src (build-path (find-lib-dir) "Starter.app")]
         [creator (let ([c (assq 'creator aux)])
                    (or (and c
                             (cdr c))
                        "MrSt"))]
         [file-types (let ([m (assq 'file-types aux)])
                       (and m
                            (pair? (cdr m))
                            (cdr m)))]
         [uti-exports (let ([m (assq 'uti-exports aux)])
                        (and m
                             (pair? (cdr m))
                             (cdr m)))]
         [resource-files (let ([m (assq 'resource-files aux)])
                           (and m
                                (cdr m)))])
    (when creator
      (unless (and (string? creator) (= 4 (string-length creator)))
        (error 'make-executable "creator is not a 4-character string: ~e" creator)))
    (when file-types
      (unless (and (list? file-types)
                   (andmap list? file-types)
                   (andmap (lambda (spec)
                             (andmap (lambda (p)
                                       (and (list? p)
                                            (= 2 (length p))
                                            (string? (car p))))
                                     spec))
                           file-types))
        (error 'make-executable "bad file-types spec: ~e" file-types)))
    (when resource-files
      (unless (and (list? resource-files)
                   (andmap path-string?
                           resource-files))
        (error 'make-executable "resource-files is not a list of paths: ~e" resource-files)))
    
    (when (or (directory-exists? dest)
              (file-exists? dest)
              (link-exists? dest))
      (delete-directory/files dest))
    (make-directory* (build-path dest "Contents" "Resources"))
    (make-directory* (build-path dest "Contents" "MacOS"))
    (copy-file exec-name (build-path dest "Contents" "MacOS" name))
    (copy-file (build-path src "Contents" "PkgInfo")
               (build-path dest "Contents" "PkgInfo"))
    (let ([icon (or (let ([icon (assq 'icns aux)])
                      (and icon
                           (cdr icon)))
                    (build-path src "Contents" "Resources" "Starter.icns"))])
      (copy-file icon
                 (build-path dest "Contents" "Resources" "Starter.icns")))
    (let ([orig-plist (call-with-input-file (build-path src
                                                        "Contents"
                                                        "Info.plist")
                        read-plist)]
          [plist-replace (lambda (plist . l)
                           (let loop ([plist plist][l l])
                             (if (null? l)
                                 plist
                                 (let ([key (car l)]
                                       [val (cadr l)])
                                   (loop `(dict
                                           ,@(let loop ([c (cdr plist)])
                                               (cond
                                                 [(null? c) (list (list 'assoc-pair key val))]
                                                 [(string=? (cadar c) key)
                                                  (cons (list 'assoc-pair key val)
                                                        (cdr c))]
                                                 [else
                                                  (cons (car c)
                                                        (loop (cdr c)))])))
                                         (cddr l))))))])
      (let* ([new-plist (plist-replace
                         orig-plist
                         
                         "CFBundleExecutable" 
                         (path->string name)
                         
                         "CFBundleSignature"
                         creator
                         
                         "CFBundleIdentifier" 
                         (format "org.racket-lang.~a" (path->string name)))]
             [new-plist (if uti-exports
                            (plist-replace
                             new-plist
                             "UTExportedTypeDeclarations"
                             (cons 'array
                                   (map (lambda (spec)
                                          (cons
                                           'dict
                                           (map (lambda (p)
                                                  (list
                                                   'assoc-pair
                                                   (car p)
                                                   (cadr p)))
                                                spec)))
                                        uti-exports)))
                            new-plist)]
             [new-plist (if file-types
                            (plist-replace
                             new-plist                                 
                             "CFBundleDocumentTypes"
                             (cons 'array
                                   (map (lambda (spec)
                                          (cons
                                           'dict
                                           (map (lambda (p)
                                                  (list
                                                   'assoc-pair
                                                   (car p)
                                                   (cadr p)))
                                                spec)))
                                        file-types)))                                
                            new-plist)])
        (call-with-output-file (build-path dest 
                                           "Contents" 
                                           "Info.plist")
          #:exists 'truncate
          (lambda (port)
            (write-plist new-plist port)))))
    (call-with-output-file (build-path dest 
                                       "Contents" 
                                       "PkgInfo")
      #:exists 'truncate
      (lambda (port)
        (fprintf port "APPL~a" creator)))
    (when resource-files
      (for-each (lambda (p)
                  (let-values ([(base name dir?) (split-path p)])
                    (copy-file p (build-path dest
                                             "Contents" 
                                             "Resources"
                                             name))))
                resource-files))
    (build-path dest "Contents" "MacOS" name)))

;; The starter-info file is now disabled. The GRacket
;; command line is handled the same as the Racket command
;; line.
(define use-starter-info? #f)
(define (finish-osx-mred dest flags exec-name keep-exe? relative?)
  (call-with-output-file (build-path dest 
                                     "Contents" 
                                     "Resources" 
                                     "starter-info")
    #:exists 'truncate
    (lambda (port)
      (write-plist 
       `(dict ,@(if keep-exe?
                    `((assoc-pair "executable name"
                                  ,(path->string 
                                    (if relative?
                                        (relativize exec-name dest
                                                    (lambda (p)
                                                      (build-path 'up 'up 'up p)))
                                        exec-name))))
                    null)
              (assoc-pair "stored arguments"
                          (array ,@flags)))
       port))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Represent modules with lists starting with the filename, so we
;; can use assoc:
(define (make-mod normal-file-path normal-module-path 
                  code name prefix full-name relative-mappings-box 
                  runtime-paths runtime-module-syms
                  actual-file-path
                  use-source?)
  (list normal-file-path normal-module-path code
        name prefix full-name relative-mappings-box
        runtime-paths runtime-module-syms 
        actual-file-path
        use-source?))

(define (mod-file m) (car m))
(define (mod-mod-path m) (cadr m))
(define (mod-code m) (caddr m))
(define (mod-name m) (list-ref m 3))
(define (mod-prefix m) (list-ref m 4))
(define (mod-full-name m) (list-ref m 5))
(define (mod-mappings m) (unbox (list-ref m 6)))
(define (mod-runtime-paths m) (list-ref m 7))
(define (mod-runtime-module-syms m) (list-ref m 8))
(define (mod-actual-file m) (list-ref m 9))
(define (mod-use-source? m) (list-ref m 10))

(define (generate-prefix)
  (format "#%embedded:~a:" (gensym)))

(define (normalize filename)
  (if (pair? filename)
      `(submod ,(normalize (cadr filename)) ,@(cddr filename))
      (let ([f (simplify-path (cleanse-path filename))])
        ;; Use normal-case-path on just the base part, to avoid
        ;; changing the filename case (which should match the
        ;; module-name case within the file):
        (let-values ([(base name dir?) (split-path f)])
          (if (path? base)
              (build-path (normal-case-path base) name)
              f)))))

(define (strip-submod a)
  (if (and (pair? a)
           (eq? 'submod (car a)))
      (cadr a)
      a))

(define (is-lib-path? a)
  (let ([a (strip-submod a)])
    (or (and (pair? a)
             (eq? 'lib (car a)))
        (symbol? a))))

(define (symbol-to-lib-form l)
  (if (symbol? l)
      `(lib ,(symbol->string l))
      l))
  
(define (unix-style-split p)
  (let ([m (regexp-match #rx"^([^/]*)/(.*)$" p)])
    (if m
        (cons (cadr m) (unix-style-split (caddr m)))
        (list p))))

(define (extract-last l)
  (let loop ([l l][dirs null])
    (if (null? (cdr l))
        (values (reverse dirs) (car l))
        (loop (cdr l) (cons (car l) dirs)))))

(define (adjust-ss/rkt-suffix path)
  (cond
   [(file-exists? path) path]
   [(regexp-match? #rx"[.]ss$" path)
    (define rkt-path (path-replace-suffix path #".rkt"))
    (if (file-exists? rkt-path)
        rkt-path
        path)]
   [(regexp-match? #rx"[.]rkt$" path)
    (define ss-path (path-replace-suffix path #".ss"))
    (if (file-exists? ss-path)
        ss-path
        path)]
   [else path]))

(define (lib-module-filename collects-dest module-path)
  (let-values ([(dir file) 
                (let ([s (lib-path->string (strip-submod module-path))])
                  (extract-last (unix-style-split s)))])
    (let ([p (build-path collects-dest
                         (apply build-path dir)
                         "compiled"
                         (path-add-suffix file #".zo"))])
      (let-values ([(base name dir?) (split-path p)])
        (make-directory* base)
        p))))

(define (file-date f)
  (with-handlers ([exn:fail:filesystem? (lambda (x) -inf.0)])
    (file-or-directory-modify-seconds f)))

(define-struct extension (path))

;; Loads module code, using .zo if there, compiling from .scm if not
(define (get-code filename module-path ready-code use-submods codes prefixes verbose? collects-dest on-extension 
                  compiler expand-namespace src-filter get-extra-imports working)
  ;; filename can have the form `(submod ,filename ,sym ...)
  (let* ([a (assoc filename (unbox codes))]
         ;; If we didn't fine `filename` as-is, check now for
         ;; using source, because in that case we'll only register the
         ;; main module even if a submodule is include in `filename`.
         [use-source?
          (and (not a)
               (src-filter (adjust-ss/rkt-suffix (strip-submod filename))))]
         ;; When using source or writing to collects, keep full modules:
         [keep-full? (or use-source? collects-dest)]
         ;; When keeping a full module, strip away submodule paths:
         [filename (or (and (not a)
                            keep-full?
                            (pair? filename)
                            (cadr filename))
                       filename)]
         ;; Maybe search again after deciding whether to strip submodules:
         [a (or a 
                (and keep-full?
                     ;; Try again:
                     (assoc filename (unbox codes))))])
    (cond
     [a
      ;; Already have this module. Make sure that library-referenced
      ;;  modules are consistently referenced through library paths:
      (let ([found-lib? (is-lib-path? (mod-mod-path a))]
            [look-lib? (is-lib-path? module-path)])
        (cond
         [(and found-lib? look-lib?)
          'ok]
         [(or found-lib? look-lib?)
          (error 'find-module
                 "module referenced both as a library and through a path: ~a"
                 filename)]
         [else 'ok]))]
     [(hash-ref working filename #f)
      ;; in the process of loading the module; a cycle
      ;; is possible through `define-runtime-path'
      'ok]
     [else
      ;; First use of the module. Get code and then get code for imports.
      (when verbose?
        (eprintf "Getting ~s as ~s\n" module-path filename))
      (let* ([submod-path (if (pair? filename)
                              (cddr filename)
                              null)]
             [just-filename (strip-submod filename)]
             [root-module-path (strip-submod module-path)]
             [actual-filename just-filename] ; `set!'ed below to adjust file suffix
             [name (let-values ([(base name dir?) (split-path just-filename)])
                     (path->string (path-replace-suffix name #"")))]
             [prefix (let ([a (assoc just-filename prefixes)])
                       (if a
                           (cdr a)
                           (generate-prefix)))]
             [full-name (string->symbol
                         (format "~a~a~a" prefix name
                                 (if (null? submod-path)
                                     ""
                                     submod-path)))])
        (hash-set! working filename full-name)
        (let ([code (or ready-code
                        (get-module-code just-filename
                                         #:submodule-path submod-path
                                         "compiled"
                                         compiler
                                         (if on-extension
                                             (lambda (f l?)
                                               (on-extension f l?)
                                               #f)
                                             (lambda (file _loader?)
                                               (if _loader?
                                                   (error 'create-embedding-executable
                                                          "cannot use a _loader extension: ~e"
                                                          file)
                                                   (make-extension file))))
                                         #:choose
                                         ;; Prefer extensions, if we're handling them:
                                         (lambda (src zo so)
                                           (set! actual-filename src) ; remember convert source name
                                           (if on-extension
                                               #f
                                               (if (and (file-exists? so)
                                                        ((file-date so) . >= . (file-date zo)))
                                                   'so
                                                   #f)))))])
          (cond
           [(extension? code)
            (when verbose?
              (eprintf " using extension: ~s\n" (extension-path code)))
            (set-box! codes
                      (cons (make-mod filename module-path code 
                                      name prefix full-name
                                      (box null) null null
                                      actual-filename
                                      #f)
                            (unbox codes)))]
           [code
            (let ([importss (module-compiled-imports code)])
              (let ([all-file-imports (filter (keep-import-dependency? keep-full? actual-filename)
                                              (apply append (map cdr importss)))]
                    [extra-paths 
                     (map symbol-to-lib-form (append (if keep-full?
                                                         (extract-full-imports module-path actual-filename code)
                                                         null)
                                                     (if use-source?
                                                         (list 'compiler/private/read-bstr)
                                                         null)
                                                     (get-extra-imports actual-filename code)))])
                (let* ([runtime-paths
                        (if (module-compiled-cross-phase-persistent? code)
                            ;; avoid potentially trying to redeclare cross-phase persistent modules,
                            ;; since redeclaration isn't allowed:
                            null
                            ;; check for run-time paths by visiting the module in an
                            ;; expand-time namespace:
                            (parameterize ([current-namespace expand-namespace])
                              (define no-submodule-code
                                ;; Strip away submodules to avoid re-declaring them:
                                (module-compiled-submodules 
                                 (module-compiled-submodules code #f null)
                                 #t
                                 null))
                              (eval no-submodule-code)
                              (let ([module-path
                                     (if (path? module-path)
                                         (path->complete-path module-path)
                                         module-path)])
                                (define e (expand `(,#'module m racket/kernel
                                                     (#%require (only ,module-path)
                                                                racket/runtime-path)
                                                     (runtime-paths ,module-path))))
                                (syntax-case e (quote)
                                  [(_ m mz (#%mb req (quote (spec ...))))
                                   (syntax->datum #'(spec ...))]
                                  [_else (error 'create-empbedding-executable
                                                "expansion mismatch when getting external paths: ~e"
                                                (syntax->datum e))]))))]
                       
                       [extra-runtime-paths (filter
                                             values
                                             (map (lambda (p)
                                                    (and (pair? p)
                                                         (eq? (car p) 'module)
                                                         (cadr p)))
                                                  runtime-paths))]
                       [renamed-code (if (symbol? (module-compiled-name code))
                                         code
                                         (module-compiled-name code (last (module-compiled-name code))))]
                       [extract-submods (lambda (l)
                                          (if (or (null? use-submods)
                                                  use-source?)
                                              null
                                              (for/list ([m (in-list l)]
                                                         #:when (or (member (last (module-compiled-name m)) use-submods)
                                                                    (declares-always-preserved? m)))
                                                m)))]
                       [pre-submods (extract-submods (module-compiled-submodules renamed-code #t))]
                       [post-submods (extract-submods (module-compiled-submodules renamed-code #f))]
                       [code (if keep-full?
                                 code
                                 (module-compiled-submodules (module-compiled-submodules
                                                              renamed-code
                                                              #f
                                                              null)
                                                             #t
                                                             null))])
                  (let ([sub-files (map (lambda (i) 
                                          ;; use `just-filename', because i has submod name embedded
                                          (normalize (resolve-module-path-index i just-filename)))
                                        all-file-imports)]
                        [sub-paths (map (lambda (i) 
                                          ;; use `root-module-path', because i has submod name embedded
                                          (collapse-module-path-index i root-module-path))
                                        all-file-imports)]
                        [normalized-extra-paths (map (lambda (i) (collapse-module-path i module-path))
                                                     (append extra-runtime-paths extra-paths))]
                        [extra-files (map (lambda (i) (normalize (resolve-module-path-index (module-path-index-join i #f)
                                                                                            filename)))
                                          ;; getting runtime-module-path symbols below
                                          ;; relies on extra-runtime-paths being first:
                                          (append extra-runtime-paths extra-paths))])
                    (define (get-one-code sub-filename sub-path ready-code)
                      (get-code sub-filename sub-path ready-code null
                                codes
                                prefixes
                                verbose?
                                collects-dest
                                on-extension
                                compiler
                                expand-namespace
                                src-filter get-extra-imports
                                working))
                    (define (get-one-submodule-code m)
                      (define name (cadr (module-compiled-name m)))
                      (define mp `(submod "." ,name))
                      (define mpi (module-path-index-join mp #f))
                      (get-one-code (resolve-module-path-index mpi filename)
                                    (if (is-lib-path? module-path)
                                        ;; Preserve `lib`-ness of module reference:
                                        (collapse-module-path-index
                                         (module-path-index-join mp module-path))
                                        ;; Ok to collapse based on filename:
                                        (collapse-module-path-index mpi filename))
                                    m))
                    ;; Add code for pre submodules:
                    (for-each get-one-submodule-code pre-submods)
                    ;; Get code for imports:
                    (for-each (lambda (sf sp) (get-one-code sf sp #f))
                              (append sub-files extra-files)
                              (append sub-paths normalized-extra-paths))
                    (when verbose?
                      (unless (null? runtime-paths)
                        (eprintf "Runtime paths for ~s: ~s\n"
                                 filename
                                 runtime-paths)))
                    (if (and collects-dest
                             (is-lib-path? module-path))
                        ;; Install code as .zo:
                        (begin
                          (with-output-to-file (lib-module-filename collects-dest module-path)
                            #:exists 'truncate/replace
                            (lambda ()
                              (write code)))
                          ;; Record module as copied
                          (set-box! codes
                                    (cons (make-mod filename module-path #f
                                                    #f #f #f
                                                    (box null) null null
                                                    actual-filename
                                                    use-source?)
                                          (unbox codes))))
                        ;; Build up relative module resolutions, relative to this one,
                        ;; that will be requested at run-time.
                        (let* ([lookup-full-name (lambda (sub-filename)
                                                   (let ([m (assoc sub-filename (unbox codes))])
                                                     (if m
                                                         (mod-full-name m)
                                                         ;; must have been a cycle...
                                                         (hash-ref working sub-filename
                                                                   (lambda ()
                                                                     ;; If `sub-filename` was included from source,
                                                                     ;; then we'll need to use a submodule path:
                                                                     `(,(hash-ref working (strip-submod sub-filename))
                                                                       ,@(cddr sub-filename)))))))]
                               [get-submod-mapping
                                (lambda (m)
                                  (define name (cadr (module-compiled-name m)))
                                  (cons `(submod "." ,name)
                                        (lookup-full-name
                                         (collapse-module-path-index 
                                          (module-path-index-join `(submod "." ,name) #f)
                                          filename))))]
                               [mappings-box
                                (box (append
                                      (filter (lambda (p) (and p (cdr p)))
                                              (map (lambda (sub-i sub-filename sub-path)
                                                     (and (not (and collects-dest
                                                                    (is-lib-path? sub-path)))
                                                          (if sub-i
                                                              (let-values ([(path base) (module-path-index-split sub-i)])
                                                                (and base ; can be #f if path isn't relative
                                                                     (begin
                                                                       ;; Assert: base should refer to this module:
                                                                       (let-values ([(path2 base2) (module-path-index-split base)])
                                                                         (when (or path2 base2)
                                                                           (error 'embed "unexpected nested module path index")))
                                                                       (cons path (lookup-full-name sub-filename)))))
                                                              ;; a run-time path:
                                                              (cons sub-path (lookup-full-name sub-filename)))))
                                                   (append all-file-imports (map (lambda (p) #f) extra-runtime-paths))
                                                   (append sub-files (take extra-files (length extra-runtime-paths)))
                                                   (append sub-paths extra-runtime-paths)))
                                      (map get-submod-mapping pre-submods)))])
                          ;; Record the module
                          (set-box! codes
                                    (cons (make-mod filename module-path code 
                                                    name prefix full-name
                                                    mappings-box
                                                    runtime-paths
                                                    ;; extract runtime-path module symbols:
                                                    (let loop ([runtime-paths runtime-paths]
                                                               [extra-files extra-files])
                                                      (cond
                                                       [(null? runtime-paths) null]
                                                       [(let ([p (car runtime-paths)])
                                                          (and (pair? p) (eq? (car p) 'module)))
                                                        (cons (lookup-full-name (car extra-files))
                                                              (loop (cdr runtime-paths) (cdr extra-files)))]
                                                       [else
                                                        (cons #f (loop (cdr runtime-paths) extra-files))]))
                                                    actual-filename
                                                    use-source?)
                                          (unbox codes)))
                          ;; Add code for post submodules:
                          (for-each get-one-submodule-code post-submods)
                          ;; Add post-submodule mappings:
                          (set-box! mappings-box
                                    (append (unbox mappings-box)
                                            (map get-submod-mapping post-submods)))))))))]
           [else
            (set-box! codes
                      (cons (make-mod filename module-path code 
                                      name #f #f
                                      null null null
                                      actual-filename
                                      use-source?)
                            (unbox codes)))])))])))

(define ((keep-import-dependency? keep-full? path) orig-x)
  (define-values (x base) (module-path-index-split orig-x))
  (not (or (and (pair? x)
                (eq? 'quote (car x)))
           (and keep-full?
                ;; Don't try to include submodules specifically if the enclosing
                ;; module is kept fully. Any needed dependencies will be
                ;; extracted via `extract-full-imports`.
                (pair? x)
                (eq? (car x) 'submod)
                (or (equal? (cadr x) ".")
                    (equal? path
                            (normalize (resolve-module-path-index (module-path-index-join (cadr x) #f)
                                                                  path))))))))

(define (extract-full-imports module-path path code)
  ;; When embedding a module from source or otherwise keeping a full
  ;; module, we need to collect all dependencies from submodules
  ;; (recursively), because they'll be needed to start again from
  ;; source.
  (let accum-from-mod ([mod code])
    (append
     (map (lambda (i) (collapse-module-path-index i module-path))
          (filter (keep-import-dependency? #t path)
                  (apply append (map cdr (module-compiled-imports mod)))))
     (apply append
            (map accum-from-mod (module-compiled-submodules mod #t)))
     (apply append
            (map accum-from-mod (module-compiled-submodules mod #f))))))

(define (declares-always-preserved? m)
  (for/or ([s (in-list
               (append (module-compiled-submodules m #t)
                       (module-compiled-submodules m #f)))])
    (eq? (last (module-compiled-name s)) 'declare-preserve-for-embedding)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-using-kernel e)
  (let ([ns (make-empty-namespace)])
    (namespace-attach-module (current-namespace) ''#%kernel ns)
    (parameterize ([current-namespace ns])
      (namespace-require ''#%kernel)
      (compile e))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lib-path->string path)
  (cond
   [(null? (cddr path))
    (if (regexp-match #rx"^[^/]*[.]" (cadr path))
        ;; mzlib
        (string-append "mzlib/" (cadr path))
        ;; new-style
        (if (regexp-match #rx"^[^/.]*$" (cadr path))
            (string-append (cadr path) "/main.ss")
            (if (regexp-match #rx"^[^.]*$" (cadr path))
                ;; need a suffix:
                (string-append (cadr path) ".ss")
                (cadr path))))]
   [else
    ;; old-style multi-string:
    (string-append (apply string-append
                          (map (lambda (s)
                                 (string-append s "/"))
                               (cddr path)))
                   (cadr path))]))

(define (make-module-name-resolver code-l)
  (let ([extensions (filter (lambda (m) (extension? (mod-code m))) code-l)])
    `(module #%resolver '#%kernel
       (let-values ([(orig) (current-module-name-resolver)]
                    [(regs) (make-hasheq)]
                    [(mapping-table) (quote
                                      ,(map
                                        (lambda (m)
                                          `(,(mod-full-name m)
                                            ,(mod-mappings m)))
                                        code-l))]
                    [(library-table) (quote
                                      ,(filter values
                                               (map (lambda (m)
                                                      (let loop ([path (mod-mod-path m)])
                                                        (cond
                                                         [(and (pair? path)
                                                               (eq? 'lib (car path)))
                                                          (cons (lib-path->string path)
                                                                (mod-full-name m))]
                                                         [(and (pair? path)
                                                               (eq? 'planet (car path)))
                                                          ;; Normalize planet path
                                                          (cons (collapse-module-path path current-directory)
                                                                (mod-full-name m))]
                                                         [(and (pair? path)
                                                               (eq? 'submod (car path)))
                                                          (define m (loop (cadr path)))
                                                          (and m
                                                               (cons `(submod ,(car m) ,@(cddr path))
                                                                     (cdr m)))]
                                                         [else #f])))
                                                    code-l)))])
         (hash-set! regs 
                    (namespace-module-registry (current-namespace))
                    (vector mapping-table library-table))
         (letrec-values ([(lookup)
                          (lambda (name rel-to stx load? for-submod? orig)
                            (if (not (module-path? name))
                                ;; Bad input
                                (orig name rel-to stx load?)
                                (let-values ([(table-vec) (hash-ref regs (namespace-module-registry (current-namespace)) #f)]
                                             [(name) (if (pair? name)
                                                         (if (eq? 'submod (car name))
                                                             (if (null? (cddr name))
                                                                 (if (equal? ".." (cadr name))
                                                                     name
                                                                     (if (equal? "." (cadr name))
                                                                         name
                                                                         (cadr name))) ; strip away `submod' without a submodule path
                                                                 name)
                                                             name)
                                                         name)])
                                  (if (not table-vec)
                                      ;; No mappings in this registry
                                      (orig name rel-to stx load?)
                                      (let-values ([(mapping-table) (vector-ref table-vec 0)]
                                                   [(library-table) (vector-ref table-vec 1)])
                                        ;; Have a relative mapping?
                                        (let-values ([(a) (if rel-to
                                                              (let-values ([(v) (assq (resolved-module-path-name rel-to) mapping-table)])
                                                                (if v
                                                                    v
                                                                    ;; It we're loading a module from source, then `rel-to` might not be
                                                                    ;; our eventual name, but `(current-module-declare-name)` provides
                                                                    ;; one, so try using that to resolve the module:
                                                                    (if (current-module-declare-name)
                                                                        (assq (resolved-module-path-name (current-module-declare-name)) mapping-table)
                                                                        #f)))
                                                              #f)]
                                                     [(ss->rkt)
                                                      (lambda (s)
                                                        (regexp-replace #rx"[.]ss$" s ".rkt"))])
                                          (if a
                                              (let-values ([(a2) (assoc name (cadr a))])
                                                (if a2
                                                    (make-resolved-module-path (cdr a2))
                                                    ;; No relative mapping found (presumably a lib)
                                                    (orig name rel-to stx load?)))
                                              (let-values ([(lname)
                                                            ;; normalize `lib' to single string (same as lib-path->string):
                                                            (let-values ([(name)
                                                                          (let-values ([(name) 
                                                                                        ;; remove submod path; added back at end
                                                                                        (if (pair? name)
                                                                                            (if (eq? 'submod (car name))
                                                                                                (cadr name)
                                                                                                name)
                                                                                            name)])
                                                                            (if (symbol? name)
                                                                                (list 'lib (symbol->string name))
                                                                                name))])
                                                              (if (pair? name)
                                                                  (if (eq? 'lib (car name))
                                                                      (if (null? (cddr name))
                                                                          (if (regexp-match #rx"^[^/]*[.]" (cadr name))
                                                                              ;; mzlib
                                                                              (string-append "mzlib/" (ss->rkt (cadr name)))
                                                                              ;; new-style
                                                                              (if (regexp-match #rx"^[^/.]*$" (cadr name))
                                                                                  (string-append (cadr name) "/main.rkt")
                                                                                  (if (regexp-match #rx"^[^.]*$" (cadr name))
                                                                                      ;; need a suffix:
                                                                                      (string-append (cadr name) ".rkt")
                                                                                      (ss->rkt (cadr name)))))
                                                                          ;; old-style multi-string
                                                                          (string-append (apply string-append
                                                                                                (map (lambda (s)
                                                                                                       (string-append s "/"))
                                                                                                     (cddr name)))
                                                                                         (ss->rkt (cadr name))))
                                                                      (if (eq? 'planet (car name))
                                                                          (letrec-values ([(split)
                                                                                           (lambda (s rx suffix-after)
                                                                                             (let-values ([(m) (regexp-match-positions 
                                                                                                                rx
                                                                                                                s)])
                                                                                               (if m
                                                                                                   (cons (substring s 0 (caar m))
                                                                                                         (split (substring s (cdar m))
                                                                                                                rx 
                                                                                                                (- suffix-after 1)))
                                                                                                   (list
                                                                                                    (if (suffix-after . <= . 0)
                                                                                                        (if (regexp-match? #rx"[.]" s)
                                                                                                            s
                                                                                                            (string-append s ".rkt"))
                                                                                                        s)))))]
                                                                                          [(last-of)
                                                                                           (lambda (l)
                                                                                             (if (null? (cdr l))
                                                                                                 (car l)
                                                                                                 (last-of (cdr l))))]
                                                                                          [(not-last)
                                                                                           (lambda (l)
                                                                                             (if (null? (cdr l))
                                                                                                 null
                                                                                                 (cons (car l) (not-last (cdr l)))))])
                                                                            (if (null? (cddr name))
                                                                                ;; need to normalize:
                                                                                (let-values ([(s) (if (symbol? (cadr name))
                                                                                                      (symbol->string (cadr name))
                                                                                                      (cadr name))])
                                                                                  (let-values ([(parts) (split s #rx"/" 2)])
                                                                                    (let-values ([(vparts) (split (cadr parts) #rx":" +inf.0)])
                                                                                      (cons 'planet
                                                                                            (cons (if (null? (cddr parts))
                                                                                                      "main.rkt"
                                                                                                      (ss->rkt (last-of parts)))
                                                                                                  (cons
                                                                                                   (cons 
                                                                                                    (car parts) 
                                                                                                    (cons (string-append (car vparts) 
                                                                                                                         ".plt")
                                                                                                          (if (null? (cddr parts))
                                                                                                              null
                                                                                                              ;; FIXME: finish version parse:
                                                                                                              (cdddr parts))))
                                                                                                   (if (null? (cddr parts))
                                                                                                       null
                                                                                                       (not-last (cddr parts)))))))))
                                                                              ;; already in long form; move subcollects to end:
                                                                              (let-values ([(s) (cadr name)])
                                                                                (let-values ([(parts) (split s #rx"/" +inf.0)])
                                                                                  (if (= 1 (length parts))
                                                                                      (list* 'planet
                                                                                             (ss->rkt (cadr name))
                                                                                             (cddr name))
                                                                                      (list* 'planet
                                                                                             (ss->rkt (last-of parts))
                                                                                             (caddr name)
                                                                                             (append
                                                                                              (cdddr name)
                                                                                              (not-last parts))))))))
                                                                          #f))
                                                                  #f))]
                                                           [(planet-match?)
                                                            (lambda (a b)
                                                              (if (equal? (cons (car a) (cddr a))
                                                                          (cons (car b) (cddr b)))
                                                                  (let-values ([(a) (cadr a)]
                                                                               [(b) (cadr b)])
                                                                    (if (equal? (car a) (car b))
                                                                        (if (equal? (cadr a) (cadr b))
                                                                            ;; Everything matches up to the version...
                                                                            ;; FIXME: check version. (Since the version isn't checked,
                                                                            ;; this currently works only when a single version of the
                                                                            ;; package is used in the executable.)
                                                                            #t
                                                                            #f)
                                                                        #f))
                                                                  #f))]
                                                           [(restore-submod) (lambda (lname)
                                                                               (if (pair? name)
                                                                                   (if (eq? (car name) 'submod)
                                                                                       (list* 'submod lname (cddr name))
                                                                                       lname)
                                                                                   lname))])
                                                ;; A library mapping that we have? 
                                                (let-values ([(a3) (if lname
                                                                       (if (string? lname)
                                                                           ;; lib
                                                                           (assoc (restore-submod lname) library-table)
                                                                           ;; planet
                                                                           (ormap (lambda (e)
                                                                                    (let-values ([(e)
                                                                                                  ;; handle submodule matching first:
                                                                                                  (if (pair? name)
                                                                                                      (if (eq? (car name) 'submod)
                                                                                                          (if (pair? (car e))
                                                                                                              (if (eq? (caar e) 'submod)
                                                                                                                  (if (equal? (cddar e) (cddr name))
                                                                                                                      (cons (cadar e) (cdr e))
                                                                                                                      #f)
                                                                                                                  #f)
                                                                                                              #f)
                                                                                                          e)
                                                                                                      e)])
                                                                                      (if e
                                                                                          (if (string? (car e))
                                                                                              #f
                                                                                              (if (planet-match? (cdar e) (cdr lname))
                                                                                                  e
                                                                                                  #f))
                                                                                          #f)))
                                                                                  library-table))
                                                                       #f)])
                                                  (if a3
                                                      ;; Have it:
                                                      (make-resolved-module-path (cdr a3))
                                                      (if (if for-submod?
                                                              (if (pair? name)
                                                                  (if (eq? (car name) 'quote)
                                                                      (assq (cadr name) mapping-table)
                                                                      #f)
                                                                  #f)
                                                              #f)
                                                          ;; Report that we have mappings relative to `name`:
                                                          (make-resolved-module-path (cadr name))
                                                          ;; Let default handler try:
                                                          (orig name rel-to stx load?))))))))))))]
                         [(embedded-resolver)
                          (case-lambda 
                           [(name from-namespace)
                            ;; A notification
                            (if from-namespace
                              ;; If the source namespace has a mapping for `name',
                              ;; then copy it to the current namespace.
                              (let-values ([(name) (if name (resolved-module-path-name name) #f)])
                                (let-values ([(src-vec) (hash-ref regs (namespace-module-registry from-namespace) #f)])
                                  (let-values ([(a) (if src-vec
                                                        (assq name (vector-ref src-vec 0))
                                                        #f)])
                                    (if a
                                        (let-values ([(vec) (hash-ref regs (namespace-module-registry (current-namespace))
                                                                      (lambda ()
                                                                        (let-values ([(vec) (vector null null)])
                                                                          (hash-set! regs (namespace-module-registry (current-namespace)) vec)
                                                                          vec)))])
                                          ;; add mapping:
                                          (vector-set! vec 0 (cons a (vector-ref vec 0)))
                                          ;; add library mappings:
                                          (vector-set! vec 1 (append 
                                                              (letrec-values ([(loop)
                                                                               (lambda (l)
                                                                                 (if (null? l)
                                                                                     null
                                                                                     (if (eq? (cdar l) name)
                                                                                         (cons (car l) (loop (cdr l)))
                                                                                         (loop (cdr l)))))])
                                                                (loop library-table))
                                                              (vector-ref vec 1))))
                                        (void)))))
                              (void))
                            (orig name from-namespace)]
                           [(name rel-to stx load?)
                            (lookup name rel-to stx load? #f
                                    (lambda (name rel-to stx load?)
                                      ;; For a submodule, if we have a mapping for the base name,
                                      ;; then don't try the original handler.
                                      (let-values ([(base)
                                                    (if (pair? name)
                                                        (if (eq? (car name) 'submod)
                                                            ;; Pass #t for `for-submod?`, which causes a
                                                            ;; resolved module name to be returned for a quoted
                                                            ;; module name if we have any relative mappings for it:
                                                            (lookup (cadr name) rel-to stx load? #t (lambda (n r s l?) #f))
                                                            #f)
                                                        #f)])
                                        (if base
                                            ;; don't chain to `orig'; try `lookup` again with `(submod "." ...)`,
                                            ;; and if that still fails, just construct a submodule path:
                                            (lookup (cons 'submod (cons "." (cddr name))) base stx load? #f
                                                    (lambda (name rel-to stx load?)
                                                      (make-resolved-module-path
                                                       (cons (resolved-module-path-name base) (cddr name)))))
                                            ;; chain to `orig':
                                            (orig name rel-to stx load?)))))])])
           (current-module-name-resolver embedded-resolver))))))

(define (ss<->rkt path mk-full)
  (cond
   [(regexp-match? #rx#"[.]ss$" path)
    (ss<->rkt (path-replace-suffix path #".rkt") mk-full)]
   [(regexp-match? #rx#"[.]rkt$" path)
    (define full-path (mk-full path))
    (if (file-exists? full-path)
        full-path
        (let ([p2 (mk-full (path-replace-suffix path #".ss"))])
          (if (file-exists? p2)
              p2
              full-path)))]
   [else (mk-full path)]))

;; Write a module bundle that can be loaded with 'load' (do not embed it
;; into an executable). The bundle is written to the current output port.
(define (do-write-module-bundle outp verbose? modules 
                                early-literal-expressions config? literal-files literal-expressions 
                                collects-dest
                                on-extension program-name compiler expand-namespace 
                                src-filter get-extra-imports on-decls-done)
  (let* ([program-name-bytes (if program-name
                                 (path->bytes program-name)
                                 #"?")]
         [module-paths (map cadr modules)]
         [use-submoduless (map (lambda (m) (if (pair? (cddr m)) (caddr m) '())) modules)]
         [resolve-one-path (lambda (mp)
                             (let ([f (resolve-module-path mp #f)])
                               (unless f
                                 (error 'write-module-bundle "bad module path: ~e" mp))
                               (normalize f)))]
         [files (map resolve-one-path module-paths)]
         [collapse-one (lambda (mp)
                         (collapse-module-path mp (build-path (current-directory) "dummy.rkt")))]
         [collapsed-mps (map collapse-one module-paths)]
         [prefix-mapping (map (lambda (f m)
                                (cons f (let ([p (car m)])
                                          (cond
                                            [(symbol? p) (symbol->string p)]
                                            [(eq? p #t) (generate-prefix)]
                                            [(not p) ""]
                                            [else (error
                                                   'write-module-bundle
                                                   "bad prefix: ~e"
                                                   p)]))))
                              files modules)]
         ;; Each element is created with `make-mod'.
         ;; As we descend the module tree, we append to the front after
         ;; loading imports, so the list in the right order.
         [codes (box null)]
         [get-code-at (lambda (f mp submods)
                        (get-code f mp #f submods codes prefix-mapping verbose? collects-dest
                                  on-extension compiler expand-namespace
                                  src-filter get-extra-imports
                                  (make-hash)))]
         [__
          ;; Load all code:
          (for-each get-code-at files collapsed-mps use-submoduless)]
         [config-infos (if config?
                           (let ([a (assoc (car files) (unbox codes))])
                             (let ([info (module-compiled-language-info (mod-code a))])
                               (and info
                                    (let ([get-info ((dynamic-require (vector-ref info 0) (vector-ref info 1))
                                                     (vector-ref info 2))])
                                      (get-info 'configure-runtime null)))))
                           null)])
    ;; Add module for runtime configuration:
    (when config-infos
      (for ([config-info (in-list config-infos)])
        (let ([mp (vector-ref config-info 0)])
          (get-code-at (resolve-one-path mp)
                       (collapse-one mp)
                       null))))
    ;; Drop elements of `codes' that just record copied libs:
    (set-box! codes (filter mod-code (unbox codes)))
    ;; Bind `module' to get started:
    (write (compile-using-kernel '(namespace-require '(only '#%kernel module))) outp)
    ;; Install a module name resolver that redirects
    ;; to the embedded modules
    (write (make-module-name-resolver (filter mod-code (unbox codes))) outp)
    (write (compile-using-kernel '(namespace-require ''#%resolver)) outp)
    ;; Write the extension table and copy module code:
    (let* ([l (reverse (unbox codes))]
           [extensions (filter (lambda (m) (extension? (mod-code m))) l)]
           [runtimes (filter (lambda (m) (pair? (mod-runtime-paths m))) l)]
           [table-mod
            (if (null? runtimes)
                #f
                (let* ([table-sym (module-path-index-resolve 
                                   (module-path-index-join '(lib "runtime-path-table.rkt" "racket" "private")
                                                           #f))]
                       [table-path (resolved-module-path-name table-sym)])
                  (assoc (normalize table-path) l)))])
      (unless (null? extensions)
        ;; The extension table:`
        (write 
         `(module #%extension-table '#%kernel
            (#%require '#%utils)
            (let-values ([(eXtEnSiOn-modules) ;; this name is magic for the exe->distribution process
                          (quote ,(map (lambda (m)
                                         (let ([p (extension-path (mod-code m))])
                                           (when verbose?
                                             (eprintf "Recording extension at ~s\n" p))
                                           (list (path->bytes p)
                                                 (mod-full-name m)
                                                 ;; The program name isn't used. It just helps ensures that
                                                 ;; there's plenty of room in the executable for patching
                                                 ;; the path later when making a distribution.
                                                 program-name-bytes)))
                                       extensions))])
              (for-each (lambda (pr)
                          (current-module-declare-name (make-resolved-module-path (cadr pr)))
                          (let-values ([(p) (bytes->path (car pr))])
                            (load-extension (if (relative-path? p)
                                                (let-values ([(d) (current-directory)])
                                                  (current-directory (find-system-path 'orig-dir))
                                                  (begin0
                                                   (let-values ([(p2) (find-executable-path (find-system-path 'exec-file) p #t)])
                                                     (if p2
                                                         p2
                                                         (path->complete-path p (current-directory))))
                                                   (current-directory d)))
                                                p))))
                        eXtEnSiOn-modules)))
         outp)
        (write (compile-using-kernel '(namespace-require ''#%extension-table)) outp))
      ;; Runtime-path table:
      (unless (null? runtimes)
        (unless table-mod
          (error 'create-embedding-executable "cannot find module for runtime-path table"))
        (write (compile-using-kernel
                `(current-module-declare-name (make-resolved-module-path 
                                               ',(mod-full-name table-mod))))
               outp)
        (write `(module runtime-path-table '#%kernel
                  (#%provide table)
                  (define-values (table)
                    (make-immutable-hash
                     (let-values ([(rUnTiMe-paths) ; this is a magic name for exe->distribution process
                                   ',(apply append
                                            (map (lambda (nc)
                                                   (map (lambda (p sym)
                                                          (list
                                                           (cons (mod-full-name nc)
                                                                 (if (path? p)
                                                                     (path->bytes p)
                                                                     (if (and (pair? p)
                                                                              (eq? 'module (car p)))
                                                                         (list 'module (cadr p))
                                                                         p)))
                                                           (let ([p (cond
                                                                     [(bytes? p) (bytes->path p)]
                                                                     [(so-spec? p) (so-find p)]
                                                                     [(and (list? p)
                                                                           (eq? 'lib (car p)))
                                                                      (let ([p (if (null? (cddr p))
                                                                                   (if (regexp-match #rx"^[^/]*[.]" (cadr p))
                                                                                       p
                                                                                       (let ([s (regexp-split #rx"/" (cadr p))])
                                                                                         (if (null? (cdr s))
                                                                                             `(lib "main.rkt" ,(cadr p))
                                                                                             (let ([s (reverse s)])
                                                                                               `(lib ,(car s) ,@(reverse (cdr s)))))))
                                                                                   p)])
                                                                        (ss<->rkt
                                                                         (cadr p)
                                                                         (lambda (file)
                                                                           (apply collection-file-path
                                                                                  file
                                                                                  (if (null? (cddr p))
                                                                                      (list "mzlib")
                                                                                      (cddr p))
                                                                                  #:check-compiled? #f))))]
                                                                     [(and (list? p)
                                                                           (eq? 'module (car p)))
                                                                      sym]
                                                                     [else p])])
                                                             (and p
                                                                  (if (symbol? p)
                                                                      p
                                                                      (path->bytes
                                                                       (simplify-path
                                                                        (if (absolute-path? p)
                                                                            p
                                                                            (build-path (path-only (mod-file nc)) p)))))))
                                                           ;; As for the extension table, a placeholder to save 
                                                           ;; room likely needed by the distribution-mangler.
                                                           ;; The extra "."s are meant to cover the relative
                                                           ;; path (even in Windows format) to runtime files,
                                                           ;; and the program name is also part of that path.
                                                           (bytes-append (make-bytes 32 (char->integer #\.)) program-name-bytes)))
                                                        (mod-runtime-paths nc)
                                                        (mod-runtime-module-syms nc)))
                                                 runtimes))])
                       rUnTiMe-paths))))
               outp))
      ;; Copy module code:
      (for-each
       (lambda (nc)
         (unless (or (extension? (mod-code nc))
                     (eq? nc table-mod))
           (when verbose?
             (eprintf "Writing module from ~s\n" (mod-file nc)))
           (write (compile-using-kernel
                   `(current-module-declare-name 
                     (make-resolved-module-path
                      ',(mod-full-name nc))))
                  outp)
           (if (mod-use-source? nc)
               (call-with-input-file* (mod-actual-file nc)
                 (lambda (inp)
                   (define bstr (port->bytes inp))
                   ;; The indirection through `compiler/private/read-bstr` ensures
                   ;; that the source module is delimited by an EOF:
                   (fprintf outp "#reader compiler/private/read-bstr ~s" bstr)))
               (write (mod-code nc) outp))))
       l))
    (write (compile-using-kernel '(current-module-declare-name #f)) outp)
    ;; Remove `module' binding before we start running user code:
    (write (compile-using-kernel '(namespace-set-variable-value! 'module #f #t)) outp)
    (write (compile-using-kernel '(namespace-undefine-variable! 'module)) outp)
    (on-decls-done outp)
    (newline outp)
    (for-each (lambda (v) (write v outp)) early-literal-expressions)
    (when config-infos
      (for ([config-info (in-list config-infos)])
        (let ([a (assoc (resolve-one-path (vector-ref config-info 0)) (unbox codes))])            
          (write (compile-using-kernel `((dynamic-require '',(mod-full-name a)
                                                          ',(vector-ref config-info 1))
                                         ',(vector-ref config-info 2)))
                 outp))))
    (for-each (lambda (f)
                (when verbose?
                  (eprintf "Copying from ~s\n" f))
                (call-with-input-file* f
                  (lambda (i)
                    (copy-port i outp))))
              literal-files)
    (for-each (lambda (v) (write v outp)) literal-expressions)))

(define (write-module-bundle #:verbose? [verbose? #f]
                             #:modules [modules null]
                             #:configure-via-first-module? [config? #f]
                             #:literal-files [literal-files null]
                             #:early-literal-expressions [early-literal-expressions null]
                             #:literal-expressions [literal-expressions null]
                             #:on-extension [on-extension #f]
                             #:expand-namespace [expand-namespace (current-namespace)]
                             #:compiler [compiler (lambda (expr)
                                                    (parameterize ([current-namespace expand-namespace])
                                                      (compile expr)))]
                             #:src-filter [src-filter (lambda (filename) #f)]
                             #:get-extra-imports [get-extra-imports (lambda (filename code) null)])
  (do-write-module-bundle (current-output-port) verbose? modules 
                          early-literal-expressions config? literal-files literal-expressions
                          #f ; collects-dest
                          on-extension
                          #f ; program-name 
                          compiler expand-namespace 
                          src-filter get-extra-imports
                          void))


;; The old interface:
(define make-embedding-executable
  (lambda (dest mred? verbose? 
                modules 
                literal-files literal-expression
                cmdline
                [aux null]
                [launcher? #f]
                [variant (cross-system-type 'gc)]
                [collects-path #f])
    (create-embedding-executable dest
                                 #:mred? mred?
                                 #:verbose? verbose?
                                 #:modules modules
                                 #:literal-files literal-files
                                 #:literal-expression literal-expression
                                 #:cmdline cmdline
                                 #:aux aux
                                 #:launcher? launcher?
                                 #:variant variant
                                 #:collects-path collects-path)))

;; Use `write-module-bundle', but figure out how to put it into an executable
(define (create-embedding-executable dest
                                     #:mred? [really-mred? #f]
                                     #:gracket? [gracket? #f]
                                     #:verbose? [verbose? #f]
                                     #:modules [modules null]
                                     #:configure-via-first-module? [config? #f]
                                     #:literal-files [literal-files null]
                                     #:early-literal-expressions [early-literal-expressions null]
                                     #:literal-expression [literal-expression #f]
                                     #:literal-expressions [literal-expressions
                                                            (if literal-expression
                                                                (list literal-expression)
                                                                null)]
                                     #:cmdline [cmdline null]
                                     #:aux [aux null]
                                     #:launcher? [launcher? #f]
                                     #:variant [variant (cross-system-type 'gc)]
                                     #:collects-path [collects-path #f]
                                     #:collects-dest [collects-dest #f]
                                     #:on-extension [on-extension #f]
                                     #:expand-namespace [expand-namespace (current-namespace)]
                                     #:compiler [compiler (lambda (expr)
                                                            (parameterize ([current-namespace expand-namespace])
                                                              (compile expr)))]
                                     #:src-filter [src-filter (lambda (filename) #f)]
                                     #:get-extra-imports [get-extra-imports (lambda (filename code) null)])
  (define mred? (or really-mred? gracket?))
  (define keep-exe? (and launcher?
                         (let ([m (assq 'forget-exe? aux)])
                           (or (not m)
                               (not (cdr m))))))
  (define unix-starter? (and (eq? (cross-system-type) 'unix)
                             (let ([m (assq 'original-exe? aux)])
                               (or (not m)
                                   (not (cdr m))))))
  (define long-cmdline? (or (eq? (cross-system-type) 'windows)
                            (eq? (cross-system-type) 'macosx)
                            unix-starter?))
  (define relative? (let ([m (assq 'relative? aux)])
                      (and m (cdr m))))
  (define collects-path-bytes (collects-path->bytes 
                               ((if (and mred?
                                         (eq? 'macosx (cross-system-type)))
                                    mac-mred-collects-path-adjust
                                    values)
                                collects-path)))
  (define word-size (if (fixnum? (expt 2 32)) 8 4))
  (unless (or long-cmdline?
              ((apply +
                      (map (lambda (s)
                             (+ word-size (bytes-length (string->bytes/utf-8 s))))
                           cmdline)) . < . 80))
    (error 'create-embedding-executable "command line too long: ~e" cmdline))
  (check-collects-path 'create-embedding-executable collects-path collects-path-bytes)
  (let ([exe (find-exe #:cross? #t mred? variant)])
    (when verbose?
      (eprintf "Copying to ~s\n" dest))
    (let-values ([(dest-exe orig-exe osx?)
                  (cond
                    [(and mred? (eq? 'macosx (cross-system-type)))
                     (values (prepare-macosx-mred exe dest aux variant) 
                             (mac-dest->executable (build-path (find-lib-dir) "Starter.app")
                                                   #t)
                             #t)]
                    [unix-starter?
                     (let ([starter (build-path (find-lib-dir) 
                                                (if (force exe-suffix?)
                                                    "starter.exe"
                                                    "starter"))])
                       (when (or (file-exists? dest)
                                 (directory-exists? dest)
                                 (link-exists? dest))
                         (delete-file dest))
                       (copy-file starter dest)
                       (values dest starter #f))]
                    [else
                     (when (or (file-exists? dest)
                               (directory-exists? dest)
                               (link-exists? dest))
                       ;; Delete-file isn't enough if the target
                       ;;  is supposed to be a directory. But
                       ;;  currently, that happens only for GRacket 
                       ;;  on Mac OS X, which is handled above.
                       (delete-file dest))
                     (copy-file exe dest)
                     (values dest exe #f)])])
      (with-handlers ([void (lambda (x)
                              (if osx?
                                  (when (directory-exists? dest)
                                    (delete-directory/files dest))
                                  (when (file-exists? dest)
                                    (delete-file dest)))
                              (raise x))])
        (define old-perms (ensure-writable dest-exe))
        (when (and (eq? 'macosx (cross-system-type))
                   (not unix-starter?))
          (let ([m (or (assq 'framework-root aux)
                       (and relative? '(framework-root . #f)))])
            (if m
                (if (cdr m)
                    (update-framework-path (cdr m) 
                                           (mac-dest->executable dest mred?)
                                           mred?)
                    (when mred?
                      ;; adjust relative path (since GRacket is normally off by one):
                      (define rel (find-relative-path (find-gui-bin-dir)
                                                      (find-lib-dir)))
                      (update-framework-path (format "@executable_path/../../../~a"
                                                     (path->directory-path rel))
                                             (mac-dest->executable dest mred?)
                                             #t)))
                ;; Check whether we need an absolute path to frameworks:
                (let ([dest (mac-dest->executable dest mred?)])
                  (when (regexp-match #rx"^@executable_path" 
                                      (get-current-framework-path dest "Racket"))
                    (update-framework-path (string-append
                                            (path->string (find-lib-dir))
                                            "/")
                                           dest
                                           mred?))))))
        (when (eq? 'windows (cross-system-type))
          (let ([m (or (assq 'dll-dir aux)
                       (and relative? '(dll-dir . #f)))])
            (if m
                (if (cdr m)
                    (update-dll-dir dest (cdr m))
                    ;; adjust relative path (since GRacket is off by one):
                    (update-dll-dir dest "lib"))
                ;; Check whether we need an absolute path to DLLs:
                (let ([dir (get-current-dll-dir dest)])
                  (when (relative-path? dir)
                    (let-values ([(orig-dir name dir?) (split-path 
                                                        (path->complete-path orig-exe))])
                      (update-dll-dir dest (build-path orig-dir dir))))))))
        (define (adjust-config-dir)
          (let ([m (or (assq 'config-dir aux)
                       (and relative? '(config-dir . #f)))]
                [dest->executable (lambda (dest)
                                    (if osx?
                                        (mac-dest->executable dest mred?)
                                        dest))])
            (if m
                (if (cdr m)
                    (update-config-dir (dest->executable dest) (cdr m))
                    (when mred?
                      (cond
                       [osx?
                        ;; adjust relative path (since GRacket is off by one):
                        (update-config-dir (mac-dest->executable dest mred?)
                                           "../../../etc/")]
                       [(eq? 'windows (cross-system-type))
                        (unless keep-exe?
                          ;; adjust relative path (since GRacket is off by one):
                          (update-config-dir dest "etc/"))])))
                ;; Check whether we need an absolute path to config:
                (let ([dir (get-current-config-dir (dest->executable dest))])
                  (when (relative-path? dir)
                    (let-values ([(orig-dir name dir?) (split-path 
                                                        (path->complete-path orig-exe))])
                      (update-config-dir (dest->executable dest)
                                         (build-path orig-dir dir))))))))
        (unless unix-starter? ; need to delay adjustment for Unix starter; see below
          (adjust-config-dir))
        (let ([write-module
               (lambda (s)
                 (define pos #f)
                 (do-write-module-bundle s
                                         verbose? modules 
                                         early-literal-expressions config? 
                                         literal-files literal-expressions collects-dest
                                         on-extension
                                         (file-name-from-path dest)
                                         compiler
                                         expand-namespace
                                         src-filter
                                         get-extra-imports
                                         (lambda (outp) (set! pos (file-position outp))))
                 pos)]
		  [make-full-cmdline
		   (lambda (start decl-end end)
		     (let ([start-s (number->string start)]
			   [decl-end-s (number->string decl-end)]
                       [end-s (number->string end)])
		       (append (if launcher?
				   (if (and (eq? 'windows (cross-system-type))
					    keep-exe?)
				       ;; argv[0] replacement:
				       (list (path->string 
					      (if relative?
						  (relativize exe dest-exe values)
						  exe)))
				       ;; No argv[0]:
				       null)
				   (list "-k" start-s decl-end-s end-s))
			       cmdline)))]
		  [make-starter-cmdline
		   (lambda (full-cmdline)
		     (apply bytes-append
			    (map (lambda (s)
				   (bytes-append 
				    (cond
				     [(path? s) (path->bytes s)]
				     [else (string->bytes/locale s)])
				    #"\0"))
				 (append
				  (list (if relative?
					    (relativize exe dest-exe values)
					    exe)
					(let ([dir (find-dll-dir)])
					  (if dir
					      (if relative?
						  (relativize dir dest-exe values)
						  dir)
					      "")))
				  full-cmdline))))]
              [write-cmdline
               (lambda (full-cmdline out)
                 (for-each
                  (lambda (s)
                    (fprintf out "~a~a~c"
                             (integer->integer-bytes 
                              (add1 (bytes-length (string->bytes/utf-8 s)) )
                              4 #t #f)
                             s
                             #\000))
                  full-cmdline)
                 (display "\0\0\0\0" out))])
          (let-values ([(start decl-end end cmdline-end)
                        (cond
                         [(eq? (cross-system-type) 'windows)
                          ;; Add as a resource
                          (define o (open-output-bytes))
                          (define decl-len (write-module o))
                          (define init-len (bytes-length (get-output-bytes o)))
                          (write-cmdline (make-full-cmdline 0 decl-len init-len) o)
                          (define bstr (get-output-bytes o))
                          (define cmdline-len (- (bytes-length bstr) init-len))
                          (define-values (pe rsrcs) (call-with-input-file*
                                                     dest-exe
                                                     read-pe+resources))
                          (define new-rsrcs (resource-set rsrcs
                                                          ;; Racket's "user-defined" type for excutable 
                                                          ;; plus command line:
                                                          257
                                                          1
                                                          1033 ; U.S. English
                                                          bstr))
                          (update-resources dest-exe pe new-rsrcs)
                          (values 0 decl-len init-len (+ init-len cmdline-len))]
                         [(and (eq? (cross-system-type) 'macosx)
                               (not unix-starter?))
                          ;; For Mach-O, we know how to add a proper segment
                          (define s (open-output-bytes))
                          (define decl-len (write-module s))
                          (let* ([s (get-output-bytes s)]
                                 [cl (let ([o (open-output-bytes)])
                                       ;; position is relative to __PLTSCHEME:
                                       (write-cmdline (make-full-cmdline 0 decl-len (bytes-length s)) o)
                                       (get-output-bytes o))])
                            (let ([start (add-plt-segment 
                                          dest-exe 
                                          (bytes-append
                                           s
                                           cl))])
                              (let ([start 0]) ; i.e., relative to __PLTSCHEME
                                (values start
                                        (+ start decl-len)
                                        (+ start (bytes-length s))
                                        (+ start (bytes-length s) (bytes-length cl))))))]
                         [else
                          ;; Unix starter: Maybe ELF, in which case we 
                          ;; can add a proper section
                          (define-values (s e dl p)
                            (if unix-starter?
                                (add-racket-section 
                                 orig-exe 
                                 dest-exe
                                 (if launcher? #".rackcmdl" #".rackprog")
                                 (lambda (start)
                                   (let ([s (open-output-bytes)])
                                     (define decl-len (write-module s))
                                     (let ([p (file-position s)])
                                       (display (make-starter-cmdline
                                                 (make-full-cmdline start 
                                                                    (+ start decl-len)
                                                                    (+ start p)))
                                                s)
                                       (values (get-output-bytes s) decl-len p)))))
                                (values #f #f #f #f)))
                          (if (and s e)
                             ;; ELF succeeded:
                             (values s (+ s dl) (+ s p) e)
                             ;; Otherwise, just add to the end of the file:
                             (let ([start (file-size dest-exe)])
                               (define decl-end
                                 (call-with-output-file* dest-exe write-module 
                                                         #:exists 'append))
                               (values start decl-end (file-size dest-exe) #f)))])])
            (when unix-starter?
              (adjust-config-dir))
            (when verbose?
              (eprintf "Setting command line\n"))
            (let ()
              (let ([full-cmdline (make-full-cmdline start decl-end end)])
                (cond
                 [collects-path-bytes
                  (when verbose?
                    (eprintf "Setting collection path\n"))
                  (set-collects-path dest-exe collects-path-bytes)]
                 [mred?
                  (cond
                   [osx?
                    ;; default path in `gracket' is off by one:
                    (set-collects-path dest-exe #"../../../collects")]
                   [(eq? 'windows (cross-system-type))
                    (unless keep-exe?
                      ;; off by one in this case, too:
                      (set-collects-path dest-exe #"collects"))])])
                (cond
                  [(and use-starter-info? osx?)
                   (finish-osx-mred dest full-cmdline exe keep-exe? relative?)]
                  [unix-starter?
                   (let ([numpos (with-input-from-file dest-exe 
                                   (lambda () (find-cmdline 
                                               "configuration"
                                               #"cOnFiG:")))]
                         [typepos (and (or mred? (eq? variant '3m))
                                       (with-input-from-file dest-exe 
                                         (lambda () (find-cmdline 
                                                     "exeuctable type"
                                                     #"bINARy tYPe:"))))]
                         [cmdline (if cmdline-end
					  #f
					  (make-starter-cmdline full-cmdline))]
                         [out (open-output-file dest-exe #:exists 'update)])
                     (let ([old-cmdline-end cmdline-end]
			       [cmdline-end (or cmdline-end (+ end (bytes-length cmdline)))]
                           [write-num (lambda (n)
                                        (write-bytes (integer->integer-bytes n 4 #t #f) out))])
                       (dynamic-wind
                        void
                        (lambda ()
                          (when typepos
                            (when mred?
                              (file-position out (+ typepos 13))
                              (write-bytes #"r" out))
                            (when (eq? variant '3m)
                              (file-position out (+ typepos 15))
                              (write-bytes #"3" out))
                            (flush-output out))
                          (file-position out (+ numpos 7))
                          (write-bytes #"!" out)
                          (write-num start)
                          (write-num decl-end)
                          (write-num end)
                          (write-num cmdline-end)
                          (write-num (length full-cmdline))
                          (write-num (if mred? 1 0))
                          (flush-output out)
			      (unless old-cmdline-end
				(file-position out end)
				(write-bytes cmdline out)
				(flush-output out)))
                        (lambda ()
                          (close-output-port out)))))]
                  [else
                   (let ([cmdpos (with-input-from-file dest-exe 
                                   (lambda () (find-cmdline 
                                               "cmdline"
                                               #"\\[Replace me for EXE hack")))]
                         [anotherpos (and mred?
                                          (eq? 'windows (cross-system-type))
                                          (let ([m (assq 'single-instance? aux)])
                                            (and m (not (cdr m))))
                                          (with-input-from-file dest-exe 
                                            (lambda () (find-cmdline 
                                                        "instance-check"
                                                        #"yes, please check for another"))))]
                         [out (open-output-file dest-exe #:exists 'update)]
                         [cmdline-done? cmdline-end])
                     (dynamic-wind
                      void
                      (lambda ()
                        (when anotherpos
                          (file-position out anotherpos)
                          (write-bytes #"no," out))
                        (if long-cmdline?
                            ;; write cmdline at end:
                            (unless cmdline-done?
                              (file-position out end))
                            (begin
                              ;; write (short) cmdline in the normal position:
                              (file-position out cmdpos)
                              (display "!" out)))
                        (unless cmdline-done?
                          (write-cmdline full-cmdline out))
                        (when long-cmdline?
                          ;; cmdline written at the end, in a resource, etc.;
                          ;; now put forwarding information at the normal cmdline pos
                          (let ([new-end (or cmdline-end
                                             (file-position out))])
                            (file-position out cmdpos)
                            (fprintf out "~a...~a~a"
                                     (if (and keep-exe? (eq? 'windows (cross-system-type))) "*" "?")
                                     (integer->integer-bytes end 4 #t #f)
                                     (integer->integer-bytes (- new-end end) 4 #t #f)))))
                      (lambda ()
                        (close-output-port out)))
                     (let ([m (and (eq? 'windows (cross-system-type))
                                   (assq 'ico aux))])
                       (when m
                         (replace-all-icos (read-icos (cdr m)) dest-exe)))
                     (let ([m (and (eq? 'windows (cross-system-type))
                                   (assq 'subsystem aux))])
                       (when m
                         (set-subsystem dest-exe (cdr m)))))]))))
          (done-writable dest-exe old-perms))))))

;; For Mac OS X GRacket, the actual executable is deep inside the
;;  nominal executable bundle
(define (mac-mred-collects-path-adjust p)
  (cond
    [(not p) #f]
    [(list? p) (map mac-mred-collects-path-adjust p)]
    [(relative-path? p) (build-path 'up 'up 'up p)]
    [else p]))

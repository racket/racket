(module dirs mzscheme
  (require (prefix config: (lib "config.ss" "config"))
           (lib "winutf16.ss" "compiler" "private")
           (lib "mach-o.ss" "compiler" "private"))

  (provide (rename config:absolute-installation? absolute-installation?))

  ;; path normalization is not really necessary by any existing code,
  ;; but there might be applications that rely on these paths, so it's
  ;; best to do some minor normalization.  This is similar to what
  ;; "main-collects.ss" does.
  (define (system-path* what)
    (simplify-path (expand-path (find-system-path what)) #f))

  ;; ----------------------------------------
  ;;  "collects"

  (define main-collects-dir
    (delay
      (let ([d (system-path* 'collects-dir)])
        (cond
         [(complete-path? d) d]
         [(absolute-path? d)
          ;; This happens only under Windows; add a drive
          ;;  specification to make the path complete
          (let ([exec (path->complete-path
                       (find-executable-path (system-path* 'exec-file))
                       (system-path* 'orig-dir))])
            (let-values ([(base name dir?) (split-path exec)])
              (path->complete-path d base)))]
         [else
          ;; Relative to executable...
          (parameterize ([current-directory (system-path* 'orig-dir)])
            (let ([p (or (find-executable-path (system-path* 'exec-file) d #t)
                         ;; If we get here, then we can't find the directory
                         #f)])
              (and p (simplify-path p))))]))))

  (provide find-collects-dir
           find-user-collects-dir
           get-collects-search-dirs)
  (define (find-collects-dir)
    (force main-collects-dir))
  (define user-collects-dir
    (delay (build-path (system-path* 'addon-dir) (version) "collects")))
  (define (find-user-collects-dir)
    (force user-collects-dir))
  (define (get-collects-search-dirs)
    (current-library-collection-paths))

  ;; ----------------------------------------
  ;; Helpers

  (define (single p) (if p (list p) null))
  (define (extra a l) (if (and a (not (member a l))) (cons a l) l))
  (define (combine-search l default)
    ;; Replace #f in list with default path:
    (if l
        (let loop ([l l])
          (cond
           [(null? l) null]
           [(not (car l)) (append default (loop (cdr l)))]
           [else (cons (car l) (loop (cdr l)))]))
        default))
  (define (cons-user u r)
    (if (use-user-specific-search-paths) (cons u r) r))

  (define-syntax define-finder
    (syntax-rules ()
      [(_ provide config:id id user-id config:search-id search-id default)
       (begin
         (define-finder provide config:id id user-id default)
         (provide search-id)
         (define (search-id)
           (combine-search (force config:search-id)
                           (cons-user (user-id) (single (id))))))]
      [(_ provide config:id id user-id config:search-id search-id
          extra-search-dir default)
       (begin
         (define-finder provide config:id id user-id default)
         (provide search-id)
         (define (search-id)
           (combine-search (force config:search-id)
                           (extra (extra-search-dir)
                                  (cons-user (user-id) (single (id)))))))]
      [(_ provide config:id id user-id default)
       (begin
         (provide id user-id)
         (define dir
           (delay
             (or (force config:id)
                 (let ([p (find-collects-dir)])
                   (and p (simplify-path (build-path p 'up default)))))))
         (define (id)
           (force dir))
         (define user-dir
           (delay (build-path (system-path* 'addon-dir) (version) default)))
         (define (user-id)
           (force user-dir)))]))

  (define-syntax no-provide (syntax-rules () [(_ . rest) (begin)]))

  ;; ----------------------------------------
  ;; "doc"

  (define delayed-#f (delay #f))

  (provide find-doc-dir
           find-user-doc-dir
           get-doc-search-dirs)
  (define-finder no-provide
    config:doc-dir
    find-doc-dir
    find-user-doc-dir
    delayed-#f
    get-new-doc-search-dirs
    "doc")
  ;; For now, include "doc" pseudo-collections in search path:
  (define (get-doc-search-dirs)
    (combine-search (force config:doc-search-dirs)
                    (append (get-new-doc-search-dirs)
                            (map (lambda (p) (build-path p "doc"))
                                 (current-library-collection-paths)))))

  ;; ----------------------------------------
  ;; "include"

  (define-finder provide
    config:include-dir
    find-include-dir
    find-user-include-dir
    config:include-search-dirs
    get-include-search-dirs
    "include")

  ;; ----------------------------------------
  ;; "lib"

  (define-finder provide
    config:lib-dir
    find-lib-dir
    find-user-lib-dir
    config:lib-search-dirs
    get-lib-search-dirs find-dll-dir
    "lib")

  ;; ----------------------------------------
  ;; Executables

  (define-finder provide
    config:bin-dir
    find-console-bin-dir
    find-user-console-bin-dir
    (case (system-type)
      [(windows) 'same]
      [(macosx unix) "bin"]))

  (define-finder provide
    config:bin-dir
    find-gui-bin-dir
    find-user-gui-bin-dir
    (case (system-type)
      [(windows macosx) 'same]
      [(unix) "bin"]))

  ;; ----------------------------------------
  ;; DLLs

  (provide find-dll-dir)
  (define dll-dir
    (delay
      (case (system-type)
        [(windows)
         ;; Extract "lib" location from binary:
         (let ([exe (parameterize ([current-directory (system-path* 'orig-dir)])
                      (find-executable-path (system-path* 'exec-file)))])
           (with-input-from-file exe
             (lambda ()
               (let ([m (regexp-match (byte-regexp
                                       (bytes-append
                                        (bytes->utf-16-bytes #"dLl dIRECTORy:")
                                        #"((?:..)*?)\0\0"))
                                      (current-input-port))])
                 (unless m
                   (error "cannot find \"dLl dIRECTORy\" tag in binary"))
                 (let-values ([(dir name dir?) (split-path exe)])
                   (if (regexp-match #rx#"^<" (cadr m))
                     ;; no DLL dir in binary
                     #f
                     ;; resolve relative directory:
                     (let ([p (bytes->path (utf-16-bytes->bytes (cadr m)))])
                       (path->complete-path p dir))))))))]
        [(macosx)
         (let* ([exe (parameterize ([current-directory (system-path* 'orig-dir)])
                       (let loop ([p (find-executable-path
                                      (system-path* 'exec-file))])
                         (if (link-exists? p)
                           (loop (let-values ([(r) (resolve-path p)]
                                              [(dir name dir?) (split-path p)])
                                   (if (and (path? dir)
                                            (relative-path? r))
                                     (build-path dir r)
                                     r)))
                           p)))]
                [rel (get/set-dylib-path exe "PLT_M[rz]" #f)])
           (cond
             [(not rel) #f] ; no framework reference found!?
             [(regexp-match
               #rx#"^(@executable_path/)?(.*?)PLT_M(?:rEd|zScheme).framework"
               rel)
              => (lambda (m)
                   (let ([b (caddr m)])
                     (if (and (not (cadr m)) (bytes=? b #""))
                       #f ; no path in exe
                       (simplify-path
                        (path->complete-path
                         (if (not (cadr m))
                           (bytes->path b)
                           (let-values ([(dir name dir?) (split-path exe)])
                             (if (bytes=? b #"")
                               dir
                               (build-path dir (bytes->path b)))))
                         (system-path* 'orig-dir))))))]
             [else (find-lib-dir)]))]
        [else
         (if (eq? 'shared (system-type 'link))
           (or (force config:dll-dir) (find-lib-dir))
           #f)])))
  (define (find-dll-dir)
    (force dll-dir)))

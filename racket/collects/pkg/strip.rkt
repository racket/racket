#lang racket/base
(require compiler/cm
         setup/getinfo
         setup/dirs
         syntax/modread
         racket/match
         racket/file
         racket/path
         racket/list
         racket/set)

(provide generate-stripped-directory
         fixup-local-redirect-reference
         strip-binary-compile-info)

(define strip-binary-compile-info (make-parameter #t))

(define (generate-stripped-directory mode dir dest-dir)
  (define drop-keep-ns (make-base-namespace))
  (define (add-drop+keeps dir base drops keeps)
    (define get-info (get-info/full dir #:namespace drop-keep-ns))
    (define (get-paths tag)
      (define l (if get-info
                    (get-info tag (lambda () null))
                    null))
      (unless (and (list? l) (andmap (lambda (p)
                                       (and (path-string? p)
                                            (relative-path? p)))
                                     l))
        (error 'strip "bad ~a value from \"info.rkt\": ~e" tag l))
      l)
    (define (intersect l1 l2)
      (set->list (set-intersect (list->set l1) (list->set l2))))
    (define (union l1 l2)
      (set->list (set-union (list->set l1) (list->set l2))))
    (define more-drops
      (case mode
        [(source) (get-paths 'source-omit-files)]
        [(binary) (get-paths 'binary-omit-files)]
        [(built) 
         (intersect (get-paths 'source-omit-files)
                    (get-paths 'binary-omit-files))]))
    (define more-keeps
      (case mode
        [(source) (get-paths 'source-keep-files)]
        [(binary) (get-paths 'binary-keep-files)]
        [(built) 
         (union (get-paths 'source-keep-files)
                (get-paths 'binary-keep-files))]))
    (define (add ht l)
      (for/fold ([ht ht]) ([i (in-list l)])
        (hash-set ht 
                  (if (eq? base 'same)
                      (if (path? i) i (string->path i))
                      (build-path base i))
                  #t)))
    (values (add drops more-drops)
            (add keeps more-keeps)))
  
  (define (drop-by-default? path get-p)
    (define bstr (path->bytes path))
    (define (immediate-doc/css-or-doc/js?)
      ;; Drop ".css" and ".js" immediately in a "doc" directory:
      (and (regexp-match? #rx#"(?:[.]css|[.]js)$" bstr)
           (let-values ([(base name dir?) (split-path (get-p))])
             (and (path? base)
                  (let-values ([(base name dir?) (split-path base)])
                    (and (path? name)
                         (equal? #"doc" (path-element->bytes name))))))))
    (or (regexp-match? #rx#"^(?:[.]git.*|[.]svn|.*~|#.*#)$"
                       bstr)
        ;; can appear as a marker in rendered documentation:
        (equal? #"synced.rktd" bstr)
        (case mode
          [(source)
           (regexp-match? #rx#"^(?:compiled|doc)$" bstr)]
          [(binary)
           (or (regexp-match? #rx#"^(?:tests|scribblings|.*(?:[.]scrbl|[.]dep|_scrbl[.]zo))$"
                              bstr)
               (and (regexp-match? #rx"[.](?:ss|rkt)$" bstr)
                    (not (equal? #"info.rkt" bstr))
                    (file-exists? (let-values ([(base name dir?) (split-path (get-p))])
                                    (build-path base "compiled" (path-add-suffix name #".zo")))))
               (immediate-doc/css-or-doc/js?)
               ;; drop these, because they're recreated on fixup:
               (equal? #"info_rkt.zo" bstr)
               (equal? #"info_rkt.dep" bstr))]
          [(built)
           (immediate-doc/css-or-doc/js?)])))

  (define (keep-override-by-default? path dir)
    (case mode
      [(binary)
       (define bstr (path->bytes path))
       (define (path-elements)
         (if (eq? dir 'base)
             null
             (map path-element->bytes (explode-path dir))))
       (cond
        [(or (equal? bstr #"doc")
             (equal? bstr #"info.rkt"))
         ;; Keep "doc" and "info.rkt" (that might be for
         ;; documentation) when under "scribblings" and not under
         ;; "tests":
         (define l (path-elements))
         (and (member #"scribblings" l)
              (not (member #"tests" l)))]
        [else #f])]
      [else #f]))
  
  (define (fixup new-p path src-base level)
    (unless (eq? mode 'source)
      (define bstr (path->bytes path))
      (cond
       [(regexp-match? #rx"[.]html$" bstr)
        (fixup-html new-p)]
       [(and (eq? mode 'binary)
             (equal? #"info.rkt" bstr))
        (fixup-info new-p src-base level)]
       [(and (eq? mode 'binary)
             (regexp-match? #rx"[.]zo$" bstr))
        (fixup-zo new-p)])))
  
  (define (explore base   ; containing directory relative to `dir`, 'base at start
                   paths  ; paths in `base'
                   drops  ; hash table of paths (relative to start) to drop
                   keeps  ; hash table of paths (relative to start) to keep
                   drop-all-by-default? ; in dropped directory?
                   level) ; 'package, 'collection, or 'subcollection
    (define next-level (case level
                         [(package) 'collection]
                         [else 'subcollection]))
    (for ([path (in-list paths)])
      (define p (if (eq? base 'same)
                    path
                    (build-path base path)))
      (define keep? (and (not (hash-ref drops p #f))
                         (or (hash-ref keeps p #f)
                             (and drop-all-by-default?
                                  (keep-override-by-default?
                                   path
                                   base))
                             (not (or drop-all-by-default?
                                      (drop-by-default?
                                       path
                                       (lambda () (build-path dir p))))))))
      (define old-p (build-path dir p))
      (define new-p (build-path dest-dir p))
      (cond
       [(and keep? (file-exists? old-p))
        (when drop-all-by-default?
          (make-directory* (path-only new-p)))
        (copy-file old-p new-p)
        (file-or-directory-modify-seconds
         new-p
         (file-or-directory-modify-seconds old-p))
        (fixup new-p path base level)]
       [(directory-exists? old-p)
        (define-values (new-drops new-keeps)
          (add-drop+keeps old-p p drops keeps))
        (when keep?
          (if drop-all-by-default?
              (make-directory* new-p)
              (make-directory new-p)))
        (explore p
                 (directory-list old-p)
                 new-drops
                 new-keeps
                 (not keep?)
                 next-level)]
       [keep? (error 'strip "file or directory disappeared?")]
       [else (void)])))

  (define-values (drops keeps)
    (add-drop+keeps dir 'same #hash() #hash()))

  (define level
    (let ([i (get-info/full dir #:namespace drop-keep-ns)])
      (cond
       [(or (not i)
            (not (eq? 'multi (i 'collection (lambda () #t)))))
        'collection] ; single-collection package
       [else 'package])))
  
  (explore 'same (directory-list dir) drops keeps #f level)
  (case mode
    [(binary built) (unmove-files dir dest-dir drop-keep-ns)]
    [else (void)])
  (case mode
    [(binary) (assume-virtual dest-dir (eq? level 'collection))]
    [else (void)]))

(define (fixup-html new-p)
  ;; strip full path to "local-[user-]redirect.js"
  (fixup-local-redirect-reference new-p ".."))

(define (fixup-zo new-p)
  ;; strip `test', `srcdoc', and `doc' submodules:
  (define mod
    (call-with-input-file* 
     new-p
     (lambda (in)
       (parameterize ([read-accept-compiled #t])
         (read in)))))
  (define (filter-mods l)
    (filter (lambda (m)
              (not (memq (last (module-compiled-name m))
                         '(test doc srcdoc))))
            l))
  (define new-mod
    (let loop ([mod mod])
      (define mod-subs (module-compiled-submodules mod #f))
      (define mod*-subs (module-compiled-submodules mod #t))
      (define new-mod-subs (map loop (filter-mods mod-subs)))
      (define new-mod*-subs (map loop (filter-mods mod*-subs)))
      (if (and (equal? mod-subs new-mod-subs)
               (equal? mod*-subs new-mod*-subs))
          mod
          (module-compiled-submodules
           (module-compiled-submodules mod
                                       #f
                                       new-mod-subs)
           #t
           new-mod*-subs))))
  (unless (eq? mod new-mod)
    (call-with-output-file*
     new-p
     #:exists 'truncate/replace
     (lambda (out) (write new-mod out)))))

(define (fixup-local-redirect-reference p js-path #:user [user-js-path js-path])
  ;; Relying on this HTML pattern (as generated by Scribble's HTML
  ;; renderer) is a little fragile. Any better idea?
  (define rx #rx"<script type=\"text/javascript\" src=\"([^\"]*)/local-(?:user-)?redirect[.]js\">")
  (define ms (call-with-input-file*
              p
              (lambda (i)
                ;; search twice to hit normal and user:
                (list (regexp-match-positions rx i)
                      (regexp-match-positions rx i)))))
  (define m (car ms))
  (define m2 (cadr ms))
  (when m
    (define start (caadr m))
    (define end (cdadr m))
    (define bstr (file->bytes p))
    (define new-bstr
      (bytes-append (subbytes bstr 0 start)
                    (string->bytes/utf-8 js-path)
                    (let ([s (subbytes bstr end)])
                      (cond
                       [m2
                        (define delta (- (cdar m) end))
                        (define start2 (caadr m2))
                        (define end2 (cdadr m2))
                        (bytes-append (subbytes s 0 (+ delta start2))
                                      (string->bytes/utf-8 user-js-path)
                                      (subbytes s (+ delta end2)))]
                       [else s]))))
    (call-with-output-file*
     p
     #:exists 'truncate/replace
     (lambda (out) (write-bytes new-bstr out)))))

;; Used in binary mode:
(define (fixup-info new-p src-base level)
  (define dir (let-values ([(base name dir?) (split-path new-p)])
                base))
  ;; check format:
  (define get-info
    (get-info/full dir #:namespace (make-base-namespace)))
  (when get-info
    ;; read in:
    (define content
      (call-with-input-file* 
       new-p
       (lambda (in)
         (begin0 
          (with-module-reading-parameterization
           (lambda () (read in)))))))
    ;; convert:
    (define (convert-mod info-lib defns)
      `(module info ,info-lib
         (#%module-begin
          (define assume-virtual-sources #t)
          . ,(filter values
                     (map (fixup-info-definition get-info) defns)))))
    (define new-content
      (match content
        [`(module info ,info-lib (#%module-begin . ,defns))
         (convert-mod info-lib defns)]
        ;; In case of `(module ...)` instead of `#lang ...`:
        [`(module info ,info-lib . ,defns)
         (convert-mod info-lib defns)]))
    ;; write updated:
    (call-with-output-file* 
     new-p
     #:exists 'truncate
     (lambda (out)
       (write new-content out)
       (newline out)))
    ;; sanity check:
    (unless (get-info/full dir #:namespace (make-base-namespace))
      (error 'pkg-binary-create "rewrite failed"))
    ;; compile it, if not top level:
    (when (strip-binary-compile-info)
      (unless (eq? level 'package)
        (managed-compile-zo new-p)))))

(define ((fixup-info-definition get-info) defn)
  (match defn
    [`(define build-deps . ,v) #f]
    [`(define assume-virtual-sources . ,v) #f]
    [`(define copy-foreign-libs . ,v)
     `(define move-foreign-libs . ,v)]
    [`(define copy-shared-files . ,v)
     `(define move-shared-files . ,v)]
    [`(define copy-man-pages . ,v)
     `(define move-man-pages . ,v)]
    [_ defn]))

(define (unmove-files dir dest-dir metadata-ns)
  ;; Determine whether any files slated for movement by
  ;; `move-foreign-libs', etc., have been installed
  ;; and need to be uninstalled, and copies moved files
  ;; to `dest-dir'.
  (define (unmove-in dir dest-dir)
    (for ([f (in-list (directory-list dir))])
      (define d (build-path dir f))
      (when (directory-exists? d)
        (unmove d (build-path dest-dir f)))))
  (define (unmove dir dest-dir)
    (define info (get-info/full dir #:namespace metadata-ns))
    (define (unmove-tag tag find-dir)
      (when info
        (define l (info tag (lambda () null)))
        (for ([f (in-list l)])
          (when (and (not (file-exists? (build-path dir f)))
                     (not (directory-exists? (build-path dir f)))
                     (or (file-exists? (build-path (find-dir) f))
                         (directory-exists? (build-path (find-dir) f))))
            (copy-directory/files (build-path (find-dir) f) 
                                  (build-path dest-dir f))))))
    (unmove-tag 'move-foreign-libs find-user-lib-dir)
    (unmove-tag 'move-shared-files find-user-share-dir)
    (unmove-tag 'move-man-pages find-user-man-dir)
    (unmove-in dir dest-dir))
  (unmove dir dest-dir))

(define (assume-virtual dest-dir in-collection?)
  ;; If an "info.rkt" file doesn't exists in a collection,
  ;; add one so that `assume-virtual-sources` is defined.
  (cond
   [in-collection?
    (define info-path (build-path dest-dir "info.rkt"))
    (unless (file-exists? info-path)
      (call-with-output-file* 
       info-path
       (lambda (o)
         (write `(module info setup/infotab (define assume-virtual-sources #t)) o)
         (newline o)))
      (when (strip-binary-compile-info)
        (managed-compile-zo info-path)))]
   [else
    (for ([f (in-list (directory-list dest-dir #:build? #t))])
      (when (directory-exists? f)
        (assume-virtual f #t)))]))

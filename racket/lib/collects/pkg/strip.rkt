#lang racket/base
(require compiler/cm
         setup/getinfo
         syntax/modread
         racket/match
         racket/file
         racket/list
         racket/set)

(provide generate-stripped-directory
         fixup-local-redirect-reference)

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
               ;; drop these, because they're recreated on fixup:
               (equal? #"info_rkt.zo" bstr)
               (equal? #"info_rkt.dep" bstr))]
          [(built)
           #f])))
  
  (define (fixup new-p path src-base)
    (unless (eq? mode 'source)
      (define bstr (path->bytes path))
      (cond
       [(regexp-match? #rx"[.]html$" bstr)
        (fixup-html new-p)]
       [(and (eq? mode 'binary)
             (equal? #"info.rkt" bstr))
        (fixup-info new-p src-base)]
       [(and (eq? mode 'binary)
             (regexp-match? #rx"[.]zo$" bstr))
        (fixup-zo new-p)])))
  
  (define (explore base paths drops keeps)
    (for ([path (in-list paths)])
      (define p (if (eq? base 'same)
                    path
                    (build-path base path)))
      (when (and (not (hash-ref drops p #f))
                 (or (hash-ref keeps p #f)
                     (not (drop-by-default? 
                           path
                           (lambda () (build-path dir p))))))
        (define old-p (build-path dir p))
        (define new-p (build-path dest-dir p))
        (cond
         [(file-exists? old-p)
          (copy-file old-p new-p)
          (file-or-directory-modify-seconds
           new-p
           (file-or-directory-modify-seconds old-p))
          (fixup new-p path base)]
         [(directory-exists? old-p)
          (define-values (new-drops new-keeps)
            (add-drop+keeps old-p p drops keeps))
          (make-directory new-p)
          (explore p
                   (directory-list old-p)
                   new-drops
                   new-keeps)]
         [else (error 'strip "file or directory disappeared?")]))))

  (define-values (drops keeps)
    (add-drop+keeps dir 'same #hash() #hash()))
  
  (explore 'same (directory-list dir) drops keeps))

(define (fixup-html new-p)
  ;; strip full path to "local-redirect.js"
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
                                       mod-subs)
           #t
           mod*-subs))))
  (unless (eq? mod new-mod)
    (call-with-output-file*
     new-p
     #:exists 'truncate/replace
     (lambda (out) (write new-mod out)))))

(define (fixup-local-redirect-reference p js-path)
  ;; Relying on this HTML pattern (as generated by Scribble's HTML
  ;; renderer) is a little fragile. Any better idea?
  (define rx #rx"<script type=\"text/javascript\" src=\"([^\"]*)/local-redirect.js\">")
  (define m (call-with-input-file*
             p
             (lambda (i) (regexp-match-positions rx i))))
  (when m
    (define start (caadr m))
    (define end (cdadr m))
    (define bstr (file->bytes p))
    (define new-bstr
      (bytes-append (subbytes bstr 0 start)
                    (string->bytes/utf-8 js-path)
                    (subbytes bstr end)))
    (call-with-output-file* 
     p
     #:exists 'truncate/replace
     (lambda (out) (write-bytes new-bstr out)))))

(define (fixup-info new-p src-base)
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
    (define new-content
      (match content
        [`(module info setup/infotab (#%module-begin . ,defns))
         `(module info setup/infotab
            (#%module-begin
             (define assume-virtual-sources '())
             . ,(filter values
                        (map (fixup-info-definition get-info) defns))))]))
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
    (unless (eq? src-base 'same)
      (managed-compile-zo new-p))))

(define ((fixup-info-definition get-info) defn)
  (match defn
    [`(define build-deps . ,v) #f]
    [`(define copy-foreign-libs . ,v)
     `(define move-foreign-libs . ,v)]
    [`(define copy-man-pages . ,v)
     `(define move-man-pages . ,v)]
    [_ defn]))

#lang scheme/base

(require scribble/struct
         scribble/manual-struct
         scribble/decode-struct
         scribble/base-render
         (prefix-in html: scribble/html-render)
         scheme/class
         setup/getinfo
         setup/dirs
         mzlib/serialize
         scheme/file)

(provide load-xref
         xref-render
         xref-index
         (struct-out entry))

(define-struct entry (words    ; list of strings: main term, sub-term, etc.
                      content  ; Scribble content to the index label
                      link-key ; for generating a Scribble link
                      desc))   ; further info that depends on the kind of index entry

;; Private:
(define-struct xrefs (renderer ri))

;; ----------------------------------------
;; Xref loading

(define-struct doc (source dest))

(define-namespace-anchor here)

(define (load-xref)
  (let* ([renderer (new (html:render-mixin render%) 
                        [dest-dir (find-system-path 'temp-dir)])]
         [dirs (find-relevant-directories '(scribblings))]
         [infos (map get-info/full dirs)]
         [docs (filter
                values
                (apply append
                       (map (lambda (i dir)
                              (let ([s (i 'scribblings)])
                                (map (lambda (d)
                                       (if (pair? d)
                                           (let ([flags (if (pair? (cdr d))
                                                            (cadr d)
                                                            null)])
                                             (let ([name (if (and (pair? (cdr d))
                                                                  (pair? (cddr d))
                                                                  (caddr d))
                                                             (cadr d)
                                                             (let-values ([(base name dir?) (split-path (car d))])
                                                               (path-replace-suffix name #"")))])
                                               (make-doc
                                                (build-path dir (car d))
                                                (if (memq 'main-doc flags)
                                                    (build-path (find-doc-dir) name)
                                                    (build-path dir "compiled" "doc" name)))))
                                           #f))
                                     s)))
                            infos
                            dirs)))]
         [ci (send renderer collect null null)])
    (map (lambda (doc)
           (parameterize ([current-namespace (namespace-anchor->empty-namespace here)])
             (with-handlers ([exn:fail? (lambda (exn) exn)])
               (let ([r (with-input-from-file (build-path (doc-dest doc) "out.sxref")
                          read)])
                 (send renderer deserialize-info (cadr r) ci)))))
         docs)
    (make-xrefs renderer (send renderer resolve null null ci))))

;; ----------------------------------------
;; Xref reading

(define (xref-index xrefs)
  (filter
   values
   (hash-table-map (collect-info-ext-ht (resolve-info-ci (xrefs-ri xrefs)))
                   (lambda (k v)
                     (and (pair? k)
                          (eq? (car k) 'index-entry)
                          (make-entry (car v) 
                                      (cadr v)
                                      (cadr k)
                                      (caddr v)))))))

(define (xref-render xrefs doc dest-file)
  (let* ([dest-file (if (string? dest-file)
                        (string->path dest-file)
                        dest-file)]
         [renderer (new (html:render-mixin render%) 
                        [dest-dir (path-only dest-file)])]
         [ci (send renderer collect (list doc) (list dest-file))])
    (send renderer transfer-info ci (resolve-info-ci (xrefs-ri xrefs)))
    (let ([ri (send renderer resolve (list doc) (list dest-file) ci)])
      (send renderer render (list doc) (list dest-file) ri)
      (void))))

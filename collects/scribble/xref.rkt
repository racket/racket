#lang scheme/base

(require scribble/struct
         scribble/base-render
         scribble/search
         (prefix-in html: scribble/html-render)
         scheme/class
         scheme/path)

(provide load-xref
         xref?
         xref-render
         xref-index
         xref-binding->definition-tag
         xref-tag->path+anchor
         xref-tag->index-entry
         xref-transfer-info
         (struct-out entry))

(define-struct entry
  (words    ; list of strings: main term, sub-term, etc.
   content  ; Scribble content to the index label
   tag      ; for generating a Scribble link
   desc))   ; further info that depends on the kind of index entry

;; Private:
(define-struct xrefs (renderer ri))

(define (xref? x) (xrefs? x))

;; ----------------------------------------
;; Xref loading

(define-namespace-anchor here)

(define (load-xref sources
                   #:render% [render% (html:render-mixin render%)]
                   #:root [root-path #f])
  (let* ([renderer (new render% [dest-dir (find-system-path 'temp-dir)])]
         [fp (send renderer traverse null null)]
         [ci (send renderer collect null null fp)])
    (for ([src sources])
      (parameterize ([current-namespace
                      (namespace-anchor->empty-namespace here)])
        (let ([v (src)])
          (when v (send renderer deserialize-info v ci #:root root-path)))))
    (make-xrefs renderer (send renderer resolve null null ci))))

;; ----------------------------------------
;; Xref reading

(define (xref-index xrefs)
  (filter
   values
   (hash-map
    (collect-info-ext-ht (resolve-info-ci (xrefs-ri xrefs)))
    (lambda (k v)
      (and (pair? k)
           (eq? (car k) 'index-entry)
           (make-entry (car v) (cadr v) (cadr k) (caddr v)))))))

;; dest-file can be #f, which will make it return a string holding the
;; resulting html
(define (xref-render xrefs doc dest-file
                     #:render% [render% (html:render-mixin render%)]
                     #:refer-to-existing-files? [use-existing? (not dest-file)])
  (let* ([dest-file (if (string? dest-file) (string->path dest-file) dest-file)]
         [renderer (new render%
                        [dest-dir (and dest-file (path-only dest-file))]
                        [refer-to-existing-files use-existing?]
                        [css-path    'inline]
                        [script-path 'inline])]
         [ci (send renderer collect (list doc) (list dest-file))]
         [_ (send renderer transfer-info ci (resolve-info-ci (xrefs-ri xrefs)))]
         [ri (send renderer resolve (list doc) (list dest-file) ci)]
         [xs (send renderer render (list doc) (list dest-file) ri)])
    (if dest-file
        (void)
        (car xs))))

(define (xref-transfer-info renderer ci xrefs)
  (send renderer transfer-info ci (resolve-info-ci (xrefs-ri xrefs))))

;; Returns (values <tag-or-#f> <form?>)
(define xref-binding-tag
  (case-lambda
   [(xrefs id/binding mode)
    (let ([search
           (lambda (id/binding)
             (let ([tag (find-scheme-tag #f (xrefs-ri xrefs) id/binding mode)])
               (if tag
                   (values tag (eq? (car tag) 'form))
                   (values #f #f))))])
      (cond
        [(identifier? id/binding)
         (search id/binding)]
        [(and (list? id/binding)
              (= 7 (length id/binding)))
         (search id/binding)]
        [(and (list? id/binding)
              (= 2 (length id/binding)))
         (let loop ([src (car id/binding)])
           (cond
            [(module-path-index? src)
             (search (list src (cadr id/binding)))]
            [(module-path? src)
             (loop (module-path-index-join src #f))]
            [else
             (raise-type-error 'xref-binding-definition->tag
                               "list starting with module path or module path index"
                               src)]))]
        [else (raise-type-error 'xref-binding-definition->tag
                                "identifier, 2-element list, or 7-element list"
                                id/binding)]))]))

(define (xref-binding->definition-tag xrefs id/binding mode)
  (let-values ([(tag form?) (xref-binding-tag xrefs id/binding mode)])
    tag))

(define (xref-tag->path+anchor xrefs tag
                               #:render% [render% (html:render-mixin render%)]
                               #:external-root-url [redirect-main #f])
  (send (let ([r (new render% [dest-dir (find-system-path 'temp-dir)])])
          (when redirect-main
            (send r set-external-root-url redirect-main))
          r)
        tag->path+anchor (xrefs-ri xrefs) tag))

(define (xref-tag->index-entry xrefs tag)
  (let ([v (hash-ref
            (collect-info-ext-ht (resolve-info-ci (xrefs-ri xrefs)))
            `(index-entry ,tag)
            #f)])
    (cond [v (make-entry (car v) (cadr v) (cadr tag) (caddr v))]
          [(and (pair? tag) (eq? 'form (car tag)))
           ;; Try again with 'def:
           (xref-tag->index-entry xrefs (cons 'def (cdr tag)))]
          [else #f])))

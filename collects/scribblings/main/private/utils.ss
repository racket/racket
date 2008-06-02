#reader scribble/reader
#lang scheme/base

(require "../config.ss"
         scribble/manual
         scribble/struct
         scribble/decode
         scheme/list
         setup/dirs)

(provide main-page script script-ref)

(define page-info
  (let ([links (filter pair? links)])
    (lambda (id)
      (cond [(assq id links) => cdr]
            [else (error 'main-page "page id not found: ~e" id)]))))

(define (script #:noscript [noscript null] . body)
  (make-script-element #f noscript "text/javascript" (flatten body)))

(define (script-ref #:noscript [noscript null] path)
  (make-script-element #f noscript "text/javascript" path))

;; the second argument specifies installation/user specific, and if
;; it's missing, then it's a page with a single version
(define (main-page id [installation-specific? '?])
  (define info (page-info id))
  (define title-string (car info))
  (define root (cadr info))
  (define path (caddr info))
  (define user-doc? (eq? installation-specific? #f))
  (define inst-doc? (eq? installation-specific? #t))
  (define up-path
    ;; massage the current path to an up string
    (regexp-replace* #rx"[^/]*/" (regexp-replace #rx"[^/]+$" path "") "../"))
  (define page-title
    (title #:style '(no-toc) title-string
           (cond [inst-doc? " (installation)"]
                 [user-doc? ""] ; can be " (user)"
                 [else ""])))
  (define toc
    (map (lambda (item)
           (let ([link-id (if (pair? item) (car item) item)])
             ((if (eq? id link-id) caddr cadr) item)))
         (front-toc-items up-path)))
  (make-splice `(,page-title
                 ,@toc
                 ,@(if user-doc?
                     (list @script{SetPLTRoot("@(version)", "@up-path")@";"})
                     '()))))

;; FIXME: Use this to avoid hard-wiring manual titles and paths in config.ss
(define (resolve s [f s])
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(format "scribblings/~a/~a.scrbl" s f))
                            #f))))

(define (front-toc-items up)
  (map (lambda (item)
         (if (eq? item '---)
           (list '--- (make-toc-element #f null '(nbsp)))
           (let ()
             (define id    (car item))
             (define info  (page-info id))
             (define label (car info))
             (define root  (cadr info))
             (define path  (caddr info))
             (define text  (make-element "tocsubseclink" (list label)))
             (define dest
               (case root
                 [(plt)  (build-path (find-doc-dir) path)]
                 [(user) (string-append up path)]
                 [(#f)   path]
                 [else (error "internal error (main-page)")]))
             (define (onclick style)
               (if (eq? root 'user)
                 (make-with-attributes
                  style
                  `([onclick
                     . ,(format "return GotoPLTRoot(\"~a\", \"~a\");"
                                (version) path)]))
                 style))
             (define (elt style)
               (make-toc-element
                #f null (list (link dest #:style (onclick style) text))))
             (list id (elt "tocviewlink") (elt "tocviewselflink")))))
       links))

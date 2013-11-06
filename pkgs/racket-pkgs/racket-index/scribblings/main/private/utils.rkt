#reader scribble/reader
#lang scheme/base

(require "../config.rkt"
         scribble/manual
         scribble/core
         scribble/html-properties
         scribble/decode
         scheme/list
         setup/dirs)

(provide main-page script script-ref not-on-the-web)

(define page-info
  (let ([links (filter pair? links)])
    (lambda (id)
      (cond [(assq id links) => cdr]
            [else (error 'main-page "page id not found: ~e" id)]))))

(define (script #:noscript [noscript null] . body)
  (make-element (make-style #f (list
                                (make-script-property
                                 "text/javascript"
                                 (flatten body))))
                noscript))

(define (script-ref #:noscript [noscript null] path)
  (make-element (make-style #f (list
                                (make-script-property
                                 "text/javascript"
                                 path)))
                noscript))

;; this is for content that should not be displayed on the web (this
;; is done by a class name that is not included in the usual css file,
;; but for the web version the css is extended with this class as
;; something that is not displayed)
(define (not-on-the-web . body)
  (make-element "hide_when_on_the_web" (decode-content body)))

;; the second argument specifies installation/user specific, and if
;; it's missing, then it's a page with a single version
(define (main-page id [installation-specific? '?] #:force-racket-css? [force-racket-css? #f])
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
    (title #:style (make-style #f (list*
                                   'no-toc
                                   'toc-hidden
                                   (if (not force-racket-css?)
                                       null
                                       (list
                                        (make-css-addition (collection-file-path "racket.css" "scribble"))))))
           title-string
           #;
           ;; the "(installation)" part shouldn't be visible on the web, but
           ;; there's no way (currently) to not have it in the window title
           ;; too.
           (cond [inst-doc? (not-on-the-web " (installation)")]
                 [user-doc? ""] ; can be " (user)"
                 [else ""])
           ))
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

;; FIXME: Use this to avoid hard-wiring manual titles and paths in config.rkt
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
               (make-style
                style
                (list (make-attributes
                       `(,@(if (eq? root 'user)
                             `([onclick
                                . ,(format "return GotoPLTRoot(\"~a\", \"~a\");"
                                           (version) path)])
                             `())
                         ;; note: root=#f means an external link, but in this
                         ;; case this is the bugs link, so *keep* it and later
                         ;; use it on the bugs page
                         [data-pltdoc . "x"])))))
             (define (elt style)
               (make-toc-element
                #f null (list (hyperlink dest #:style (onclick style) text))))
             (list id (elt "tocviewlink") (elt "tocviewselflink")))))
       links))

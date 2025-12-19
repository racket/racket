#lang at-exp racket/base

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
(define (main-page id [installation-specific? '?]
                   #:style [style-in #f]
                   #:title-content [title-content-in #f]
                   #:self-path [self-path #f]
                   #:bug-url [bug-url #f]
                   #:force-racket-css? [force-racket-css? #f]
                   #:show-root-info? [show-root-info? #f]
                   #:extra-additions [extra-additions '()]
                   #:default-language-family [default-language-family #f]
                   #:doc-properties [doc-properties #f]
                   #:version [doc-version #f]
                   #:date [doc-date #f]
                   #:can-find-inst? [can-find-inst? #t])
  (define info (page-info id))
  (define title-content (or title-content-in (car info)))
  (define root (cadr info))
  (define path (or self-path (caddr info)))
  (define user-doc? (eq? installation-specific? #f))
  (define inst-doc? (eq? installation-specific? #t))
  (define up-path
    ;; massage the current path to an up string
    (regexp-replace* #rx"[^/]*/" (regexp-replace #rx"[^/]+$" path "") "../"))
  (define page-title
    (title #:style (make-style (cond
                                 [(style? style-in) (style-name style-in)]
                                 [(string? style-in) style-in]
                                 [(symbol? style-in) style-in]
                                 [else #f])
                               (list*
                                'no-toc
                                'toc-hidden
                                (append
                                 (cond
                                   [(list? style-in) style-in]
                                   [(style? style-in) (style-properties style-in)]
                                   [else null])
                                 (if force-racket-css?
                                     (list (make-css-addition (collection-file-path "racket.css" "scribble")))
                                     null)
                                 (if (not show-root-info?)
                                     null
                                     (list
                                      (make-css-addition (collection-file-path "root-info.css" "scribblings/main/private"))
                                      (make-js-addition (collection-file-path "root-info.js" "scribblings/main/private"))))
                                 extra-additions)))
           title-content
           #:tag-prefix (let* ([ht #hasheq()]
                               [ht (if doc-properties
                                       (hash-set ht 'doc-properties doc-properties)
                                       ht)]
                               [ht (if default-language-family
                                       (hash-set ht 'default-language-family default-language-family)
                                       ht)])
                          (and (positive? (hash-count ht)) ht))
           #:version doc-version
           #:date doc-date
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
         (front-toc-items id up-path (and (eq? id 'start) title-content) bug-url inst-doc? can-find-inst?)))
  (make-splice `(,page-title
                 ,@toc
                 ,@(if show-root-info?
                     (list @script{var racket_root_version = "@(get-installation-name)"@";"})
                     '())
                 ,@(if user-doc?
                     (list @script{SetPLTRoot("@(get-installation-name)", "@up-path")@";"})
                     '()))))

;; FIXME: Use this to avoid hard-wiring manual titles and paths in config.rkt
(define (resolve s [f s])
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(format "scribblings/~a/~a.scrbl" s f))
                            #f))))

(define (front-toc-items for-id up title-content bug-url inst-doc? can-find-inst?)
  (map (lambda (item)
         (if (eq? item '---)
           (list '--- (make-toc-element #f null '(nbsp)))
           (let ()
             (define id    (car item))
             (define info  (page-info id))
             (define label (if (and title-content (eq? id 'start))
                               title-content
                               (car info)))
             (define root  (if (eq? id for-id)
                               #f
                               (cadr info)))
             (define path  (cond
                             [(and bug-url (eq? id 'bugreport))
                              bug-url]
                             [(eq? id for-id)
                              "index.html"]
                             [else
                              (caddr info)]))
             (define text  (make-element "tocsubseclink" (list label)))
             (define dest
               (case root
                 [(plt) (if (and can-find-inst? (not inst-doc?))
                            (let ([m (regexp-match "^[^/]*" path)])
                              (if m
                                  `(lib ,(format "scribblings/main/~a.scrbl" (car m)))
                                  (string-append up path)))
                            (string-append up path))]
                 [(user) (string-append up path)]
                 [(#f)   path]
                 [else (error "internal error (main-page)")]))
             (define (onclick style)
               (make-style
                style
                (list (make-attributes
                       `(,@(case root
                             [(user)
                              `([onclick . ,(format "return GotoPLTRoot(\"~a\", \"~a\");"
                                                    (get-installation-name) path)])]
                             [(plt)
                              (if inst-doc?
                                  '()
                                  ;; workaround specific to searcg page to find "license",
                                  ;; which may be adjacent or may be in main "doc"
                                  (let ([m (regexp-match "^[^/]*" path)])
                                    (if m
                                        `([onclick . ,(format "return GotoDocIndex(\"~a\", \"~a\");"
                                                              (get-installation-name) (car m))])
                                        '())))]
                             [else '()])
                         [id . ,(format "~a-link" id)]
                         ;; note: root=#f means an external link, but in this
                         ;; case this is the bugs link, so *keep* it and later
                         ;; use it on the bugs page
                         [data-pltdoc . "x"])))))
             (define (elt style)
               (make-toc-element
                #f null (list (if (string? dest)
                                  (hyperlink dest #:style (onclick style) text)
                                  (seclink "top" #:doc dest text)))))
             (list id (elt "tocviewlink") (elt "tocviewselflink")))))
       links))

#lang at-exp s-exp meta/web/html

(require (for-syntax racket/base syntax/name) "utils.rkt" "resources.rkt")

(define-for-syntax (process-contents who layouter stx xs)
  (let loop ([xs xs] [kws '()] [id? #f])
    (syntax-case xs ()
      [(k v . xs) (keyword? (syntax-e #'k))
       (loop #'xs (list* #'v #'k kws) (or id? (eq? '#:id (syntax-e #'k))))]
      [_ (with-syntax ([layouter layouter]
                       [(x ...) (reverse kws)]
                       [(id ...)
                        (if id?
                          '()
                          (let ([name (or (syntax-property stx 'inferred-name)
                                          (syntax-local-name))])
                            (if name (list '#:id `',name) '())))]
                       ;; delay body, allow definitions
                       [body #`(lambda () (text #,@xs))])
           #'(layouter id ... x ... body))])))

(define (get-path who id file sfx dir)
  (define file*
    (or file
        (let ([f (and id (symbol->string (force id)))])
          (cond [(and f (regexp-match #rx"[.]" f)) f]
                [(and f sfx)
                 (string-append f (regexp-replace #rx"^[.]?" sfx "."))]
                [else (error who "missing `#:file', or `#:id'~a"
                             (if sfx "" " and `#:suffix'"))]))))
  (if dir (web-path dir file*) file*))

;; The following are not intended for direct use, see
;; `define+provide-context' below (it could be used with #f for the
;; directory if this ever gets used for a flat single directory web
;; page.)

;; for plain text files
(define-syntax (plain stx)
  (syntax-case stx () [(_ . xs) (process-contents 'plain #'plain* stx #'xs)]))
(define (plain* #:id [id #f] #:suffix [suffix #f]
                #:dir [dir #f] #:file [file #f]
                #:referrer
                [referrer (lambda (url)
                            (error 'plain "no referrer for ~e" file))]
                #:newline [newline? #t]
                content)
  (resource (get-path 'plain id file suffix dir)
            (file-writer output (list content (and newline? "\n")))
            referrer))

;; page layout function
(define-syntax (page stx)
  (syntax-case stx () [(_ . xs) (process-contents 'page #'page* stx #'xs)]))
(define (page* #:id [id #f] #:dir [dir #f] #:file [file #f]
               ;; if this is true, return only the html -- don't create
               ;; a resource -- therefore no file is made, and no links
               ;; to it can be made (useful only for stub templates)
               #:html-only [html-only? #f]
               #:title [label (if id
                                (let* ([id (->string (force id))]
                                       [id (regexp-replace #rx"^.*/" id "")]
                                       [id (regexp-replace #rx"-" id " ")])
                                  (string-titlecase id))
                                (error 'page "missing `#:id' or `#:title'"))]
               #:link-title [linktitle label]
               #:window-title [wintitle @list{Racket: @label}]
               ;; can be #f (default), 'full: full page (and no div),
               ;; otherwise, a css width
               #:width [width #f]
               #:extra-headers [headers #f]
               #:extra-body-attrs [body-attrs #f]
               #:resources resources ; see below
               #:referrer [referrer
                           (lambda (url . more)
                             (a href: url (if (null? more) linktitle more)))]
               ;; will be used instead of `this' to determine navbar highlights
               #:part-of [part-of #f]
               content)
  (define (page)
    (let* ([head    (resources 'head wintitle headers)]
           [navbar  (resources 'navbar (or part-of this))]
           [content (case width
                      [(full) content]
                      [(#f) (div class: 'bodycontent content)]
                      [else (div class: 'bodycontent
                                 style: @list{width: @|width|@";"}
                                 content)])]
           [content (list navbar content)])
      @xhtml{@head
             @(if body-attrs
                (apply body `(,@body-attrs ,content))
                (body content))}))
  (define this (and (not html-only?)
                    (resource (get-path 'plain id file "html" dir)
                              (file-writer output-xml page)
                              referrer)))
  (when this (pages->part-of this (or part-of this)))
  (or this page))

;; maps pages to their parts, so symbolic values can be used to determine it
(define pages->part-of
  (let ([t (make-hasheq)])
    (case-lambda [(page) (hash-ref t page page)]
                 [(page part-of) (hash-set! t page part-of)])))

(provide set-navbar!)
(define-syntax-rule (set-navbar! pages help)
  (if (unbox navbar-info)
    ;; since generation is delayed, it won't make sense to change the navbar
    (error 'set-navbar! "called twice")
    (set-box! navbar-info (list (lazy pages) (lazy help)))))

(define navbar-info (box #f))
(define (navbar-maker logo)
  (define pages-promise
    (lazy (car (or (unbox navbar-info)
                   (error 'navbar "no navbar info set")))))
  (define help-promise
    (lazy (cadr (unbox navbar-info))))
  (define pages-parts-of-promise
    (lazy (map pages->part-of (force pages-promise))))
  (define (middle-text size x)
    (span style: `("font-size: ",size"px; vertical-align: middle;")
          class: 'navtitle
          x))
  (define OPEN
    (list (middle-text 100 "(")
          (middle-text 80 "(")
          (middle-text 60 "(")
          (middle-text 40 nbsp)))
  (define CLOSE
    (list (middle-text 80 "Racket")
          (middle-text 40 nbsp)
          (middle-text 60 ")")
          (middle-text 80 ")")
          (middle-text 100 ")")))
  (define (header-cell logo)
    (td OPEN
        (img src: logo alt: "[logo]"
             style: '("vertical-align: middle; "
                      "margin: 13px 0.25em 0 0; border: 0;"))
        CLOSE))
  (define (links-table this)
    (table width: "100%"
      (tr (map (lambda (nav navpart)
                 (td class: 'navlinkcell
                   (span class: 'navitem
                     (span class: (if (eq? (pages->part-of this) navpart)
                                    'navcurlink 'navlink)
                       nav))))
               (force pages-promise)
               (force pages-parts-of-promise)))))
  (lambda (this)
    (div class: 'racketnav
      (div class: 'navcontent
        (table border: 0 cellspacing: 0 cellpadding: 0 width: "100%"
          (tr (header-cell logo)
              (td class: 'helpiconcell
                  (let ([help (force help-promise)])
                    (span class: 'helpicon (if (eq? this help) nbsp help)))))
          (tr (td colspan: 2 (links-table this))))))))

(define (html-favicon-maker icon)
  (define headers
    (list @link[rel: "icon" href: icon type: "image/ico"]
          @link[rel: "shortcut icon" href: icon]))
  (lambda () headers))

(define (html-head-maker style favicon)
  (define headers
    (list @meta[name: "generator" content: "Racket"]
          @meta[http-equiv: "Content-Type" content: "text/html; charset=utf-8"]
          favicon
          style))
  (lambda (title* more-headers) (head @title[title*] headers more-headers)))

(define (make-resources icon logo style)
  (let* ([favicon     (html-favicon-maker icon)]
         [make-head   (html-head-maker style favicon)]
         [make-navbar (navbar-maker logo)])
    (lambda (what . more)
      (apply (case what
               [(head)   make-head]
               [(navbar) make-navbar]
               [(favicon-headers) favicon]
               [else (error 'resources "internal error")])
             more))))

;; `define+provide-context' should be used in each toplevel directory (= each
;; site) to have its own resources (and possibly other customizations).
(provide define+provide-context define-context)
(define-for-syntax (make-define+provide-context stx provide?)
  (define (make-it dir [resources #f])
    (with-syntax ([dir dir]
                  [page-id      (datum->syntax stx 'page)]
                  [plain-id     (datum->syntax stx 'plain)]
                  [copyfile-id  (datum->syntax stx 'copyfile)]
                  [symlink-id   (datum->syntax stx 'symlink)]
                  [resources-id (datum->syntax stx 'the-resources)])
      (with-syntax ([resources (or resources
                                   #'(make-resources (make-icon dir)
                                                     (make-logo dir)
                                                     (make-style dir)))]
                    [provides (if provide?
                                #'(provide page-id plain-id copyfile-id
                                           symlink-id resources-id)
                                #'(begin))])
        #'(begin
            (define resources-id resources)
            (define-syntax-rule (page-id . xs)
              (page #:resources resources-id #:dir dir . xs))
            (define-syntax-rule (plain-id . xs)
              (plain #:dir dir . xs))
            (define (copyfile-id source [target #f] [referrer values])
              (copyfile-resource source target referrer #:dir dir))
            (define (symlink-id source [target #f] [referrer values])
              (symlink-resource source target referrer #:dir dir))
            provides))))
  (syntax-case stx ()
    [(_ dir) (make-it #'dir)]
    [(_ dir #:resources resources) (make-it #'dir #'resources)]))
(define-syntax (define+provide-context stx)
  (make-define+provide-context stx #t))
(define-syntax (define-context stx)
  (make-define+provide-context stx #f))

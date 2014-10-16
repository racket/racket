#lang scheme/base
(require "core.rkt"
         "private/render-utils.rkt"
         "html-properties.rkt"
         "private/literal-anchor.rkt"
         scheme/class
         scheme/path
         scheme/file
         scheme/port
         scheme/list
         scheme/string
         file/convertible
         mzlib/runtime-path
         setup/main-doc
         setup/collects
         setup/dirs
         net/url
         net/uri-codec
         net/base64
         scheme/serialize
         racket/draw/gif
         (prefix-in xml: xml/xml)
         (for-syntax scheme/base)
         "search.rkt"
         (except-in "base.rkt" url))
(provide render-mixin
         render-multi-mixin)

(define as-literal
  (let ([loc (xml:make-location 0 0 0)])
    (lambda strings (xml:make-cdata loc loc (string-append* strings)))))
(define (ref-style path)
  `(link ([rel "stylesheet"] [type "text/css"] [href ,path] [title "default"])))
(define (inlined-style . body)
  `(style ([type "text/css"])
     ,(apply as-literal
             `("\n"
               ,@(map (lambda (x) (if (string? x) x (format "~a" x))) body)
               "\n"))))
(define (ref-script path)
  `(script ([type "text/javascript"] [src ,path])))
(define (inlined-script . body)
  `(script ([type "text/javascript"])
     ,(apply as-literal
             `("\n"
               ,@(map (lambda (x) (if (string? x) x (format "~a" x))) body)
               "\n"))))

(define-runtime-path scribble-css "scribble.css")
(define-runtime-path scribble-style-css "scribble-style.css")
(define-runtime-path scribble-prefix-html "scribble-prefix.html")
(define-runtime-path scribble-js  "scribble-common.js")
;; utilities for render-one-part
(define-values (scribble-css-contents scribble-js-contents)
  (let* ([read-file
          (lambda (file)
            (with-input-from-file file
              (lambda ()
                ;; note: file-size can be bigger than the string, but
                ;; that's fine.
                (read-string (file-size file)))))]
         [adjust-rel
          (lambda (depth p)
            (if (and (relative-path? p)
                     (positive? depth))
                (let loop ([d depth] [p p])
                  (if (zero? d)
                      p
                      (loop (sub1 d) (string-append "../" p))))
                p))]
         [file-getter
          (lambda (default-file make-inline make-ref)
            (let ([c #f])
              (lambda (file path depth)
                (cond [(bytes? file)
                       (make-inline (bytes->string/utf-8 file))]
                      [(url? file)
                       (make-ref (url->string* file))]
                      [(not (eq? 'inline path))
                       (make-ref (adjust-rel
                                  depth
                                  (or path (let-values ([(base name dir?)
                                                         (split-path file)])
                                             (path->string name)))))]
                      [(or (not file) (equal? file default-file))
                       (unless c
                         (set! c (make-inline (read-file default-file))))
                       c]
                      [else (make-inline (read-file file))]))))])
    (values (file-getter scribble-css inlined-style  ref-style)
            (file-getter scribble-js  inlined-script ref-script))))

(define (lookup-path path mapping)
  (ormap (lambda (p)
           (and (equal? (car p) path)
                (cdr p)))
         mapping))

(define current-subdirectory (make-parameter #f))
(define current-output-file (make-parameter #f))
(define current-top-part (make-parameter #f))
(define on-separate-page-ok (make-parameter #t))
(define collecting-sub (make-parameter 0))
(define collecting-whole-page (make-parameter #t))
(define current-no-links (make-parameter #f))
(define extra-breaking? (make-parameter #f))
(define current-version (make-parameter (version)))
(define current-part-files (make-parameter #f))
(define current-render-convertible-requests (make-parameter '(png@2x-bytes png-bytes svg-bytes)))

(define (url->string* u)
  (parameterize ([current-url-encode-mode 'unreserved])
    (url->string u)))

;; HTML anchors should be case-insensitively unique. To make them
;;  distinct, add a "." in front of capital letters.  Also clean up
;;  characters that give browsers trouble (i.e., the ones that are not
;;  allowed as-is in URI components) by using "~" followed by a hex
;;  encoding.  (The idea is that the result is still readable, so the
;;  link can be used as a rough indication of where you'll get to.)
(define (anchor-name v)
  (define (encode-byte b)
    (string-append (if (< b 16) "~0" "~") (number->string b 16)))
  (define (encode-bytes str)
    (string->bytes/utf-8 (encode-byte (bytes-ref str 0))))
  (if (literal-anchor? v)
    (literal-anchor-string v)
    (let* ([v (string->bytes/utf-8 (format "~a" v))]
           [v (regexp-replace* #rx#"[A-Z.]" v #".&")]
           [v (regexp-replace* #rx#" " v #"._")]
           [v (regexp-replace* #rx#"\"" v #".'")]
           [v (regexp-replace* #rx#"[^-a-zA-Z0-9_!+*'()/.,]" v encode-bytes)])
      (bytes->string/utf-8 v))))

(define (color->string c)
  (if (string? c)
      c
      (string-append*
       "#"
       (map (lambda (v)
              (let ([s (number->string v 16)])
                (if (< v 16) (string-append "0" s) s)))
            c))))

(define (merge-styles s cls l)
  ;; merge multiple 'style and 'class attributes into one
  (cond
   [(null? l) (append
               (if s
                   (list (list 'style s))
                   null)
               (if cls
                   (list (list 'class cls))
                   null))]
   [(eq? 'style (caar l))
    (merge-styles (if s (string-append s "; " (cadar l)) (cadar l))
                  cls
                  (cdr l))]
   [(eq? 'class (caar l))
    (merge-styles s
                  (if cls (string-append cls " " (cadar l)) (cadar l))
                  (cdr l))]
   [else (cons (car l) (merge-styles s cls (cdr l)))]))

(define (style->attribs style [extras null])
  (let ([a (merge-styles
            #f
            #f
            (apply
             append
             extras
             (map (lambda (v)
                    (cond
                     [(attributes? v)
                      (map (lambda (v) (list (car v) (cdr v))) (attributes-assoc v))]
                     [(color-property? v)
                      `((style ,(format "color: ~a" (color->string (color-property-color v)))))]
                     [(background-color-property? v)
                      `((style ,(format "background-color: ~a" (color->string (background-color-property-color v)))))]
                     [(hover-property? v)
                      `((title ,(hover-property-text v)))]
                     [else null]))
                  (style-properties style))))])
    (let ([name (style-name style)])
      (if (string? name)
          (if (assq 'class a)
              (for/list ([i (in-list a)])
                (if (eq? (car i) 'class)
                    (list 'class (string-append name " " (cadr i)))
                    i))
              (cons `[class ,name]
                    a))
          a))))

;; combine a 'class attribute from both `cl' and `al'
;;  if `cl' starts with one
(define (combine-class cl al)
  (cond
   [(and (pair? cl)
         (eq? (caar cl) 'class)
         (for/or ([i (in-list al)])
           (and (eq? (car i) 'class) (cadr i))))
    => (lambda (s)
         (cons
          `[class ,(string-append (cadar cl) " " s)]
          (append
           (cdr cl)
           (for/list ([i (in-list al)]
                      #:unless (eq? 'class (car i)))
             i))))]
   [else
    (append cl al)]))

(define (style->tag style)
  (for/or ([s (in-list (style-properties style))])
    (and (alt-tag? s)
         (string->symbol (alt-tag-name s)))))

(define (make-search-box top-path) ; appears on every page
  (let ([sa         string-append]
        [emptylabel "...search manuals..."]
        [dimcolor   "#888"])
    `(form ([class "searchform"])
       (input
        ([class "searchbox"]
         [style ,(sa "color: "dimcolor";")]
         [type "text"]
         [value ,emptylabel]
         [title "Enter a search string to search the manuals"]
         [onkeypress ,(format "return DoSearchKey(event, this, ~s, ~s);"
                              (version) top-path)]
         [onfocus ,(sa "this.style.color=\"black\"; "
                       "this.style.textAlign=\"left\"; "
                       "if (this.value == \""emptylabel"\") this.value=\"\";")]
         [onblur ,(sa "if (this.value.match(/^ *$/)) {"
                      " this.style.color=\""dimcolor"\";"
                      " this.style.textAlign=\"center\";"
                      " this.value=\""emptylabel"\"; }")])))))
(define search-box (make-search-box "../"))
(define top-search-box (make-search-box ""))

(define (part-tags/nonempty p)
  (define l (part-tags p))
  (if (null? l)
      (list `(part "???"))
      l))

(define (part-parent d ri)
  (collected-info-parent (part-collected-info d ri)))

(define (with-output-to-file/clean fn thunk)
  ;; We use 'replace instead of the usual 'truncate/replace
  ;;  to avoid problems where a filename changes only in case,
  ;;  in which case some platforms will see the old file
  ;;  as matching the new name, while others don't. Replacing
  ;;  the file syncs the case with the current uses.
  (with-handlers ([exn? ; delete file on breaks, too
                   (lambda (exn)
                     (delete-file fn)
                     (raise exn))])
    (with-output-to-file fn #:exists 'replace thunk)))

;; ----------------------------------------
;;  main mixin

(define (render-mixin %)
  (class %
    (inherit render-block
             render-part
             collect-part
             install-file
             get-dest-directory
             format-number
             number-depth
             quiet-table-of-contents
             extract-part-style-files
             extract-version
             extract-authors
             extract-pretitle)
    (inherit-field prefix-file style-file style-extra-files image-preferences)

    (init-field [alt-paths null]
                ;; `up-path' is either a link "up", or #t which goes
                ;; to the start page (using cookies to get to the
                ;; user start page). If it's a path, then it's also
                ;; used for the "top" link on the page.
                [up-path #f]
                [script-path #f]
                [script-file #f]
                [search-box? #f])

    (define/override (current-render-mode)
      '(html))

    (define/override (get-suffix) #".html")

    (define/override (index-manual-newlines?)
      #t)

    (define/override (auto-extra-files? v) (html-defaults? v))
    (define/override (auto-extra-files-paths v) (html-defaults-extra-files v))
    (define/override (skip-extra-file? p)
      (lookup-path (if (path? p)
                       p
                       (collects-relative->path p))
                   alt-paths))

    ;; ----------------------------------------

    (inherit path->root-relative
             root-relative->path
             root-relative?)

    (define path-cache (make-hash))

    (define (path->relative p)
      (let ([p (path->main-doc-relative p)])
        (if (path? p)
          (let ([p (path->root-relative p)])
            (if (path? p)
              (let ([p (path->collects-relative p #:cache path-cache)])
                (if (path? p)
                    p
                    (intern-taglet p)))
              (intern-taglet p)))
          (intern-taglet p))))

    (define (relative->path p)
      (if (root-relative? p)
          (root-relative->path p)
          (let ([p (if (or (not (pair? p))
                           (eq? (car p) 'doc))
                       (main-doc-relative->path p)
                       p)])
            (if (path? p)
                p
                (collects-relative->path p)))))
    
    ;; ----------------------------------------

    (define/override (start-collect ds fns ci)
      (map (lambda (d fn)
             (parameterize ([current-output-file fn]
                            [current-top-part d])
               (collect-part d #f ci null 1)))
           ds
           fns))

    (define/public (part-whole-page? p ri)
      (let ([dest (resolve-get p ri (car (part-tags/nonempty p)))])
        (and dest (dest-page? dest))))

    (define/public (current-part-whole-page? d)
      (eq? d (current-top-part)))

    (define/override (fresh-tag-collect-context? d ci)
      (current-part-whole-page? d))
    (define/override (fresh-tag-resolve-context? d ri)
      (part-whole-page? d ri))
    (define/override (fresh-tag-render-context? d ri)
      (part-whole-page? d ri))

    (define/override (collect-part-tags d ci number)
      (define redirect (let ([s (part-style d)])
                         (and s
                              (for/or ([p (in-list (style-properties s))])
                                (and (part-link-redirect? p)
                                     (part-link-redirect-url p))))))
      (for ([t (part-tags d)])
        (let ([key (generate-tag t ci)])
          (collect-put! ci key
                        (let ([v (vector (or (part-title-content d) '("???"))
                                         (add-current-tag-prefix key)
                                         number ; for consistency with base
                                         (and (current-output-file)
                                              (path->relative (current-output-file)))
                                         (current-part-whole-page? d))])
                          (if redirect
                              (list->vector (append (vector->list v)
                                                    (list (url->string* redirect))))
                              v))))))

    (define/override (collect-target-element i ci)
      (let ([key (generate-tag (target-element-tag i) ci)])
        (collect-put! ci key
                      (vector (let ([tag (target-element-tag i)])
                                (if (and (pair? tag) (eq? 'part (car tag)))
                                    (element-content i)
                                    #f))
                              (if (redirect-target-element? i)
                                  (make-literal-anchor
                                   (redirect-target-element-alt-anchor i))
                                  (add-current-tag-prefix key))
                              #f ; for consistency with 'part info
                              (path->relative
                               (let ([p (current-output-file)])
                                 (if (redirect-target-element? i)
                                     (let-values ([(base name dir?) (split-path p)])
                                       (build-path base
                                                   (redirect-target-element-alt-path i)))
                                     p)))
                              (page-target-element? i)))))

    (define (dest-path dest)
      (vector-ref dest 3))
    (define (dest-title dest)
      (vector-ref dest 0))
    (define (dest-page? dest)
      (vector-ref dest 4))
    (define (dest-anchor dest)
      (vector-ref dest 1))
    (define (dest-redirect dest)
      (if ((vector-length dest) . > . 5)
          (vector-ref dest 5)
          #f))

    ;; ----------------------------------------

    (define external-tag-path #f)
    (define/override (set-external-tag-path p)
      (set! external-tag-path p))

    (define external-root-url #f)
    (define/override (set-external-root-url p)
      (set! external-root-url p))

    (define extra-script-files null)
    (define/override (add-extra-script-file s)
      (set! extra-script-files (cons s extra-script-files)))

    (define (try-relative-to-external-root dest)
      (cond
       [(let ([rel (find-relative-path
                    (find-doc-dir)
                    (relative->path (dest-path dest)))])
          (and (relative-path? rel)
               rel))
        => (lambda (rel)
             (cons
              (url->string*
               (struct-copy
                url
                (combine-url/relative
                 (string->url external-root-url)
                 (string-join (map path-element->string
                                   (explode-path rel))
                              "/"))))
              (and (not (dest-page? dest))
                   (anchor-name (dest-anchor dest)))))]
       [else #f]))

    (define/public (tag->path+anchor ri tag)
      ;; Called externally; not used internally
      (let-values ([(dest ext?) (resolve-get/ext? #f ri tag)])
        (cond [(not dest) (values #f #f)]
              [(and ext? external-root-url
                    (try-relative-to-external-root dest))
               => (lambda (p)
                    (values (car p) (cdr p)))]
              [(and ext? external-tag-path)
               (values (string->url external-tag-path) (format "~a" (serialize tag)))]
              [else (values (relative->path (dest-path dest))
                            (and (not (dest-page? dest))
                                 (anchor-name (dest-anchor dest))))])))

    (define/public (tag->url-string ri tag #:absolute? [abs? #f])
      ;; Called externally; not used internally
      (let-values ([(dest ext?) (resolve-get/ext? #f ri tag)])
        (cond [(not dest) ""]
              [else (dest->url dest abs?)])))

    (define/public (tag->query-string tag)
      (define (simple? s)
        (or (symbol? s)
            (string? s)
            (number? s)
            (and (list? s) (andmap simple? s))))
      (anchor-name (format "~s" (if (simple? tag)
                                    tag
                                    (serialize tag)))))

    (define/private (link-element-indirect? e)
      (memq 'indirect-link 
            (let ([s (element-style e)])
              (or (and (style? s)
                       (style-properties s))
                  null))))

    (define/override (resolve-content i d ri)
      (cond
        [(and (link-element? i)
              external-tag-path
              (link-element-indirect? i))
         ;; don't resolve indirect link
         (resolve-content (element-content i) d ri)]
        [else
         (super resolve-content i d ri)]))

    ;; ----------------------------------------

    (define/private (reveal-subparts? p) ;!!! need to use this
      (part-style? p 'reveal))

    (define/public (toc-wrap table)
      null)

    (define/private (dest->url dest [abs? #f])
      (if dest
          (or (dest-redirect dest)
              (format "~a~a~a"
                      (let ([p (relative->path (dest-path dest))])
                        (if abs?
                            (path->url-string (path->complete-path p))
                            (if (equal? p (current-output-file))
                                ""
                                (from-root p (get-dest-directory)))))
                      (if (dest-page? dest) "" "#")
                      (if (dest-page? dest)
                          ""
                          (uri-unreserved-encode
                           (anchor-name (dest-anchor dest))))))
          "???"))

    (define/private (dest->url-in-doc dest ext-id)
      (and dest
           (not (dest-redirect dest))
           (format "~a~a~a"
                   ;; The path within the document directory is normally
                   ;; just a single element, but detect nested paths
                   ;; (for "r5rs-std", for example) when the containing
                   ;; directory doesn't match `ext-id`:
                   (let loop ([path (relative->path (dest-path dest))]
                              [empty-ok? #f])
                     (let-values ([(base name dir?) (split-path path)])
                       (cond
                        [(and empty-ok?
                              dir? 
                              (equal? (format "~a" name) (format "~a" ext-id)))
                         #f]
                        [(path? base)
                         (define r (loop base #t))
                         (if r (build-path r name) name)]
                        [else name])))
                   (if (dest-page? dest) "" "#")
                   (if (dest-page? dest)
                       ""
                       (uri-unreserved-encode
                        (anchor-name (dest-anchor dest)))))))

    (inherit sort-image-requests)
    (define/override (render ds fns ri)
      (parameterize ([current-render-convertible-requests
                      (sort-image-requests (current-render-convertible-requests)
                                           image-preferences)])
        (render-top ds fns ri)))

    (define/public (render-top ds fns ri)
      (super render ds fns ri))

    (define/public (render-toc-view d ri)
      (define has-sub-parts?
        (pair? (part-parts d)))
      (define sub-parts-on-other-page?
        (and has-sub-parts?
             (part-whole-page? (car (part-parts d)) ri)))
      (define toc-chain
        (let loop ([d d] [r (if has-sub-parts? (list d) '())])
          (cond [(collected-info-parent (part-collected-info d ri))
                 => (lambda (p) (loop p (cons p r)))]
                [(pair? r) r]
                ;; we have no toc, so use just the current part
                [else (list d)])))
      (define top (car toc-chain))
      (define (toc-item->title+num t show-mine?)
        (values
         `((a ([href ,(dest->url (resolve-get t ri (car (part-tags/nonempty t))))]
               [class ,(if (or (eq? t d) (and show-mine? (memq t toc-chain)))
                         "tocviewselflink"
                         "tocviewlink")]
               [data-pltdoc "x"])
              ,@(render-content (or (part-title-content t) '("???")) d ri)))
         (format-number (collected-info-number (part-collected-info t ri))
                        '(nbsp))))
      (define (toc-item->block t i)
        (define-values (title num) (toc-item->title+num t #f))
        (define children  ; note: might be empty
          (filter (lambda (p) (not (part-style? p 'toc-hidden)))
                  (part-parts t)))
        (define id (format "tocview_~a" i))
        (define last? (eq? t (last toc-chain)))
        (define expand? (or (and last? 
                                 (or (not has-sub-parts?)
                                     sub-parts-on-other-page?))
                            (and has-sub-parts?
                                 (not sub-parts-on-other-page?)
                                 ;; next-to-last?
                                 (let loop ([l toc-chain])
                                   (cond
                                    [(null? l) #f]
                                    [(eq? t (car l))
                                     (and (pair? (cdr l)) (null? (cddr l)))]
                                    [else (loop (cdr l))])))))
        (define top? (eq? t top))
        (define header
          `(table ([cellspacing "0"] [cellpadding "0"])
             (tr ()
               (td ([style "width: 1em;"])
                 ,(if (null? children)
                    'bull
                    `(a ([href "javascript:void(0);"]
                         [title "Expand/Collapse"]
                         [class "tocviewtoggle"]
                         [onclick ,(format "TocviewToggle(this,\"~a\");" id)])
                       ,(if expand? 9660 9658))))
               (td () ,@num)
               (td () ,@title))))
        `(div ([class ,(if top?
                           "tocviewlist tocviewlisttopspace"
                           "tocviewlist")])
           ,(if top? `(div ([class "tocviewtitle"]) ,header) header)
           ,(if (null? children)
              ""
              `(div ([class ,(cond
                              [(and top? last?) "tocviewsublistonly"]
                              [top? "tocviewsublisttop"]
                              [last? "tocviewsublistbottom"]
                              [else "tocviewsublist"])]
                     [style ,(format "display: ~a;" (if expand? 'block 'none))]
                     [id ,id])
                 (table ([cellspacing "0"] [cellpadding "0"])
                   ,@(for/list ([c children])
                       (let-values ([(t n) (toc-item->title+num c #t)])
                         `(tr () (td ([align "right"]) ,@n) (td () ,@t)))))))))
      (define (toc-content)
        ;; no links -- the code constructs links where needed
        (parameterize ([current-no-links #t]
                       [extra-breaking? #t])
          (for/list ([t toc-chain] [i (in-naturals)])
            (toc-item->block t i))))
      `((div ([class "tocset"])
          ,@(if (part-style? d 'no-toc)
              null
              ;; toc-wrap determines if we get the toc or just the title !!!
              `((div ([class "tocview"]) ,@(toc-content))))
          ,@(if (part-style? d 'no-sidebar)
                null
                (render-onthispage-contents
                 d ri top (if (part-style? d 'no-toc) "tocview" "tocsub")
                 sub-parts-on-other-page?))
          ,@(parameterize ([extra-breaking? #t])
              (append-map (lambda (e)
                            (let loop ([e e])
                              (cond
                               [(and (table? e)
                                     (memq 'aux (style-properties (table-style e)))
                                     (pair? (table-blockss e)))
                                (render-table e d ri #f)]
                               [(delayed-block? e)
                                (loop (delayed-block-blocks e ri))]
                               [(traverse-block? e)
                                (loop (traverse-block-block e ri))]
                               [(compound-paragraph? e)
                                (append-map loop (compound-paragraph-blocks e))]
                               [else null])))
                          (part-blocks d))))))

    (define/public (get-onthispage-label)
      null)

    (define/public (nearly-top? d ri top)
      #f)

    (define hidden-memo (make-weak-hasheq))
    (define/public (all-toc-hidden? p)
      (hash-ref hidden-memo
                p
                (lambda ()
                  (define h? (and (part-style? p 'toc-hidden)
                                  (andmap (lambda (s) (all-toc-hidden? s))
                                          (part-parts p))))
                  (hash-set! hidden-memo p h?)
                  h?)))

    (define/private (render-onthispage-contents d ri top box-class sections-in-toc?)
        (let ([nearly-top? (lambda (d) 
                             ;; If ToC would be collapsed, then 
                             ;; no section is nearly the top
                             (if (not sections-in-toc?)
                                 #f
                                 (nearly-top? d ri top)))])
          (define (flow-targets flow)
            (append-map block-targets flow))
          (define (block-targets e)
            (cond [(table? e) (table-targets e)]
                  [(paragraph? e) (para-targets e)]
                  [(itemization? e)
                   (append-map flow-targets (itemization-blockss e))]
                  [(nested-flow? e)
                   (append-map block-targets (nested-flow-blocks e))]
                  [(compound-paragraph? e)
                   (append-map block-targets (compound-paragraph-blocks e))]
                  [(delayed-block? e) null]
                  [(traverse-block? e) (block-targets (traverse-block-block e ri))]))
          (define (para-targets para)
            (let loop ([a (paragraph-content para)])
              (cond
                [(list? a) (append-map loop a)]
                [(toc-target-element? a) (list a)]
                [(toc-element? a) (list a)]
                [(element? a) (loop (element-content a))]
                [(delayed-element? a) (loop (delayed-element-content a ri))]
                [(traverse-element? a) (loop (traverse-element-content a ri))]
                [(part-relative-element? a) (loop (part-relative-element-content a ri))]
                [else null])))
          (define  (table-targets table)
            (append-map
             (lambda (blocks)
               (append-map (lambda (f) (if (eq? f 'cont) null (block-targets f)))
                           blocks))
             (table-blockss table)))
          (define ps
            ((if (or (nearly-top? d) (eq? d top)) values (lambda (p) (if (pair? p) (cdr p) null)))
             (let flatten ([d d] [prefixes null] [top? #t])
               (let ([prefixes (if (and (not top?) (part-tag-prefix d))
                                   (cons (part-tag-prefix d) prefixes)
                                   prefixes)])
                 (append*
                  ;; don't include the section if it's in the TOC
                  (if (or (nearly-top? d) 
                          (part-style? d 'toc-hidden))
                      null 
                      (list (cons d prefixes)))
                  ;; get internal targets:
                  (map (lambda (v) (cons v prefixes)) (append-map block-targets (part-blocks d)))
                  (map (lambda (p) (if (or (part-whole-page? p ri) 
                                           (and (part-style? p 'toc-hidden)
                                                (all-toc-hidden? p)))
                                       null
                                       (flatten p prefixes #f)))
                       (part-parts d)))))))
          (define any-parts? (ormap (compose part? car) ps))
          (if (null? ps)
            null
            `((div ([class ,box-class])
                ,@(get-onthispage-label)
                (table ([class "tocsublist"] [cellspacing "0"])
                  ,@(map (lambda (p)
                           (let ([p (car p)]
                                 [prefixes (cdr p)]
                                 [add-tag-prefixes
                                  (lambda (t prefixes)
                                    (if (null? prefixes)
                                        t
                                        (cons (car t) (append prefixes (cdr t)))))])
                             `(tr
                               (td
                                ,@(if (part? p)
                                      `((span ([class "tocsublinknumber"])
                                              ,@(format-number
                                                 (collected-info-number
                                                  (part-collected-info p ri))
                                                 '((tt nbsp)))))
                                      '(""))
                                ,@(if (toc-element? p)
                                      (render-content (toc-element-toc-content p)
                                                      d ri)
                                      (parameterize ([current-no-links #t]
                                                     [extra-breaking? #t])
                                        `((a ([href
                                               ,(format
                                                 "#~a"
                                                 (uri-unreserved-encode
                                                  (anchor-name
                                                   (add-tag-prefixes
                                                    (tag-key (if (part? p)
                                                                 (car (part-tags/nonempty p))
                                                                 (target-element-tag p))
                                                             ri)
                                                    prefixes))))]
                                              [class
                                                  ,(cond
                                                    [(part? p) "tocsubseclink"]
                                                    [any-parts? "tocsubnonseclink"]
                                                    [else "tocsublink"])]
                                              [data-pltdoc "x"])
                                             ,@(render-content
                                                (if (part? p)
                                                    (or (part-title-content p)
                                                        "???")
                                                    (if (toc-target2-element? p)
                                                        (toc-target2-element-toc-content p)
                                                        (element-content p)))
                                                d ri)))))))))
                         ps)))))))

    (define/private (extract-inherited d ri pred extract)
      (or (ormap (lambda (v)
                   (and (pred v)
                        (extract v)))
                 (style-properties (part-style d)))
          (let ([p (part-parent d ri)])
            (and p (extract-inherited p ri pred extract)))))

    (define/public (extract-part-body-id d ri)
      (extract-inherited d ri body-id? body-id-value))

    (define/public (extract-part-source d ri)
      (extract-inherited d ri document-source? document-source-module-path))

    (define/public (part-nesting-depth d ri)
      0)

    (define/public (render-one-part d ri fn number)
      (parameterize ([current-output-file fn])
        (let* ([defaults (let loop ([d d])
                           (or (ormap (lambda (v) (and (html-defaults? v) v))
                                      (style-properties (part-style d)))
                               (let ([p (part-parent d ri)])
                                 (and p (loop p)))))]
               [prefix-file (or prefix-file 
                                (and defaults
                                     (let ([v (html-defaults-prefix-path defaults)])
                                       (if (bytes? v)
                                           v
                                           (collects-relative->path v))))
                                scribble-prefix-html)]
               [style-file (or style-file 
                               (and defaults
                                    (let ([v (html-defaults-style-path defaults)])
                                      (if (bytes? v)
                                          v
                                          (collects-relative->path v))))
                               scribble-style-css)]
               [script-file (or script-file scribble-js)]
               [title (cond [(part-title-content d)
                             => (lambda (c)
                                  `(title ,@(format-number number '(nbsp))
                                          ,(content->string (strip-aux c) this d ri)))]
                            [else `(title)])]
               [dir-depth (part-nesting-depth d ri)]
               [extract (lambda (pred get) (extract-part-style-files 
                                            d
                                            ri
                                            (lambda (p) (part-whole-page? p ri))
                                            pred
                                            get))])
          (unless (bytes? style-file)
            (unless (lookup-path style-file alt-paths) 
              (install-file style-file)))
          (unless (lookup-path scribble-css alt-paths) 
            (install-file scribble-css))
          (unless (lookup-path script-file alt-paths) 
            (install-file script-file))
          (if (bytes? prefix-file)
              (display prefix-file)
              (call-with-input-file*
               prefix-file
               (lambda (in)
                 (copy-port in (current-output-port)))))
          (parameterize ([xml:empty-tag-shorthand xml:html-empty-tags])
            (xml:write-xexpr
              `(html ,(style->attribs (part-style d))
                 (head ()
                   (meta ([http-equiv "content-type"]
                          [content "text/html; charset=utf-8"]))
                   ,title
                   ,(scribble-css-contents scribble-css
                                           (lookup-path scribble-css alt-paths) 
                                           dir-depth)
                   ,@(map (lambda (style-file)
                            (if (or (bytes? style-file) (url? style-file))
                                (scribble-css-contents style-file #f dir-depth)
                                (let ([p (lookup-path style-file alt-paths)])
                                  (unless p (install-file style-file))
                                  (scribble-css-contents style-file p dir-depth))))
                          (append (extract css-addition? css-addition-path)
                                  (list style-file)
                                  (extract css-style-addition? css-style-addition-path)
                                  style-extra-files))
                   ,(scribble-js-contents script-file
                                          (lookup-path script-file alt-paths)
                                          dir-depth)
                   ,@(map (lambda (script-file)
                            (if (or (bytes? script-file) (url? script-file))
                                (scribble-js-contents script-file #f dir-depth)
                                (let ([p (lookup-path script-file alt-paths)])
                                  (unless p (install-file script-file))
                                  (scribble-js-contents script-file p dir-depth))))
                          (append
                           (extract js-addition? js-addition-path)
                           (extract js-style-addition? js-style-addition-path)
                           (reverse extra-script-files)))
                   ,(xml:comment "[if IE 6]><style type=\"text/css\">.SIEHidden { overflow: hidden; }</style><![endif]")
                   ,@(for/list ([p (style-properties (part-style d))]
                                #:when (head-extra? p))
                       (head-extra-xexpr p)))
                 (body ([id ,(or (extract-part-body-id d ri)
                                 "scribble-racket-lang-org")])
                   ,@(render-toc-view d ri)
                   (div ([class "maincolumn"])
                     (div ([class "main"])
                       ,@(parameterize ([current-version (extract-version d)])
                           (render-version d ri))
                       ,@(navigation d ri #t)
                       ,@(render-part d ri)
                       ,@(navigation d ri #f)))
                   (div ([id "contextindicator"]) nbsp))))))))

    (define (toc-part? d ri)
      (and (part-style? d 'toc)
           ;; topmost part doesn't count as toc, since it
           (part-parent d ri)))

    (define/private (find-siblings d ri)
      (let ([parent (collected-info-parent (part-collected-info d ri))])
        (let loop ([l (cond
                        [parent (part-parts parent)]
                        [(or (null? (part-parts d))
                             (not (part-whole-page? (car (part-parts d)) ri)))
                         (list d)]
                        [else (list d (car (part-parts d)))])]
                   [prev #f])
          (if (eq? (car l) d)
            (values prev (and (pair? (cdr l)) (cadr l)))
            (loop (cdr l) (car l))))))

    (define top-content      "top")
    (define contents-content "contents")
    (define index-content    "index")
    (define prev-content     '(larr " prev"))
    (define up-content       "up")
    (define next-content     '("next " rarr))
    (define sep-element      '(nbsp nbsp))

    (define/public (derive-filename d ci ri depth) "bad.html")

    (define/public (include-navigation?) search-box?)

    (define/private (navigation d ri top?)
      (define parent (part-parent d ri))
      (define-values (prev0 next0) (find-siblings d ri))
      (define prev
        (if prev0
          (let loop ([p prev0])
            (if (and (toc-part? p ri) (pair? (part-parts p)))
              (loop (last (part-parts p)))
              p))
          (and parent (toc-part? parent ri) parent)))
      (define next
        (cond [(and (toc-part? d ri) (pair? (part-parts d))) (car (part-parts d))]
              [(not next0)
               (let loop ([p parent])
                 (and p 
                      (toc-part? p ri)
                      (let-values ([(prev next) (find-siblings p ri)])
                        (or next
                            (loop (part-parent p ri))))))]
              [else next0]))
      (define index
        (let loop ([d d])
          (let ([p (part-parent d ri)])
            (if p
              (loop p)
              (let ([subs (part-parts d)])
                (and (pair? subs)
                     (let ([d (last subs)])
                       (and (eq? (style-name (part-style d)) 'index)
                            d))))))))
      (define (render . content)
        (render-content (filter values content) d ri))
      (define (titled-url label x #:title-from [tfrom #f] . more)
        (define-values (url title)
          (cond [(part? x)
                 (values
                  (dest->url (resolve-get x ri (car (part-tags/nonempty x))))
                  (string-append
                   "\""
                   (content->string
                    (append (format-number (collected-info-number
                                            (part-collected-info x ri))
                                           '(" "))
                            (part-title-content x)))
                   "\""))]
                [(equal? x "index.html") (values x "the manual top")]
                [(equal? x "../index.html") (values x "the documentation top")]
                [(string? x) (values x #f)]
                [(path? x) (values (url->string* (path->url x)) #f)]
                [else (error 'navigation "internal error ~e" x)]))
        (define title*
          (if (and tfrom (part? tfrom))
            (string-append
             "\"" (content->string (part-title-content tfrom)) "\"")
            title))
        (make-style
         #f
         (list
          (make-target-url (if (equal? url "")
                               "#"
                               url))
          (make-attributes
           `([title . ,(if title* (string-append label " to " title*) label)]
             [data-pltdoc . "x"]
             ,@more)))))
      (define top-link
        (titled-url
         "up" (if (path? up-path)
                  (url->string* (path->url up-path))
                  "../index.html")
         `[onclick . ,(format "return GotoPLTRoot(\"~a\");" (version))]))
      (define navleft
        `(span ([class "navleft"])
           ,@(if search-box?
                 (list (if up-path search-box top-search-box))
                 (list `(div ([class "nosearchform"]))))
           ,@(render
              sep-element
              (and up-path (make-element top-link top-content))
              ;; sep-element
              ;; (make-element
              ;;  (if parent (make-target-url "index.html" #f) "nonavigation")
              ;;  contents-content)
              ;; sep-element
              ;; (if (or (not index) (eq? d index))
              ;;   (make-element "nonavigation" index-content)
              ;;   (make-link-element #f index-content (car (part-tags/nonempty index))))
              )))
      (define navright
        (if (not (or parent up-path next))
          ""
          `(span ([class "navright"])
             ,@(render
                ;; put space here for text browsers and to avoid an Opera issue
                sep-element
                (make-element
                 (cond [(not parent) "nonavigation"]
                       [prev (titled-url "backward" prev)]
                       [else (titled-url "backward" "index.html"
                                         #:title-from
                                         (and (part? parent) parent))])
                 prev-content)
                sep-element
                (make-element
                 (cond
                   [(and (part? parent) (toc-part? parent ri)
                         (part-parent parent ri))
                    (titled-url "up" parent)]
                   [parent (titled-url "up" "index.html" #:title-from parent)]
                   ;; up-path = #t => go up to the start page, using
                   ;; cookies to get to the user's version of it (see
                   ;; scribblings/main/private/utils for the code that
                   ;; creates these cookies.)
                   [(eq? #t up-path) top-link]
                   [up-path (titled-url "up" up-path)]
                   [else "nonavigation"])
                 up-content)
                sep-element
                (make-element
                 (if next (titled-url "forward" next) "nonavigation")
                 next-content)))))
      (define navbar
        `(div ([class ,(if top? "navsettop" "navsetbottom")])
           ,navleft ,navright nbsp)) ; need nbsp to make the navset bg visible
      (if (include-navigation?)
          (list navbar)
          null))

    (define/override (render-one d ri fn)
      (render-one-part d ri fn null))

    (define/public (render-version d ri)
      (let ([v (current-version)])
        (if (equal? v "")
            ;; don't show empty version:
            null
            ;; show version:
            `((div ([class "versionbox"])
                   ,@(render-content
                      (list (make-element (if (include-navigation?)
                                              "version"
                                              "versionNoNav")
                                          v))
                      d
                      ri))))))

    (define/public (extract-render-convertible-as d)
      (for/or ([v (in-list (style-properties (part-style d)))])
        (and (render-convertible-as? v)
             (render-convertible-as-types v))))
    
    (define/override (render-part-content d ri)
      (parameterize ([current-render-convertible-requests (or (extract-render-convertible-as d) 
                                                              (current-render-convertible-requests))])
        (let ([number (collected-info-number (part-collected-info d ri))])
          `(,@(let ([pres (extract-pretitle d)])
                (append-map (lambda (pre)
                              (do-render-paragraph pre d ri #f #t))
                            pres))
            ,@(cond
                [(and (not (part-title-content d)) (null? number)) null]
                [(part-style? d 'hidden)
                 (map (lambda (t)
                        `(a ((name ,(format "~a" (anchor-name 
                                                  (add-current-tag-prefix
                                                   (tag-key t ri))))))))
                      (part-tags d))]
                [else `((,(case (number-depth number)
                            [(0) 'h2]
                            [(1) 'h3]
                            [(2) 'h4]
                            [else 'h5])
                         ,(let ([src (extract-part-source d ri)]
                                [taglet (for/or ([t (in-list (part-tags d))])
                                          (and (pair? t)
                                               (eq? 'part (car t))
                                               (= 2 (length t))
                                               (cadr t)))])
                            (if (and src taglet)
                                `([x-source-module ,(format "~s" src)]
                                  [x-part-tag ,(format "~s" taglet)])
                                '()))
                         ,@(format-number number '((tt nbsp)))
                         ,@(map (lambda (t)
                                  `(a ([name ,(format "~a" (anchor-name
                                                            (add-current-tag-prefix
                                                             (tag-key t ri))))])))
                                (part-tags d))
                         ,@(if (part-title-content d)
                               (render-content (part-title-content d) d ri)
                               null)))])
            ,@(let ([auths (extract-authors d)])
                (if (null? auths)
                    null
                    `((div ([class "SAuthorListBox"])
                           (span ([class "SAuthorList"])
                                 ,@(apply
                                    append
                                    (for/list ([auth (in-list auths)]
                                               [pos (in-naturals)])
                                      (let ([v (do-render-paragraph auth d ri #f #t)])
                                        (if (zero? pos)
                                            v
                                            (cons '(span ([class "SAuthorSep"]) (br)) v))))))))))
            ,@(render-flow* (part-blocks d) d ri #f #f)
            ,@(let loop ([pos 1]
                         [secs (part-parts d)])
                (if (null? secs)
                    null
                    (append (render-part (car secs) ri)
                            (loop (add1 pos) (cdr secs)))))))))

    (define/private (render-flow* p part ri starting-item? special-last?)
      ;; Wrap each table with <p>, except for a trailing table
      ;;  when `special-last?' is #t
      (let loop ([f p] [starting-item? starting-item?])
        (cond
          [(null? f) null]
          [(and (table? (car f))
                (or (not special-last?) (not (null? (cdr f)))))
           (cons `(p ,@(render-block (car f) part ri starting-item?))
                 (loop (cdr f) #f))]
          [else (append (render-block (car f) part ri starting-item?)
                        (loop (cdr f) #f))])))

    (define/override (render-flow p part ri starting-item?)
      (render-flow* p part ri starting-item? #t))

    (define/private (do-render-paragraph p part ri flatten-unstyled? show-pre?)
      (let* ([contents (super render-paragraph p part ri)]
             [style (paragraph-style p)]
             [attrs (style->attribs style)])
        (if (and (not show-pre?)
                 (or (eq? (style-name style) 'author)
                     (eq? (style-name style) 'pretitle)))
            null
            (if (and flatten-unstyled?
                     (not (style-name style))
                     (null? attrs))
                contents
                `((,(or (style->tag style)
                        (if (memq 'div (style-properties style)) 
                            'div 
                            'p))
                   [,@(combine-class
                       (case (style-name style)
                         [(author) '([class "author"])]
                         [(pretitle) '([class "SPretitle"])]
                         [(wraps) null]
                         [else null])
                       attrs)]
                   ,@contents))))))

    (define/override (render-paragraph p part ri)
      (do-render-paragraph p part ri #f #f))

    (define/override (render-intrapara-block p part ri first? last? starting-item?)
      `((div ([class "SIntrapara"])
             ,@(cond
                [(paragraph? p) (do-render-paragraph p part ri #t #f)]
                [else (render-block p part ri starting-item?)]))))

    (define/private (content-style e)
      (cond
       [(element? e) (element-style e)]
       [(multiarg-element? e) (multiarg-element-style e)]
       [else #f]))

    (define/private (content-attribs e [extras null])
      (let ([s (content-style e)])
          (if (style? s)
              (element-style->attribs (style-name s) s extras)
              (element-style->attribs s #f extras))))

    (define/override (render-content e part ri)
      (define (attribs [extras null]) (content-attribs e extras))
      (cond
        [(string? e) (super render-content e part ri)] ; short-cut for common case
        [(list? e) (super render-content e part ri)] ; also a short-cut
        [(and (convertible? e)
              (render-as-convertible e (current-render-convertible-requests)))
         => values]
        [(image-element? e)
         (let* ([src (collects-relative->path (image-element-path e))]
                [suffixes (image-element-suffixes e)]
                [scale (image-element-scale e)]
                [to-scaled-num
                 (lambda (s)
                   (number->string
                    (inexact->exact
                     (floor (* scale (if (number? s)
                                         s
                                         (integer-bytes->integer s #f #t)))))))]
                [src (select-suffix src suffixes '(".png" ".gif" ".svg"))]
                [svg? (regexp-match? #rx#"[.]svg$" (if (path? src) (path->bytes src) src))]
                [sz (cond
                     [svg?
                      (call-with-input-file*
                       src
                       (lambda (in)
                         (with-handlers ([exn:fail? (lambda (exn) 
                                                      (log-warning
                                                       (format "warning: error while reading SVG file for size: ~a"
                                                               (if (exn? exn)
                                                                   (exn-message exn)
                                                                   (format "~e" exn))))
                                                      null)])
                           (let* ([d (xml:read-xml in)]
                                  [attribs (xml:element-attributes 
                                            (xml:document-element d))]
                                  [check-name (lambda (n)
                                                (lambda (a)
                                                  (and (eq? n (xml:attribute-name a))
                                                       (xml:attribute-value a))))]
                                  [w (ormap (check-name 'width) attribs)]
                                  [h (ormap (check-name 'height) attribs)])
                             (if (and w h)
                                 `([width ,w][height ,h])
                                 null)))))]
                     [else
                      ;; Try to extract file size:
                      (call-with-input-file*
                       src
                       (lambda (in)
                         (cond
                          [(regexp-try-match #px#"^\211PNG.{12}" in)
                           `([width ,(to-scaled-num (read-bytes 4 in))]
                             [height ,(to-scaled-num (read-bytes 4 in))])]
                          [(regexp-try-match #px#"^(?=GIF8)" in)
                           (define-values (w h rows) (gif->rgba-rows in))
                           `([width ,(to-scaled-num w)]
                             [height ,(to-scaled-num h)])]
                          [else
                           null])))])])
           (let ([srcref (let ([p (install-file src)])
                           (if (path? p)
                               (url->string* (path->url (path->complete-path p)))
                               p))])
             `((,(if svg? 'object 'img)
                ([,(if svg? 'data 'src) ,srcref]
                 [alt ,(content->string (element-content e))]
                 ,@(if svg?
                       `([type "image/svg+xml"])
                       null)
                 ,@sz
                 ,@(attribs))
                ,@(if svg? 
                      `((param ([name "src"] [value ,srcref])))
                      null)))))]
        [(and (or (element? e) (multiarg-element? e))
              (ormap (lambda (v) (and (script-property? v) v))
                     (let ([s (if (element? e)
                                  (element-style e)
                                  (multiarg-element-style e))])
                       (if (style? s) (style-properties s) null))))
         =>
         (lambda (v)
           (let* ([t `[type ,(script-property-type v)]]
                  [s (script-property-script v)]
                  [s (if (list? s)
                         `(script (,t ,@(attribs)) ,(apply as-literal `("\n" ,@s "\n")))
                         `(script (,t ,@(attribs) [src ,s])))])
             (list s
                   `(noscript ,@(render-plain-content e part ri)))))]
        [(target-element? e)
         `((a ([name ,(format "~a" (anchor-name (add-current-tag-prefix
                                                 (tag-key (target-element-tag e)
                                                          ri))))]
               ,@(attribs)))
           ,@(render-content (element-content e) part ri))]
        [(and (link-element? e) (not (current-no-links)))
         (parameterize ([current-no-links #t])
           (define indirect-link? (link-element-indirect? e))
           (let-values ([(dest ext-id)
                         (if (and indirect-link?
                                  external-tag-path)
                             (values #f #f)
                             (resolve-get/ext-id part ri (link-element-tag e)))])
             (if (or indirect-link? dest)
                 `((a ([href
                      ,(cond
                        [(and ext-id external-root-url
                              (let ([rel (find-relative-path
                                          (find-doc-dir)
                                          (relative->path (dest-path dest)))])
                                (and (relative-path? rel)
                                     rel)))
                         => (lambda (rel)
                              (url->string*
                               (struct-copy
                                url
                                (combine-url/relative
                                 (string->url external-root-url)
                                 (string-join (map (lambda (s)
                                                     (case s
                                                       [(up) ".."]
                                                       [(same) "."]
                                                       [else (path-element->string s)]))
                                                   (explode-path rel))
                                              "/"))
                                [fragment
                                 (and (not (dest-page? dest))
                                      (anchor-name (dest-anchor dest)))])))]
                        [(or indirect-link?
                             (and ext-id external-tag-path))
                         ;; Redirected to search:
                         (url->string*
                          (let ([u (string->url (or external-tag-path
                                                    "http://doc.racket-lang.com/"))])
                            (struct-copy
                             url
                             u
                             [query
                              (if (string? ext-id)
                                  (list* (cons 'doc ext-id)
                                         (cons 'rel (or (dest->url-in-doc dest ext-id) "???"))
                                         (url-query u))
                                  (cons (cons 'tag (tag->query-string (link-element-tag e)))
                                        (url-query u)))])))]
                        [else
                         ;; Normal link:
                         (dest->url dest)])]
                     ,@(attribs (if (or indirect-link?
                                        (and ext-id external-tag-path))
                                    '([class "Sq"])
                                    null))
                     [data-pltdoc "x"])
                    ,@(if (empty-content? (element-content e))
                          (render-content (strip-aux (dest-title dest)) part ri)
                          (render-content (element-content e) part ri))))
               (begin
                 (when #f
                   (eprintf "Undefined link: ~s\n"
                            (tag-key (link-element-tag e) ri)))
                 `((font ([class "badlink"])
                     ,@(if (empty-content? (element-content e))
                         `(,(format "~s" (tag-key (link-element-tag e) ri)))
                         (render-plain-content e part ri))))))))]
        [else 
         (render-plain-content e part ri)]))
    
    (define/private (render-as-convertible e requests)
      (for/or ([request (in-list requests)])
        (cond
          [(case request
             [(png-bytes)
              (or (convert e 'png-bytes+bounds8)
                  (convert e 'png-bytes+bounds)
                  (convert e 'png-bytes))]
             [(png@2x-bytes)
              (or (convert e 'png@2x-bytes+bounds8)
                  (convert e 'png@2x-bytes+bounds)
                  (convert e 'png@2x-bytes))]
             [else #f])
           => 
           (lambda (cvt)
             (let* ([bstr (if (list? cvt) (first cvt) cvt)]
                    [w (if (list? cvt)
                           (list-ref cvt 1)
                           (integer-bytes->integer (subbytes bstr 16 20) #f #t))]
                    [h (if (list? cvt)
                           (list-ref cvt 2)
                           (integer-bytes->integer (subbytes bstr 20 24) #f #t))]
                    [scale (lambda (v)
                             (if (and (not (list? cvt))
                                      (equal? request 'png@2x-bytes))
                                 (/ v 2.0)
                                 v))]
                    [number->decimal-string (lambda (s)
                                              (number->string
                                               (if (integer? s)
                                                   s
                                                   (exact->inexact s))))])
               (list
                (add-padding
                 cvt
                 `(img ([src ,(install-file "pict.png" bstr)]
                        [alt "image"]
                        [width ,(number->decimal-string (scale w))]
                        [height ,(number->decimal-string (scale h))]))))))]
          [(case request
             [(svg-bytes)
              (or (convert e 'svg-bytes+bounds8)
                  (convert e 'svg-bytes))]
             [else #f])
           => (lambda (cvt)
                (let* ([bstr (if (list? cvt) (first cvt) cvt)])
                  (list
                   (add-padding
                    cvt
                    `(object
                      ([data ,(install-file "pict.svg" bstr)]
                       [type "image/svg+xml"]))))))]
          [else #f])))

    ;; Add padding for a bounding-box conversion reply:
    (define/private (add-padding cvt e)
      (define descent (and (list? cvt)
                           ((length cvt) . >= . 5)
                           (list-ref cvt 3)))
      (define padding (and (list? cvt)
                           ((length cvt) . >= . 9)
                           (take (list-tail cvt 5) 4)))
      (cond
       [(and (or (not descent)
                 (zero? descent))
             (or (not padding)
                 (andmap zero? padding)))
        e]
       [else
        ;; Descent and padding:
        (define-values (left top right bottom) (apply values padding))
        `(,(car e) ,(cons `[style ,(format "vertical-align: ~apx; margin: ~apx ~apx ~apx ~apx;"
                                           (- (- descent bottom))
                                           (- top) (- right)
                                           (- bottom) (- left))]
                          (cadr e))
          ,@(cddr e))]))

    (define/private (render-plain-content e part ri)
      (define (attribs) (content-attribs e))
      (let* ([properties (let ([s (content-style e)])
                           (if (style? s)
                               (style-properties s)
                               null))]
             [name (let ([s (content-style e)])
                     (if (style? s)
                         (style-name s)
                         s))]
             [alt-tag
              (let ([s (content-style e)])
                (and (style? s)
                     (style->tag s)))]
             [resources (for/list ([p (in-list properties)]
                                   #:when (install-resource? p))
                          (install-resource-path p))]
             [link-resource (for/or ([p (in-list properties)]
                                     #:when (link-resource? p))
                              (link-resource-path p))]
             [link? (and (or (ormap target-url? properties)
                             link-resource)
                         (not (current-no-links)))]
             [anchor? (ormap url-anchor? properties)]
             [attribs
              (append
               (if (null? properties)
                   null
                   (append-map (lambda (v)
                                 (cond
                                  [(target-url? v)
                                   (if (current-no-links)
                                       null
                                       `([href ,(let ([addr (target-url-addr v)])
                                                  (if (path? addr)
                                                      (from-root addr (get-dest-directory))
                                                      addr))]))]
                                  [else null]))
                               properties))
               (attribs))]
             [newline? (eq? name 'newline)]
             [check-render
              (lambda ()
                (when (render-element? e)
                  ((render-element-render e) this part ri)))])
        (for ([r (in-list resources)])
          (install-file r))
        (let-values ([(content) (cond
                                 [link?
                                  (parameterize ([current-no-links #t])
                                    (super render-content e part ri))]
                                 [newline? (check-render) null]
                                 [(eq? 'hspace name)
                                  (check-render)
                                  (let ([str (content->string e)])
                                    (map (lambda (c) 'nbsp) (string->list str)))]
                                 [else
                                  (super render-content e part ri)])])
          (if (and (null? attribs) 
                   (not link?)
                   (not anchor?)
                   (not newline?)
                   (not alt-tag))
              content
              `(,@(if anchor?
                      (append-map (lambda (v)
                                    (if (url-anchor? v)
                                        `((a ([name ,(url-anchor-name v)])))
                                        null))
                                  properties)
                      null)
                (,(cond
                   [alt-tag alt-tag]
                   [link? 'a]
                   [newline? 'br]
                   [else 'span]) 
                 ,(append
                   (if link-resource
                       `([href ,(install-file link-resource)])
                       null)
                   attribs)
                 ,@content))))))

    (define/private (element-style->attribs name style [extras null])
      (combine-class
       (cond
        [(symbol? name)
         (case name
           [(italic) '([style "font-style: italic"])]
           [(bold) '([style "font-weight: bold"])]
           [(tt) '([class "stt"])]
           [(roman) '([class "sroman"])]
           [(url) '([class "url"])]
           [(no-break) '([class "nobreak"])]
           [(sf) '([class "ssansserif"])]
           [(superscript) '([style "vertical-align: super; font-size: 80%"])]
           [(subscript) '([style "vertical-align: sub; font-size: 80%"])]
           [(smaller) '([class "Smaller"])]
           [(larger) '([class "Larger"])]
           [(hspace) '([class "hspace"])]
           [(newline) '()]
           [else (error 'html-render "unrecognized style symbol: ~e" name)])]
        [(string? name) (if style null `([class ,name]))]
        [else null])
       (if style
           (style->attribs style extras)
           (if (pair? extras)
               (style->attribs (make-style #f null) extras)
               null))))

    (define/override (render-table t part ri starting-item?)
      (define (make-row flows column-styles)
        `(tr
          ,@(let loop ([ds flows]
                       [column-styles column-styles]
                       [first? #t])
              (cond
               [(null? ds) null]
               [(eq? (car ds) 'cont)
                (loop (cdr ds) (cdr column-styles) first?)]
               [else
                (let ([d (car ds)] [column-style (car column-styles)])
                  (cons
                   `(td (,@(cond
                            [(not column-style) null]
                            [(memq 'right (style-properties column-style)) '([align "right"])]
                            [(memq 'left (style-properties column-style)) '([align "left"])]
                            [(memq 'center (style-properties column-style)) '([align "center"])]
                            [else null])
                         ,@(cond
                            [(not column-style) null]
                            [(memq 'top (style-properties column-style)) '([valign "top"])]
                            [(memq 'baseline (style-properties column-style)) '([valign "baseline"])]
                            [(memq 'vcenter (style-properties column-style)) '([valign "center"])]
                            [(memq 'bottom (style-properties column-style)) '([valign "bottom"])]
                            [else null])
                         ,@(if (and column-style
                                    (string? (style-name column-style)))
                               `([class ,(style-name column-style)])
                               null)
                         ,@(if (and column-style
                                    (pair? (style-properties column-style)))
                               (style->attribs (make-style
                                                #f
                                                (filter (lambda (a)
                                                          (or (attributes? a)
                                                              (color-property? a)
                                                              (background-color-property? a)))
                                                        (style-properties column-style)))
                                               (let ([ps (style-properties column-style)])
                                                 (cond
                                                  [(memq 'border ps)
                                                   `([style "border: 1px solid black;"])]
                                                  [else
                                                   (define (check sym sfx)
                                                     (if (memq sym ps)
                                                         `([style ,(format "border-~a: 1px solid black;" sfx)])
                                                         null))
                                                   (append
                                                    (check 'top-border 'top)
                                                    (check 'bottom-border 'bottom)
                                                    (check 'left-border 'left)
                                                    (check 'right-border 'right))])))
                               null)
                         ,@(if (and (pair? (cdr ds))
                                    (eq? 'cont (cadr ds)))
                               `([colspan
                                  ,(number->string
                                    (let loop ([n 2] [ds (cddr ds)])
                                      (cond [(null? ds) n]
                                            [(eq? 'cont (car ds))
                                             (loop (+ n 1) (cdr ds))]
                                            [else n])))])
                               null))
                        ,@(if (and (paragraph? d)
                                   (memq 'omitable (style-properties (paragraph-style d))))
                              (render-content (paragraph-content d) part ri)
                              (render-block d part ri #f)))
                   (loop (cdr ds) (cdr column-styles) #f)))]))))
      (define cell-styless (extract-table-cell-styles t))
      `((table ([cellspacing "0"]
                [cellpadding "0"]
                ,@(combine-class
                   (case (style-name (table-style t))
                     [(boxed)    '([class "boxed"])]
                     [(centered) '([align "center"])]
                     [else '()])
                   (style->attribs (table-style t)
                                   (append
                                    (if starting-item?
                                        '([style "display: inline-table; vertical-align: text-top;"])
                                        null)
                                    (if (for/or ([cell-styles (in-list cell-styless)])
                                          (for/or ([cell-style (in-list cell-styles)])
                                            (and cell-style
                                                 (let ([ps (style-properties cell-style)])
                                                   (or (memq 'border ps)
                                                       (memq 'left-border ps)
                                                       (memq 'right-border ps)
                                                       (memq 'bottom-border ps)
                                                       (memq 'top-border ps))))))
                                        `([style "border-collapse: collapse;"])
                                        '())))))
          ,@(let ([columns (ormap (lambda (p)
                                    (and (table-columns? p)
                                         (map (lambda (s)
                                                (ormap (lambda (a)
                                                         (and (column-attributes? a)
                                                              a))
                                                       (style-properties s)))
                                              (table-columns-styles p))))
                                  (style-properties (table-style t)))])
              (if (and columns (ormap values columns))
                  `((colgroup ,@(for/list ([col (in-list columns)])
                                  `(col ,(if col
                                             (map (lambda (v) (list (car v) (cdr v))) (column-attributes-assoc col))
                                             null)))))
                  null))
          ,@(if (null? (table-blockss t))
                `((tr (td)))
                (map make-row
                     (table-blockss t)
                     cell-styless)))))

    (define/override (render-nested-flow t part ri starting-item?)
      `((,(or (style->tag (nested-flow-style t)) 'blockquote)
         [,@(combine-class
             (cond
              [(eq? 'code-inset (style-name (nested-flow-style t)))
               `([class "SCodeFlow"])]
              [(eq? 'vertical-inset (style-name (nested-flow-style t)))
               `([class "SVInsetFlow"])]
              [(and (not (string? (style-name (nested-flow-style t))))
                    (not (eq? 'inset (style-name (nested-flow-style t)))))
               `([class "SubFlow"])]
              [else null])
             (style->attribs (nested-flow-style t)))]
         ,@(apply append
                  (super render-nested-flow t part ri starting-item?)))))

    (define/override (render-compound-paragraph t part ri starting-item?)
      (let ([style (compound-paragraph-style t)])
        `((,(or (style->tag style) 'p)
           ,(style->attribs style)
           ,@(super render-compound-paragraph t part ri starting-item?)))))

    (define/override (render-itemization t part ri)
      (let ([style-str (and (string? (style-name (itemization-style t)))
                            (style-name (itemization-style t)))])
        `((,(if (eq? 'ordered (style-name (itemization-style t)))
                'ol
                'ul)
           (,@(style->attribs (itemization-style t))
            ,@(if (eq? 'compact (style-name (itemization-style t)))
                  `([class "compact"])
                  '()))
           ,@(map (lambda (flow) `(li ,(if style-str
                                           `([class ,(string-append style-str "Item")])
                                           `())
                                      ,@(render-flow flow part ri #t)))
                  (itemization-blockss t))))))

    (define/override (render-other i part ri)
      (cond
        [(string? i)
         (let ([m (and (extra-breaking?)
                       (regexp-match-positions #rx"[-:/+_](?=.)|[a-z](?=[A-Z])" i))])
           (if m
             (list* (substring i 0 (cdar m))
                    ;; Most browsers wrap after a hyphen. The one that
                    ;; doesn't, Firefox, pays attention to wbr.  Some
                    ;; browsers ignore wbr, but at least they don't do
                    ;; strange things with it.
                    (if (equal? #\- (string-ref i (caar m)))
                        '(wbr)
                        '(span ([class "mywbr"]) " " nbsp))
                    (render-other (substring i (cdar m)) part ri))
             (ascii-ize i)))]
        [(symbol? i)
         (case i
           [(mdash) '(#x2014 (wbr))] ;; <wbr> encourages breaking after rather than before

           ;; FIXME: 'lang and 'rang do not match `&rang;' and `&lang;' in HTML 4 or 5.
           ;; Happened because of the thread:
           ;;   <http://lists.racket-lang.org/users/archive/2008-June/025126.html>
           ;; ("Fonts with proper angle brackets")
           ;;
           ;; Do we still need this?  See test page at <http://jsbin.com/okizeb/3>.
           ;; 
           ;; More background:
           ;;
           ;; HTML 4 says (in HTMLsymbol.dtd):
           ;;
           ;; <!ENTITY lang     CDATA "&#9001;" -- left-pointing angle bracket = bra,
           ;;                                      U+2329 ISOtech -->
           ;; <!-- lang is NOT the same character as U+003C 'less than'
           ;;      or U+2039 'single left-pointing angle quotation mark' -->
           ;; <!ENTITY rang     CDATA "&#9002;" -- right-pointing angle bracket = ket,
           ;;                                      U+232A ISOtech -->
           ;; <!-- rang is NOT the same character as U+003E 'greater than'
           ;;      or U+203A 'single right-pointing angle quotation mark' -->
           ;;
           ;; HTML 5 says (in <https://github.com/w3c/html/raw/4b354c25cdc7025fef9f561bbc98fee2d9d241c1/entities.json>, dated 2012-10-12):
           ;;
           ;;   "&lang;": { "codepoints": [10216], "characters": "\u27E8" },
           ;;   "&rang;": { "codepoints": [10217], "characters": "\u27E9" },
           ;;
           [(lang) '(#x2039)] ; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
           [(rang) '(#x203a)] ; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK

           [else (list i)])]
        [else 
         (log-error (format "Unrecognized element in content: ~e" i))
         (list (format "~s" i))]))
    
    (define/private (ascii-ize s)
      (if (= (string-utf-8-length s) (string-length s))
          (list s)
          (let ([m (regexp-match-positions #rx"[^\u01-\u7E]" s)])
            (if m
                (append (ascii-ize (substring s 0 (caar m)))
                        (list (char->integer (string-ref s (caar m))))
                        (ascii-ize (substring s (cdar m))))
                (list s)))))

    ;; ----------------------------------------

    (super-new)))

;; ----------------------------------------
;; multi-file output

(define (render-multi-mixin %)
  (class %
    (inherit render-one
             render-one-part
             render-content
             part-whole-page?
             format-number
             install-extra-files
             report-output?
             all-toc-hidden?)

    (define directory-depth 1)
    (define/override (set-directory-depth n)
      (set! directory-depth (max 1 n)))

    (define/override (get-suffix) #"")

    (define/override (get-dest-directory [create? #f])
      (or (and (current-subdirectory)
               (let ([d (build-path (or (super get-dest-directory)
                                        (current-directory))
                                    (current-subdirectory))])
                 (when (and create? (not (directory-exists? d)))
                   (make-directory* d))
                 d))
          (super get-dest-directory create?)))

    (define/private (append-part-prefixes d ci ri)
      (let ([parents (drop-right
                      (if ci
                          (cons d (collect-info-parents ci))
                          (let loop ([d d])
                            (if d
                                (cons d
                                      (loop (collected-info-parent (part-collected-info d ri))))
                                null)))
                      1)])
        (apply
         string-append
         (for/list ([p (in-list parents)])
           (or (part-tag-prefix p) "")))))

    (define/override (part-nesting-depth d ri)
      (min (part-depth d ri) (sub1 directory-depth)))

    (define/private (part-depth d ri)
      (define p (collected-info-parent (part-collected-info d ri)))
      (if (not p)
          0
          (add1 (part-depth p ri))))

    (define/override (derive-filename d ci ri depth)
      (let ([base (regexp-replace*
                   "[^-a-zA-Z0-9_=]"
                   (string-append
                    (append-part-prefixes d ci ri)
                    (let ([s (cadr (car (part-tags/nonempty d)))])
                      (cond [(string? s) s]
                            [(part-title-content d)
                             (content->string (part-title-content d))]
                            [else
                             ;; last-ditch effort to make up a unique name:
                             (format "???~a" (eq-hash-code d))])))
                   "_")])
        (let ([fn (if (depth . < . directory-depth)
                      (path->string (build-path base "index.html"))
                      (format "~a.html" base))])
          (when ((string-length fn) . >= . 48)
            (error "file name too long (need a tag):" fn))
          fn)))

    (define/override (include-navigation?) #t)

    (define/override (collect ds fns fp [demand (lambda (key ci) #f)])
      (super collect 
             ds 
             (map (lambda (fn) (build-path fn "index.html")) fns)
             fp
             demand))

    (define/override (current-part-whole-page? d)
      (collecting-whole-page))

    (define/override (start-collect ds fns ci)
      (parameterize ([current-part-files (make-hash)])
        (map (lambda (d fn)
               (parameterize ([collecting-sub
                               (if (part-style? d 'non-toc)
                                   1
                                   0)])
                 (super start-collect (list d) (list fn) ci)))
             ds
             fns)))

    (define/private (check-duplicate-filename orig-s)
      (let ([s (string-downcase (path->string orig-s))])
        (when (hash-ref (current-part-files) s #f)
          (error 'htmls-render "multiple parts have the same filename (modulo case): ~e"
                 orig-s))
        (hash-set! (current-part-files) s #t)))

    (define/override (collect-part d parent ci number sub-init-number)
      (let ([prev-sub (collecting-sub)])
        (parameterize ([collecting-sub (if (part-style? d 'toc)
                                           1 
                                           (add1 prev-sub))]
                       [collecting-whole-page (prev-sub . <= . 1)])
          (if (and (current-part-whole-page? d)
                   (not (eq? d (current-top-part))))
              (let* ([filename (derive-filename d ci #f (length number))]
                     [full-filename (build-path (path-only (current-output-file))
                                                filename)])
                (make-directory* (path-only full-filename))
                (check-duplicate-filename full-filename)
                (parameterize ([current-output-file full-filename])
                  (super collect-part d parent ci number sub-init-number)))
            (super collect-part d parent ci number sub-init-number)))))

    (define/override (render-top ds fns ri)
      (map (lambda (d fn)
             (when (report-output?)
               (printf " [Output to ~a/index.html]\n" fn))
             (unless (directory-exists? fn)
               (make-directory fn))
             (parameterize ([current-subdirectory (file-name-from-path fn)]
                            [current-top-part d])
               ;; install files for each directory
               (install-extra-files ds)
               (let ([fn (build-path fn "index.html")])
                 (with-output-to-file/clean
                  fn
                  (lambda () (render-one d ri fn))))))
           ds
           fns))

    (define/override (nearly-top? d ri top)
      (eq? top (collected-info-parent (part-collected-info d ri))))

    (define/override (get-onthispage-label)
      `((div ([class "tocsubtitle"]) "On this page:")))

    (define/override (toc-wrap p)
      (list p))

    (inherit render-table
             render-paragraph
             extract-version)

    (define/override (render-part d ri)
      (parameterize ([current-version (extract-version d)])
        (let ([number (collected-info-number (part-collected-info d ri))])
          (if (and (on-separate-page-ok)
                   (part-whole-page? d ri)
                   (not (eq? d (current-top-part))))
            ;; Put the actual content in a new file:
            (let* ([filename (derive-filename d #f ri (part-depth d ri))]
                   [full-path (build-path (path-only (current-output-file))
                                          filename)])
              (parameterize ([on-separate-page-ok #f]
                             [current-subdirectory (let ([p (path-only filename)])
                                                     (if p
                                                         (build-path (current-subdirectory) p)
                                                         (current-subdirectory)))])
                (with-output-to-file/clean
                 full-path
                 (lambda () (render-one-part d ri full-path number)))
                null))
            (parameterize ([on-separate-page-ok #t])
              ;; Normal section render
              (super render-part d ri))))))

    (super-new)))

;; ----------------------------------------
;; utils

(define (explode p) (explode-path p))

(define in-plt?
  (let ([roots (map explode (filter values (list (find-doc-dir) (find-collects-dir))))])
    (lambda (path)
      (for/or ([root (in-list roots)])
        (let loop ([path  path] [root root])
          (or (null? root)
              (and (pair? path)
                   (equal? (car path) (car root))
                   (loop (cdr path) (cdr root)))))))))

(define (from-root p d)
  (define c-p (path->complete-path p))
  (define e-p (explode c-p))
  (define e-d (and d (explode (path->complete-path d))))
  (define p-in? (in-plt? e-p))
  (define d-in? (and d (in-plt? e-d)))
  ;; use an absolute link if the link is from outside the plt tree
  ;; going in (or if d is #f)
  (if (not (and d (cond
                    [(equal? p-in? d-in?) #t]
                    [d-in? (error 'from-root
                                  "got a link from the PLT tree going out; ~e"
                                  p)]
                    [else #f])))
    (path->url-string c-p)
    (let loop ([e-d e-d] [e-p e-p])
      (cond
        [(null? e-d)
         (string-append*
          (let loop ([e-p e-p])
            (cond [(null? e-p) '("/")]
                  [(null? (cdr e-p)) (list (path->string (car e-p)))]
                  [(eq? 'same (car e-p)) (loop (cdr e-p))]
                  [(eq? 'up (car e-p)) (cons "../" (loop (cdr e-p)))]
                  [else (cons (path->string (car e-p)) (cons "/" (loop (cdr e-p))))])))]
        [(equal? (car e-d) (car e-p)) (loop (cdr e-d) (cdr e-p))]
        [(eq? 'same (car e-d)) (loop (cdr e-d) e-p)]
        [(eq? 'same (car e-p)) (loop e-d (cdr e-p))]
        [else (string-append (string-append* (map (lambda (x) "../") e-d))
                             (loop null e-p))]))))

(define (path->url-string p)
  (if (eq? 'unix (path-convention-type p))
      (let ([p (simplify-path p #f)])
        (if (regexp-match? #rx#"^[-a-zA-Z0-9_/.]*$" (path->bytes p))
            (string-append "file://" (path->string p))
            (url->string (path->url p))))
      (url->string (path->url p))))

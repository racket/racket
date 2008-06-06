#lang scheme/base

(require "struct.ss"
         scheme/class
         scheme/path
         scheme/file
         scheme/list
         scheme/string
         mzlib/runtime-path
         setup/main-doc
         setup/main-collects
         setup/dirs
         net/url
         net/base64
         scheme/serialize
         (prefix-in xml: xml/xml)
         (for-syntax scheme/base)
         "search.ss"
         "basic.ss")
(provide render-mixin
         render-multi-mixin)

(xml:empty-tag-shorthand xml:html-empty-tags)

(define literal
  (let ([loc (xml:make-location 0 0 0)])
    (lambda strings (xml:make-cdata loc loc (string-append* strings)))))
(define (ref-style path)
  `(link ([rel "stylesheet"] [type "text/css"] [href ,path] [title "default"])))
(define (inlined-style . body)
  `(style ([type "text/css"])
     ,(apply literal
             `("\n"
               ,@(map (lambda (x) (if (string? x) x (format "~a" x))) body)
               "\n"))))
(define (ref-script path)
  `(script ([type "text/javascript"] [src ,path])))
(define (inlined-script . body)
  `(script ([type "text/javascript"])
     ,(apply literal
             `("\n"
               ,@(map (lambda (x) (if (string? x) x (format "~a" x))) body)
               "\n"))))

(define-runtime-path scribble-css "scribble.css")
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
         [file-getter
          (lambda (default-file make-inline make-ref)
            (let ([c #f])
              (lambda (file path)
                (cond [(not (eq? 'inline path))
                       (make-ref (or path (let-values ([(base name dir?)
                                                        (split-path file)])
                                            (path->string name))))]
                      [(or (not file) (equal? file default-file))
                       (unless c
                         (set! c (make-inline (read-file default-file))))
                       c]
                      [else (make-inline (read-file file))]))))])
    (values (file-getter scribble-css inlined-style  ref-style)
            (file-getter scribble-js  inlined-script ref-script))))

(define current-subdirectory (make-parameter #f))
(define current-output-file (make-parameter #f))
(define current-top-part (make-parameter #f))
(define on-separate-page (make-parameter #t))
(define next-separate-page (make-parameter #f))
(define collecting-sub (make-parameter 0))
(define current-no-links (make-parameter #f))
(define extra-breaking? (make-parameter #f))
(define current-version (make-parameter (version)))

(define (toc-part? d)
  (part-style? d 'toc))

;; HTML anchors should be case-insensitively unique. To make them
;;  distinct, add a "." in front of capital letters.  Also clean up
;;  characters that give browers trouble (i.e., the ones that are not
;;  allowed as-in in URI codecs) by using "~" followed by a hex
;;  encoding.  (The idea is that the result is still readable, so the
;;  link can be used as a rough indication of where you'll get to.)
(define (anchor-name v)
  (define (encode-byte b)
    (string-append (if (< b 16) "~0" "~") (number->string b 16)))
  (define (encode-str str)
    (if (regexp-match? #px"[^[:ascii:]]" str)
      (string-append* (map encode-byte (bytes->list (string->bytes/utf-8 str))))
      (encode-byte (char->integer (string-ref str 0)))))
  (if (literal-anchor? v)
    (literal-anchor-string v)
    (let* ([v (format "~a" v)]
           [v (regexp-replace* #rx"[A-Z.]" v ".&")]
           [v (regexp-replace* #rx" " v "._")]
           [v (regexp-replace* #rx"\"" v ".'")]
           [v (regexp-replace* #rx"[^-a-zA-Z0-9_!+*'()/.,]" v encode-str)])
      v)))

(define-serializable-struct literal-anchor (string))

#; ; no need for these index-local searches
#reader scribble/reader (begin ; easier to format

(define search-script
  @inlined-script{
    var search_nodes = null;
    var last_search_terms = null;
    function node_to_text(node) {
      if (node.nodeType == 3) return node.nodeValue;
      var r = "";
      var children = node.childNodes;
      for (var i=0@";" i<children.length@";" i++) {
        r = r + node_to_text(children[i]);
      }
      return r;
    }
    var search_box = null;
    function initialize_search() {
      var all_links = document.getElementsByTagName("a");
      search_nodes = new Array();
      for (var i=0@";" i<all_links.length@";" i++)
        if (all_links[i].className == "indexlink") {
          all_links[i].flat_text = node_to_text(all_links[i]).toLowerCase();
          search_nodes.push(all_links[i]);
        }
      search_box = document.getElementById("search_box");
      if (location.search.length > 0) {
        var paramstrs = location.search.substring(1).split(/[@";"&]/);
        for (var i in paramstrs) {
          var param = paramstrs[i].split(/=/);
          if (param.length == 2 && param[0] == "q") {
            search_box.value = unescape(param[1]).replace(/\+/g," ");
            break;
          }
        }
      }
      if (search_box.value != "") do_search(search_box.value);
      search_box.focus();
      search_box.select();
    }
    window.onload = initialize_search;
    function do_search(terms) {
      terms = terms.toLowerCase();
      if (terms == last_search_terms) return;
      last_search_terms = terms;
      terms = terms.split(/ +/);
      var none = true;
      for (var i=0@";" i<search_nodes.length@";" i++) {
        var show = true, curtext = search_nodes[i].flat_text;
        for (var j=0@";" j<terms.length@";" j++) {
          if (terms[j] != "" && curtext.indexOf(terms[j]) < 0) {
            show = false;
            break;
          }
        }
        if (show) none = false;
        var style = search_nodes[i].style;
        var newdisp = show ? "block" : "none";
        if (newdisp != style.display) style.display = newdisp;
      }
      search_box.style.backgroundColor = none ? "#ffe0e0" : "white";
    }
    var search_timer = null;
    function delayed_search(str, event) {
      if (event && event.keyCode == 13) {
        do_search(str);
      } else {
        if (search_timer != null) {
          var t = search_timer;
          search_timer = null;
          clearTimeout(t);
        }
        search_timer = setTimeout(function(){do_search(str)@";"}, 1000);
      }
    }})

(define search-field
  @`p{Search: @(input ([type "text"] [id "search_box"]
                       [onchange "delayed_search(this.value,event);"]
                       [onkeyup  "delayed_search(this.value,event);"]))})

)

(define (search-index-box) ; appears on every page
  (let ([sa string-append])
    `(input
      ([style ,(sa "font-size: 75%; margin: 0px; padding: 0px; border: 1px;"
                   " background-color: #eee; color: #888;")]
       [type "text"]
       [value "...search..."]
       [onkeypress ,(format "return DoSearchKey(event, this, ~s);" (version))]
       [onfocus ,(sa "this.style.color=\"black\";"
                     " if (this.value.indexOf(\"...search...\")>=0)"
                     " this.value=\"\";")]
       [onblur ,(sa "if (this.value.match(/^ *$/)) {"
                    " this.style.color=\"#888\";"
                    " this.value=\"...search...\"; }")]))))

;; ----------------------------------------
;;  main mixin

(define (render-mixin %)
  (class %
    (inherit render-content
             render-block
             collect-part
             install-file
             get-dest-directory
             format-number
             quiet-table-of-contents)

    (init-field [css-path #f]
                ;; up-path is either a link "up", or #t which uses
                ;; goes to start page (using cookies to get to the
                ;; user start page)
                [up-path #f]
                [style-file #f]
                [script-path #f]
                [script-file #f])

    (define/override (get-suffix) #".html")

    (define/override (index-manual-newlines?)
      #t)

    ;; ----------------------------------------

    (inherit path->root-relative
             root-relative->path)

    (define (path->relative p)
      (let ([p (path->main-doc-relative p)])
        (if (path? p)
          (let ([p (path->main-collects-relative p)])
            (if (path? p)
              (path->root-relative p)
              (intern-taglet p)))
          (intern-taglet p))))

    (define (relative->path p)
      (let ([p (main-doc-relative->path p)])
        (if (path? p)
          p
          (let ([p (main-collects-relative->path p)])
            (if (path? p)
              p
              (root-relative->path p))))))

    ;; ----------------------------------------

    (define/override (start-collect ds fns ci)
      (map (lambda (d fn)
             (parameterize ([current-output-file fn]
                            [current-top-part d])
               (collect-part d #f ci null)))
           ds
           fns))

    (define/public (part-whole-page? p ri)
      (let ([dest (resolve-get p ri (car (part-tags p)))])
        (dest-page? dest)))

    (define/public (current-part-whole-page? d)
      (eq? d (current-top-part)))

    (define/override (collect-part-tags d ci number)
      (for ([t (part-tags d)])
        (let ([key (generate-tag t ci)])
          (collect-put! ci key
                        (vector (and (current-output-file)
                                     (path->relative (current-output-file)))
                                (or (part-title-content d) '("???"))
                                (current-part-whole-page? d)
                                key)))))

    (define/override (collect-target-element i ci)
      (let ([key (generate-tag (target-element-tag i) ci)])
        (collect-put! ci key
           (vector (path->relative
                    (let ([p (current-output-file)])
                      (if (redirect-target-element? i)
                        (let-values ([(base name dir?) (split-path p)])
                          (build-path base
                                      (redirect-target-element-alt-path i)))
                        p)))
                   #f
                   (page-target-element? i)
                   (if (redirect-target-element? i)
                     (make-literal-anchor
                      (redirect-target-element-alt-anchor i))
                     key)))))

    (define (dest-path dest)
      (if (vector? dest) ; temporary
        (vector-ref dest 0)
        (list-ref dest 0)))
    (define (dest-title dest)
      (if (vector? dest)
        (vector-ref dest 1)
        (list-ref dest 1)))
    (define (dest-page? dest)
      (if (vector? dest)
        (vector-ref dest 2)
        (list-ref dest 2)))
    (define (dest-anchor dest)
      (if (vector? dest)
        (vector-ref dest 3)
        (list-ref dest 3)))

    ;; ----------------------------------------

    (define external-tag-path #f)
    (define/public (set-external-tag-path p)
      (set! external-tag-path p))

    (define/public (tag->path+anchor ri tag)
      ;; Called externally; not used internally
      (let-values ([(dest ext?) (resolve-get/ext? #f ri tag)])
        (cond [(not dest) (values #f #f)]
              [(and ext? external-tag-path)
               (values external-tag-path (format "~a" (serialize tag)))]
              [else (values (relative->path (dest-path dest))
                            (and (not (dest-page? dest))
                                 (anchor-name (dest-anchor dest))))])))

    ;; ----------------------------------------

    (define/private (reveal-subparts? p)
      (part-style? p 'reveal))

    (define/public (toc-wrap table)
      null)

    (define/public (render-toc-view d ri)
      (define-values (top mine)
        (let loop ([d d] [mine d])
          (let ([p (collected-info-parent (part-collected-info d ri))])
            (if p
              (loop p (if (reveal-subparts? d) mine d))
              (values d mine)))))
      (define (do-part pp)
        (let ([p (car pp)] [show-number? (cdr pp)])
          `(tr (td ([align "right"])
                 ,@(if show-number?
                     (format-number
                      (collected-info-number (part-collected-info p ri))
                      '((tt nbsp)))
                     '("-" nbsp)))
               (td (a ([href
                        ,(let ([dest (resolve-get p ri (car (part-tags p)))])
                           (format "~a~a~a"
                                   (from-root (relative->path (dest-path dest))
                                              (get-dest-directory))
                                   (if (dest-page? dest) "" "#")
                                   (if (dest-page? dest)
                                     ""
                                     (anchor-name (dest-anchor dest)))))]
                       [class ,(if (eq? p mine)
                                 "tocviewselflink" "tocviewlink")])
                     ,@(render-content (or (part-title-content p) '("???"))
                                       d ri))))))
      (define toc-content
        (parameterize ([extra-breaking? #t])
          (map do-part
               (let loop ([l (map (lambda (v) (cons v #t)) (part-parts top))])
                 (cond [(null? l) null]
                       [(reveal-subparts? (caar l))
                        (cons (car l)
                              (loop (append (map (lambda (v) (cons v #f))
                                                 (part-parts (caar l)))
                                            (cdr l))))]
                       [else (cons (car l) (loop (cdr l)))])))))
      `((div ([class "tocset"])
          ,@(if (part-style? d 'no-toc)
              null
              (let* ([content (render-content
                               (or (part-title-content top) '("???"))
                               d ri)]
                     [content (if (null? toc-content)
                                content
                                `((a ([href "index.html"] [class "tocviewlink"])
                                     ,@content)))])
                `((div ([class "tocview"])
                       (div ([class "tocviewtitle"]) ,@content)
                       (div nbsp)
                       ,@(if (null? toc-content)
                           '()
                           (toc-wrap
                            `(table ([class "tocviewlist"] [cellspacing "0"])
                                    ,@toc-content)))))))
          ,@(render-onthispage-contents
             d ri top (if (part-style? d 'no-toc) "tocview" "tocsub"))
          ,@(parameterize ([extra-breaking? #t])
              (append-map
               (lambda (t)
                 (let loop ([t t])
                   (if (table? t)
                     (render-table t d ri #f)
                     (loop (delayed-block-blocks t ri)))))
               (filter (lambda (e)
                         (let loop ([e e])
                           (or (and (auxiliary-table? e)
                                    (pair? (table-flowss e)))
                               (and (delayed-block? e)
                                    (loop (delayed-block-blocks e ri))))))
                       (flow-paragraphs (part-flow d))))))))

    (define/public (get-onthispage-label)
      null)

    (define/public (nearly-top? d ri top)
      #f)

    (define/private (render-onthispage-contents d ri top box-class)
      (if (ormap (lambda (p) (part-whole-page? p ri))
                 (part-parts d))
        null
        (let ([nearly-top? (lambda (d) (nearly-top? d ri top))])
          (define (flow-targets flow)
            (append-map block-targets (flow-paragraphs flow)))
          (define (block-targets e)
            (cond [(table? e) (table-targets e)]
                  [(paragraph? e) (para-targets e)]
                  [(itemization? e)
                   (append-map flow-targets (itemization-flows e))]
                  [(blockquote? e)
                   (append-map block-targets (blockquote-paragraphs e))]
                  [(delayed-block? e) null]))
          (define (para-targets para)
            (let loop ([c (paragraph-content para)])
              (define a (and (pair? c) (car c)))
              (cond
                [(null? c) null]
                [(toc-target-element? a) (cons a (loop (cdr c)))]
                [(toc-element? a) (cons a (loop (cdr c)))]
                [(element? a)
                 (append (loop (element-content a)) (loop (cdr c)))]
                [(delayed-element? a)
                 (loop (append (delayed-element-content a ri) (cdr c)))]
                [(part-relative-element? a)
                 (loop (append (part-relative-element-content a ri) (cdr c)))]
                [else (loop (cdr c))])))
          (define  (table-targets table)
            (append-map
             (lambda (flows)
               (append-map (lambda (f) (if (eq? f 'cont) null (flow-targets f)))
                           flows))
             (table-flowss table)))
          (define ps
            ((if (nearly-top? d) values cdr)
             (let flatten ([d d])
               (append*
                ;; don't include the section if it's in the TOC
                (if (nearly-top? d) null (list d))
                ;; get internal targets:
                (append-map block-targets (flow-paragraphs (part-flow d)))
                (map flatten (part-parts d))))))
          (define any-parts? (ormap part? ps))
          (if (null? ps)
            null
            `((div ([class ,box-class])
                ,@(get-onthispage-label)
                (table ([class "tocsublist"] [cellspacing "0"])
                  ,@(map (lambda (p)
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
                                             (anchor-name
                                              (tag-key (if (part? p)
                                                         (car (part-tags p))
                                                         (target-element-tag p))
                                                       ri)))]
                                          [class
                                           ,(cond
                                              [(part? p) "tocsubseclink"]
                                              [any-parts? "tocsubnonseclink"]
                                              [else "tocsublink"])])
                                         ,@(render-content
                                            (if (part? p)
                                              (or (part-title-content p)
                                                  '("???"))
                                              (element-content p))
                                            d ri))))))))
                         ps))))))))

    (define/public (render-one-part d ri fn number)
      (parameterize ([current-output-file fn])
        (let* ([style-file  (or style-file scribble-css)]
               [script-file (or script-file scribble-js)]
               [title (cond [(part-title-content d)
                             => (lambda (c)
                                  `(title ,@(format-number number '(nbsp))
                                          ,(content->string c this d ri)))]
                            [else `(title)])])
          (unless css-path    (install-file style-file))
          (unless script-path (install-file script-file))
          (printf "<!DOCTYPE html PUBLIC ~s ~s>\n"
                  "-//W3C//DTD HTML 4.0 Transitional//EN"
                  "http://www.w3.org/TR/html4/loose.dtd")
          (xml:write-xml/content
           (xml:xexpr->xml
            `(html ()
               (head ()
                 (meta ([http-equiv "content-type"]
                        [content "text-html; charset=utf-8"]))
                 ,title
                 ,(scribble-css-contents style-file  css-path)
                 ,(scribble-js-contents  script-file script-path))
               (body () ,@(render-toc-view d ri)
                 (div ([class "maincolumn"])
                   (div ([class "main"])
                     ,@(render-version d ri)
                     ,@(navigation d ri #f)
                     ,@(render-part d ri)
                     ,@(navigation d ri #t))))))))))

    (define/private (part-parent d ri)
      (collected-info-parent (part-collected-info d ri)))

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

    (define contents-content '("contents"))
    (define index-content '("index"))
    (define prev-content '(larr " prev"))
    (define up-content '("up"))
    (define next-content '("next " rarr))
    (define sep-element (make-element #f '(nbsp nbsp)))

    (define/public (derive-filename d) "bad.html")

    (define/private (navigation d ri pre-space?)
      (define parent (part-parent d ri))
      (define-values (prev0 next0) (find-siblings d ri))
      (define prev
        (if prev0
          (let loop ([p prev0])
            (if (and (toc-part? p) (pair? (part-parts p)))
              (loop (last (part-parts p)))
              p))
          (and parent (toc-part? parent) parent)))
      (define next
        (cond [(and (toc-part? d) (pair? (part-parts d))) (car (part-parts d))]
              [(not next0)
               (let loop ([p parent])
                 (and p (toc-part? p)
                      (let-values ([(prev next) (find-siblings p ri)])
                        (or next (loop (part-parent p ri))))))]
              [else next0]))
      (define index
        (let loop ([d d])
          (let ([p (part-parent d ri)])
            (if p
              (loop p)
              (let ([subs (part-parts d)])
                (and (pair? subs)
                     (let ([d (last subs)])
                       (and (part-style? d 'index)
                            d))))))))
      (define (render . content) (render-content content d ri))
      (if (not (or prev next parent index up-path))
        null
        `(,@(if pre-space? '((p nbsp)) null)
          (div ([class "navleft"])
            ,@(render (make-element
                       (if parent
                         (make-target-url "index.html" #f)
                         "nonavigation")
                       contents-content))
            ,@(if index
                `(nbsp
                  ,@(render (if (eq? d index)
                              (make-element "nonavigation" index-content)
                              (make-link-element
                               #f index-content (car (part-tags index)))))
                  #; ; no need for these index-local searches
                  ,@(if (eq? d index)
                      null
                      `((span ([class "smaller"]) nbsp ,(search-index-box)))))
                null)
            ,@(if up-path
                `(nbsp (span ([class "smaller"]) ,(search-index-box)))
                null))
          (div ([class "navright"])
            ,@(render
               (make-element
                (if parent
                  (make-target-url (if prev (derive-filename prev) "index.html")
                                   #f)
                  "nonavigation")
                prev-content)
               sep-element
               (make-element
                (cond
                  ;; up-path = #t => go up to the start page, using
                  ;; cookies to get to the user's version of it (see
                  ;; scribblings/main/private/utils for the code that
                  ;; creates these cookies.)
                  [(and (eq? #t up-path) (not parent))
                   (make-target-url
                    "../index.html"
                    (make-with-attributes
                     #f `([onclick . ,(format "return GotoPLTRoot(\"~a\");"
                                              (version))])))]
                  [(or parent up-path)
                   (make-target-url
                    (cond [(not parent) up-path]
                          [(and (toc-part? parent) (part-parent parent ri))
                           (derive-filename parent)]
                          [else "index.html"])
                    #f)]
                  [else "nonavigation"])
                up-content)
               sep-element
               (make-element (if next
                               (make-target-url (derive-filename next) #f)
                               "nonavigation")
                             next-content)))
          (p nbsp))))

    (define/override (render-one d ri fn)
      (render-one-part d ri fn null))

    (define/public (render-version d ri)
      `((div ([class "versionbox"])
          ,@(render-content
             (list (make-element "version"
                                 (list "Version: " (current-version))))
             d
             ri))))

    (define/override (render-part d ri)
      (let ([number (collected-info-number (part-collected-info d ri))])
        `(,@(cond
              [(and (not (part-title-content d)) (null? number)) null]
              [(part-style? d 'hidden)
               (map (lambda (t)
                      `(a ((name ,(format "~a" (anchor-name (tag-key t ri)))))))
                    (part-tags d))]
              [else `((,(case (length number)
                          [(0) 'h2]
                          [(1) 'h3]
                          [(2) 'h4]
                          [else 'h5])
                       ,@(format-number number '((tt nbsp)))
                       ,@(map (lambda (t)
                                `(a ([name ,(format "~a" (anchor-name
                                                          (tag-key t ri)))])))
                              (part-tags d))
                       ,@(if (part-title-content d)
                           (render-content (part-title-content d) d ri)
                           null)))])
          ,@(render-flow* (part-flow d) d ri #f #f)
          ,@(let loop ([pos 1]
                       [secs (part-parts d)])
              (if (null? secs)
                null
                (append (render-part (car secs) ri)
                        (loop (add1 pos) (cdr secs))))))))

    (define/private (render-flow* p part ri start-inline? special-last?)
      ;; Wrap each table with <p>, except for a trailing table
      ;;  when `special-last?' is #t
      (let loop ([f (flow-paragraphs p)] [inline? start-inline?])
        (cond
          [(null? f) null]
          [(and (table? (car f))
                (or (not special-last?) (not (null? (cdr f)))))
           (cons `(p ,@(render-block (car f) part ri inline?))
                 (loop (cdr f) #f))]
          [else (append (render-block (car f) part ri inline?)
                        (loop (cdr f) #f))])))

    (define/override (render-flow p part ri start-inline?)
      (render-flow* p part ri start-inline? #t))

    (define/override (render-paragraph p part ri)
      `((p ,(if (styled-paragraph? p)
              `([class ,(styled-paragraph-style p)])
              `())
          ,@(super render-paragraph p part ri))))

    (define/override (render-element e part ri)
      (cond
        [(hover-element? e)
         `((span ([title ,(hover-element-text e)])
             ,@(render-plain-element e part ri)))]
        [(script-element? e)
         (let* ([t `[type ,(script-element-type e)]]
                [s (script-element-script e)]
                [s (if (list? s)
                     `(script (,t) ,(apply literal `("\n" ,@s "\n")))
                     `(script (,t [src ,s])))])
           (list s
                 ;; mynoscript hack doesn't always work (see the
                 ;; (commented) hack in scribble-common.js)
                 `(noscript ,@(render-plain-element e part ri))))]
        [(target-element? e)
         `((a ([name ,(format "~a" (anchor-name (tag-key (target-element-tag e)
                                                         ri)))]))
           ,@(render-plain-element e part ri))]
        [(and (link-element? e) (not (current-no-links)))
         (parameterize ([current-no-links #t])
           (let-values ([(dest ext?)
                         (resolve-get/ext? part ri (link-element-tag e))])
             (if dest
               `((a [(href
                      ,(if (and ext? external-tag-path)
                         ;; Redirected to search:
                         (format "~a;tag=~a"
                                 external-tag-path
                                 (base64-encode
                                  (string->bytes/utf-8
                                   (format "~a" (serialize
                                                 (link-element-tag e))))))
                         ;; Normal link:
                         (format "~a~a~a"
                                 (from-root (relative->path (dest-path dest))
                                            (get-dest-directory))
                                 (if (dest-page? dest) "" "#")
                                 (if (dest-page? dest)
                                   ""
                                   (anchor-name (dest-anchor dest))))))
                     ,@(if (string? (element-style e))
                         `([class ,(element-style e)])
                         null)]
                    ,@(if (null? (element-content e))
                        (render-content (strip-aux (dest-title dest)) part ri)
                        (render-content (element-content e) part ri))))
               (begin
                 (when #f
                   (fprintf (current-error-port)
                            "Undefined link: ~s~n"
                            (tag-key (link-element-tag e) ri)))
                 `((font ([class "badlink"])
                     ,@(if (null? (element-content e))
                         `(,(format "~s" (tag-key (link-element-tag e) ri)))
                         (render-plain-element e part ri))))))))]
        [else (render-plain-element e part ri)]))

    (define/private (render-plain-element e part ri)
      (let* ([raw-style (flatten-style (and (element? e) (element-style e)))]
             [style (if (with-attributes? raw-style)
                      (with-attributes-style raw-style)
                      raw-style)]
             [attribs (lambda ()
                        (if (with-attributes? raw-style)
                          (map (lambda (p) (list (car p) (cdr p)))
                               (with-attributes-assoc raw-style))
                          null))]
             [super-render/attribs
              (lambda ()
                (if (with-attributes? raw-style)
                  `((span ,(attribs) ,@(super render-element e part ri)))
                  (super render-element e part ri)))])
        (cond
         [(symbol? style)
          (case style
            [(italic) `((i ,(attribs) ,@(super render-element e part ri)))]
            [(bold) `((b ,(attribs) ,@(super render-element e part ri)))]
            [(tt) `((span ([class "stt"] . ,(attribs)) ,@(super render-element e part ri)))]
            [(no-break) `((span ([class "nobreak"] . ,(attribs))
                                ,@(super render-element e part ri)))]
            [(sf) `((b (font ([size "-1"] [face "Helvetica"] . ,(attribs))
                             ,@(super render-element e part ri))))]
            [(subscript) `((sub ,(attribs) ,@(super render-element e part ri)))]
            [(superscript) `((sup ,(attribs) ,@(super render-element e part ri)))]
            [(hspace) `((span ([class "hspace"] . ,(attribs))
                              ,@(let ([str (content->string (element-content e))])
                                  (map (lambda (c) 'nbsp) (string->list str)))))]
            [(newline) `((br ,(attribs)))]
            [else (error 'html-render "unrecognized style symbol: ~e" style)])]
         [(string? style)
          `((span ([class ,style] . ,(attribs)) ,@(super render-element e part ri)))]
         [(and (pair? style) (memq (car style) '(color bg-color)))
          (unless (and (list? style)
                       (or (and (= 4 (length style))
                                (andmap byte? (cdr style)))
                           (and (= 2 (length style))
                                (member (cadr style)
                                        '("white" "black" "red" "green" "blue"
                                          "cyan" "magenta" "yellow")))))
            (error 'render-font "bad color style: ~e"  style))
          `((font ([style
                       ,(format "~acolor: ~a"
                                (if (eq? (car style) 'bg-color) "background-" "")
                                (if (= 2 (length style))
                                    (cadr style)
                                    (string-append*
                                     "#"
                                     (map (lambda (v)
                                            (let ([s (number->string v 16)])
                                              (if (< v 16) (string-append "0" s) s)))
                                          (cdr style)))))]
                   . ,(attribs))
                  ,@(super render-element e part ri)))]
         [(target-url? style)
          (if (current-no-links)
            (super-render/attribs)
            (parameterize ([current-no-links #t])
              `((a ([href ,(let ([addr (target-url-addr style)])
                             (if (path? addr)
                               (from-root addr (get-dest-directory))
                               addr))]
                    ;; The target-url chains to another style,
                    ;; flatten-style above takes care of it though.
                    ,@(let ([style (target-url-style style)])
                        (if (string? style)
                          `([class ,style])
                          null))
                    . ,(attribs))
                   ,@(super render-element e part ri)))))]
         [(url-anchor? style)
          `((a ([name ,(url-anchor-name style)] . ,(attribs))
               ,@(super render-element e part ri)))]
         [(image-file? style)
          (let* ([src (main-collects-relative->path (image-file-path style))]
                 [scale (image-file-scale style)]
                 [to-num
                  (lambda (s)
                    (number->string
                     (inexact->exact
                      (floor (* scale (integer-bytes->integer s #f #t))))))]
                 [sz (if (= 1.0 scale)
                         null
                         ;; Try to extract file size:
                         (call-with-input-file*
                          src
                          (lambda (in)
                            (if (regexp-try-match #px#"^\211PNG.{12}" in)
                                `([width ,(to-num (read-bytes 4 in))]
                                  [height ,(to-num (read-bytes 4 in))])
                                null))))])
            `((img ([src ,(let ([p (install-file src)])
                            (if (path? p)
                                (url->string (path->url (path->complete-path p)))
                                p))]
                    . ,(attribs))
                   ,@sz)))]
         [else (super-render/attribs)])))

    (define/override (render-table t part ri need-inline?)
      (define t-style (table-style t))
      (define t-style-get (if (and (pair? t-style) (list? t-style))
                            (lambda (k) (assoc k t-style))
                            (lambda (k) #f)))
      (define index? (eq? 'index t-style))
      (define (make-row flows style)
        `(tr (,@(if style `([class ,style]) null))
           ,@(let loop ([ds flows]
                        [as (cdr (or (t-style-get 'alignment)
                                     (cons #f (map (lambda (x) #f) flows))))]
                        [vas (cdr (or (t-style-get 'valignment)
                                      (cons #f (map (lambda (x) #f) flows))))])
               (cond
                 [(null? ds) null]
                 [(eq? (car ds) 'cont)
                  (loop (cdr ds) (cdr as) (cdr vas))]
                 [else
                  (let ([d (car ds)] [a (car as)] [va (car vas)])
                    (cons
                     `(td (,@(case a
                               [(#f) null]
                               [(right) '([align "right"])]
                               [(center) '([align "center"])]
                               [(left) '([align "left"])])
                           ,@(case va
                               [(#f) null]
                               [(top) '((valign "top"))]
                               [(baseline) '((valign "baseline"))]
                               [(bottom) '((valign "bottom"))])
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
                          ,@(render-flow d part ri #f))
                     (loop (cdr ds) (cdr as) (cdr vas))))]))))
      `(#; ; no need for these index-local searches
        ,@(if index? `(,search-script ,search-field) '())
        (table ([cellspacing "0"]
                ,@(if need-inline?
                    '([style "display: inline; vertical-align: top;"])
                    null)
                ,@(case t-style
                    [(boxed)    '([class "boxed"])]
                    [(centered) '([align "center"])]
                    [(at-right) '([align "right"])]
                    [(at-left)  '([align "left"])]
                    [else null])
                ,@(let ([a (t-style-get 'style)])
                    (if (and a (string? (cadr a))) `([class ,(cadr a)]) null))
                ,@(if (string? t-style) `([class ,t-style]) null))
          ,@(map make-row
                 (table-flowss t)
                 (cdr (or (t-style-get 'row-styles)
                          (cons #f (map (lambda (x) #f) (table-flowss t)))))))))

    (define/override (render-blockquote t part ri)
      `((blockquote ,(if (string? (blockquote-style t))
                       `([class ,(blockquote-style t)])
                       `())
          ,@(append-map (lambda (i) (render-block i part ri #f))
                        (blockquote-paragraphs t)))))

    (define/override (render-itemization t part ri)
      `((ul ,(if (and (styled-itemization? t)
                      (string? (styled-itemization-style t)))
               `([class ,(styled-itemization-style t)])
               `())
          ,@(map (lambda (flow) `(li ,@(render-flow flow part ri #t)))
                 (itemization-flows t)))))

    (define/override (render-other i part ri)
      (cond
        [(string? i)
         (let ([m (and (extra-breaking?)
                       (regexp-match-positions #rx"[-:/+_]|[a-z](?=[A-Z])" i))])
           (if m
             (list* (substring i 0 (cdar m))
                    ;; Most browsers wrap after a hyphen. The one that
                    ;; doesn't, Firefox, pays attention to wbr. Some
                    ;; browsers ignore wbr, but at least they don't do
                    ;; strange things with it.
                    (if (equal? #\- (string-ref i (caar m)))
                      '(wbr)
                      `(span ([class "mywbr"]) " "))
                    (render-other (substring i (cdar m)) part ri))
             (ascii-ize i)))]
        [(eq? i 'mdash) `(" " ndash " ")]
        [(symbol? i) (list i)]
        [else (list (format "~s" i))]))

    (define/private (ascii-ize s)
      (let ([m (regexp-match-positions #rx"[^\u01-\u7E]" s)])
        (if m
          (append (ascii-ize (substring s 0 (caar m)))
                  (list (char->integer (string-ref s (caar m))))
                  (ascii-ize (substring s (cdar m))))
          (list s))))

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
             format-number)

    (inherit-field report-output?)

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

    (define/override (derive-filename d)
      (let ([fn (format "~a.html"
                        (regexp-replace*
                         "[^-a-zA-Z0-9_=]"
                         (let ([s (cadr (car (part-tags d)))])
                           (cond [(string? s) s]
                                 [(part-title-content d)
                                  (content->string (part-title-content d))]
                                 [else
                                  ;; last-ditch effort to make up a unique name:
                                  (format "???~a" (eq-hash-code d))]))
                         "_"))])
        (when ((string-length fn) . >= . 48)
          (error "file name too long (need a tag):" fn))
        fn))

    (define/override (collect ds fns)
      (super collect ds (map (lambda (fn) (build-path fn "index.html")) fns)))

    (define/override (current-part-whole-page? d)
      ((collecting-sub) . <= . 2))

    (define/override (collect-part d parent ci number)
      (let ([prev-sub (collecting-sub)])
        (parameterize ([collecting-sub (if (toc-part? d) 1 (add1 prev-sub))])
          (if (= 1 prev-sub)
            (let ([filename (derive-filename d)])
              (parameterize ([current-output-file
                              (build-path (path-only (current-output-file))
                                          filename)])
                (super collect-part d parent ci number)))
            (super collect-part d parent ci number)))))

    (define/override (render ds fns ri)
      (map (lambda (d fn)
             (when report-output?
               (printf " [Output to ~a/index.html]\n" fn))
             (unless (directory-exists? fn)
               (make-directory fn))
             (parameterize ([current-subdirectory (file-name-from-path fn)])
               (let ([fn (build-path fn "index.html")])
                 (with-output-to-file fn #:exists 'truncate/replace
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
             render-paragraph)

    (define/override (render-part d ri)
      (parameterize ([current-version
                      (if (and (versioned-part? d)
                               (versioned-part-version d))
                        (versioned-part-version d)
                        (current-version))])
        (let ([number (collected-info-number (part-collected-info d ri))])
          (if (and (not (on-separate-page))
                   (or (= 1 (length number))
                       (next-separate-page)))
            ;; Render as just a link, and put the actual content in a
            ;; new file:
            (let* ([filename (derive-filename d)]
                   [full-path (build-path (path-only (current-output-file))
                                          filename)])
              (parameterize ([on-separate-page #t])
                (with-output-to-file full-path #:exists 'truncate/replace
                  (lambda () (render-one-part d ri full-path number)))
                null))
            (let ([sep? (on-separate-page)])
              (parameterize ([next-separate-page (toc-part? d)]
                             [on-separate-page #f])
                ;; Normal section render
                (super render-part d ri)))))))

    (super-new)))

;; ----------------------------------------
;; utils

(define (explode p)
  (reverse (let loop ([p p])
             (let-values ([(base name dir?) (split-path p)])
               (let ([name (if base
                             (if (path? name)
                               (path-element->string name)
                               name)
                             name)])
                 (if (path? base)
                   (cons name (loop base))
                   (list name)))))))

(define in-plt?
  (let ([roots (map explode (list (find-doc-dir) (find-collects-dir)))])
    (lambda (path)
      (ormap (lambda (root)
               (let loop ([path  path] [root root])
                 (or (null? root)
                     (and (pair? path)
                          (equal? (car path) (car root))
                          (loop (cdr path) (cdr root))))))
             roots))))

(define (from-root p d)
  (define e-p (explode (path->complete-path p (current-directory))))
  (define e-d (and d (explode (path->complete-path d (current-directory)))))
  (define p-in? (in-plt? e-p))
  (define d-in? (and d (in-plt? e-d)))
  ;; use an absolute link if the link is from outside the plt tree
  ;; going in (or if d is #f)
  (if (not (and d (cond [(equal? p-in? d-in?) #t]
                        [d-in? (error 'from-root
                                      "got a link from the PLT going out; ~e"
                                      p)]
                        [else #f])))
    (url->string (path->url (path->complete-path p)))
    (let loop ([e-d e-d] [e-p e-p])
      (cond
        [(null? e-d)
         (let loop ([e-p e-p])
           (cond [(null? e-p) "/"]
                 [(null? (cdr e-p)) (car e-p)]
                 [(eq? 'same (car e-p)) (loop (cdr e-p))]
                 [(eq? 'up (car e-p)) (string-append "../" (loop (cdr e-p)))]
                 [else (string-append (car e-p) "/" (loop (cdr e-p)))]))]
        [(equal? (car e-d) (car e-p)) (loop (cdr e-d) (cdr e-p))]
        [(eq? 'same (car e-d)) (loop (cdr e-d) e-p)]
        [(eq? 'same (car e-p)) (loop e-d (cdr e-p))]
        [else (string-append (string-append* (map (lambda (x) "../") e-d))
                             (loop null e-p))]))))

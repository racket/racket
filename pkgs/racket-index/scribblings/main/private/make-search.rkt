#lang at-exp racket/base

(require scribble/decode
         scribble/decode-struct
         scribble/basic
         scribble/core
         scribble/racket
         scribble/html-properties
         scribble/manual-struct
         racket/list
         racket/string
         racket/match
         racket/path
         racket/file
         net/url
         (only-in racket/class send)
         (only-in xml
                  xexpr->string
                  read-xml
                  document-element
                  xml->xexpr)
         racket/runtime-path
         syntax/location
         setup/path-to-relative
         (only-in setup/dirs
                  find-doc-dir
                  get-main-language-family)
         "utils.rkt"
         (for-syntax racket/base)
         (for-syntax racket/runtime-path)
         (for-syntax compiler/cm-accomplice)
         "index-scope.rkt"
         "pkg.rkt")

(provide make-search)

(define-runtime-path search-script "search.js")
(define-runtime-path search-merge-script "search-merge.js")
(define-runtime-path help-svg "help.svg")
(define-runtime-path settings-svg "settings.svg")
(define-runtime-path clear-svg "clear.svg")
(define-runtime-path prev-svg "prev.svg")
(define-runtime-path next-svg "next.svg")

;; this file is used as a trampoline to set a context (a pre-filter cookie) and
;; then hop over to the search page (the search page can do it itself, but it's
;; to heavy to load twice).
(define-runtime-path search-context-page "search-context.html")

;;; FIXME: Making make-search.zo depend on these files is a HACK!
;; The point is to make sure the latest versions get installed;
;; ideally we could just inform scribble/raco that they need
;; installing, and they would just do that when appropriate.
(begin-for-syntax
  (define-runtime-path search-style "search.css")
  (define-runtime-path search-script "search.js")
  (define-runtime-path search-merge-script "search-merge.js")
  (define-runtime-path search-context-page "search-context.html")
  (define-runtime-path help-svg "help.svg")
  (define-runtime-path settings-svg "settings.svg")
  (define-runtime-path clear-svg "clear.svg")
  (define-runtime-path prev-svg "prev.svg")
  (define-runtime-path next-svg "next.svg")
  (register-external-file search-style)
  (register-external-file search-script)
  (register-external-file search-merge-script)
  (register-external-file search-context-page)
  (register-external-file help-svg)
  (register-external-file settings-svg)
  (register-external-file clear-svg)
  (register-external-file prev-svg)
  (register-external-file next-svg))

(define (quote-string val)
  (define (hex4 ch)
    (let ([s (number->string (char->integer (string-ref ch 0)) 16)])
      (string-append "\\u" (case (string-length s)
                             [(1) (string-append "000" s)]
                             [(2) (string-append "00" s)]
                             [(3) (string-append "0" s)]
                             [else s]))))
  (define str (format "~s" val))
  (if (= (string-utf-8-length str) (string-length str))
      ;; It's ASCII:
      str
      ;; Quote unicode chars:
      (regexp-replace* #px"[^[:ascii:]]" str hex4)))

(define (format-list elems)
  (apply string-append
         (add-between #:before-first '("[")
                      elems '(",")
                      #:after-last '("]")
                      #:splice? #t)))

(define (make-script as-empty? user-dir? renderer sec ri)
  (define dest-dir (send renderer get-dest-directory #t))
  (define span-classes null)
  ;; To make the index smaller, html contents is represented as one of these:
  ;; - a string
  ;; - an array of contents to be concatenated
  ;; - a two-item array [idx, content], where idx is an index into the
  ;;   span-classes table holding a class name.
  ;; In addition, a "file:/main-doc.../path..." url is saved as ">path..."
  ;; This function does the url compacting.
  (define main-url ; (make sure that it terminates with a slash)
    (if user-dir?
      (regexp-replace #rx"/*$" (url->string (path->url (find-doc-dir))) "/")
      "../"))
  (define compact-url
    (let ([rx (regexp (string-append "^" (regexp-quote main-url)))])
      (lambda (url) (regexp-replace rx url ">"))))
  ;; This function does the html compacting.
  (define (compact-body xexprs)
    (define (compact xexprs)
      (match xexprs
        [`() xexprs]
        [`("" . ,r) (compact r)]
        [`(,(? string? s) ...)
         (list (xexpr->string (apply string-append xexprs)))]
        [`(,(? string? s1) ,(? string? s2) . ,r)
         (compact `(,(string-append s1 s2) . ,r))]
        [`((span ([class ,c]) . ,b1) (span ([class ,c]) . ,b2) . ,r)
         (compact `((span ([class ,c]) ,@b1 ,@b2) . ,r))]
        [`((span ([class ,c]) . ,b) . ,r)
         (let ([c (cond [(assoc c span-classes) => cdr]
                        [else (let ([n (length span-classes)])
                                (set! span-classes
                                      (cons (cons c n) span-classes))
                                n)])])
           (cons `(,c . ,(compact-body b)) (compact r)))]
        [`(,x . ,r) 
         (cons (xexpr->string x) (compact r))]))
    ;; generate javascript array code
    (let loop ([body (compact xexprs)])
      (if (andmap string? body)
        (quote-string (string-append* body))
        (let ([body (for/list ([x (in-list body)])
                      (if (string? x)
                          (quote-string x)
                          (format "[~a,~a]" (car x) (cdr x))))])
          (if (= 1 (length body))
            (car body)
            (string-append* `("[" ,@(add-between body ",") "]")))))))
  (define manual-refs (make-hash))
  (define idx -1)
  (define index-entries (if as-empty?
                            null
                            (get-index-entries sec ri)))
  (define ((make-lookup-extra desc) key default)
    (cond
      [(index-desc? desc) (hash-ref (index-desc-extras desc) key default)]
      [(exported-index-desc*? desc) (hash-ref (exported-index-desc*-extras desc) key default)]
      [else default]))
  (define (get-language-families)
    (hash-values
     (for/fold ([ht (hash)]) ([i (in-list index-entries)])
       (define-values (tag texts elts desc pre-pkg-name) (apply values i))
       (define fams ((make-lookup-extra desc) 'language-family '("Racket")))
       (for/fold ([ht ht]) ([f (in-list fams)])
         (define norm-f (string-foldcase f))
         (hash-set ht norm-f (let ([v (hash-ref ht norm-f #f)])
                               (cond
                                 [(not v) f]
                                 ;; prefer non-case-folded
                                 [(equal? v norm-f) f]
                                 [else v])))))))
  (define l-all
    (for/list ([i (in-list index-entries)]
               ;; don't index constructors (the class itself is already indexed)
               #:unless (let ([desc (list-ref i 3)])
                          (or (constructor-index-desc? desc)
                              (and (exported-index-desc*? desc)
                                   (hash-ref (exported-index-desc*-extras desc) 'hidden? #f)))))
      (set! idx (add1 idx))
      ;; i is (list tag (text ...) (element ...) index-desc pkg)
      (define-values (tag texts elts desc pre-pkg-name) (apply values i))
      (define text (string-downcase (string-join texts)))
      (define lookup-extra (make-lookup-extra desc))
      (define-values (href html)
        (let* ([e (add-between elts ", ")]
               [e (cond
                    [(method-index-desc? desc)
                     ;; Old approach. It's better for the index entry to have the extra
                     ;; text in it; `method-index-desc` isn't used in that case
                     `(,@e ,(make-element
                             "smaller"
                             `(" (method of "
                               ,(make-element 
                                 symbol-color
                                 (list
                                  (make-element 
                                   value-link-color
                                   (list (symbol->string
                                          (exported-index-desc-name desc))))))
                               ")")))]
                    [else e])]
               [e (make-link-element "indexlink" e tag)]
               [e (send renderer render-content e sec ri)])
          (match e ; should always render to a single `a'
            [`((a ([href ,href] [class "indexlink"] [data-pltdoc ,_]) . ,body))
             (cond [(and (part-index-desc? desc)
                         (regexp-match #rx"(?:^|/)([^/]+)/index\\.html$" href))
                    => (lambda (man) (hash-set! manual-refs (cadr man) idx))])
             (let (;; throw away tooltips, we don't need them
                   [body (match body
                           [`((span ((title ,label)) . ,body))
                            (if (regexp-match? #rx"^Provided from: " label)
                              body
                              ;; if this happens, this code should be updated
                              (error 'make-script
                                     "internal error: unexpected tooltip"))]
                           [_ body])])
               (values (compact-url href) (compact-body body)))]
            [_
             (log-error "search script: unrecognized index-entry shape: ~e" e)
             (values #f #f)])))
      (define (lib->name lib)
        (quote-string (let loop ([lib lib])
                        (match lib
                          [`',lib (string-append "'" (loop lib))]
                          [_ (format "~s" lib)]))))
      (define from-libs
        (cond
          [(exported-index-desc? desc)
           (let ([libs (map lib->name (exported-index-desc-from-libs desc))])
             (string-append* `("[" ,@(add-between libs ",") "]")))]
          [(or (and (index-desc? desc)
                    (hash-ref (index-desc-extras desc) 'module-kind #f))
               (cond
                 [(language-index-desc? desc) 'lang]
                 [(reader-index-desc? desc) 'reader]
                 [(module-path-index-desc? desc) 'mod]
                 [else #f]))
           => (lambda (mod-kind)
                (case mod-kind
                  [(lang)
                   "\"language\""]
                  [(reader)
                   "\"reader\""]
                  [else
                   "\"module\""]))]
          [else "false"]))
      (define-values (display-from-libs key-from-libs)
        (cond
          [(lookup-extra 'display-from-libs #f)
           => (lambda (display-from-libs)
                (values
                 (format-list
                  (for/list ([display-from-lib (in-list display-from-libs)])
                    (compact-body (send renderer render-content display-from-lib sec ri))))
                 (format-list
                  (map (lambda (c) (quote-string (content->string c))) display-from-libs))))]
          [else (values "false" "false")]))
      (define pkg-name (if pre-pkg-name (quote-string pre-pkg-name) "false"))
      (define sort-order (format "~a" (lookup-extra 'sort-order 0)))
      (define language-family (format-list (map quote-string
                                                (lookup-extra 'language-family '("Racket")))))
      (and href
           (string-append "[" (quote-string text) ","
                          (quote-string href) ","
                          html "," from-libs ","
                          pkg-name "," sort-order "," language-family ","
                          display-from-libs "," key-from-libs "]"))))
  (define l (filter values l-all))

  (define user (if user-dir? "user_" ""))
  (define main-language-family (get-main-language-family))

  (with-output-to-file (build-path dest-dir "plt-index.js") #:exists 'truncate
    (lambda ()
      (for-each
       display
       @`{// Autogenerated by @;
             @,(path->relative-string/library (quote-module-path))
          @||
          // the url of the main doc tree, for compact url
          // representation (see also the UncompactUrl function)
          var plt_main_url = @,(quote-string main-url);
          @||
          // classes to be used for compact representation of html strings in
          // plt_search_data below (see also the UncompactHtml function)
          var plt_@,|user|span_classes = [
            @,@(add-between (map (lambda (x) (quote-string (car x)))
                                 (reverse span-classes))
                            ",\n  ")
          ];
          @||
          // this array has an entry of four items for each index link:
          // - text is a string holding the indexed text
          // - url holds the link (">" prefix means relative to plt_main_url)
          // - html holds either a string, or [idx, html] where idx is an
          //   index into plt_span_classes (note: this is recursive)
          // - from_lib is an array of module names for bound identifiers,
          //   or the string "module" for a module entry
          var plt_@,|user|search_data = [
          @,@(add-between l ",\n")
          ];
          @||
          // array of pointers to the previous array, for items that are manuals
          var plt_@,|user|manual_ptrs = {
            @,@(let* ([ms (hash-map manual-refs cons)]
                      [ms (sort ms < #:key cdr)]
                      [ms (map (lambda (x)
                                 (string-append (quote-string (car x)) ": "
                                                (number->string (cdr x))))
                               ms)])
                 (add-between ms ",\n  "))
          };
          @||
          // an array of language families
          var plt_@,|user|language_families = [
            @,@(add-between (map quote-string (get-language-families)) ",\n  ")
          ];
          @||
          // an array of (transitive) dependencies of base documentation
          var plt_base_pkgs = [
            @,@(add-between (map quote-string (get-base-pkgs)) ",\n  ")
          ];
          @||
          // an array of (transitive) dependencies of main-distribution
          var plt_main_dist_pkgs = [
            @,@(add-between (map quote-string (get-main-dist-pkgs)) ",\n  ")
          ];
          @||
          // an array of (transitive) dependencies of main-distribution
          var plt_main_language_family = @,(quote-string main-language-family);
          @||})))

  (for ([src (append (list search-script search-context-page)
                     (if user-dir? (list search-merge-script) null))])
    (define dest (build-path dest-dir (file-name-from-path src)))
    (when (file-exists? dest) (delete-file dest))
    (copy-file src dest)))

(define (svg-icon name svg)
  (define x (xml->xexpr (document-element (call-with-input-file svg read-xml))))
  (match x
    [`(svg ,attrs ,@content)
     (element (style #f
                (list (xexpr-property `(svg ([style "display: none"])
                                            (symbol ([id ,name])
                                                    ,@content))
                                      "")))
       "")]))

(define (make-search in-user-dir?)
  (define main-at-user? (index-at-user?))
  (define user-dir? (and in-user-dir? (not main-at-user?)))
  (make-splice
   (list
    (make-paragraph
     plain
     (append
      (if user-dir?
          (list (script-ref (url->string
                             (path->url
                              (build-path (find-doc-dir) "search" "plt-index.js")))))
          null)
      (list
       (script-ref "plt-index.js"
                   #:noscript
                   @list{Sorry, you must have JavaScript to use this page.})
       (script-ref "search.js"))
      (if user-dir?
          (list (script-ref "search-merge.js"))
          null)
      (list
       (svg-icon "help" help-svg)
       (svg-icon "settings" settings-svg)
       (svg-icon "clear" clear-svg)
       (svg-icon "next" next-svg)
       (svg-icon "prev" prev-svg))
      (list
       (make-render-element #f null
                            (lambda (r s i) (make-script
                                             ;; If there's no installaton-scope docs,
                                             ;; don't both creating "main" search page:
                                             (and main-at-user?
                                                  (not in-user-dir?))
                                             user-dir?
                                             r s i))))))
    (make-paragraph (make-style #f
                                (list 'div
                                      (make-attributes '([id . "plt_search_container"]))))
                    '()))))

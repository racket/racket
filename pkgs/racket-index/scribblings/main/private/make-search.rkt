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
         net/url
         (only-in racket/class send)
         (only-in xml xexpr->string)
         racket/runtime-path
         syntax/location
         setup/path-to-relative
         (only-in setup/dirs find-doc-dir)
         "utils.rkt"
         (for-syntax racket/base)
         (for-syntax racket/runtime-path)
         (for-syntax compiler/cm-accomplice)
         "index-scope.rkt")

(provide make-search)

(define-runtime-path search-script "search.js")
(define-runtime-path search-merge-script "search-merge.js")

;; this file is used as a trampoline to set a context (a pre-filter cookie) and
;; then hop over to the search page (the search page can do it itself, but it's
;; to heavy to load twice).
(define-runtime-path search-context-page "search-context.html")

;;; FIXME: Making make-search.zo depend on these files is a HACK!
;; The point is to make sure the latest versions get installed;
;; ideally we could just inform scribble/raco that they need
;; installing, and they would just do that when appropriate.
(begin-for-syntax
  (define-runtime-path search-script "search.js")
  (define-runtime-path search-merge-script "search-merge.js")
  (define-runtime-path search-context-page "search-context.html")
  (register-external-file search-script)
  (register-external-file search-merge-script)
  (register-external-file search-context-page))

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
  (define main-url ; (make sure that it teminates with a slash)
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
  (define l
    (for/list ([i (if as-empty?
                      null
                      (get-index-entries sec ri))] 
               ;; don't index constructors (the class itself is already indexed)
               #:unless (constructor-index-desc? (list-ref i 3)))
      (set! idx (add1 idx))
      ;; i is (list tag (text ...) (element ...) index-desc)
      (define-values (tag texts elts desc) (apply values i))
      (define text (string-downcase (string-join texts)))
      (define-values (href html)
        (let* ([e (add-between elts ", ")]
               ;; !!HACK!! The index entry for methods should have the extra
               ;; text in it (when it does, this should go away)
               [e (if (method-index-desc? desc)
                    `(,@e ,(make-element "smaller"
                             `(" (method of "
                               ,(make-element 
                                 symbol-color
                                 (list
                                  (make-element 
                                   value-link-color
                                   (list (symbol->string
                                          (exported-index-desc-name desc))))))
                               ")")))
                    e)]
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
                           [else body])])
               (values (compact-url href) (compact-body body)))]
            [else (error 'make-script "unexpected value rendered: ~e" e)])))
      (define (lib->name lib)
        (quote-string (let loop ([lib lib])
                        (match lib
                          [`',lib (string-append "'" (loop lib))]
                          [else (format "~s" lib)]))))
      (define from-libs
        (cond
          [(exported-index-desc? desc)
           (let ([libs (map lib->name (exported-index-desc-from-libs desc))])
             (string-append* `("[" ,@(add-between libs ",") "]")))]
          [(module-path-index-desc? desc) "\"module\""]
          [else "false"]))
      (string-append "[" (quote-string text) ","
                         (quote-string href) ","
                         html "," from-libs "]")))

  (define user (if user-dir? "user_" ""))

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
                 (add-between ms ",\n  "))};
          @||})))

  (for ([src (append (list search-script search-context-page)
                     (if user-dir? (list search-merge-script) null))])
    (define dest (build-path dest-dir (file-name-from-path src)))
    (when (file-exists? dest) (delete-file dest))
    (copy-file src dest)))

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

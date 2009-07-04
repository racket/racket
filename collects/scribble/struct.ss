#lang scheme/base
(require scheme/serialize
         scheme/contract
         (for-syntax scheme/base))

;; ----------------------------------------

(define-struct collect-info (ht ext-ht parts tags gen-prefix relatives parents))
(define-struct resolve-info (ci delays undef searches))

(define (part-collected-info part ri)
  (hash-ref (collect-info-parts (resolve-info-ci ri))
            part))

(define (collect-put! ci key val)
  (let ([ht (collect-info-ht ci)])
    (let ([old-val (hash-ref ht key #f)])
      (when old-val
        (fprintf (current-error-port)
                 "WARNING: collected information for key multiple times: ~e; values: ~e ~e\n"
                 key old-val val))
    (hash-set! ht key val))))

(define (resolve-get/where part ri key)
  (let ([key (tag-key key ri)])
    (let ([v (hash-ref (if part
                         (collected-info-info (part-collected-info part ri))
                         (collect-info-ht (resolve-info-ci ri)))
                       key
                       #f)])
      (cond
        [v (values v #f)]
        [part (resolve-get/where
               (collected-info-parent (part-collected-info part ri))
               ri key)]
        [else
         (values (hash-ref (collect-info-ext-ht (resolve-info-ci ri)) key #f)
                 #t)]))))

(define (resolve-get/ext? part ri key)
  (let-values ([(v ext?) (resolve-get/where part ri key)])
    (when ext?
      (hash-set! (resolve-info-undef ri) (tag-key key ri) #t))
    (values v ext?)))

(define (resolve-get part ri key)
  (let-values ([(v ext?) (resolve-get/ext? part ri key)])
    v))

(define (resolve-get/tentative part ri key)
  (let-values ([(v ext?) (resolve-get/where part ri key)])
    v))

(define (resolve-search search-key part ri key)
  (let ([s-ht (hash-ref (resolve-info-searches ri)
                        search-key
                        (lambda ()
                          (let ([s-ht (make-hash)])
                            (hash-set! (resolve-info-searches ri)
                                       search-key s-ht)
                            s-ht)))])
    (hash-set! s-ht key #t))
  (resolve-get part ri key))

(define (resolve-get-keys part ri key-pred)
  (let ([l null])
    (hash-for-each
     (collected-info-info (part-collected-info part ri))
     (lambda (k v) (when (key-pred k) (set! l (cons k l)))))
    l))

(provide (struct-out collect-info)
         (struct-out resolve-info))

;; ----------------------------------------

(provide provide-structs)

(define-syntax (provide-structs stx)
  (syntax-case stx ()
    [(_ (id ([field ct] ...)) ...)
     #`(begin
         (define-serializable-struct id (field ...)) ...
         (provide/contract
          #,@(let ([ids (syntax->list #'(id ...))]
                   [fields+cts (syntax->list #'(([field ct] ...) ...))])
               (define (get-fields super-id)
                 (ormap (lambda (id  fields+cts)
                          (if (identifier? id)
                            (and (free-identifier=? id super-id)
                                 fields+cts)
                            (syntax-case id ()
                              [(my-id next-id)
                               (free-identifier=? #'my-id super-id)
                               #`[#,@(get-fields #'next-id)
                                  #,@fields+cts]]
                              [_else #f])))
                        ids fields+cts))
               (map (lambda (id fields+cts)
                      (if (identifier? id)
                        #`[struct #,id #,fields+cts]
                        (syntax-case id ()
                          [(id super)
                           #`[struct id (#,@(get-fields #'super) 
                                         #,@fields+cts)]])))
                    ids
                    fields+cts))))]))

(provide tag?)
(define (tag? s)
  (and (pair? s)
       (symbol? (car s))
       (pair? (cdr s))
       (or (string? (cadr s))
           (generated-tag? (cadr s))
           (and (pair? (cadr s))
                (list? (cadr s))))
       (null? (cddr s))))

(provide block?)
(define (block? p)
  (or (paragraph? p)
      (table? p)
      (itemization? p)
      (blockquote? p)
      (compound-paragraph? p)
      (delayed-block? p)))

(define (string-without-newline? s)
  (and (string? s)
       (not (regexp-match? #rx"\n" s))))

(provide-structs
 [part ([tag-prefix (or/c false/c string?)]
        [tags (listof tag?)]
        [title-content (or/c false/c list?)]
        [style any/c]
        [to-collect list?]
        [flow flow?]
        [parts (listof part?)])]
 [(unnumbered-part part) ()]
 [(versioned-part part) ([version (or/c string? false/c)])]
 [flow ([paragraphs (listof block?)])]
 [paragraph ([content list?])]
 [(styled-paragraph paragraph) ([style any/c])]
 [(omitable-paragraph paragraph) ()]
 [table ([style any/c]
         [flowss (listof (listof (or/c flow? (one-of/c 'cont))))])]
 [(auxiliary-table table) ()]
 [delayed-block ([resolve (any/c part? resolve-info? . -> . block?)])]
 [itemization ([flows (listof flow?)])]
 [(styled-itemization itemization) ([style any/c])]
 [blockquote ([style any/c]
              [paragraphs (listof block?)])]
 [compound-paragraph ([style any/c]
                      [blocks (listof block?)])]
 ;; content = list of elements
 [element ([style any/c]
           [content list?])]
 [(toc-element element) ([toc-content list?])]
 [(target-element element) ([tag tag?])]
 [(toc-target-element target-element) ()]
 [(page-target-element target-element) ()]
 [(redirect-target-element target-element) ([alt-path path-string?]
                                            [alt-anchor string?])]
 [(link-element element) ([tag tag?])]
 [(index-element element) ([tag tag?]
                           [plain-seq (and/c pair? (listof string-without-newline?))]
                           [entry-seq list?]
                           [desc any/c])]
 [(aux-element element) ()]
 [(hover-element element) ([text string?])]
 [(script-element element) ([type string?]
                            [script (or/c path-string? (listof string?))])]
 ;; specific renders support other elements, especially strings

 [with-attributes ([style any/c]
                   [assoc (listof (cons/c symbol? string?))])]

 [collected-info ([number (listof (or/c false/c integer?))]
                  [parent (or/c false/c part?)]
                  [info any/c])]

 [target-url ([addr path-string?] [style any/c])]
 [url-anchor ([name string?])]
 [image-file ([path (or/c path-string?
                          (cons/c (one-of/c 'collects)
                                  (listof bytes?)))]
              [scale real?])])

;; ----------------------------------------

;; Delayed element has special serialization support:
(define-struct delayed-element (resolve sizer plain)
  #:property
  prop:serializable
  (make-serialize-info
   (lambda (d)
     (let ([ri (current-serialize-resolve-info)])
       (unless ri
         (error 'serialize-delayed-element
                "current-serialize-resolve-info not set"))
       (with-handlers ([exn:fail:contract?
                        (lambda (exn)
                          (error 'serialize-delayed-element
                                 "serialization failed (wrong resolve info? delayed element never rendered?); ~a"
                                 (exn-message exn)))])
         (vector
          (let ([l (delayed-element-content d ri)])
            (if (and (pair? l) (null? (cdr l)))
              (car l)
              (make-element #f l)))))))
   #'deserialize-delayed-element
   #f
   (or (current-load-relative-directory) (current-directory))))

(provide/contract
 (struct delayed-element ([resolve (any/c part? resolve-info? . -> . list?)]
                          [sizer (-> any)]
                          [plain (-> any)])))

(provide deserialize-delayed-element)
(define deserialize-delayed-element
  (make-deserialize-info values values))

(provide delayed-element-content)
(define (delayed-element-content e ri)
  (hash-ref (resolve-info-delays ri) e))

(provide delayed-block-blocks)
(define (delayed-block-blocks p ri)
  (hash-ref (resolve-info-delays ri) p))

(provide current-serialize-resolve-info)
(define current-serialize-resolve-info (make-parameter #f))

;; ----------------------------------------

;; part-relative element has special serialization support:
(define-struct part-relative-element (collect sizer plain)
  #:property
  prop:serializable
  (make-serialize-info
   (lambda (d)
     (let ([ri (current-serialize-resolve-info)])
       (unless ri
         (error 'serialize-part-relative-element
                "current-serialize-resolve-info not set"))
       (with-handlers ([exn:fail:contract?
                        (lambda (exn)
                          (error 'serialize-part-relative-element
                                 "serialization failed (wrong resolve info? part-relative element never rendered?); ~a"
                                 (exn-message exn)))])
         (vector
          (make-element #f (part-relative-element-content d ri))))))
   #'deserialize-part-relative-element
   #f
   (or (current-load-relative-directory) (current-directory))))

(provide/contract
 (struct part-relative-element ([collect (collect-info? . -> . list?)]
                                [sizer (-> any)]
                                [plain (-> any)])))

(provide deserialize-part-relative-element)
(define deserialize-part-relative-element
  (make-deserialize-info values values))

(provide part-relative-element-content)
(define (part-relative-element-content e ci/ri)
  (hash-ref (collect-info-relatives
             (if (resolve-info? ci/ri) (resolve-info-ci ci/ri) ci/ri))
            e))

(provide collect-info-parents)

;; ----------------------------------------

;; Delayed index entry also has special serialization support.
;; It uses the same delay -> value table as delayed-element
(define-struct delayed-index-desc (resolve)
  #:mutable
  #:property
  prop:serializable 
  (make-serialize-info
   (lambda (d)
     (let ([ri (current-serialize-resolve-info)])
       (unless ri
         (error 'serialize-delayed-index-desc
                "current-serialize-resolve-info not set"))
       (with-handlers ([exn:fail:contract?
                        (lambda (exn)
                          (error 'serialize-index-desc
                                 "serialization failed (wrong resolve info?); ~a"
                                 (exn-message exn)))])
         (vector
          (delayed-element-content d ri)))))
   #'deserialize-delayed-index-desc
   #f
   (or (current-load-relative-directory) (current-directory))))

(provide/contract
 (struct delayed-index-desc ([resolve (any/c part? resolve-info? . -> . any)])))

(provide deserialize-delayed-index-desc)
(define deserialize-delayed-index-desc
  (make-deserialize-info values values))

;; ----------------------------------------

(define-struct (collect-element element) (collect)
  #:mutable
  #:property
  prop:serializable
  (make-serialize-info
   (lambda (d)
     (vector (make-element
              (element-style d)
              (element-content d))))
   #'deserialize-collect-element
   #f
   (or (current-load-relative-directory) (current-directory))))

(provide deserialize-collect-element)
(define deserialize-collect-element
  (make-deserialize-info values values))

(provide/contract
 [struct collect-element ([style any/c]
                          [content list?]
                          [collect (collect-info? . -> . any)])])

;; ----------------------------------------

(define-struct (render-element element) (render)
  #:property
  prop:serializable
  (make-serialize-info
   (lambda (d)
     (vector (make-element
              (element-style d)
              (element-content d))))
   #'deserialize-render-element
   #f
   (or (current-load-relative-directory) (current-directory))))

(provide deserialize-render-element)
(define deserialize-render-element
  (make-deserialize-info values values))

(provide/contract
 [struct render-element ([style any/c]
                         [content list?]
                         [render (any/c part? resolve-info? . -> . any)])])

;; ----------------------------------------

(define-struct generated-tag ()
  #:property
  prop:serializable
  (make-serialize-info
   (lambda (g)
     (let ([ri (current-serialize-resolve-info)])
       (unless ri
         (error 'serialize-generated-tag
                "current-serialize-resolve-info not set"))
       (let ([t (hash-ref (collect-info-tags (resolve-info-ci ri)) g #f)])
         (if t
           (vector t)
           (error 'serialize-generated-tag
                  "serialization failed (wrong resolve info?)")))))
   #'deserialize-generated-tag
   #f
   (or (current-load-relative-directory) (current-directory))))

(provide (struct-out generated-tag))

(provide deserialize-generated-tag)
(define deserialize-generated-tag
  (make-deserialize-info values values))

(provide generate-tag tag-key
         current-tag-prefixes
         add-current-tag-prefix)

(define (generate-tag tg ci)
  (if (generated-tag? (cadr tg))
      (let ([t (cadr tg)])
        (list (car tg)
              (let ([tags (collect-info-tags ci)])
                (or (hash-ref tags t #f)
                    (let ([key (list* 'gentag
                                      (hash-count tags)
                                      (collect-info-gen-prefix ci))])
                      (hash-set! tags t key)
                      key)))))
      tg))

(define (tag-key tg ri)
  (if (generated-tag? (cadr tg))
      (list (car tg)
            (hash-ref (collect-info-tags (resolve-info-ci ri)) (cadr tg)))
      tg))

(define current-tag-prefixes (make-parameter null))
(define (add-current-tag-prefix t)
  (let ([l (current-tag-prefixes)])
    (if (null? l)
        t
        (cons (car t) (append l (cdr t))))))

;; ----------------------------------------

(provide content->string
         element->string
         strip-aux)

(define content->string
  (case-lambda
    [(c) (c->s c element->string)]
    [(c renderer sec ri)
     (c->s c (lambda (e) (element->string e renderer sec ri)))]))

(define (c->s c do-elem)
  (apply string-append (map do-elem c)))

(define element->string
  (case-lambda
    [(c)
     (cond
       [(element? c) (content->string (element-content c))]
       [(part-relative-element? c) (element->string ((part-relative-element-plain c)))]
       [(delayed-element? c) (element->string ((delayed-element-plain c)))]
       [(string? c) c]
       [else (case c
               [(mdash) "---"]
               [(ndash) "--"]
               [(ldquo rdquo) "\""]
               [(rsquo) "'"]
               [(rarr) "->"]
               [(lang) "<"]
               [(rang) ">"]
               [else (format "~s" c)])])]
    [(c renderer sec ri)
     (cond
       [(and (link-element? c)
             (null? (element-content c)))
        (let ([dest (resolve-get sec ri (link-element-tag c))])
          ;; FIXME: this is specific to renderer
          (if dest
            (content->string (strip-aux
                              (if (pair? dest) (cadr dest) (vector-ref dest 1)))
                             renderer sec ri)
            "???"))]
       [(element? c) (content->string (element-content c) renderer sec ri)]
       [(delayed-element? c)
        (content->string (delayed-element-content c ri) renderer sec ri)]
       [(part-relative-element? c)
        (content->string (part-relative-element-content c ri) renderer sec ri)]
       [else (element->string c)])]))

(define (strip-aux content)
  (cond
    [(null? content) null]
    [(aux-element? (car content)) (strip-aux (cdr content))]
    [else (cons (car content) (strip-aux (cdr content)))]))

;; ----------------------------------------

(provide block-width
         element-width)

(define (element-width s)
  (cond
    [(string? s) (string-length s)]
    [(element? s) (apply + (map element-width (element-content s)))]
    [(delayed-element? s) (element-width ((delayed-element-sizer s)))]
    [(part-relative-element? s) (element-width ((part-relative-element-sizer s)))]
    [else 1]))

(define (paragraph-width s)
  (apply + (map element-width (paragraph-content s))))

(define (flow-width f)
  (apply max 0 (map block-width (flow-paragraphs f))))

(define (block-width p)
  (cond
    [(paragraph? p) (paragraph-width p)]
    [(table? p) (table-width p)]
    [(itemization? p) (itemization-width p)]
    [(blockquote? p) (blockquote-width p)]
    [(compound-paragraph? p) (compound-paragraph-width p)]
    [(delayed-block? p) 1]))

(define (table-width p)
  (let ([flowss (table-flowss p)])
    (if (null? flowss)
      0
      (let loop ([flowss flowss])
        (if (null? (car flowss))
          0
          (+ (apply max 0 (map flow-width (map car flowss)))
             (loop (map cdr flowss))))))))

(define (itemization-width p)
  (apply max 0 (map flow-width (itemization-flows p))))

(define (blockquote-width p)
  (+ 4 (apply max 0 (map block-width (blockquote-paragraphs p)))))

(define (compound-paragraph-width p)
  (apply max 0 (map block-width (compound-paragraph-blocks p))))

;; ----------------------------------------

(provide part-style?)

(define (part-style? p s)
  (let ([st (part-style p)])
    (or (eq? s st)
        (and (list? st) (memq s st)))))

;; ----------------------------------------

(define (info-key? l)
  (and (pair? l)
       (symbol? (car l))
       (pair? (cdr l))))

(provide info-key?)
(provide/contract
 [part-collected-info (part? resolve-info? . -> . collected-info?)]
 [collect-put! (collect-info? info-key?  any/c . -> . any)]
 [resolve-get ((or/c part? false/c) resolve-info? info-key? . -> . any)]
 [resolve-get/tentative ((or/c part? false/c) resolve-info? info-key? . -> . any)]
 [resolve-get/ext? ((or/c part? false/c) resolve-info? info-key? . -> . any)]
 [resolve-search (any/c (or/c part? false/c) resolve-info? info-key? . -> . any)]
 [resolve-get-keys ((or/c part? false/c) resolve-info? (info-key? . -> . any/c) . -> . any/c)])

;; ----------------------------------------

(define (flatten-style s)
  (cond
   [(with-attributes? s)
    (let ([rest (flatten-style (with-attributes-style s))])
      (if (with-attributes? rest)
          ;; collapse nested with-attributes
          (make-with-attributes 
           (with-attributes-style rest)
           (append (with-attributes-assoc s)
                   (with-attributes-assoc rest)))
          ;; rebuild with flattened inner:
          (make-with-attributes 
           rest
           (with-attributes-assoc s))))]
   [(target-url? s)
    (let ([rest (flatten-style (target-url-style s))])
      (if (with-attributes? rest)
          ;; lift nested attributes out:
          (make-with-attributes 
           (make-target-url
            (target-url-addr s)
            (with-attributes-style rest))
           (with-attributes-assoc rest))
          ;; rebuild with flattened inner:
          (make-target-url
           (target-url-addr s)
           rest)))]
   [else s]))

(provide flatten-style)

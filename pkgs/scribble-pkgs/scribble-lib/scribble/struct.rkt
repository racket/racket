#lang scheme/base
(require (rename-in (except-in "core.rkt"
                               target-url struct:target-url target-url? target-url-addr
                               deserialize-info:target-url-v0)
                    [make-target-url core:make-target-url])
         "private/provide-structs.rkt"
         "html-properties.rkt"
         scheme/provide-syntax
         scheme/struct-info
         racket/contract/base
         (for-syntax scheme/base))

(define-provide-syntax (compat**-out stx)
  (syntax-case stx ()
    [(_ struct-out o) 
     (let ([id (syntax-case #'o ()
                 [(id (field-id ...)) #'id]
                 [id #'id])])
       (with-syntax ([make-id (datum->syntax id
                                             (string->symbol (format "make-~a" (syntax-e id)))
                                             id)]
                     [make-id/compat (datum->syntax id
                                                    (string->symbol (format "make-~a/compat" (syntax-e id)))
                                                    id)])
         #'(combine-out
            (except-out (struct-out o) make-id)
            (rename-out [make-id/compat make-id]))))]
    [(_ struct-out o ...) #'(combine-out (compat**-out struct-out o) ...)]))

(define-provide-syntax (compat-out stx)
  (syntax-case stx ()
    [(_ . outs) #'(compat**-out struct-out . outs)]))

(define-provide-syntax (compat*-out stx)
  (syntax-case stx ()
    [(_ . outs) #'(compat**-out struct*-out . outs)]))

(define-provide-syntax (struct*-out stx)
  (syntax-case stx ()
    [(_ [id (field-id ...)]) 
     (with-syntax ([id? (datum->syntax #'id
                                       (string->symbol (format "~a?" (syntax-e #'id)))
                                       #'id)]
                   [struct:id (datum->syntax #'id
                                             (string->symbol (format "struct:~a" (syntax-e #'id)))
                                             #'id)]
                   [make-id (datum->syntax #'id
                                           (string->symbol (format "make-~a" (syntax-e #'id)))
                                           #'id)]
                   [(sel-id ...)
                    (map (lambda (field-id)
                           (datum->syntax field-id
                                          (string->symbol (format "~a-~a" (syntax-e #'id) (syntax-e field-id)))
                                          field-id))
                         (syntax->list #'(field-id ...)))])
              #'(combine-out
                 id struct:id make-id id? sel-id ...))]
    [(_ [id (field-id ...)]...)
     #'(combine-out (struct*-out [id (field-id ...)]) ...)]))

(provide (struct-out collect-info)
         (struct-out resolve-info)
         tag? block?
         
         make-flow flow? flow-paragraphs

         (except-out (compat-out part) part-title-content)
         (rename-out [part-blocks part-flow]
                     [part-title-content/compat part-title-content])
         make-versioned-part versioned-part?
         make-unnumbered-part unnumbered-part?

         (except-out (compat-out paragraph) paragraph-content)
         (rename-out [paragraph-content/compat paragraph-content])
         make-styled-paragraph
         (rename-out [paragraph? styled-paragraph?]
                     [paragraph-style styled-paragraph-style])
         make-omitable-paragraph omitable-paragraph?

         (compat-out table) 
         table-flowss
         make-auxiliary-table auxiliary-table?

         (struct-out delayed-block)

         (compat-out itemization)
         (rename-out [itemization-blockss itemization-flows]
                     [itemization? styled-itemization?]
                     [itemization-style styled-itemization-style])
         make-styled-itemization

         make-blockquote

         (compat-out compound-paragraph)

         (except-out (compat-out element) element? element-style element-content)
         (rename-out [element?/compat element?]
                     [element-style/compat element-style]
                     [element-content/compat element-content])
         (except-out (compat*-out [toc-element (toc-content)])
                     toc-element-toc-content)
         (rename-out [toc-element-toc-content/compat toc-element-toc-content])
         (compat*-out [target-element (tag)]
                      [toc-target-element ()]
                      [page-target-element ()]
                      [redirect-target-element (alt-path alt-anchor)]
                      [link-element (tag)]
                      [index-element (tag plain-seq entry-seq desc)])
         make-aux-element aux-element?
         make-hover-element hover-element? hover-element-text
         make-script-element script-element? script-element-type script-element-script

         (struct-out collected-info)

         (struct-out delayed-element)
         ; delayed-element-content delayed-block-blocks current-serialize-resolve-info
         
         (struct-out part-relative-element)
         ; part-relative-element-content collect-info-parents

         (struct-out delayed-index-desc)

         (struct*-out [collect-element (collect)])

         (struct*-out [render-element (render)])

         (struct-out generated-tag)
         ; generate-tag tag-key current-tag-prefixes add-current-tag-prefix

         content->string
         (rename-out [content->string element->string]
                     [content-width element-width])
         ; strip-aux

         block-width

         info-key? part-collected-info collect-put!
         resolve-get resolve-get/tentative resolve-get/ext? resolve-search resolve-get-keys)

(provide-structs
 [with-attributes ([style any/c]
                   [assoc (listof (cons/c symbol? string?))])]
 [image-file ([path (or/c path-string?
                          (cons/c (one-of/c 'collects)
                                  (listof bytes?)))]
              [scale real?])]
 [target-url ([addr path-string?] [style any/c])])

(define (make-flow l) l)
(define (flow? l) (and (list? l) (andmap block? l)))
(define (flow-paragraphs l) l)

(define (list->content l)
  (if (and (pair? l) (null? (cdr l)))
      (car l)
      l))

(define (content->list v)
  (if (list? v)
      v
      (list v)))

(define (make-part/compat tag-prefix tags title-content orig-style to-collect flow parts)
  (make-part tag-prefix 
             tags
             (list->content title-content)
             (convert-style orig-style)
             to-collect 
             (flow-paragraphs flow)
             parts))

(define (part-title-content/compat p)
  (list (part-title-content p)))

(define (make-versioned-part tag-prefix tags title-content orig-style to-collect flow parts version)
  (make-part tag-prefix 
             tags
             (list->content title-content)
             (let ([s (convert-style orig-style)])
               (make-style (style-name s)
                           (cons
                            (make-document-version version)
                            (style-properties s))))
             to-collect 
             (flow-paragraphs flow)
             parts))
(define (versioned-part? p)
  (and (part? p) (ormap document-version? (style-properties (part-style p)))))

(define (make-unnumbered-part tag-prefix tags title-content orig-style to-collect flow parts)
  (make-part tag-prefix 
             tags
             (list->content title-content)
             (let ([s (convert-style orig-style)])
               (make-style (style-name s)
                           (cons 'unnumbered (style-properties s))))
             to-collect 
             (flow-paragraphs flow)
             parts))
(define (unnumbered-part? p)
  (and (part? p) (memq 'unnumbered (style-properties (part-style p)))))

(define (make-paragraph/compat content)
  (make-paragraph plain (list->content content)))
(define (paragraph-content/compat p)
  (content->list (paragraph-content p)))
(define (make-styled-paragraph content style)
  (make-paragraph (convert-style style) (list->content content)))

(define (make-omitable-paragraph content)
  (make-paragraph (make-style #f '(omitable)) (list->content content)))
(define (omitable-paragraph? p)
  (and (paragraph? p) (memq 'omitable (style-properties (paragraph-style p)))))

(define (make-table/compat style cellss)
  (make-table (convert-style style)
              (map (lambda (cells)
                     (map (lambda (cell)
                            (cond
                             [(eq? cell 'cont) 'cont]
                             [(= 1 (length cell)) (car cell)]
                             [else (make-nested-flow plain cell)]))
                          cells))
                   cellss)))
(define (table-flowss t)
  (map (lambda (row) (map (lambda (c) (make-flow (list c))) row))
       (table-blockss t)))

(define (make-auxiliary-table style cells)
  (let ([t (make-table/compat style cells)])
    (make-table (make-style (style-name (table-style t))
                            (cons 'aux
                                  (style-properties (table-style t))))
                (table-blockss t))))

(define (auxiliary-table? t)
  (ormap (lambda (v) (eq? v 'aux) (style-properties (table-style t)))))

(define (make-itemization/compat flows)
  (make-itemization plain flows))
(define (make-styled-itemization style flows)
  (make-itemization (convert-style style) flows))

(define (make-blockquote style blocks)
  (make-nested-flow (convert-style (or style 'inset)) blocks))

(define (make-compound-paragraph/compat style blocks)
  (make-compound-paragraph (convert-style style) blocks))

(define (element-style-name s)
  (if (style? s)
      (style-name s)
      s))
(define (element-style-properties s)
  (if (style? s)
      (style-properties s)
      null))

(define (add-element-property v e)
  (make-element (make-style (element-style-name (element-style e))
                            (cons v
                                  (element-style-properties (element-style e))))
                (element-content e)))
(define (check-element-style e pred)
  (ormap pred (style-properties (element-style e))))

(define (handle-image-style ctr style . args)
  (if (image-file? style)
      (make-image-element #f (list (apply ctr #f args)) 
                          (image-file-path style)
                          null
                          (image-file-scale style))
      (apply ctr (convert-element-style style) args)))

(define (convert-element-style style)
  (cond
   [(not style) style]
   [(string? style) style]
   [(symbol? style) style]
   [else (convert-style style)]))

(define (element?/compat e)
  (or (element? e) (and (list? e) (content? e))))
(define (element-content/compat e)
  (cond
   [(element? e) (content->list (element-content e))]
   [else e]))
(define (element-style/compat e)
  (cond
   [(element? e) (element-style e)]
   [else #f]))

(define (make-element/compat style content)
  (handle-image-style make-element style (list->content content)))
(define (make-toc-element/compat style content toc-content)
  (handle-image-style make-toc-element style (list->content content) (list->content toc-content)))
(define (toc-element-toc-content/compat e)
  (content->list (toc-element-toc-content e)))
(define (make-target-element/compat style content tag)
  (handle-image-style make-target-element style (list->content content) tag))
(define (make-toc-target-element/compat style content tag)
  (handle-image-style make-toc-target-element style (list->content content) tag))
(define (make-page-target-element/compat style content tag)
  (handle-image-style make-page-target-element style (list->content content) tag))
(define (make-redirect-target-element/compat style content tag alt-path alt-anchor)
  (handle-image-style make-redirect-target-element style (list->content content) tag alt-path alt-anchor))
(define (make-link-element/compat style content tag)
  (handle-image-style make-link-element style (list->content content) tag))
(define (make-index-element/compat style content tag plain-seq etry-seq desc)
  (handle-image-style make-index-element style (list->content content) tag plain-seq etry-seq desc))

(define (make-aux-element style content)
  (add-element-property 'aux (make-element/compat style content)))
(define (aux-element? e)
  (check-element-style e (lambda (v) (eq? v 'aux))))

(define (make-hover-element style content text)
  (add-element-property (make-hover-property text)
                        (make-element/compat style content)))
(define (hover-element? e)
  (check-element-style e hover-property?))
(define (hover-element-text e)
  (ormap (lambda (v)
           (and (hover-property? v) (hover-property-text e)))
         (style-properties (element-style e))))

(define (make-script-element style content type script)
  (add-element-property (make-script-property type script)
                        (make-element/compat style content)))
(define (script-element? e)
  (check-element-style e script-property?))
(define (script-element-type e)
  (ormap (lambda (v)
           (and (script-property? v) (script-property-type e)))
         (style-properties (element-style e))))
(define (script-element-script e)
  (ormap (lambda (v)
           (and (script-property? v) (script-property-script e)))
         (style-properties (element-style e))))

;; ----------------------------------------

(define (convert-style s)
  (cond
   [(not s) plain]
   [(style? s) s]
   [(string? s) (make-style s null)]
   [(symbol? s) (make-style s null)]
   [(and (list? s) (andmap symbol? s)) (make-style #f s)]
   [(with-attributes? s) (let* ([wa (flatten-style s)]
                                [s (convert-style (with-attributes-style wa))])
                           (make-style (style-name s)
                                       (cons
                                        (make-attributes (with-attributes-assoc wa))
                                        (style-properties s))))]
   [(target-url? s) (let ([s (convert-style (target-url-style s))])
                      (make-style (style-name s)
                                       (cons
                                        (core:make-target-url (target-url-addr s))
                                        (style-properties s))))]
   [(image-file? s) (make-style #f null)]
   [(and (list? s) (pair? s) (eq? (car s) 'color))
    (make-style #f (list (make-color-property
                          (if (string? (cadr s)) (cadr s) (cdr s)))))]
   [(and (list? s) (pair? s) (eq? (car s) 'bg-color))
    (make-style #f (list (make-background-color-property
                          (if (string? (cadr s)) (cadr s) (cdr s)))))]
   [(and (pair? s)
         (list? s)
         (andmap (lambda (v) (and (pair? v) 
                                  (memq (car v) '(alignment valignment row-styles style))))
                 s))
    (let ([gen-columns (lambda (sn a va)
                         (map (lambda (sn a va)
                                (make-style sn
                                            (append (if a (list a) null)
                                                    (if va (list va) null))))
                              (cdr (or sn (map (lambda (x) #f) (or va a))))
                              (cdr (or a (map (lambda (x) #f) (or va sn))))
                              (cdr (or va (map (lambda (x) #f) (or a sn))))))])
      (make-style (let ([s (assq 'style s)])
                    (and s (cadr s)))
                  (let ([a (assq 'alignment s)]
                        [va (assq 'valignment s)])
                    (if (or a va)
                        (list (make-table-columns (gen-columns #f a va)))
                        (let ([l (cdr (assq 'row-styles s))])
                          (list
                           (make-table-cells
                            (map (lambda (row)
                                   (let ([sn (assq 'style row)]
                                         [a (assq 'alignment row)]
                                         [va (assq 'valignment row)])
                                     (if (or sn a va)
                                         (gen-columns sn a va)
                                         (error 'convert-style "no row style found"))))
                                 l))))))))]
   [else (error 'convert-style "unrecognized style: ~e" s)]))

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

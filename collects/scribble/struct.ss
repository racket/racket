
(module struct (lib "lang.ss" "big")
  (require (lib "contract.ss")
           (lib "serialize.ss"))

  ;; ----------------------------------------
  
  (define-struct collect-info (ht ext-ht parts tags gen-prefix))
  (define-struct resolve-info (ci delays undef))

  (define (part-collected-info part ri)
    (hash-table-get (collect-info-parts (resolve-info-ci ri))
                    part))


  (define (collect-put! ci key val)
    (hash-table-put! (collect-info-ht ci)
                     key
                     val))

  (define (resolve-get/where part ri key)
    (let ([key (tag-key key ri)])
      (let ([v (hash-table-get (if part
                                   (collected-info-info (part-collected-info part ri))
                                   (collect-info-ht (resolve-info-ci ri)))
                               key
                               #f)])
        (cond
         [v (values v #f)]
         [part (resolve-get/where (collected-info-parent
                                   (part-collected-info part ri))
                                  ri
                                  key)]
         [else
          (let ([v (hash-table-get (collect-info-ext-ht (resolve-info-ci ri))
                                   key
                                   #f)])
            (values v #t))]))))

  (define (resolve-get part ri key)
    (let-values ([(v ext?) (resolve-get/where part ri key)])
      (when ext?
        (hash-table-put! (resolve-info-undef ri)
                         (tag-key key ri)
                         #t))
      v))

  (define (resolve-get/tentative part ri key)
    (let-values ([(v ext?) (resolve-get/where part ri key)])
      v))

  (provide 
   (struct collect-info (ht ext-ht parts tags gen-prefix))
   (struct resolve-info (ci delays undef))
   part-collected-info
   collect-put!
   resolve-get
   resolve-get/tentative)

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
                 (letrec ([get-fields (lambda (super-id)
                                        (ormap (lambda (id  fields+cts)
                                                 (if (identifier? id)
                                                     (and (module-identifier=? id super-id)
                                                          fields+cts)
                                                     (syntax-case id ()
                                                       [(my-id next-id)
                                                        (module-identifier=? #'my-id super-id)
                                                        #`[#,@(get-fields #'next-id)
                                                           #,@fields+cts]]
                                                       [_else #f])))
                                               ids fields+cts))])
                   (map (lambda (id fields+cts)
                          (if (identifier? id)
                              #`[struct #,id #,fields+cts]
                              (syntax-case id ()
                                [(id super)
                                 #`[struct id (#,@(get-fields #'super) 
                                               #,@fields+cts)]])))
                        ids
                        fields+cts)))))]))

  (provide tag?)
  (define (tag? s) (and (pair? s)
                        (symbol? (car s))
                        (pair? (cdr s))
                        (or (string? (cadr s))
                            (generated-tag? (cadr s)))
                        (null? (cddr s))))

  (provide flow-element?)
  (define (flow-element? p)
    (or (paragraph? p)
        (table? p)
        (itemization? p)
        (blockquote? p)
        (delayed-flow-element? p)))

  (provide-structs 
   [part ([tag-prefix (or/c false/c string?)]
          [tags (listof tag?)]
          [title-content (or/c false/c list?)]
          [style any/c]
          [to-collect list?]
          [flow flow?]
          [parts (listof part?)])]
   [(unnumbered-part part) ()]
   [flow ([paragraphs (listof flow-element?)])]
   [paragraph ([content list?])]
   [(styled-paragraph paragraph) ([style any/c])]
   [table ([style any/c]
           [flowss (listof (listof (or/c flow? (one-of/c 'cont))))])]
   [(auxiliary-table table) ()]
   [delayed-flow-element ([resolve (any/c part? resolve-info? . -> . flow-element?)])]
   [itemization ([flows (listof flow?)])]
   [blockquote ([style any/c]
                [paragraphs (listof flow-element?)])]
   ;; content = list of elements
   [element ([style any/c]
             [content list?])]
   [(target-element element) ([tag tag?])]
   [(toc-target-element target-element) ()]
   [(page-target-element target-element) ()]
   [(link-element element) ([tag tag?])]
   [(index-element element) ([tag tag?]
                             [plain-seq (listof string?)]
                             [entry-seq list?])]
   [(aux-element element) ()]
   [(hover-element element) ([text string?])]
   ;; specific renders support other elements, especially strings

   [collected-info ([number (listof (or/c false/c integer?))]
                    [parent (or/c false/c part?)]
                    [info any/c])]

   [target-url ([addr string?])]
   [image-file ([path path-string?])])
  
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
                                   "serialization failed (wrong resolve info?); ~a"
                                   (exn-message exn)))])
           (vector
            (make-element #f (delayed-element-content d ri))))))
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
    (hash-table-get (resolve-info-delays ri) e))

  (provide delayed-flow-element-flow-elements)
  (define (delayed-flow-element-flow-elements p ri)
    (hash-table-get (resolve-info-delays ri) p))

  (provide current-serialize-resolve-info)
  (define current-serialize-resolve-info (make-parameter #f))

  ;; ----------------------------------------

  (define-struct (collect-element element) (collect)
    #:property 
    prop:serializable 
    (make-serialize-info
     (lambda (d)
       (vector (collect-element-collect d)))
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

  (define-struct generated-tag ()
    #:property
    prop:serializable 
    (make-serialize-info
     (lambda (g)
       (let ([ri (current-serialize-resolve-info)])
         (unless ri
           (error 'serialize-generated-tag
                  "current-serialize-resolve-info not set"))
         (let ([t (hash-table-get (collect-info-tags
                                   (resolve-info-ci ri))
                                  g
                                  #f)])
           (if t
               (vector t)
               (error 'serialize-generated-tag
                      "serialization failed (wrong resolve info?)")))))
     #'deserialize-generated-tag
     #f
     (or (current-load-relative-directory) (current-directory))))

  (provide
   (struct generated-tag ()))

  (provide deserialize-generated-tag)
  (define deserialize-generated-tag
    (make-deserialize-info values values))

  (provide generate-tag tag-key)

  (define (generate-tag tg ci)
    (if (generated-tag? (cadr tg))
        (let ([t (cadr tg)])
          (list (car tg)
                (let ([tags (collect-info-tags ci)])
                  (or (hash-table-get tags t #f)
                      (let ([key (format "gentag:~a~a" 
                                         (collect-info-gen-prefix ci)
                                         (hash-table-count tags))])
                        (hash-table-put! tags t key)
                        key)))))
        tg))

  (define (tag-key tg ri)
    (if (generated-tag? (cadr tg))
        (list (car tg)
              (hash-table-get (collect-info-tags
                               (resolve-info-ci ri))
                              (cadr tg)))
        tg))

  ;; ----------------------------------------

  (provide content->string
           element->string)

  (define content->string
    (case-lambda
     [(c) (c->s c element->string)]
     [(c renderer sec ri) (c->s c (lambda (e)
                                    (element->string e renderer sec ri)))]))

  (define (c->s c do-elem)
    (apply string-append
           (map do-elem c)))

  (define element->string
    (case-lambda
     [(c)
      (cond
       [(element? c) (content->string (element-content c))]
       [(delayed-element? c) (element->string ((delayed-element-plain c)))]
       [(string? c) c]
       [else (case c
               [(ndash) "--"]
               [(ldquo rdquo) "\""]
               [(rsquo) "'"]
               [(rarr) "->"]
               [else (format "~s" c)])])]
     [(c renderer sec ri)
      (cond
       [(element? c) (content->string (element-content c) renderer sec ri)]
       [(delayed-element? c) 
        (content->string (delayed-element-content c ri)
                         renderer sec ri)]
       [else (element->string c)])]))

  ;; ----------------------------------------
  
  (provide flow-element-width
           element-width)

  (define (element-width s)
    (cond
     [(string? s) (string-length s)]
     [(element? s) (apply + (map element-width (element-content s)))]
     [(delayed-element? s) (element-width ((delayed-element-sizer s)))]
     [else 1]))

  (define (paragraph-width s)
    (apply + (map element-width (paragraph-content s))))

  (define (flow-width f)
    (apply max 0 (map flow-element-width (flow-paragraphs f))))

  (define (flow-element-width p)
    (cond
     [(paragraph? p) (paragraph-width p)]
     [(table? p) (table-width p)]
     [(itemization? p) (itemization-width p)]
     [(blockquote? p) (blockquote-width p)]
     [(delayed-flow-element? p) 1]))

  (define (table-width p)
    (let ([flowss (table-flowss p)])
      (if (null? flowss)
          0
          (let loop ([flowss flowss])
            (if (null? (car flowss))
                0
                (+ (apply max 
                          0
                          (map flow-width
                               (map car flowss)))
                   (loop (map cdr flowss))))))))

  (define (itemization-width p)
    (apply max 0 (map flow-width (itemization-flows p))))

  (define (blockquote-width p)
    (+ 4 (apply max 0 (map paragraph-width (blockquote-paragraphs p)))))

  ;; ----------------------------------------

  (provide part-style?)

  (define (part-style? p s)
    (let ([st (part-style p)])
      (or (eq? s st)
          (and (list? st) (memq s st)))))

  ;; ----------------------------------------

  )


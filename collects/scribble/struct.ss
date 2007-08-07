
(module struct mzscheme
  (require (lib "contract.ss")
           (lib "serialize.ss"))

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
  (define (tag? s) (or (string? s)
                       (and (pair? s)
                            (symbol? (car s))
                            (pair? (cdr s))
                            (string? (cadr s))
                            (null? (cddr s)))))

  (provide flow-element?)
  (define (flow-element? p)
    (or (paragraph? p)
        (table? p)
        (itemization? p)
        (blockquote? p)
        (delayed-flow-element? p)))

  (provide-structs 
   [part ([tags (listof tag?)]
          [title-content (or/c false/c list?)]
          [collected-info (or/c false/c collected-info?)]
          [to-collect list?]
          [flow flow?]
          [parts (listof part?)])]
   [(styled-part part) ([style any/c])]
   [(unnumbered-part styled-part) ()]
   [flow ([paragraphs (listof flow-element?)])]
   [paragraph ([content list?])]
   [(styled-paragraph paragraph) ([style any/c])]
   [table ([style any/c]
           [flowss (listof (listof (or/c flow? (one-of/c 'cont))))])]
   [delayed-flow-element ([render (any/c part? any/c . -> . flow-element?)])]
   [itemization ([flows (listof flow?)])]
   [blockquote ([style any/c]
                [paragraphs (listof flow-element?)])]
   ;; content = list of elements
   [element ([style any/c]
             [content list?])]
   [(target-element element) ([tag tag?])]
   [(toc-target-element target-element) ()]
   [(link-element element) ([tag tag?])]
   [(index-element element) ([tag tag?]
                             [plain-seq (listof string?)]
                             [entry-seq list?])]
   [(aux-element element) ()]
   ;; specific renders support other elements, especially strings

   [collected-info ([number (listof (or/c false/c integer?))]
                    [parent (or/c false/c part?)]
                    [info any/c])]

   [target-url ([addr string?])]
   [image-file ([path path-string?])])

  ;; ----------------------------------------

  ;; Delayed element has special serialization support:
  (define-values (struct:delayed-element
                  make-delayed-element
                  delayed-element?
                  delayed-element-ref
                  delayed-element-set!)
    (make-struct-type 'delayed-element #f
                      3 1 #f
                      (list (cons prop:serializable 
                                  (make-serialize-info
                                   (lambda (d)
                                     (unless (delayed-element-ref d 3)
                                       (error 'serialize-delayed-element
                                              "cannot serialize a delayed element that was not resolved: ~e"
                                              d))
                                     (vector (delayed-element-ref d 3)))
                                   #'deserialize-delayed-element
                                   #f
                                   (or (current-load-relative-directory) (current-directory)))))))
  (define-syntax delayed-element (list-immutable #'struct:delayed-element
                                                 #'make-delayed-element
                                                 #'delayed-element?
                                                 (list-immutable #'delayed-element-plain 
                                                                 #'delayed-element-sizer
                                                                 #'delayed-element-render)
                                                 (list-immutable #'set-delayed-element-plain!
                                                                 #'set-delayed-element-sizer!
                                                                 #'set-delayed-element-render!)
                                                 #t))
  (define delayed-element-render (make-struct-field-accessor delayed-element-ref 0))
  (define delayed-element-sizer (make-struct-field-accessor delayed-element-ref 1))
  (define delayed-element-plain (make-struct-field-accessor delayed-element-ref 2))
  (define set-delayed-element-render! (make-struct-field-mutator delayed-element-set! 0))
  (define set-delayed-element-sizer! (make-struct-field-mutator delayed-element-set! 1))
  (define set-delayed-element-plain! (make-struct-field-mutator delayed-element-set! 2))
  (provide/contract
   (struct delayed-element ([render (any/c part? any/c . -> . list?)]
                            [sizer (-> any)]
                            [plain (-> any)])))
  
  (provide deserialize-delayed-element)
  (define deserialize-delayed-element
    (make-deserialize-info values values))
  
  (provide force-delayed-element)
  (define (force-delayed-element d renderer sec ht)
    (or (delayed-element-ref d 3)
        (let ([v ((delayed-element-ref d 0) renderer sec ht)])
          (delayed-element-set! d 3 v)
          v)))

  ;; ----------------------------------------

  (provide content->string
           element->string)

  (define content->string
    (case-lambda
     [(c) (c->s c element->string)]
     [(c renderer sec ht) (c->s c (lambda (e)
                                    (element->string e renderer sec ht)))]))

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
     [(c renderer sec ht)
      (cond
       [(element? c) (content->string (element-content c) renderer sec ht)]
       [(delayed-element? c) 
        (content->string (force-delayed-element c renderer sec ht)
                         renderer sec ht)]
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

  )


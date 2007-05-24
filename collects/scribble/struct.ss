
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
        (delayed-flow-element? p)))

  (provide-structs 
   [part ([tag (or/c false/c tag?)]
          [title-content (or/c false/c list?)]
          [collected-info (or/c false/c collected-info?)]
          [flow flow?]
          [parts (listof part?)])]
   [(unnumbered-part part) ()]
   [flow ([paragraphs (listof flow-element?)])]
   [paragraph ([content list?])]
   [table ([style any/c]
           [flowss (listof (listof flow?))])]
   [delayed-flow-element ([render (any/c part? any/c . -> . flow-element?)])]
   [itemization ([flows (listof flow?)])]
   ;; content = list of elements
   [element ([style any/c]
             [content list?])]
   [(target-element element) ([tag tag?])]
   [(link-element element) ([tag tag?])]
   [(index-element element) ([tag tag?]
                             [plain-seq (listof string?)]
                             [entry-seq list?])]
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
                      1 1 #f
                      (list (cons prop:serializable 
                                  (make-serialize-info
                                   (lambda (d)
                                     (unless (delayed-element-ref d 1)
                                       (error 'serialize-delayed-element
                                              "cannot serialize a delayed element that was not resolved: ~e"
                                              d))
                                     (vector (delayed-element-ref d 1)))
                                   #'deserialize-delayed-element
                                   #f
                                   (or (current-load-relative-directory) (current-directory)))))))
  (define-syntax delayed-element (list-immutable #'struct:delayed-element
                                                 #'make-delayed-element
                                                 #'delayed-element?
                                                 (list-immutable #'delayed-element-render)
                                                 (list-immutable #'set-delayed-element-render!)
                                                 #t))
  (define delayed-element-render (make-struct-field-accessor delayed-element-ref 0))
  (define set-delayed-element-render! (make-struct-field-mutator delayed-element-set! 0))
  (provide/contract
   (struct delayed-element ([render (any/c part? any/c . -> . list?)])))

  (provide deserialize-delayed-element)
  (define deserialize-delayed-element
    (make-deserialize-info values values))
  
  (provide force-delayed-element)
  (define (force-delayed-element d renderer sec ht)
    (or (delayed-element-ref d 1)
        (let ([v ((delayed-element-ref d 0) renderer sec ht)])
          (delayed-element-set! d 1 v)
          v)))

  ;; ----------------------------------------

  (provide content->string)

  (define (content->string c)
    (apply string-append
           (map (lambda (e)
                  (element->string e))
                c)))

  (define (element->string c)
    (cond
     [(element? c) (content->string (element-content c))]
     [(string? c) c]
     [else (case c
             [(ndash) "--"]
             [(ldquo rdquo) "\""]
             [(rsquo) "'"]
             [(rarr) "->"]
             [else (format "~s" c)])]))

  )


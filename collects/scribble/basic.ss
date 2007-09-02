
(module basic (lib "lang.ss" "big")
  (require "decode.ss"
           "struct.ss"
           "config.ss"
           (lib "list.ss")
           (lib "class.ss")
           (lib "main-collects.ss" "setup")
           (lib "modresolve.ss" "syntax"))
  
  (provide title 
           section
           subsection
           subsubsection
           subsubsub*section
           include-section)

  (define (gen-tag content)
    (regexp-replace* "[^-a-zA-Z0-9_=]" 
                     (content->string content)
                     "_"))

  (define (prefix->string p)
    (and p
         (if (string? p)
             p
             (module-path-prefix->string p))))

  (define (title #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f] . str)
    (let ([content (decode-content str)])
      (make-title-decl (prefix->string prefix)
                       `((part ,(or tag (gen-tag content)))) 
                       style
                       content)))
  
  (define (section #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f] . str)
    (let ([content (decode-content str)])
      (make-part-start 0 (prefix->string prefix)
                       `((part ,(or tag (gen-tag content)))) 
                       style
                       content)))

  (define (subsection #:tag [tag #f] #:tag-prefix [prefix #f] . str)
    (let ([content (decode-content str)])
      (make-part-start 1 
                       (prefix->string prefix)
                       `((part ,(or tag (gen-tag content))))
                       #f 
                       content)))

  (define (subsubsection #:tag [tag #f] #:tag-prefix [prefix #f] . str)
    (let ([content (decode-content str)])
      (make-part-start 2 
                       (prefix->string prefix)
                       `((part ,(or tag (gen-tag content))))
                       #f
                       content)))

  (define (subsubsub*section #:tag [tag #f] . str)
    (let ([content (decode-content str)])
      (make-paragraph (list (make-element 'bold content)))))

  (define-syntax include-section 
    (syntax-rules ()
      [(_ mod)
       (begin
         (require (only mod doc))
         doc)]))

  ;; ----------------------------------------

  (provide module-path-prefix->string)

  (define (module-path-prefix->string p)
    (format "~a" (path->main-collects-relative
                  (resolve-module-path p #f))))
           
  ;; ----------------------------------------

  (provide itemize item item?)

  (define (itemize . items)
    (let ([items (filter (lambda (v) (not (whitespace? v))) items)])
      (for-each (lambda (v)
                  (unless (an-item? v)
                    (error 'itemize
                           "expected an item, found something else: ~e"
                           v)))
                items)
      (make-itemization (map an-item-flow items))))

  (define-struct an-item (flow))
  (define (item? x) (an-item? x))

  (define (item . str)
    (make-an-item (decode-flow str)))

  ;; ----------------------------------------

  (provide hspace
           elem aux-elem
           italic bold
           tt span-class
           subscript superscript)

  (define (hspace n)
    (make-element 'hspace (list (make-string n #\space))))

  (define (elem . str)
    (make-element #f (decode-content str)))

  (define (aux-elem . s)
    (make-aux-element #f (decode-content s)))

  (define (italic . str)
    (make-element 'italic (decode-content str)))

  (define (bold . str)
    (make-element 'bold (decode-content str)))

  (define (tt . str)
    (let ([l (decode-content str)])
      (let ([l (let ([m (and (pair? l)
                             (string? (car l))
                             (regexp-match-positions #rx"^ +" (car l)))])
                 (if m
                     (cons (hspace (- (cdar m) (caar m)))
                           (cons
                            (substring (car l) (cdar m))
                            (cdr l)))
                     l))])
        (if (andmap string? l)
            (make-element 'tt l)
            (make-element #f (map (lambda (s)
                                    (if (or (string? s)
                                            (symbol? s))
                                        (make-element 'tt (list s))
                                        s))
                                  l))))))

  (define (span-class classname . str)
    (make-element classname (decode-content str)))

  (define (subscript . str)
    (make-element 'subscript (decode-content str)))

  (define (superscript . str)
    (make-element 'superscript (decode-content str)))

  ;; ----------------------------------------

  (provide section-index index index* as-index index-section)

  (define (section-index . elems)
    (make-part-index-decl (map element->string elems) elems))

  (define (record-index word-seq element-seq tag content)
    (make-index-element
     #f
     (list (make-target-element #f content `(idx ,tag)))
     `(idx ,tag)
     word-seq
     element-seq))

  (define (index* word-seq content-seq . s)
    (let ([key (make-generated-tag)])
      (record-index word-seq
                    content-seq
                    key
                    (decode-content s))))

  (define (index word-seq . s)
    (let ([word-seq (if (string? word-seq)
                        (list word-seq)
                        word-seq)])
      (apply index* word-seq word-seq s)))

  (define (as-index . s)
    (let ([key (make-generated-tag)]
          [content (decode-content s)])
      (record-index (list (content->string content))
                    (list (make-element #f content)) 
                    key
                    content)))

  (define (index-section tag)
    (make-unnumbered-part
     #f
     `((part , (or tag 
                   (make-generated-tag))))
     '("Index")
     'index
     null
     (make-flow (list (make-delayed-flow-element
                       (lambda (renderer sec ri)
                         (let ([l null])
                           (hash-table-for-each 
                            (collected-info-info 
                             (part-collected-info
                              (collected-info-parent
                               (part-collected-info sec ri))
                              ri))
                            (lambda (k v)
                              (if (and (pair? k)
                                       (eq? 'index-entry (car k)))
                                  (set! l (cons (cons (cadr k) v) l)))))
                           (let ([l (sort 
                                     l
                                     (lambda (a b)
                                       (let loop ([a (cadr a)][b (cadr b)])
                                         (cond
                                          [(null? a) #t]
                                          [(null? b) #f]
                                          [(string-ci=? (car a) (car b))
                                           (loop (cdr a) (cdr b))]
                                          [else
                                           (string-ci<? (car a) (car b))]))))]
                                 [commas (lambda (l)
                                           (if (or (null? l)
                                                   (null? (cdr l)))
                                               l
                                               (cdr (apply append (map (lambda (i)
                                                                         (list ", " i))
                                                                       l)))))])
                             (make-table
                              'index
                              (map (lambda (i)
                                     (list (make-flow
                                            (list
                                             (make-paragraph
                                              (list
                                               (make-link-element
                                                "indexlink"
                                                (commas (caddr i))
                                                (car i))))))))
                                   l))))))))
     null))

  ;; ----------------------------------------

  (provide table-of-contents
           local-table-of-contents)

  (define (table-of-contents)
    (make-delayed-flow-element
     (lambda (renderer part ri)
       (send renderer table-of-contents part ri))))

  (define (local-table-of-contents)
    (make-delayed-flow-element
     (lambda (renderer part ri)
       (send renderer local-table-of-contents part ri)))))




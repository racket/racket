
(module basic mzscheme
  (require "decode.ss"
           "struct.ss"
           "config.ss"
           (lib "kw.ss")
           (lib "list.ss")
           (lib "class.ss"))
  
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

  (define/kw (title #:key [tag #f] [style #f] #:body str)
    (let ([content (decode-content str)])
      (make-title-decl (or tag (gen-tag content)) style content)))
  
  (define/kw (section #:key [tag #f] #:body str)
    (let ([content (decode-content str)])
      (make-part-start 0 (or tag (gen-tag content)) content)))

  (define/kw (subsection #:key [tag #f] #:body str)
    (let ([content (decode-content str)])
      (make-part-start 1 (or tag (gen-tag content)) content)))

  (define/kw (subsubsection #:key [tag #f] #:body str)
    (let ([content (decode-content str)])
      (make-part-start 2 (or tag (gen-tag content)) content)))

  (define/kw (subsubsub*section #:key [tag #f] #:body str)
    (let ([content (decode-content str)])
      (make-paragraph (list (make-element 'bold content)))))

  (define-syntax include-section 
    (syntax-rules ()
      [(_ mod)
       (begin
         (require (only mod doc))
         doc)]))

  ;; ----------------------------------------

  (provide itemize item item?)

  (define/kw (itemize #:body items)
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

  (define/kw (item #:body str)
    (make-an-item (decode-flow str)))

  ;; ----------------------------------------

  (provide hspace
           elem
           italic bold
           tt span-class
           subscript superscript)

  (define (hspace n)
    (make-element 'hspace (list (make-string n #\space))))

  (define/kw (elem #:body str)
    (make-element #f (decode-content str)))

  (define/kw (italic #:body str)
    (make-element 'italic (decode-content str)))

  (define/kw (bold #:body str)
    (make-element 'bold (decode-content str)))

  (define/kw (tt #:body str)
    (make-element 'tt (decode-content str)))

  (define/kw (span-class classname #:body str)
    (make-element classname (decode-content str)))

  (define/kw (subscript #:body str)
    (make-element 'subscript (decode-content str)))

  (define/kw (superscript #:body str)
    (make-element superscript (decode-content str)))

  ;; ----------------------------------------

  (provide index index* as-index index-section)

  (define (gen-target)
    (format "index:~s:~s" (current-seconds) (gensym)))
  
  (define (record-index word-seq element-seq tag content)
    (make-index-element
     #f
     (list (make-target-element #f content tag))
     tag
     word-seq
     element-seq))

  (define/kw (index* word-seq content-seq #:body s)
    (let ([key (gen-target)])
      (record-index word-seq
                    content-seq
                    key
                    (decode-content s))))

  (define/kw (index word-seq #:body s)
    (let ([word-seq (if (string? word-seq)
                        (list word-seq)
                        word-seq)])
      (apply index* word-seq word-seq s)))

  (define/kw (as-index #:body s)
    (let ([key (gen-target)]
          [content (decode-content s)])
      (record-index (list (content->string content))
                    (list (make-element #f content)) 
                    key
                    content)))

  (define (index-section tag)
    (make-unnumbered-part
     tag
     (list "Index")
     #f
     (make-flow (list (make-delayed-flow-element
                       (lambda (renderer sec ht)
                         (let ([l null])
                           (hash-table-for-each 
                            (collected-info-info 
                             (part-collected-info
                              (collected-info-parent
                               (part-collected-info sec))))
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
                                           (string-ci<? (car a) (car b))]))))])
                             (make-table
                              'index
                              (map (lambda (i)
                                     (list (make-flow
                                            (list
                                             (make-paragraph
                                              (list
                                               (make-link-element
                                                #f
                                                (caddr i)
                                                (car i))))))))
                                   l))))))))
     null))

  ;; ----------------------------------------

  (provide table-of-contents
           local-table-of-contents)

  (define (table-of-contents)
    (make-delayed-flow-element
     (lambda (renderer part ht)
       (send renderer table-of-contents part ht))))

  (define (local-table-of-contents)
    (make-delayed-flow-element
     (lambda (renderer part ht)
       (send renderer local-table-of-contents part ht)))))




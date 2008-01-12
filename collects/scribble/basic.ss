
(module basic scheme/base
  (require "decode.ss"
           "struct.ss"
           "config.ss"
           mzlib/list
           mzlib/class
           setup/main-collects
           syntax/modresolve
           (for-syntax scheme/base))
  
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

  (define (convert-tag tag content)
    (if (list? tag)
        (apply append (map (lambda (t) (convert-tag t content)) tag))
        `((part ,(or tag (gen-tag content))))))
  
  (define (title #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f]  #:version [version #f] . str)
    (let ([content (decode-content str)])
      (make-title-decl (prefix->string prefix)
                       (convert-tag tag content)
                       version
                       style
                       content)))
  
  (define (section #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f] . str)
    (let ([content (decode-content str)])
      (make-part-start 0 (prefix->string prefix)
                       (convert-tag tag content)
                       style
                       content)))

  (define (subsection #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f] . str)
    (let ([content (decode-content str)])
      (make-part-start 1 
                       (prefix->string prefix)
                       (convert-tag tag content)
                       style
                       content)))

  (define (subsubsection #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f] . str)
    (let ([content (decode-content str)])
      (make-part-start 2 
                       (prefix->string prefix)
                       (convert-tag tag content)
                       style
                       content)))

  (define (subsubsub*section #:tag [tag #f] . str)
    (let ([content (decode-content str)])
      (make-paragraph (list (make-element 'bold content)))))
  
  (define-syntax (include-section stx)
    (syntax-case stx ()
      [(_ mod)
       (with-syntax ([mod (syntax-local-introduce #'mod)])
         #'(begin
             (require (only-in mod doc))
             doc))]))

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

  (provide section-index index index* as-index index-section index-flow-elements)

  (define (section-index . elems)
    (make-part-index-decl (map element->string elems) elems))

  (define (record-index word-seq element-seq tag content)
    (make-index-element
     #f
     (list (make-target-element #f content `(idx ,tag)))
     `(idx ,tag)
     word-seq
     element-seq
     #f))

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

  (define (index-section #:title [title "Index"] #:tag [tag #f])
    (make-unnumbered-part
     #f
     `((part ,(or tag "doc-index")))
     (list title)
     'index
     null
     (make-flow (index-flow-elements))
     null))

  (define (index-flow-elements)
    (list (make-delayed-flow-element
           (lambda (renderer sec ri)
             (let ([l null])
               (hash-table-for-each 
                (let ([parent (collected-info-parent
                               (part-collected-info sec ri))])
                   (if parent
                       (collected-info-info 
                        (part-collected-info
                         parent
                         ri))
                       (collect-info-ext-ht (resolve-info-ci ri))))
                (lambda (k v)
                  (when (and (pair? k)
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
                                                           l)))))]
                     [alpha-starts (make-hash-table)])
                 (make-table
                  'index
                  (list*
                   (list
                    (make-flow
                     (list
                      (make-paragraph
                       (let ([add-letter 
                              (lambda (letter l)
                                (list* (make-element "nonavigation"
                                                     (list (string letter)))
                                       " "
                                       l))])
                         (let loop ([i l]
                                    [alpha (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")])
                           (cond
                            [(null? alpha) null]
                            [(null? i) (add-letter (car alpha)
                                                   (loop i (cdr alpha)))]
                            [else (let* ([strs (cadr (car i))]
                                         [letter (if (or (null? strs)
                                                         (string=? "" (car strs)))
                                                     #f
                                                     (string-ref (car strs) 0))])
                                    (cond
                                     [(not letter) (loop (cdr i) alpha)]
                                     [(char-ci>? letter (car alpha))
                                      (add-letter (car alpha)
                                                  (loop i (cdr alpha)))]
                                     [(char-ci=? letter (car alpha))
                                      (hash-table-put! alpha-starts (car i) letter)
                                      (list* (make-element (make-target-url 
                                                            (format "#alpha:~a" letter)
                                                            #f)
                                                           (list (string (car alpha))))
                                             " "
                                             (loop (cdr i) (cdr alpha)))]
                                     [else (loop (cdr i) alpha)]))])))))))
                   (list (make-flow (list (make-paragraph (list 'nbsp)))))
                   (map (lambda (i)
                          (list (make-flow
                                 (list
                                  (make-paragraph
                                   (list
                                    (let ([e (make-link-element
                                              "indexlink"
                                              (commas (caddr i))
                                              (car i))])
                                      (let ([letter (hash-table-get alpha-starts i #f)])
                                        (if letter
                                            (make-element (make-url-anchor (format "alpha:~a" letter))
                                                          (list e))
                                            e)))))))))
                        l)))))))))

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





(module html-render mzscheme
  (require "struct.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "runtime-path.ss")
           (prefix xml: (lib "xml.ss" "xml")))
  (provide render-mixin
           render-multi-mixin)

  (xml:empty-tag-shorthand xml:html-empty-tags)

  (define-runtime-path scribble-css "scribble.css")

  (define current-subdirectory (make-parameter #f))
  (define current-output-file (make-parameter #f))
  (define on-separate-page (make-parameter #t))
  (define next-separate-page (make-parameter #f))
  (define collecting-sub (make-parameter 0))
  (define current-no-links (make-parameter #f))
  (define extra-breaking? (make-parameter #f))

  ;; ----------------------------------------
  ;;  main mixin

  (define (render-mixin %)
    (class %
      (inherit render-content
               render-flow-element
               collect-part
               install-file
               get-dest-directory
               format-number
               strip-aux
               lookup
               quiet-table-of-contents)

      (define/override (get-suffix) #".html")

      ;; ----------------------------------------

      (define/override (collect ds fns)
        (let ([ht (make-hash-table 'equal)])
          (map (lambda (d fn)
                 (parameterize ([current-output-file fn])
                   (collect-part d #f ht null)))
               ds
               fns)
          ht))

      (define/public (part-whole-page? p ht)
        (let ([dest (lookup p ht `(part ,(car (part-tags p))))])
          (caddr dest)))

      (define/public (current-part-whole-page?)
        #f)

      (define/override (collect-part-tags d ht number)
        (for-each (lambda (t)
                    (hash-table-put! ht 
                                     `(part ,t)
                                     (list (current-output-file)
                                           (part-title-content d)
                                           (current-part-whole-page?))))
                  (part-tags d)))

      (define/override (collect-target-element i ht)
        (hash-table-put! ht 
                         (target-element-tag i)
                         (list (current-output-file) #f #f)))

      ;; ----------------------------------------

      (define/public (render-toc-view d ht)
        (let-values ([(top mine)
                      (let loop ([d d][mine d])
                        (let ([p (collected-info-parent (part-collected-info d))])
                          (if p
                              (loop p d)
                              (values d mine))))])
          `((div ((class "tocset"))
                 (div ((class "tocview"))
                      (div ((class "tocviewtitle"))
                           (a ((href "index.html")
                               (class "tocviewlink"))
                              ,@(render-content (part-title-content top) d ht)))
                      (div nbsp)
                      (table 
                       ((class "tocviewlist")
                        (cellspacing "0"))
                       ,@(map (lambda (p)
                                `(tr
                                  (td 
                                   ((align "right"))
                                   ,@(format-number (collected-info-number (part-collected-info p))
                                                    '((tt nbsp))))
                                  (td
                                   (a ((href ,(let ([dest (lookup p ht `(part ,(car (part-tags p))))])
                                                (format "~a~a~a" 
                                                        (from-root (car dest)
                                                                   (get-dest-directory))
                                                        (if (caddr dest)
                                                            ""
                                                            "#")
                                                        (if (caddr dest)
                                                            ""
                                                            `(part ,(car (part-tags p)))))))
                                       (class ,(if (eq? p mine)
                                                   "tocviewselflink"
                                                   "tocviewlink")))
                                      ,@(render-content (part-title-content p) d ht)))))
                              (part-parts top))))
                 ,@(if (ormap (lambda (p) (part-whole-page? p ht)) (part-parts d))
                       null
                       (let ([ps (cdr
                                  (let flatten ([d d])
                                    (cons d 
                                          (apply
                                           append
                                           (letrec ([flow-targets
                                                     (lambda (flow)
                                                       (apply append (map flow-element-targets (flow-paragraphs flow))))]
                                                    [flow-element-targets
                                                     (lambda (e)
                                                       (cond
                                                        [(table? e) (table-targets e)]
                                                        [(paragraph? e) (para-targets e)]
                                                        [(itemization? e)
                                                         (apply append (map flow-targets (itemization-flows e)))]
                                                        [(blockquote? e)
                                                         (apply append (map flow-element-targets (blockquote-paragraphs e)))]
                                                        [(delayed-flow-element? e)
                                                         null]))]
                                                    [para-targets
                                                     (lambda (para)
                                                       (let loop ([c (paragraph-content para)])
                                                         (cond
                                                          [(empty? c) null]
                                                          [else (let ([a (car c)])
                                                                  (cond
                                                                   [(toc-target-element? a)
                                                                    (cons a (loop (cdr c)))]
                                                                   [(element? a)
                                                                    (append (loop (element-content a))
                                                                            (loop (cdr c)))]
                                                                   [(delayed-element? a)
                                                                    (loop (cons (force-delayed-element a this d ht)
                                                                                (cdr c)))]
                                                                   [else
                                                                    (loop (cdr c))]))])))]
                                                    [table-targets
                                                     (lambda (table)
                                                       (apply append 
                                                              (map (lambda (flows)
                                                                     (apply append (map (lambda (f)
                                                                                          (if (eq? f 'cont)
                                                                                              null
                                                                                              (flow-targets f)))
                                                                                        flows)))
                                                                   (table-flowss table))))])
                                             (apply append (map flow-element-targets (flow-paragraphs (part-flow d)))))
                                           (map flatten (part-parts d))))))])
                         (if (null? ps)
                             null
                             `((div ((class "tocsub"))
                                    (div ((class "tocsubtitle"))
                                         "On this page:")
                                    (table
                                     ((class "tocsublist")
                                      (cellspacing "0"))
                                     ,@(map (lambda (p)
                                              (parameterize ([current-no-links #t]
                                                             [extra-breaking? #t])
                                                `(tr
                                                  (td 
                                                   ,@(if (part? p)
                                                         `((span ((class "tocsublinknumber"))
                                                                 ,@(format-number (collected-info-number (part-collected-info p))
                                                                                  '((tt nbsp)))))
                                                         '(""))
                                                   (a ((href ,(if (part? p)
                                                                  (let ([dest (lookup p ht `(part ,(car (part-tags p))))])
                                                                    (format "#~a" 
                                                                            `(part ,(car (part-tags p)))))
                                                                  (format "#~a" (target-element-tag p))))
                                                       (class ,(if (part? p)
                                                                   "tocsubseclink"
                                                                   "tocsublink")))
                                                      ,@(if (part? p)
                                                            (render-content (part-title-content p) d ht)
                                                            (render-content (element-content p) d ht)))))))
                                            ps)))))))))))

      (define/public (render-one-part d ht fn number)
        (parameterize ([current-output-file fn])
          (let ([xpr `(html () 
                            (head
                             (meta ((http-equiv "content-type")
                                    (content "text-html; charset=utf-8")))
                             ,@(let ([c (part-title-content d)])
                                 (if c
                                     `((title ,@(format-number number '(nbsp)) ,(content->string c this d ht)))
                                     null))
                             (link ((rel "stylesheet")
                                    (type "text/css")
                                    (href "scribble.css")
                                    (title "default"))))
                            (body ,@(render-toc-view d ht)
                                  (div ((class "main")) ,@(render-part d ht))))])
            (install-file scribble-css)
            (xml:write-xml/content (xml:xexpr->xml xpr)))))

      (define/override (render-one d ht fn)
        (render-one-part d ht fn null))

      (define/override (render-part d ht)
        (let ([number (collected-info-number (part-collected-info d))])
          `(,@(if (and (not (part-title-content d))
                       (null? number))
                  null
                  `((,(case (length number)
                        [(0) 'h2]
                        [(1) 'h3]
                        [(2) 'h4]
                        [else 'h5])
                     ,@(format-number number '((tt nbsp)))
                     ,@(map (lambda (t)
                              `(a ((name ,(format "~a" `(part ,t))))))
                            (part-tags d))
                     ,@(if (part-title-content d)
                           (render-content (part-title-content d) d ht)
                           null))))
            ,@(render-flow* (part-flow d) d ht #f)
            ,@(let loop ([pos 1]
                         [secs (part-parts d)])
                (if (null? secs)
                    null
                    (append
                     (render-part (car secs) ht)
                     (loop (add1 pos) (cdr secs))))))))

      (define/private (render-flow* p part ht special-last?)
        ;; Wrap each table with <p>, except for a trailing table
        ;;  when `special-last?' is #t
        (let loop ([f (flow-paragraphs p)])
          (cond
           [(null? f) null]
           [(and (table? (car f)) 
                 (or (not special-last?)
                     (not (null? (cdr f)))))
            (cons `(p ,@(render-flow-element (car f) part ht))
                  (loop (cdr f)))]
           [else
            (append (render-flow-element (car f) part ht)
                    (loop (cdr f)))])))

      (define/override (render-flow p part ht)
        (render-flow* p part ht #t))

      (define/override (render-paragraph p part ht)
        `((p ,@(if (styled-paragraph? p)
                   `(((class ,(styled-paragraph-style p))))
                   null)
             ,@(super render-paragraph p part ht))))

      (define/override (render-element e part ht)
        (cond
         [(target-element? e)
          `((a ((name ,(target-element-tag e))))
            ,@(render-plain-element e part ht))]
         [(and (link-element? e)
               (not (current-no-links)))
          (parameterize ([current-no-links #t])
            (let ([dest (lookup part ht (link-element-tag e))])
              (if dest
                  `((a ((href ,(format "~a~a~a" 
                                       (from-root (car dest)
                                                  (get-dest-directory))
                                       (if (caddr dest)
                                           ""
                                           "#")
                                       (if (caddr dest)
                                           ""
                                           (link-element-tag e))))
                        ,@(if (string? (element-style e))
                              `((class ,(element-style e)))
                              null))
                       ,@(if (null? (element-content e))
                             (render-content (strip-aux (cadr dest)) part ht)
                             (render-content (element-content e) part ht))))
                  (begin (fprintf (current-error-port) "Undefined link: ~s~n" (link-element-tag e)) ; XXX Add source info
                         `((font ((class "badlink")) 
                                 ,@(if (null? (element-content e))
                                       `(,(format "~s" (link-element-tag e)))
                                       (render-plain-element e part ht))))))))]
         [else (render-plain-element e part ht)]))

      (define/private (render-plain-element e part ht)
        (let ([style (and (element? e)
                          (element-style e))])
          (cond
           [(symbol? style)
            (case style
              [(italic) `((i ,@(super render-element e part ht)))]
              [(bold) `((b ,@(super render-element e part ht)))]
              [(tt) `((tt ,@(super render-element e part ht)))]
              [(sf) `((b (font ([size "-1"][face "Helvetica"]) ,@(super render-element e part ht))))]
              [(subscript) `((sub ,@(super render-element e part ht)))]
              [(superscript) `((sup ,@(super render-element e part ht)))]
              [(hspace) `((span ([class "hspace"])
                                ,@(let ([str (content->string (element-content e))])
                                    (map (lambda (c) 'nbsp) (string->list str)))))]
              [else (error 'html-render "unrecognized style symbol: ~e" style)])]
           [(string? style) 
            `((span ([class ,style]) ,@(super render-element e part ht)))]
           [(and (pair? style)
                 (eq? (car style) 'show-color))
            `((font ((style ,(format "background-color: ~a"
                                    (apply string-append "#"
                                           (map (lambda (v) (let ([s (format "0~x" v)])
                                                              (substring s (- (string-length s) 2))))
                                                (cdr style))))))
                    (tt nbsp nbsp nbsp nbsp nbsp))
              nbsp
              ,@(super render-element e part ht))]
           [(target-url? style)
            (if (current-no-links)
                (super render-element e part ht)
                (parameterize ([current-no-links #t])
                  `((a ((href ,(target-url-addr style))) ,@(super render-element e part ht)))))]
           [(image-file? style) `((img ((src ,(install-file (image-file-path style))))))]
           [else (super render-element e part ht)])))

      (define/override (render-table t part ht)
        `((table ((cellspacing "0") 
                  ,@(case (table-style t)
                      [(boxed) '((class "boxed"))]
                      [(centered) '((align "center"))]
                      [(at-right) '((align "right"))]
                      [(at-left) '((align "left"))]
                      [else null])
                  ,@(let ([a (and (list? (table-style t))
                                  (assoc 'style (table-style t)))])
                      (if (and a (string? (cadr a)))
                          `((class ,(cadr a)))
                          null))
                  ,@(if (string? (table-style t))
                        `((class ,(table-style t)))
                        null))
                 ,@(map (lambda (flows style)
                          `(tr (,@(if style
                                      `((class ,style))
                                      null))
                               ,@(let loop ([ds flows]
                                            [as (cdr (or (and (list? (table-style t))
                                                              (assoc 'alignment (or (table-style t) null)))
                                                         (cons #f (map (lambda (x) #f) flows))))]
                                            [vas
                                             (cdr (or (and (list? (table-style t))
                                                           (assoc 'valignment (or (table-style t) null)))
                                                      (cons #f (map (lambda (x) #f) flows))))])
                                   (if (null? ds)
                                       null
                                       (if (eq? (car ds) 'cont)
                                           (loop (cdr ds) (cdr as) (cdr vas))
                                           (let ([d (car ds)]
                                                 [a (car as)]
                                                 [va (car vas)])
                                             (cons
                                              `(td (,@(case a
                                                        [(#f) null]
                                                        [(right) '((align "right"))]
                                                        [(center) '((align "center"))]
                                                        [(left) '((align "left"))])
                                                    ,@(case va
                                                        [(#f) null]
                                                        [(top) '((valign "top"))]
                                                        [(baseline) '((valign "baseline"))]
                                                        [(bottom) '((valign "bottom"))])
                                                    ,@(if (and (pair? (cdr ds))
                                                               (eq? 'cont (cadr ds)))
                                                          `((colspan
                                                             ,(number->string
                                                               (let loop ([n 2]
                                                                          [ds (cddr ds)])
                                                                 (cond
                                                                  [(null? ds) n]
                                                                  [(eq? 'cont (car ds)) (loop (+ n 1) (cdr ds))]
                                                                  [else n])))))
                                                          null))
                                                   ,@(render-flow d part ht))
                                              (loop (cdr ds) (cdr as) (cdr vas)))))))))
                        (table-flowss t)
                        (cdr (or (and (list? (table-style t))
                                      (assoc 'row-styles (or (table-style t) null)))
                                 (cons #f (map (lambda (x) #f) (table-flowss t)))))))))

      (define/override (render-blockquote t part ht)
        `((blockquote ,@(if (string? (blockquote-style t))
                            `(((class ,(blockquote-style t))))
                            null)
                      ,@(apply append
                               (map (lambda (i)
                                      (render-flow-element i part ht))
                                    (blockquote-paragraphs t))))))

      (define/override (render-itemization t part ht)
        `((ul
           ,@(map (lambda (flow)
                    `(li ,@(render-flow flow part ht)))
                  (itemization-flows t)))))

      (define/override (render-other i part ht)
        (cond
         [(string? i) (let ([m (and (extra-breaking?)
                                    (regexp-match-positions #rx":" i))])
                        (if m
                            (list* (substring i 0 (cdar m))
                                   `(span ((class "mywbr")) " ")
                                   (render-other (substring i (cdar m)) part ht))
                            (list i)))]
         [(eq? i 'mdash) `(" " ndash " ")]
         [(eq? i 'hline) `((hr))]
         [(symbol? i) (list i)]
         [else (list (format "~s" i))]))
      
      ;; ----------------------------------------

      (super-new)))

  ;; ----------------------------------------
  ;; multi-file output 

  (define (render-multi-mixin %)
    (class %
      (inherit render-one
               render-one-part
               render-content)

      (define/override (get-suffix) #"")

      (define/override (get-dest-directory)
        (or (build-path (or (super get-dest-directory) (current-directory))
                        (current-subdirectory))
            (super get-dest-directory)))

      (define/private (derive-filename d ht)
        (let ([fn (format "~a.html" (regexp-replace*
                                     "[^-a-zA-Z0-9_=]"
                                     (format "~a" (car (part-tags d)))
                                     "_"))])
          (when ((string-length fn) . >= . 48)
            (error "file name too long (need a tag):" fn))
          fn))

      (define/override (collect ds fns)
        (super collect ds (map (lambda (fn)
                                 (build-path fn "index.html"))
                               fns)))

      (define/override (current-part-whole-page?)
        ((collecting-sub) . <= . 2))

      (define/private (toc-part? d)
        (and (styled-part? d)
             (let ([st (styled-part-style d)])
               (or (eq? 'toc st)
                   (and (list? st) (memq 'toc st))))))

      (define/override (collect-part d parent ht number)
        (let ([prev-sub (collecting-sub)])
          (parameterize ([collecting-sub (if (toc-part? d)
                                             1
                                             (add1 prev-sub))])
            (if (= 1 prev-sub)
                (let ([filename (derive-filename d ht)])
                  (parameterize ([current-output-file (build-path (path-only (current-output-file))
                                                                  filename)])
                    (super collect-part d parent ht number)))
                (super collect-part d parent ht number)))))
      
      (define/override (render ds fns ht)
        (map (lambda (d fn)
               (printf " [Output to ~a/index.html]\n" fn)
               (unless (directory-exists? fn)
                 (make-directory fn))
               (parameterize ([current-subdirectory (file-name-from-path fn)])
                 (let ([fn (build-path fn "index.html")])
                   (with-output-to-file fn
                     (lambda ()
                       (render-one d ht fn))
                     'truncate/replace))))
             ds
             fns))

      (define contents-content '("contents"))
      (define index-content '("index"))
      (define prev-content '(larr " prev"))
      (define up-content '("up"))
      (define next-content '("next " rarr))
      (define no-next-content next-content)
      (define sep-element (make-element #f '(nbsp nbsp)))
      
      (inherit render-table)

      (define/private (find-siblings d)
        (let ([parent (collected-info-parent (part-collected-info d))])
          (let loop ([l (if parent
                            (part-parts parent)
                            (if (null? (part-parts d))
                                (list d)
                                (list d (car (part-parts d)))))]
                     [prev #f])
            (cond
             [(eq? (car l) d) (values prev 
                                      (and (pair? (cdr l)) 
                                           (cadr l)))]
             [else (loop (cdr l) (car l))]))))

      (define/private (part-parent d)
        (collected-info-parent (part-collected-info d)))
        
      (define/private (navigation d ht)
        (let ([parent (part-parent d)])
          (let*-values ([(prev next) (find-siblings d)]
                        [(prev) (if prev
                                    (let loop ([prev prev])
                                      (if (and (toc-part? prev)
                                               (pair? (part-parts prev)))
                                          (loop (car (last-pair (part-parts prev))))
                                          prev))
                                    (and parent
                                         (toc-part? parent)
                                         parent))]
                        [(next) (cond
                                 [(and (toc-part? d)
                                       (pair? (part-parts d)))
                                  (car (part-parts d))]
                                 [(and (not next)
                                       parent
                                       (toc-part? parent))
                                  (let-values ([(prev next)
                                                (find-siblings parent)])
                                    next)]
                                 [else next])]
                        [(index) (let loop ([d d])
                                   (let ([p (part-parent d)])
                                     (if p
                                         (loop p)
                                         (let ([subs (part-parts d)])
                                           (and (pair? subs)
                                                (let ([d (car (last-pair subs))])
                                                  (and (equal? '("Index") (part-title-content d))
                                                       d)))))))])
            `(,@(render-table (make-table
                               'at-left
                               (list
                                (cons
                                 (make-flow
                                  (list
                                   (make-paragraph
                                    (list
                                     (make-element
                                      (if parent
                                          (make-target-url "index.html")
                                          "nonavigation")
                                      contents-content)))))
                                 (if index
                                     (list
                                      (make-flow
                                       (list
                                        (make-paragraph
                                         (list
                                          'nbsp
                                          (if (eq? d index)
                                              (make-element
                                               "nonavigation"
                                               index-content)
                                              (make-link-element
                                               #f
                                               index-content
                                               `(part ,(car (part-tags index))))))))))
                                     null))))
                              d ht)
              ,@(render-table (make-table
                               'at-right
                               (list 
                                (list 
                                 (make-flow
                                  (list
                                   (make-paragraph
                                    (list
                                     (make-element
                                      (if parent
                                          (make-target-url (if prev
                                                               (derive-filename prev ht)
                                                               "index.html"))
                                          "nonavigation")
                                      prev-content)
                                     sep-element
                                     (make-element
                                      (if parent
                                          (make-target-url 
                                           (if (toc-part? parent)
                                               (derive-filename parent ht)
                                               "index.html"))
                                          "nonavigation")
                                      up-content)
                                     sep-element
                                     (make-element
                                      (if next
                                          (make-target-url (derive-filename next ht))
                                          "nonavigation")
                                      next-content))))))))
                              d
                              ht)))))

      (define/override (render-part d ht)
        (let ([number (collected-info-number (part-collected-info d))])
          (cond
           [(and (not (on-separate-page))
                 (or (= 1 (length number))
                     (next-separate-page)))
            ;; Render as just a link, and put the actual 
            ;; content in a new file:
            (let* ([filename (derive-filename d ht)]
                   [full-path (build-path (path-only (current-output-file))
                                          filename)])
              (parameterize ([on-separate-page #t])
                (with-output-to-file full-path
                  (lambda ()
                    (render-one-part d ht full-path number))
                  'truncate/replace)
                null))]
           [else
            (let ([sep? (on-separate-page)])
              (parameterize ([next-separate-page (toc-part? d)]
                             [on-separate-page #f])
                (if sep?
                    ;; Navigation bars;
                    `(,@(navigation d ht)
                      (p nbsp)
                      ,@(super render-part d ht)
                      (p nbsp)
                      ,@(navigation d ht)
                      (p nbsp))
                    ;; Normal section render
                    (super render-part d ht))))])))

      (super-new)))

  ;; ----------------------------------------
  ;; utils

  (define (from-root p d)
    (if d
        (let ([e-d (explode (path->complete-path d (current-directory)))]
              [e-p (explode (path->complete-path p (current-directory)))])
          (let loop ([e-d e-d]
                     [e-p e-p])
            (cond
             [(null? e-d) (let loop ([e-p e-p])
                            (cond
                             [(null? e-p) "/"]
                             [(null? (cdr e-p)) (car e-p)]
                             [(eq? 'same (car e-p)) (loop (cdr e-p))]
                             [(eq? 'up (car e-p)) 
                              (string-append "../" (loop (cdr e-p)))]
                             [else (string-append (car e-p)
                                                  "/"
                                                  (loop (cdr e-p)))]))]
             [(equal? (car e-d) (car e-p))
              (loop (cdr e-d) (cdr e-p))]
             [(eq? 'same (car e-d))
              (loop (cdr e-d) e-p)]
             [(eq? 'same (car e-p))
              (loop e-d (cdr e-p))]
             [else
              (string-append
               (apply string-append (map (lambda (x) "../") e-d))
               (loop null e-p))])))
        p))

  (define (explode p)
    (reverse (let loop ([p p])
               (let-values ([(base name dir?) (split-path p)])
                 (let ([name (if base
                                 (if (path? name)
                                     (path-element->string name)
                                     name)
                                 name)])
                   (if (path? base)
                       (cons name (loop base))
                       (list name))))))))

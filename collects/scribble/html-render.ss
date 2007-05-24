
(module html-render mzscheme
  (require "struct.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "runtime-path.ss")
           (prefix xml: (lib "xml.ss" "xml")))
  (provide render-mixin
           render-multi-mixin)

  (xml:empty-tag-shorthand xml:html-empty-tags)

  (define-runtime-path scribble-css "scribble.css")

  (define current-subdirectory (make-parameter #f))
  (define current-output-file (make-parameter #f))
  (define on-separate-page (make-parameter #f))
  (define collecting-sub (make-parameter 0))

  ;; ----------------------------------------
  ;;  main mixin

  (define (render-mixin %)
    (class %
      (inherit render-content
               render-flow-element
               collect-part
               install-file
               get-dest-directory
               format-number)

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

      (define/override (collect-part-tag d ht)
        (hash-table-put! ht 
                         `(part ,(part-tag d)) 
                         (list (current-output-file)
                               (part-title-content d))))

      (define/override (collect-target-element i ht)
        (hash-table-put! ht 
                         (target-element-tag i)
                         (list (current-output-file) #f)))

      ;; ----------------------------------------

      (define/public (render-one-part d ht fn number)
        (parameterize ([current-output-file fn])
          (let ([xpr `(html () 
                            (head
                             (meta ((http-equiv "content-type")
                                    (content "text-html; charset=utf-8")))
                             ,@(let ([c (part-title-content d)])
                                 (if c
                                     `((title ,@(render-content c d ht)))
                                     null))
                             (link ((rel "stylesheet")
                                    (type "text/css")
                                    (href "scribble.css")
                                    (title "default"))))
                            (body ,@(render-part d ht)))])
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
                        [else 'h4])
                     ,@(format-number number '((tt nbsp)))
                     ,@(if (part-tag d)
                           `((a ((name ,(format "~a" `(part ,(part-tag d)))))))
                           null)
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
        `((p ,@(super render-paragraph p part ht))))

      (define/override (render-element e part ht)
        (cond
         [(target-element? e)
          `((a ((name ,(target-element-tag e))) ,@(render-plain-element e part ht)))]
         [(link-element? e)
          (let ([dest (hash-table-get ht (link-element-tag e) #f)])
            (if dest
                `((a ((href ,(format "~a#~a" 
                                     (from-root (car dest)
                                                (get-dest-directory))
                                     (link-element-tag e)))
                      ,@(if (string? (element-style e))
                            `((class ,(element-style e)))
                            null))
                     ,@(if (null? (element-content e))
                           (render-content (cadr dest) part ht)
                           (render-content (element-content e) part ht))))
                `((font ((class "badlink")) 
                        ,@(if (null? (element-content e))
                              `(,(format "~s" (link-element-tag e)))
                              (render-plain-element e part ht))))))]
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
              [(hspace) `((tt ,@(map (lambda (c) 'nbsp) (string->list (content->string (element-content e))))))]
              [else (error 'html-render "unrecognized style symbol: ~e" style)])]
           [(string? style) 
            `((span ([class ,style]) ,@(super render-element e part ht)))]
           [(target-url? style)
            `((a ((href ,(target-url-addr style))) ,@(super render-element e part ht)))]
           [(image-file? style) `((img ((src ,(install-file (image-file-path style))))))]
           [else (super render-element e part ht)])))

      (define/override (render-table t part ht)
        `((table ((cellspacing "0") ,@(case (table-style t)
                                        [(boxed) '((width "100%") (bgcolor "lightgray"))]
                                        [(centered) '((align "center"))]
                                        [else null]))
                 ,@(map (lambda (flows)
                          `(tr ,@(map (lambda (d a)
                                        `(td ,@(case a
                                                 [(#f) null]
                                                 [(right) '(((align "right")))]
                                                 [(left) '(((align "left")))])
                                             ,@(render-flow d part ht)))
                                      flows
                                      (cdr (or (and (list? (table-style t))
                                                    (assoc 'alignment (or (table-style t) null)))
                                               (cons #f (map (lambda (x) #f) flows)))))))
                        (table-flowss t)))))

      (define/override (render-itemization t part ht)
        `((ul
           ,@(map (lambda (flow)
                    `(li ,@(render-flow flow part ht)))
                  (itemization-flows t)))))

      (define/override (render-other i part ht)
        (list (cond
               [(string? i) i]
               [(eq? i 'mdash) `(span " " ndash " ")]
               [(symbol? i) i]
               [else (format "~s" i)])))
      
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

      (define/private (derive-filename d)
        (format "~a.html" (regexp-replace*
                           "[^-a-zA-Z0-9_=]"
                           (or (format "~a" (part-tag d))
                               (content->string (part-title-content d)))
                           "_")))

      (define/override (collect ds fns)
        (super collect ds (map (lambda (fn)
                                 (build-path fn "index.html"))
                               fns)))

      (define/override (collect-part d parent ht number)
        (let ([prev-sub (collecting-sub)])
          (parameterize ([collecting-sub (add1 prev-sub)])
            (if (= 1 prev-sub)
                (let ([filename (derive-filename d)])
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

      (define/override (render-part d ht)
        (let ([number (collected-info-number (part-collected-info d))])
          (cond
           [(and (not (on-separate-page))
                 (= 1 (length number)))
            ;; Render as just a link, and put the actual 
            ;; content in a new file:
            (let* ([filename (derive-filename d)]
                   [full-path (build-path (path-only (current-output-file))
                                          filename)])
              (parameterize ([on-separate-page #t])
                (with-output-to-file full-path
                  (lambda ()
                    (render-one-part d ht full-path number))
                  'truncate/replace)
                null
                #;
                `((table 
                   ((width "90%") (cellspacing "0") (align "center"))
                   ,@(render-toc-entry d filename ht number)))))]
           [else
            ;; Normal section render
            (super render-part d ht)])))

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

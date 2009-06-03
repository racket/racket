#lang scheme/base

(require "struct.ss"
         scheme/class
         scheme/runtime-path
         scheme/port
         scheme/path
         scheme/string
         scheme/list
         setup/main-collects)
(provide render-mixin)

(define current-table-mode (make-parameter #f))
(define rendering-tt (make-parameter #f))
(define show-link-page-numbers (make-parameter #f))
(define done-link-page-numbers (make-parameter #f))
(define disable-images (make-parameter #f))

(define-struct (toc-paragraph paragraph) ())

(define-runtime-path scribble-prefix-tex "scribble-prefix.tex")
(define-runtime-path scribble-tex "scribble.tex")

(define (gif-to-png p)
  (if (equal? (filename-extension p) #"gif")
      (path-replace-suffix p #".png")
      p))

(define (render-mixin %)
  (class %
    (inherit-field prefix-file style-file style-extra-files)

    (define/override (get-suffix) #".tex")

    (inherit render-block
             render-content
             render-part
             install-file
             format-number
             extract-part-style-files)

    (define/override (render-one d ri fn)
      (let ([style-file (or style-file scribble-tex)]
            [prefix-file (or prefix-file scribble-prefix-tex)])
        (for-each
         (lambda (style-file)
           (with-input-from-file style-file
             (lambda ()
               (copy-port (current-input-port) (current-output-port)))))
         (list* prefix-file style-file 
                (append style-extra-files
                        (extract-part-style-files
                         d
                         ri
                         'tex
                         (lambda (p) #f)))))
        (printf "\\begin{document}\n\\preDoc\n")
        (when (part-title-content d)
          (let ([m (ormap (lambda (v)
                            (and (styled-paragraph? v)
                                 (equal? "author" (styled-paragraph-style v))
                                 v))
                          (flow-paragraphs (part-flow d)))])
            (when m
              (do-render-paragraph m d ri #t)))
          (let ([vers (or (and (versioned-part? d) (versioned-part-version d))
                          (version))])
            (printf "\\titleAnd~aVersion{" (if (equal? vers "") "Empty" ""))
            (render-content (part-title-content d) d ri)
            (printf "}{~a}\n" vers)))
        (render-part d ri)
        (printf "\n\n\\postDoc\n\\end{document}\n")))

    (define/override (render-part-content d ri)
      (let ([number (collected-info-number (part-collected-info d ri))])
        (when (and (part-title-content d) (pair? number))
          (when (part-style? d 'index)
            (printf "\\twocolumn\n\\parskip=0pt\n\\addcontentsline{toc}{section}{Index}\n"))
          (let ([no-number? (and (pair? number) 
                                 (or (not (car number))
                                     ((length number) . > . 3)))])
            (printf "\n\n\\~a~a~a"
                    (case (length number)
                      [(0 1) "sectionNewpage\n\n\\section"]
                      [(2) "subsection"]
                      [(3) "subsubsection"]
                      [else "subsubsection"])
                    (if (and (part-style? d 'hidden) (not no-number?))
                      "hidden" "")
                    (if no-number? "*" ""))
            (when (not (or (part-style? d 'hidden) no-number?))
              (printf "[")
              (parameterize ([disable-images #t])
                (render-content (part-title-content d) d ri))
              (printf "]")))
          (printf "{")
          (render-content (part-title-content d) d ri)
          (printf "}")
          (when (part-style? d 'index) (printf "\n\n")))
        (for ([t (part-tags d)])
          (printf "\\label{t:~a}\n\n" (t-encode (add-current-tag-prefix (tag-key t ri)))))
        (render-flow (part-flow d) d ri #f)
        (for ([sec (part-parts d)]) (render-part sec ri))
        (when (part-style? d 'index) (printf "\\onecolumn\n\n"))
        null))

    (define/override (render-paragraph p part ri)
      (do-render-paragraph p part ri #f))

    (define/private (do-render-paragraph p part ri author?)
      (let ([style (and (styled-paragraph? p)
                        (let ([s (flatten-style
                                  (styled-paragraph-style p))])
                          (if (with-attributes? s)
                              (let ([base (with-attributes-style s)])
                                (if (eq? base 'div)
                                    (let ([a (assq 'class (with-attributes-assoc s))])
                                      (if a
                                          (cdr a)
                                          base))
                                    base))
                              s)))])
        (unless (and (not author?)
                     (equal? style "author"))
          (when (string? style)
            (printf "\\~a{" style))
          (if (toc-paragraph? p)
              (printf "\\newpage \\tableofcontents \\newpage")
              (super render-paragraph p part ri))
          (when (string? style) (printf "}"))))
      null)

    (define/override (render-element e part ri)
      (when (render-element? e)
        ((render-element-render e) this part ri))
      (let ([part-label? (and (link-element? e)
                              (pair? (link-element-tag e))
                              (eq? 'part (car (link-element-tag e)))
                              (null? (element-content e)))])
        (parameterize ([done-link-page-numbers (or (done-link-page-numbers)
                                                   (link-element? e))])
          (when (target-element? e)
            (printf "\\label{t:~a}"
                    (t-encode (add-current-tag-prefix (tag-key (target-element-tag e) ri)))))
          (when part-label?
            (printf "\\SecRef{")
            (render-content
             (let ([dest (resolve-get part ri (link-element-tag e))])
               (if dest
                 (if (list? (cadr dest))
                   (format-number (cadr dest) null)
                   (begin (fprintf (current-error-port)
                                   "Internal tag error: ~s -> ~s\n"
                                   (link-element-tag e)
                                   dest)
                          '("!!!")))
                 (list "???")))
             part ri)
            (printf "}{"))
          (let ([style (and (element? e)
                            (let ([s (flatten-style (element-style e))])
                              (if (with-attributes? s)
                                (with-attributes-style s)
                                s)))]
                [wrap (lambda (e s tt?)
                        (printf "\\~a{" s)
                        (parameterize ([rendering-tt (or tt? (rendering-tt))])
                          (super render-element e part ri))
                        (printf "}"))])
            (cond
              [(symbol? style)
               (case style
                 [(italic) (wrap e "textit" #f)]
                 [(bold) (wrap e "textbf" #f)]
                 [(tt) (wrap e "Scribtexttt" #t)]
                 [(url) (wrap e "nolinkurl" 'exact)]
                 [(no-break) (super render-element e part ri)]
                 [(sf) (wrap e "textsf" #f)]
                 [(subscript) (wrap e "textsub" #f)]
                 [(superscript) (wrap e "textsuper" #f)]
                 [(hspace)
                  (let ([s (content->string (element-content e))])
                    (case (string-length s)
                      [(0) (void)]
                      [else
                       (printf "\\mbox{\\hphantom{\\Scribtexttt{~a}}}"
                               (regexp-replace* #rx"." s "x"))]))]
                 [(newline) (printf "\\\\")]
                 [else (error 'latex-render
                              "unrecognzied style symbol: ~s" style)])]
              [(target-url? style)
               (wrap e (format "href{~a}" (target-url-addr style)) #f)]
              [(string? style)
               (wrap e style (regexp-match? #px"^scheme(?!error)" style))]
              [(and (pair? style) (memq (car style) '(bg-color color)))
               (wrap e (format
                        "~a{~a}"
                        (format (if (eq? (car style) 'bg-color)
                                  "in~acolorbox" "intext~acolor")
                                (if (= (length style) 2) "" "rgb"))
                        (if (= (length style) 2)
                          (cadr style)
                          (format "~a,~a,~a"
                                  (/ (cadr style) 255.0)
                                  (/ (caddr style) 255.0)
                                  (/ (cadddr style) 255.0))))
                     #f)]
              [(image-file? style)
               (if (disable-images)
                 (void)
                 (let ([fn (install-file
                            (gif-to-png 
                             (main-collects-relative->path
                              (image-file-path style))))])
                   (printf "\\includegraphics[scale=~a]{~a}"
                           (image-file-scale style) fn)))]
              [else (super render-element e part ri)])))
        (when part-label?
          (printf "}"))
        (when (and (link-element? e)
                   (show-link-page-numbers)
                   (not (done-link-page-numbers)))
          (printf ", \\pageref{t:~a}"
                  (t-encode 
                   (let ([v (resolve-get part ri (link-element-tag e))])
                     (and v (last v))))))
        null))

    (define/private (t-encode s)
      (string-append*
       (map (lambda (c)
              (cond
                [(and (or (char-alphabetic? c) (char-numeric? c))
                      ((char->integer c) . < . 128))
                 (string c)]
                [(char=? c #\space) "_"]
                [else (format "x~x" (char->integer c))]))
            (string->list (format "~s" s)))))

    (define/override (render-flow p part ri start-inline?)
      (if (null? (flow-paragraphs p))
          null
          (begin
            (render-block (car (flow-paragraphs p)) part ri start-inline?)
            (for ([b (in-list (cdr (flow-paragraphs p)))])
              (printf "\n\n")
              (render-block b part ri #f))
            null)))

    (define/override (render-table t part ri inline-table?)
      (let* ([boxed? (eq? 'boxed (table-style t))]
             [index? (eq? 'index (table-style t))]
             [tableform
              (cond [index? "list"]
                    [(and (not (current-table-mode)) (not inline-table?))
                     "bigtabular"]
                    [else "tabular"])]
             [opt (cond [(equal? tableform "bigtabular") "[l]"]
                        [(equal? tableform "tabular") "[t]"]
                        [else ""])]
             [flowss (if index? (cddr (table-flowss t)) (table-flowss t))]
             [row-styles (cdr (or (and (list? (table-style t))
                                       (assoc 'row-styles (table-style t)))
                                  (cons #f (map (lambda (x) #f) flowss))))]
             [inline?
              (and (not boxed?) 
                   (not index?)
                   (ormap (lambda (rs) (equal? rs "inferencetop")) row-styles)
                   (or (null? (table-flowss t))
                       (= 1 (length (car (table-flowss t)))))
                   (let ([m (current-table-mode)])
                     (and m
                          (equal? "bigtabular" (car m))
                          (= 1 (length (car (table-flowss (cadr m))))))))]
             [boxline "{\\setlength{\\unitlength}{\\linewidth}\\begin{picture}(1,0)\\put(0,0){\\line(1,0){1}}\\end{picture}}"]
             [twidth (if (null? (table-flowss t))
                         1
                         (length (car (table-flowss t))))])
        (unless (or (null? flowss) (null? (car flowss)))
          (parameterize ([current-table-mode
                          (if inline? (current-table-mode) (list tableform t))]
                         [show-link-page-numbers
                          (or index? (show-link-page-numbers))])
            (cond
              [index? (printf "\\begin{list}{}{\\parsep=0pt \\itemsep=1pt \\leftmargin=2ex \\itemindent=-2ex}\n")]
              [inline? (void)]
              [else
               (printf "~a\\begin{~a}~a{@{}~a}\n~a"
                       (if (string? (table-style t))
                           (format "\\begin{~a}" (table-style t))
                           "")
                       tableform
                       opt
                       (string-append*
                        (map (lambda (i align)
                               (format "~a@{}"
                                       (case align
                                         [(center) "c"]
                                         [(right) "r"]
                                         [else "l"])))
                             (car flowss)
                             (cdr (or (and (list? (table-style t))
                                           (assoc 'alignment
                                                  (or (table-style t) null)))
                                      (cons #f (map (lambda (x) #f)
                                                    (car flowss)))))))
                       (if boxed? 
                           (if (equal? tableform "bigtabular")
                               (format "~a \\SEndFirstHead\n" boxline)
                               (format "\\multicolumn{~a}{@{}l@{}}{~a} \\\\\n" 
                                       (length (car flowss))
                                       boxline))
                           ""))])
            (let loop ([flowss flowss]
                       [row-styles row-styles])
              (let ([flows (car flowss)]
                    [row-style (car row-styles)])
                (let loop ([flows flows]
                           [col-v-styles (or (and (list? row-style)
                                                  (let ([p (assoc 'valignment row-style)])
                                                    (and p (cdr p))))
                                             (let ([p (and (list? (table-style t))
                                                           (assoc 'valignment (table-style t)))])
                                               (and p (cdr p))))])
                  (unless (null? flows)
                    (when index? (printf "\n\\item "))
                    (unless (eq? 'cont (car flows))
                      (let ([cnt (let loop ([flows (cdr flows)][n 1])
                                   (cond [(null? flows) n]
                                         [(eq? (car flows) 'cont)
                                          (loop (cdr flows) (add1 n))]
                                         [else n]))])
                        (unless (= cnt 1) (printf "\\multicolumn{~a}{l}{" cnt))
                        (render-table-flow (car flows) part ri twidth (and col-v-styles
                                                                           (car col-v-styles)))
                        (unless (= cnt 1) (printf "}"))
                        (unless (null? (list-tail flows cnt)) (printf " &\n"))))
                    (unless (null? (cdr flows)) (loop (cdr flows)
                                                      (and col-v-styles (cdr col-v-styles))))))
                (unless (or index? (null? (cdr flowss)))
                  (printf " \\\\\n")
                  (when (equal? row-style "inferencetop") (printf "\\hline\n")))
                (unless (null? (cdr flowss))
                  (loop (cdr flowss) (cdr row-styles)))))
            (unless inline?
              (printf "\\end{~a}~a"
                      tableform
                      (if (string? (table-style t))
                           (format "\\end{~a}" (table-style t))
                           ""))))))
      null)

    (define/private (render-table-flow p part ri twidth vstyle)
      ;; Emit a \\ between blocks in single-column mode,
      ;; used a nested table otherwise for multiple elements.
      (let ([in-table? (or (and (not (= twidth 1))
                                ((length (flow-paragraphs p)) . > . 1))
                           (eq? vstyle 'top))])
        (when in-table?
          (printf "\\begin{tabular}~a{@{}l@{}}\n"
                  (cond
                   [(eq? vstyle 'top) "[t]"]
                   [(eq? vstyle 'center) "[c]"]
                   [else ""])))
        (let loop ([ps (flow-paragraphs p)])
          (cond
           [(null? ps) (void)]
           [else
            (let ([minipage? (or (not (or (paragraph? (car ps))
                                          (table? (car ps))))
                                 (eq? vstyle 'center))])
              (when minipage?
                (printf "\\begin{minipage}~a{~a\\linewidth}\n"
                        (cond
                         [(eq? vstyle 'top) "[t]"]
                         [(eq? vstyle 'center) "[c]"]
                         [else ""])
                        (/ 1.0 twidth)))
              (render-block (car ps) part ri #f)
              (when minipage?
                (printf " \\end{minipage}\n"))
              (unless (null? (cdr ps))
                (printf " \\\\\n")
                (when in-table?
                  (printf " ~ \\\\\n"))
                (loop (cdr ps))))]))
        (when in-table?
          (printf "\n\\end{tabular}"))
        null))

    (define/override (render-itemization t part ri)
      (let* ([style-str (and (styled-itemization? t)
                             (string? (styled-itemization-style t))
                             (styled-itemization-style t))]
             [mode (or style-str
                       (if (and (styled-itemization? t)
                                (eq? (styled-itemization-style t) 'ordered))
                           "enumerate"
                           "itemize"))])
        (printf "\\begin{~a}" mode)
        (for ([flow (itemization-flows t)])
          (printf "\n\n\\~a" (if style-str
                                  (format "~aItem{" style-str)
                                  "item "))
          (render-flow flow part ri #t)
          (when style-str
            (printf "}")))
        (printf "\\end{~a}" mode)
        null))

    (define/override (render-blockquote t part ri)
      (let ([kind (or (blockquote-style t) "quote")])
        (if (regexp-match #rx"^[\\]" kind)
            (printf "~a{" kind)
            (printf "\\begin{~a}" kind))
        (parameterize ([current-table-mode (list "blockquote" t)])
          (render-flow (make-flow (blockquote-paragraphs t)) part ri #f))
        (if (regexp-match #rx"^[\\]" kind)
            (printf "}")
            (printf "\\end{~a}" kind))
        null))

    (define/override (render-other i part ri)
      (cond
        [(string? i) (display-protected i)]
        [(symbol? i)
         (display (case i
                    [(nbsp) "~"]
                    [(mdash) "---"]
                    [(ndash) "--"]
                    [(ldquo) "``"]
                    [(rdquo) "''"]
                    [(rsquo) "'"]
                    [(prime) "$'$"]
                    [(rarr) "$\\rightarrow$"]
                    [(alpha) "$\\alpha$"]
                    [(infin) "$\\infty$"]
                    [(lang) "$\\langle$"]
                    [(rang) "$\\rangle$"]
                    [else (error 'render "unknown symbol element: ~e" i)]))]
        [else (display-protected (format "~s" i))])
      null)

    (define/private (display-protected s)
      (if (eq? (rendering-tt) 'exact)
          (display s)
          (let ([len (string-length s)])
            (let loop ([i 0])
              (unless (= i len)
                (let ([c (string-ref s i)])
                  (display
                   (case c
                     [(#\\) (if (rendering-tt)
                                "{\\char`\\\\}"
                                "$\\backslash$")]
                     [(#\_) (if (rendering-tt)
                                "{\\char`\\_}"
                                "$\\_$")]
                     [(#\^) "{\\char'136}"]
                     [(#\>) (if (rendering-tt) "{\\texttt >}" "$>$")]
                     [(#\<) (if (rendering-tt) "{\\texttt <}" "$<$")]
                     [(#\|) (if (rendering-tt) "{\\texttt |}" "$|$")]
                     [(#\? #\! #\. #\:)
                      (if (rendering-tt) (format "{\\hbox{\\texttt{~a}}}" c) c)]
                     [(#\~) "$\\sim$"]
                     [(#\{ #\}) (if (rendering-tt)
                                    (format "{\\char`\\~a}" c)
                                    (format "\\~a" c))]
                     [(#\# #\% #\& #\$) (format "\\~a" c)]
                     [(#\uA0) "~"]
                     [(#\uDF) "{\\ss}"]
                     [(#\u039A) "K"] ; kappa
                     [(#\u0391) "A"] ; alpha
                     [(#\u039F) "O"] ; omicron
                     [(#\u03A3) "$\\Sigma$"]
                     [(#\u03BA) "$\\kappa$"]
                     [(#\u03B1) "$\\alpha$"]
                     [(#\u03B2) "$\\beta$"]
                     [(#\u03B3) "$\\gamma$"]
                     [(#\u03BF) "o"] ; omicron
                     [(#\u03C3) "$\\sigma$"]
                     [(#\u03C2) "$\\varsigma$"]
                     [(#\u03BB) "$\\lambda$"]
                     [(#\u039B) "$\\Lambda$"]
                     [(#\u03BC) "$\\mu$"]
                     [(#\u03C0) "$\\pi$"]
                     [(#\∞) "$\\infty$"]
                     [(#\à) "\\`{a}"]
                     [else c])))
                (loop (add1 i)))))))

    ;; ----------------------------------------

    (define/override (table-of-contents sec ri)
      ;; FIXME: isn't local to the section
      (make-toc-paragraph null))

    (define/override (local-table-of-contents part ri style)
      (make-paragraph null))

    ;; ----------------------------------------

    (super-new)))

#lang scheme/base

(require "struct.ss"
         mzlib/class
         scheme/runtime-path
         scheme/port
         scheme/path
         scheme/string
         setup/main-collects)
(provide render-mixin)

(define current-table-mode (make-parameter #f))
(define rendering-tt (make-parameter #f))
(define show-link-page-numbers (make-parameter #f))
(define done-link-page-numbers (make-parameter #f))
(define disable-images (make-parameter #f))

(define-struct (toc-paragraph paragraph) ())

(define-runtime-path scribble-tex "scribble.tex")

(define (gif-to-png p)
  (if (equal? (filename-extension p) #"gif")
      (path-replace-suffix p #".png")
      p))

(define (render-mixin %)
  (class %
    (init-field [style-file #f]
                [style-extra-files null])

    (define/override (get-suffix) #".tex")

    (inherit render-flow
             render-block
             render-content
             install-file
             format-number)

    (define/override (render-one d ri fn)
      (let ([style-file (or style-file scribble-tex)])
        (for-each
         (lambda (style-file)
           (with-input-from-file style-file
             (lambda ()
               (copy-port (current-input-port) (current-output-port)))))
         (cons style-file style-extra-files))
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
        (printf "\\postDoc\n\\end{document}\n")))

    (define/override (render-part d ri)
      (let ([number (collected-info-number (part-collected-info d ri))])
        (when (and (part-title-content d) (pair? number))
          (when (part-style? d 'index)
            (printf "\\twocolumn\n\\parskip=0pt\n\\addcontentsline{toc}{section}{Index}\n"))
          (let ([no-number? (and (pair? number) 
                                 (or (not (car number))
                                     ((length number) . > . 3)))])
            (printf "\\~a~a~a"
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
          (printf "\\label{t:~a}" (t-encode (tag-key t ri))))
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
          (printf "\n\n")
          (when (string? style)
            (printf "\\~a{" style))
          (if (toc-paragraph? p)
              (printf "\\newpage \\tableofcontents \\newpage")
              (super render-paragraph p part ri))
          (when (string? style) (printf "}"))
          (printf "\n\n")))
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
                    (t-encode (tag-key (target-element-tag e) ri))))
          (when part-label?
            (printf "\\S")
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
            (printf " ``"))
          (let ([style (and (element? e)
                            (let ([s (flatten-style (element-style e))])
                              (if (with-attributes? s)
                                (with-attributes-style s)
                                s)))]
                [wrap (lambda (e s tt?)
                        (printf "{\\~a{" s)
                        (parameterize ([rendering-tt (or tt? (rendering-tt))])
                          (super render-element e part ri))
                        (printf "}}"))])
            (cond
              [(symbol? style)
               (case style
                 [(italic) (wrap e "textit" #f)]
                 [(bold) (wrap e "textbf" #f)]
                 [(tt) (wrap e "mytexttt" #t)]
                 [(no-break) (super render-element e part ri)]
                 [(sf) (wrap e "textsf" #f)]
                 [(subscript) (wrap e "textsub" #f)]
                 [(superscript) (wrap e "textsuper" #f)]
                 [(hspace)
                  (let ([s (content->string (element-content e))])
                    (case (string-length s)
                      [(0) (void)]
                      [else
                       (printf "\\mbox{\\hphantom{\\mytexttt{~a}}}"
                               (regexp-replace* #rx"." s "x"))]))]
                 [(newline) (printf "\\\\")]
                 [else (error 'latex-render
                              "unrecognzied style symbol: ~s" style)])]
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
          (printf "''"))
        (when (and (link-element? e)
                   (show-link-page-numbers)
                   (not (done-link-page-numbers)))
          (printf ", \\pageref{t:~a}"
                  (t-encode (tag-key (link-element-tag e) ri))))
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

    (define/override (render-table t part ri inline-table?)
      (let* ([boxed? (eq? 'boxed (table-style t))]
             [index? (eq? 'index (table-style t))]
             [inline?
              (and (not boxed?) (not index?)
                   (or (null? (table-flowss t))
                       (= 1 (length (car (table-flowss t)))))
                   (let ([m (current-table-mode)])
                     (and m
                          (equal? "bigtabular" (car m))
                          (= 1 (length (car (table-flowss (cadr m))))))))]
             [tableform
              (cond [index? "list"]
                    [(and (not (current-table-mode)) (not inline-table?))
                     "bigtabular"]
                    [else "tabular"])]
             [opt (cond [(equal? tableform "bigtabular") "[l]"]
                        [(equal? tableform "tabular") "[t]"]
                        [else ""])]
             [flowss (if index? (cddr (table-flowss t)) (table-flowss t))])
        (unless (or (null? flowss) (null? (car flowss)))
          (parameterize ([current-table-mode
                          (if inline? (current-table-mode) (list tableform t))]
                         [show-link-page-numbers
                          (or index? (show-link-page-numbers))])
            (cond
              [index? (printf "\\begin{list}{}{\\parsep=0pt \\itemsep=1pt \\leftmargin=2ex \\itemindent=-2ex}\n")]
              [inline? (void)]
              [else
               (printf "\n\n~a\\begin{~a}~a{@{}~a}\n"
                       (if boxed?
                         (format "{~a\\begin{picture}(1,0)\\put(0,0){\\line(1,0){1}}\\end{picture}}~a\n\\nopagebreak\n"
                                 "\\setlength{\\unitlength}{\\linewidth}"
                                 (if (equal? tableform "bigtabular")
                                   "\\bigtabline"
                                   "\n\n"))
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
                                                    (car flowss))))))))])
            (let loop ([flowss flowss]
                       [row-styles
                        (cdr (or (and (list? (table-style t))
                                      (assoc 'row-styles (table-style t)))
                                 (cons #f (map (lambda (x) #f) flowss))))])
              (let ([flows (car flowss)]
                    [row-style (car row-styles)])
                (let loop ([flows flows])
                  (unless (null? flows)
                    (when index? (printf "\\item "))
                    (unless (eq? 'cont (car flows))
                      (let ([cnt (let loop ([flows (cdr flows)][n 1])
                                   (cond [(null? flows) n]
                                         [(eq? (car flows) 'cont)
                                          (loop (cdr flows) (add1 n))]
                                         [else n]))])
                        (unless (= cnt 1) (printf "\\multicolumn{~a}{l}{" cnt))
                        (render-table-flow (car flows) part ri)
                        (unless (= cnt 1) (printf "}"))
                        (unless (null? (list-tail flows cnt)) (printf " &\n"))))
                    (unless (null? (cdr flows)) (loop (cdr flows)))))
                (unless (or index? (null? (cdr flowss)))
                  (printf " \\\\\n")
                  (when (equal? row-style "inferencetop") (printf "\\hline\n")))
                (unless (null? (cdr flowss))
                  (loop (cdr flowss) (cdr row-styles)))))
            (unless inline?
              (printf "~a\n\n\\end{~a}\n"
                      (if (equal? tableform "bigtabular") "\n\\\\" "")
                      tableform)))))
      null)

    (define/private (render-table-flow p part ri)
      ;; Emit a \\ between blocks:
      (let loop ([ps (flow-paragraphs p)])
        (cond
         [(null? ps) (void)]
         [else
          (let ([minipage? (not (or (paragraph? (car ps))
                                    (table? (car ps))))])
            (when minipage?
              (printf "\\begin{minipage}{\\linewidth}\n"))
            (render-block (car ps) part ri #f)
            (when minipage?
              (printf " \\end{minipage}\n"))
            (unless (null? (cdr ps))
              (printf " \\\\\n")
              (loop (cdr ps))))]))
      null)

    (define/override (render-itemization t part ri)
      (printf "\n\n\\begin{itemize}\n")
      (for ([flow (itemization-flows t)])
        (printf "\n\n\\item ")
        (render-flow flow part ri #t))
      (printf "\n\n\\end{itemize}\n")
      null)

    (define/override (render-blockquote t part ri)
      (let ([kind (or (blockquote-style t) "quote")])
        (printf "\n\n\\begin{~a}\n" kind)
        (parameterize ([current-table-mode (list "blockquote" t)])
          (for ([e (blockquote-paragraphs t)]) (render-block e part ri #f)))
        (printf "\n\n\\end{~a}\n" kind)
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
      (let ([len (string-length s)])
        (let loop ([i 0])
          (unless (= i len)
            (let ([c (string-ref s i)])
              (display
               (case c
                 [(#\\) "$\\backslash$"]
                 [(#\_) "$\\_$"]
                 [(#\^) "{\\char'136}"]
                 [(#\>) (if (rendering-tt) "{\\texttt >}" "$>$")]
                 [(#\<) (if (rendering-tt) "{\\texttt <}" "$<$")]
                 [(#\|) (if (rendering-tt) "{\\texttt |}" "$|$")]
                 [(#\? #\! #\. #\:)
                  (if (rendering-tt) (format "{\\hbox{\\texttt{~a}}}" c) c)]
                 [(#\~) "$\\sim$"]
                 [(#\{ #\} #\# #\% #\& #\$) (format "\\~a" c)]
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
                 [else c])))
            (loop (add1 i))))))

    ;; ----------------------------------------

    (define/override (table-of-contents sec ri)
      ;; FIXME: isn't local to the section
      (make-toc-paragraph null))

    (define/override (local-table-of-contents part ri style)
      (make-paragraph null))

    ;; ----------------------------------------

    (super-new)))

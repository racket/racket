
(module latex-render mzscheme
  (require "struct.ss"
           (lib "class.ss"))
  (provide render-mixin)

  (define current-table-mode (make-parameter #f))
  (define rendering-tt (make-parameter #f))
  (define show-link-page-numbers (make-parameter #f))

  (define-struct (toc-paragraph paragraph) ())

  (define (render-mixin %)
    (class %
      (define/override (get-suffix) #".tex")

      (inherit render-flow
               render-flow-element
               render-content
               install-file
               format-number)

      (define (define-color s s2)
        (printf "\\newcommand{\\~a}[1]{{\\mytexttt{\\color{~a}{#1}}}}\n" s s2))

      (define/override (render-one d ri fn)
        (printf "\\documentclass{article}\n")
        (printf "\\parskip=10pt%\n")
        (printf "\\parindent=0pt%\n")
        (printf "\\usepackage{graphicx}\n")
        (printf "\\usepackage{hyperref}\n")
        (printf "\\renewcommand{\\rmdefault}{ptm}\n")
        ;; (printf "\\usepackage{fullpage}\n")
        (printf "\\usepackage{longtable}\n")
        (printf "\\usepackage[usenames,dvipsnames]{color}\n")
        (printf "\\hypersetup{bookmarks=true,bookmarksopen=true,bookmarksnumbered=true}\n")
        (printf "\\newcommand{\\mytexttt}[1]{{\\small \\texttt{#1}}}\n")
        (define-color "schemeplain" "black")
        (printf "\\newcommand{\\schemekeyword}[1]{{\\color{black}{\\mytexttt{\\textbf{#1}}}}}\n")
        (printf "\\newcommand{\\schemesyntaxlink}[1]{\\schemekeyword{#1}}\n")
        (printf "\\definecolor{CommentColor}{rgb}{0.76,0.45,0.12}\n")
        (printf "\\definecolor{ParenColor}{rgb}{0.52,0.24,0.14}\n")
        (printf "\\definecolor{IdentifierColor}{rgb}{0.15,0.15,0.50}\n")
        (printf "\\definecolor{ResultColor}{rgb}{0.0,0.0,0.69}\n")
        (printf "\\definecolor{ValueColor}{rgb}{0.13,0.55,0.13}\n")
        (printf "\\definecolor{OutputColor}{rgb}{0.59,0.00,0.59}\n")
        (define-color "schemecomment" "CommentColor")
        (define-color "schemeparen" "ParenColor")
        (define-color "schemeinputbg" "ParenColor")
        (define-color "schemesymbol" "IdentifierColor")
        (define-color "schemevalue" "ValueColor")
        (define-color "schemevaluelink" "blue")
        (define-color "schemeresult" "ResultColor")
        (define-color "schemestdout" "OutputColor")
        (define-color "schememeta" "IdentifierColor")
        (define-color "schememod" "black")
        (define-color "schemereader" "black")
        (define-color "schemevariablecol" "IdentifierColor")
        (printf "\\newcommand{\\schemevariable}[1]{{\\schemevariablecol{\\textsl{#1}}}}\n")
        (define-color "schemeerrorcol" "red")
        (printf "\\newcommand{\\schemeerror}[1]{{\\schemeerrorcol{\\textrm{\\textit{#1}}}}}\n")
        (printf "\\newcommand{\\schemeopt}[1]{#1}\n")
        (printf "\\newcommand{\\textsub}[1]{$_{#1}$}\n")
        (printf "\\newcommand{\\textsuper}[1]{$^{#1}$}\n")
        (printf "\\newcommand{\\refcontent}[1]{#1}\n")
        (printf "\\newcommand{\\smaller}[1]{{\\footnotesize #1}}\n")
        (printf "\\definecolor{PaleBlue}{rgb}{0.90,0.90,1.0}\n")
        (printf "\\definecolor{LightGray}{rgb}{0.90,0.90,0.90}\n")
        (printf "\\newcommand{\\schemeinput}[1]{\\colorbox{LightGray}{\\hspace{-0.5ex}\\schemeinputbg{#1}\\hspace{-0.5ex}}}\n")
        (printf "\\newcommand{\\highlighted}[1]{\\colorbox{PaleBlue}{\\hspace{-0.5ex}\\schemeinputbg{#1}\\hspace{-0.5ex}}}\n")
        (printf "\\newcommand{\\techlink}[1]{#1}\n")
        (printf "\\newcommand{\\indexlink}[1]{#1}\n")
        (printf "\\newcommand{\\imageleft}[1]{} % drop it\n")
        (printf "\\begin{document}\n\\sloppy\n")
        (when (part-title-content d)
          (printf "\\title{")
          (render-content (part-title-content d) d ri)
          (printf "}\\maketitle\n"))
        (render-part d ri)
        (printf "\\end{document}\n"))

      (define/override (render-part d ri)
        (let ([number (collected-info-number (part-collected-info d ri))])
          (when (and (part-title-content d)
                     (pair? number))
            (when (part-style? d 'index)
              (printf "\\twocolumn\n\\parskip=0pt\n\\addcontentsline{toc}{section}{Index}\n"))
            (printf "\\~a~a{"
                    (case (length number)
                      [(0 1) "newpage\n\n\\section"]
                      [(2) "subsection"]
                      [(3) "subsubsection"]
                      [else "subsubsection*"])
                    (if (and (pair? number)
                             (not (car number)))
                        "*"
                        ""))
            (render-content (part-title-content d) d ri)
            (printf "}")
            (when (part-style? d 'index)
              (printf "\n\n")))
          (for-each (lambda (t)
                      (printf "\\label{t:~a}" (t-encode (tag-key t ri))))
                    (part-tags d))
          (render-flow (part-flow d) d ri)
          (for-each (lambda (sec) (render-part sec ri))
                    (part-parts d))
          null))
      
      (define/override (render-paragraph p part ri)
        (printf "\n\n")
        (let ([margin? (and (styled-paragraph? p)
                            (equal? "refpara" (styled-paragraph-style p)))])
          (when margin?
            (printf "\\marginpar{\\footnotesize "))
          (if (toc-paragraph? p)
              (printf "\\newpage \\tableofcontents \\newpage")
              (super render-paragraph p part ri))
          (when margin?
            (printf "}")))
        (printf "\n\n")
        null)

      (define/override (render-element e part ri)
        (let ([part-label? (and (link-element? e)
                                (pair? (link-element-tag e))
                                (eq? 'part (car (link-element-tag e)))
                                (null? (element-content e)))])
          (parameterize ([show-link-page-numbers #f])
            (when (target-element? e)
              (printf "\\label{t:~a}" (t-encode (tag-key (target-element-tag e) ri))))
            (when part-label?
              (printf "\\S")
              (render-content (let ([dest (resolve-get part ri (link-element-tag e))])
                                (if dest
                                    (if (list? (cadr dest))
                                        (format-number (cadr dest) null)
                                        (begin
                                          (fprintf (current-error-port)
                                                   "Internal tag error: ~s -> ~s\n"
                                                   (link-element-tag e)
                                                   dest)
                                          '("!!!")))
                                    (list "???")))
                              part
                              ri)
              (printf " ``"))
            (let ([style (and (element? e)
                              (element-style e))]
                  [wrap (lambda (e s tt?)
                          (printf "{\\~a{" s)
                          (parameterize ([rendering-tt (or tt?
                                                           (rendering-tt))])
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
                  [(hspace) (let ([s (content->string (element-content e))])
                              (case (string-length s)
                                [(0) (void)]
                                [else
                                 (printf "{\\mytexttt{~a}}"
                                         (regexp-replace* #rx"." s "~"))]))]
                  [else (error 'latex-render "unrecognzied style symbol: ~s" style)])]
               [(string? style)
                (wrap e style (regexp-match? #px"^scheme(?!error)" style))]
               [(image-file? style) 
                (let ([fn (install-file (image-file-path style))])
                  (printf "\\includegraphics{~a}" fn))]
               [else (super render-element e part ri)])))
          (when part-label?
            (printf "''"))
          (when (and (link-element? e)
                     (show-link-page-numbers))
            (printf ", \\pageref{t:~a}" (t-encode (tag-key (link-element-tag e) ri))))
          null))

      (define/private (t-encode s)
        (apply
         string-append
         (map (lambda (c)
                (cond
                 [(and (or (char-alphabetic? c)
                           (char-numeric? c))
                       ((char->integer c) . < . 128))
                  (string c)]
                 [(char=? c #\space) "_"]
                 [else
                  (format "x~x" (char->integer c))]))
              (string->list (format "~s" s)))))

      (define/override (render-table t part ri)
        (let* ([boxed? (eq? 'boxed (table-style t))]
               [index? (eq? 'index (table-style t))]
               [inline? (and (not boxed?)
                             (not index?)
                             (or (null? (table-flowss t))
                                 (= 1 (length (car (table-flowss t)))))
                             (let ([m (current-table-mode)])
                               (and m
                                    (equal? "longtable" (car m))
                                    (= 1 (length (car (table-flowss (cadr m))))))))]
               [tableform (cond
                           [index? "list"]
                           [(not (current-table-mode))
                            "longtable"]
                           [else "tabular"])]
               [opt (cond
                     [(equal? tableform "longtable") "[l]"]
                     [(equal? tableform "tabular") "[t]"]
                     [else ""])])
          (unless (or (null? (table-flowss t))
                      (null? (car (table-flowss t))))
            (parameterize ([current-table-mode (if inline?
                                                   (current-table-mode)
                                                   (list tableform t))]
                           [show-link-page-numbers (or index?
                                                       (show-link-page-numbers))])
              (cond
               [index? (printf "\\begin{list}{}{\\parsep=0pt \\itemsep=1pt \\leftmargin=2ex \\itemindent=-2ex}\n")]
               [inline? (void)]
               [else
                (printf "\n\n~a\\begin{~a}~a{@{}~a}\n"
                        (if boxed? 
                            (format "{~a\\begin{picture}(1,0)\\put(0,0){\\line(1,0){1}}\\end{picture}}~a\n\\nopagebreak\n" 
                                    "\\setlength{\\unitlength}{\\linewidth}"
                                    (if (equal? tableform "longtable")
                                        "\\vspace{-5ex}"
                                        "\n\n"))
                            "")
                        tableform
                        opt
                        (apply string-append
                               (map (lambda (i align) 
                                      (format "~a@{}"
                                              (case align
                                                [(center) "c"]
                                                [(right) "r"]
                                                [else "l"])))
                                    (car (table-flowss t))
                                    (cdr (or (and (list? (table-style t))
                                                  (assoc 'alignment (or (table-style t) null)))
                                             (cons #f (map (lambda (x) #f) (car (table-flowss t)))))))))])
              (let loop ([flowss (table-flowss t)]
                         [row-styles (cdr (or (and (list? (table-style t))
                                                   (assoc 'row-styles (table-style t)))
                                              (cons #f (map (lambda (x) #f) (table-flowss t)))))])
                (let ([flows (car flowss)]
                      [row-style (car row-styles)])
                  (let loop ([flows flows])
                    (unless (null? flows)
                      (when index?
                        (printf "\\item "))
                      (unless (eq? 'cont (car flows))
                        (let ([cnt (let loop ([flows (cdr flows)][n 1])
                                     (cond
                                      [(null? flows) n]
                                      [(eq? (car flows) 'cont) (loop (cdr flows) (add1 n))]
                                      [else n]))])
                          (unless (= cnt 1)
                            (printf "\\multicolumn{~a}{l}{" cnt))
                          (render-flow (car flows) part ri)
                          (unless (= cnt 1)
                            (printf "}"))
                          (unless (null? (list-tail flows cnt))
                            (printf " &\n"))))
                      (unless (null? (cdr flows))
                        (loop (cdr flows)))))
                  (unless (or index?
                              (null? (cdr flowss)))
                    (printf " \\\\\n")
                    (when (equal? row-style "inferencetop")
                      (printf "\\hline\n")))
                  (unless (null? (cdr flowss))
                    (loop (cdr flowss) (cdr row-styles)))))
              (unless inline?
                (printf "\n\n\\end{~a}~a\n" 
                        tableform
                        (if (equal? tableform "longtable")
                            "\\vspace{-3ex}" ;; counteracts mysterious space added after longtable
                            ""))))))
        null)

      (define/override (render-itemization t part ri)
        (printf "\n\n\\begin{itemize}\n")
        (for-each (lambda (flow)
                    (printf "\n\n\\item ")
                    (render-flow flow part ri))
                  (itemization-flows t))
        (printf "\n\n\\end{itemize}\n")
        null)

      (define/override (render-blockquote t part ri)
        (printf "\n\n\\begin{quote}\n")
        (parameterize ([current-table-mode (list "blockquote" t)])
          (for-each (lambda (e)
                      (render-flow-element e part ri))
                    (blockquote-paragraphs t)))
        (printf "\n\n\\end{quote}\n")
        null)

      (define/override (render-other i part ri)
        (cond
         [(string? i) (display-protected i)]
         [(symbol? i) (display
                       (case i
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
                         [else (error 'render "unknown symbol element: ~e" i)]))]
         [else (display-protected (format "~s" i))])
        null)

      (define/private (display-protected s)
        (let ([len (string-length s)])
          (let loop ([i 0])
            (unless (= i len)
              (let ([c (string-ref s i)])
                (case c
                  [(#\\) (display "$\\backslash$")]
                  [(#\_) (display "$\\_$")]
                  [(#\^) (display "{\\char'136}")]
                  [(#\>) (if (rendering-tt)
                             (display "{\\texttt >}")
                             (display "$>$"))]
                  [(#\<) (if (rendering-tt)
                             (display "{\\texttt <}")
                             (display "$<$"))]
                  [(#\|) (if (rendering-tt)
                             (display "{\\texttt |}")
                             (display "$|$"))]
                  [(#\? #\! #\. #\:) (if (rendering-tt)
                                         (printf "{\\hbox{\\texttt{~a}}}" c)
                                         (display c))]
                  [(#\~) (display "$\\sim$")]
                  [(#\{ #\} #\# #\% #\& #\$) (display "\\") (display c)]
                  [(#\uDF) (display "{\\ss}")]
                  [(#\u039A) (display "K")] ; kappa
                  [(#\u0391) (display "A")] ; alpha
                  [(#\u039F) (display "O")] ; omicron
                  [(#\u03A3) (display "$\\Sigma$")]
                  [(#\u03BA) (display "$\\kappa$")]
                  [(#\u03B1) (display "$\\alpha$")]
                  [(#\u03BF) (display "o")] ; omicron
                  [(#\u03C3) (display "$\\sigma$")]
                  [(#\u03C2) (display "$\\varsigma$")]
                  [(#\u03BB) (display "$\\lambda$")]
                  [(#\u039B) (display "$\\Lambda$")]
                  [(#\u03BC) (display "$\\mu$")]
                  [else (display c)]))
              (loop (add1 i))))))
                  
      ;; ----------------------------------------

      (define/override (table-of-contents sec ri)
        ;; FIXME: isn't local to the section
        (make-toc-paragraph null))

      (define/override (local-table-of-contents part ri)
        (make-paragraph null))

      ;; ----------------------------------------

      (super-new))))

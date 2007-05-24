
(module latex-render mzscheme
  (require "struct.ss"
           (lib "class.ss"))
  (provide render-mixin)

  (define current-table-depth (make-parameter 0))

  (define-struct (toc-paragraph paragraph) ())

  (define (render-mixin %)
    (class %
      (define/override (get-suffix) #".tex")

      (inherit render-flow
               render-content
               install-file)

      (define (define-color s s2)
        (printf "\\newcommand{\\~a}[1]{{\\texttt{\\color{~a}{#1}}}}\n" s s2))

      (define/override (render-one d ht fn)
        (printf "\\documentclass{article}\n")
        (printf "\\parskip=10pt%\n")
        (printf "\\parindent=0pt%\n")
        (printf "\\usepackage{graphicx}\n")
        (printf "\\usepackage{fullpage}\n")
        (printf "\\usepackage{longtable}\n")
        (printf "\\usepackage[usenames,dvipsnames]{color}\n")
        (define-color "schemeplain" "black")
        (printf "\\newcommand{\\schemekeyword}[1]{{\\color{black}{\\texttt{\\textbf{#1}}}}}\n")
        (printf "\\newcommand{\\schemesyntaxlink}[1]{\\schemekeyword{#1}}\n")
        (define-color "schemecomment" "Brown")
        (define-color "schemeparen" "BrickRed")
        (define-color "schemeinputcol" "BrickRed")
        (define-color "schemesymbol" "NavyBlue")
        (define-color "schemevalue" "ForestGreen")
        (define-color "schemevaluelink" "blue")
        (define-color "schemeresult" "blue")
        (define-color "schemestdout" "Purple")
        (define-color "schemevariablecol" "NavyBlue")
        (printf "\\newcommand{\\schemevariable}[1]{{\\schemevariablecol{\\textsl{#1}}}}\n")
        (define-color "schemeerrorcol" "red")
        (printf "\\newcommand{\\schemeerror}[1]{{\\schemeerrorcol{\\textit{#1}}}}\n")
        (printf "\\newcommand{\\schemeopt}[1]{#1}\n")
        (printf "\\newcommand{\\textsub}[1]{$_{#1}$}\n")
        (printf "\\newcommand{\\textsuper}[1]{$^{#1}$}\n")
        (printf "\\definecolor{LightGray}{rgb}{0.85,0.85,0.85}\n")
        (printf "\\newcommand{\\schemeinput}[1]{\\colorbox{LightGray}{\\schemeinputcol{#1}}}\n")
        (printf "\\begin{document}\n")
        (when (part-title-content d)
          (printf "\\title{")
          (render-content (part-title-content d) d ht)
          (printf "}\\maketitle\n"))
        (render-part d ht)
        (printf "\\end{document}\n"))

      (define/override (render-part d ht)
        (let ([number (collected-info-number (part-collected-info d))])
          (when (and (part-title-content d)
                     (pair? number))
            (printf "\\~a~a{"
                    (case (length number)
                      [(0 1) "section"]
                      [(2) "subsection"]
                      [(3) "subsubsection"]
                      [else "subsubsection*"])
                    (if (and (pair? number)
                             (not (car number)))
                        "*"
                        ""))
            (render-content (part-title-content d) d ht)
            (printf "}"))
          (when (part-tag d)
            (printf "\\label{section:~a}" (part-tag d)))
          (render-flow (part-flow d) d ht)
          (for-each (lambda (sec) (render-part sec ht))
                    (part-parts d))
          null))
      
      (define/override (render-paragraph p part ht)
        (printf "\n\n")
        (if (toc-paragraph? p)
            (printf "\\tableofcontents")
            (super render-paragraph p part ht))
        (printf "\n\n")
        null)

      (define/override (render-element e part ht)
        (when (and (link-element? e)
                   (pair? (link-element-tag e))
                   (eq? 'part (car (link-element-tag e))))
          (printf "\\S\\ref{section:~a} " (cadr (link-element-tag e))))
        (let ([style (and (element? e)
                          (element-style e))]
              [wrap (lambda (e s)
                      (printf "{\\~a{" s)
                      (super render-element e part ht)
                      (printf "}}"))])
          (cond
           [(symbol? style)
            (case style
              [(italic) (wrap e "textit")]
              [(bold) (wrap e "textbf")]
              [(tt) (wrap e "texttt")]
              [(sf) (wrap e "textsf")]
              [(subscript) (wrap e "textsub")]
              [(superscript) (wrap e "textsuper")]
              [(hspace) (let ([s (content->string (element-content e))])
                          (unless (zero? (string-length s))
                            (printf "{\\texttt ~a}"
                                    (regexp-replace* #rx"." s "~"))))]
              [else (error 'latex-render "unrecognzied style symbol: ~s" style)])]
           [(string? style)
            (wrap e style)]
           [(image-file? style) 
            (let ([fn (install-file (image-file-path style))])
              (printf "\\includegraphics{~a}" fn))]
           [else (super render-element e part ht)]))
        null)

      (define/override (render-table t part ht)
        (let* ([boxed? (eq? 'boxed (table-style t))]
               [index? (eq? 'index (table-style t))]
               [tableform (cond
                           [index? "theindex"]
                           [(zero? (current-table-depth))
                            "longtable"]
                           [else "tabular"])]
               [opt (if (zero? (current-table-depth))
                        "[l]"
                        "")])
          (unless (null? (table-flowss t))
            (parameterize ([current-table-depth (add1 (current-table-depth))])
              (if index?
                  (printf "\n\n\\begin{theindex}\n")
                  (printf "\n\n~a\\begin{~a}~a{@{}~a@{}}\n"
                          (if boxed? "\\vspace{4ex}\\hrule\n\\vspace{-2ex}\n" "")
                          tableform
                          opt
                          (make-string (length (car (table-flowss t))) #\l)))
              (for-each (lambda (flows)
                          (let loop ([flows flows])
                            (unless (null? flows)
                              (render-flow (car flows) part ht)
                              (unless (null? (cdr flows))
                                (printf " &\n")
                                (loop (cdr flows)))))
                          (unless index?
                            (printf " \\\\\n")))
                        (table-flowss t))
              (printf "\n\n\\end{~a}\n" tableform))))
        null)

      (define/override (render-itemization t part ht)
        (printf "\n\n\\begin{itemize}\n")
        (for-each (lambda (flow)
                    (printf "\n\n\\item ")
                    (render-flow flow part ht))
                  (itemization-flows t))
        (printf "\n\n\\end{itemize}\n")
        null)

      (define/override (render-other i part ht)
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
                         [(rarr) "$\\rightarrow$"]))]
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
                  [(#\>) (display "$>$")]
                  [(#\<) (display "$<$")]
                  [(#\~) (display "$\\sim$")]
                  [(#\{ #\} #\# #\% #\&) (display "\\") (display c)]
                  [(#\uDF) (display "{\\ss}")]
                  [(#\u039A #\u0391 #\u039F #\u03A3
                    #\u03BA #\u03b1 #\u03BF #\u03C3)
                   (printf "$\\backslash$u~a"
                           (let ([s (format "0000~x" (char->integer c))])
                             (substring s (- (string-length s) 4))))]
                  [else (display c)]))
              (loop (add1 i))))))
                  
      ;; ----------------------------------------

      (define/override (table-of-contents sec ht)
        ;; FIXME: isn't local to the section
        (make-toc-paragraph null))

      ;; ----------------------------------------

      (super-new))))

#lang scheme/base
(require "core.rkt"
         "latex-properties.rkt"
         "private/render-utils.rkt"
         scheme/class
         scheme/runtime-path
         scheme/port
         scheme/string
         scheme/list
         setup/main-collects
         file/convertible)
(provide render-mixin
         make-render-part-mixin)

(define current-table-mode (make-parameter #f))
(define rendering-tt (make-parameter #f))
(define show-link-page-numbers (make-parameter #f))
(define done-link-page-numbers (make-parameter #f))
(define disable-images (make-parameter #f))
(define escape-brackets (make-parameter #f))
(define suppress-newline-content (make-parameter #f))

(define-struct (toc-paragraph paragraph) ())

(define-runtime-path scribble-prefix-tex "scribble-prefix.tex")
(define-runtime-path scribble-tex "scribble.tex")
(define-runtime-path scribble-style-tex "scribble-style.tex")

(define (color->string c)
  (if (string? c)
      c
      (format "~a,~a,~a"
              (/ (car c) 255.0)
              (/ (cadr c) 255.0)
              (/ (caddr c) 255.0))))

(define (make-render-part-mixin n)
  (lambda (%)
    (class (render-mixin %)
      (define/override (render-part-depth) n)
      (super-new))))

(define-runtime-path skull-tex "scribble-skull.tex")
(define skull-style (make-style #f (list (tex-addition skull-tex))))

(define (render-mixin %)
  (class %
    (inherit-field prefix-file style-file style-extra-files)

    (define/override (current-render-mode)
      '(latex))

    (define/override (get-suffix) #".tex")

    (inherit render-block
             render-part
             install-file
             format-number
             extract-part-style-files
             extract-version
             extract-date
             extract-authors
             extract-pretitle)

    (define/override (auto-extra-files? v) (latex-defaults? v))
    (define/override (auto-extra-files-paths v) (latex-defaults-extra-files v))

    (define/public (render-part-depth) #f)

    (define/override (render-one d ri fn)
      (let* ([defaults (ormap (lambda (v) (and (latex-defaults? v) v))
                              (style-properties (part-style d)))]
             [prefix-file (or prefix-file
                              (and defaults
                                   (let ([v (latex-defaults-prefix defaults)])
                                     (cond
                                      [(bytes? v) v]
                                      [else (main-collects-relative->path v)])))
                              scribble-prefix-tex)]
             [style-file (or style-file 
                             (and defaults
                                  (let ([v (latex-defaults-style defaults)])
                                    (cond
                                     [(bytes? v) v]
                                     [else (main-collects-relative->path v)])))
                             scribble-style-tex)]
             [all-style-files (cons scribble-tex
                                    (append (extract-part-style-files
                                             d
                                             ri
                                             'tex
                                             (lambda (p) #f)
                                             tex-addition?
                                             tex-addition-path)
                                            (list style-file)
                                            style-extra-files))]
             [whole-doc? (not (render-part-depth))])
        (if whole-doc?
            (for ([style-file (in-list (cons prefix-file all-style-files))])
              (if (bytes? style-file)
                  (display style-file)
                  (with-input-from-file style-file
                    (lambda ()
                      (copy-port (current-input-port) (current-output-port))))))
            (for ([style-file (in-list all-style-files)])
              (install-file style-file)))
        (when whole-doc?
          (printf "\\begin{document}\n\\preDoc\n")
          (when (part-title-content d)
            (let ([vers (extract-version d)]
                  [date (extract-date d)]
                  [pres (extract-pretitle d)]
                  [auths (extract-authors d)])
              (for ([pre (in-list pres)])
                (printf "\n\n")
                (do-render-paragraph pre d ri #t #f))
              (when date (printf "\\date{~a}\n" date))
              (printf "\\titleAnd~aVersionAnd~aAuthors{" 
                      (if (equal? vers "") "Empty" "")
                      (if (null? auths) "Empty" ""))
              (render-content (part-title-content d) d ri)
              (printf "}{~a}{" vers)
              (for/fold ([first? #t]) ([auth (in-list auths)])
                (unless first? (printf "\\SAuthorSep{}"))
                (do-render-paragraph auth d ri #t #f)
                #f)
              (printf "}\n"))))
        (render-part d ri)
        (when whole-doc?
          (printf "\n\n\\postDoc\n\\end{document}\n"))))

    (define/override (render-part-content d ri)
      (let ([number (collected-info-number (part-collected-info d ri))]
            [completely-hidden?
             (and (part-style? d 'hidden)
                  (equal? "" (content->string (part-title-content d))))])
        (when (and (part-title-content d) 
                   (or (pair? number)
                       (let ([d (render-part-depth)])
                         (and d (positive? d)))))
          (when (eq? (style-name (part-style d)) 'index)
            (printf "\\twocolumn\n\\parskip=0pt\n\\addcontentsline{toc}{section}{Index}\n"))
          (let ([pres (extract-pretitle d)])
            (for ([pre (in-list pres)])
              (printf "\n\n")
              (do-render-paragraph pre d ri #t #f)))
          (cond
           [completely-hidden?
            (printf "\n\n\\notitlesection")]
           [else
            (let ([no-number? (and (pair? number) 
                                   (or (not (car number))
                                       ((length number) . > . 3)))])
              (printf "\n\n\\~a~a~a"
                      (case (+ (length number) (or (render-part-depth) 0))
                        [(0 1) "sectionNewpage\n\n\\section"]
                        [(2) "subsection"]
                        [(3) "subsubsection"]
                        [else "subsubsection"])
                      (if (and (part-style? d 'hidden) (not no-number?))
                          "hidden" "")
                      (if no-number? "*" ""))
              (when (not (or (part-style? d 'hidden) no-number?))
                (printf "[")
                (parameterize ([disable-images #t]
                               [escape-brackets #t])
                  (render-content (part-title-content d) d ri))
                (printf "]")))
            (printf "{")
            (render-content (part-title-content d) d ri)
            (printf "}")
            (when (eq? (style-name (part-style d)) 'index) (printf "\n\n"))]))
        (for ([t (part-tags d)])
          (printf "\\label{t:~a}~a" (t-encode (add-current-tag-prefix (tag-key t ri)))
                  (if completely-hidden? "" "\n\n")))
        (render-flow (part-blocks d) d ri #f)
        (for ([sec (part-parts d)]) (render-part sec ri))
        (when (eq? (style-name (part-style d)) 'index) (printf "\\onecolumn\n\n"))
        null))

    (define/override (render-paragraph p part ri)
      (do-render-paragraph p part ri #f #f))

    (define/private (do-render-paragraph p part ri show-pre? as-box-mode)
      (let* ([sn (style-name (paragraph-style p))]
             [style (cond
                     [as-box-mode
                      (or
                       (ormap (lambda (a)
                                (and (box-mode? a)
                                     ((box-mode-selector as-box-mode) a)))
                              (style-properties
                               (paragraph-style p)))
                       "hbox")]
                     [(eq? sn 'author) "SAuthor"]
                     [(eq? sn 'pretitle) #f]
                     [(eq? sn 'wraps) #f]
                     [else sn])])
        (unless (and (not show-pre?)
                     (or (eq? sn 'author)
                         (eq? sn 'pretitle)))
          (let ([use-style? (string? style)])
            (when use-style?
              (printf "\\~a{" style))
            (if (toc-paragraph? p)
                (printf "\\newpage \\tableofcontents \\newpage")
                (if as-box-mode
                    (parameterize ([suppress-newline-content #t])
                      (super render-paragraph p part ri))
                    (super render-paragraph p part ri)))
            (when use-style? (printf "}")))))
      null)

    (define/private (no-noindent? p ri)
      (cond
       [(delayed-block? p)
        (no-noindent? (delayed-block-blocks p ri) ri)]
       [(traverse-block? p)
        (no-noindent? (traverse-block-block p ri) ri)]
       [else
        (or
         (memq 'never-indents 
               (style-properties 
                (cond
                 [(paragraph? p) (paragraph-style p)]
                 [(compound-paragraph? p) (compound-paragraph-style p)]
                 [(nested-flow? p) (nested-flow-style p)]
                 [(table? p) (table-style p)]
                 [(itemization? p) (itemization-style p)]
                 [else plain])))
         (and (nested-flow? p)
              (pair? (nested-flow-blocks p))
              (no-noindent? (car (nested-flow-blocks p)) ri))
         (and (compound-paragraph? p)
              (pair? (compound-paragraph-blocks p))
              (no-noindent? (car (compound-paragraph-blocks p)) ri)))]))

    (define/override (render-intrapara-block p part ri first? last? starting-item?)
      (unless first?
        (printf "\n\n")
        (unless (no-noindent? p ri)
          (printf "\\noindent ")))
      (super render-intrapara-block p part ri first? last? starting-item?))

    (define/override (render-content e part ri)
      (let ([part-label? (and (link-element? e)
                              (pair? (link-element-tag e))
                              (eq? 'part (car (link-element-tag e)))
                              (empty-content? (element-content e)))])
        (parameterize ([done-link-page-numbers (or (done-link-page-numbers)
                                                   (link-element? e))])
          (when (target-element? e)
            (printf "\\label{t:~a}"
                    (t-encode (add-current-tag-prefix (tag-key (target-element-tag e) ri)))))
          (when part-label?
            (let* ([dest (resolve-get part ri (link-element-tag e))]
                   [number (and dest (vector-ref dest 2))])
              (printf "\\~aRef~a{"
                      (case (and dest (length number))
                        [(0) "Book"]
                        [(1) "Chap"]
                        [else "Sec"])
                      (if (let ([s (element-style e)])
                            (and (style? s) (memq 'uppercase (style-properties s))))
                          "UC"
                          ""))
              (render-content
               (if dest
                   (if (list? number)
                       (format-number number null)
                       (begin (eprintf "Internal tag error: ~s -> ~s\n"
                                       (link-element-tag e)
                                       dest)
                              '("!!!")))
                   (list "???"))
               part ri)
              (printf "}{")))
          (let* ([es (cond
                      [(element? e) (element-style e)]
                      [(multiarg-element? e) (multiarg-element-style e)]
                      [else #f])]
                 [style-name (if (style? es)
                                 (style-name es)
                                 es)]
                 [style (and (style? es) es)]
                 [check-render
                  (lambda ()
                    (when (render-element? e)
                      ((render-element-render e) this part ri)))]
                 [core-render (lambda (e tt?)
                                (cond
                                 [(and (image-element? e)
                                       (not (disable-images)))
                                  (check-render)
                                  (let ([fn (install-file
                                             (select-suffix 
                                              (main-collects-relative->path
                                               (image-element-path e))
                                              (image-element-suffixes e) 
                                              '(".pdf" ".ps" ".png")))])
                                    (printf "\\includegraphics[scale=~a]{~a}"
                                            (image-element-scale e) fn))]
                                 [(and (convertible? e)
                                       (not (disable-images))
                                       (let ([ftag (lambda (v suffix) (and v (list v suffix)))]
                                             [xlist (lambda (v) (and v (list v #f #f #f #f)))])
                                         (or (ftag (convert e 'pdf-bytes+bounds) ".pdf")
                                             (ftag (xlist (convert e 'pdf-bytes)) ".pdf")
                                             (ftag (xlist (convert e 'eps-bytes)) ".ps")
                                             (ftag (xlist (convert e 'png-bytes)) ".png"))))
                                  => (lambda (bstr+info+suffix)
                                       (check-render)
                                       (let* ([bstr (list-ref (list-ref bstr+info+suffix 0) 0)]
                                              [suffix (list-ref bstr+info+suffix 1)]
                                              [width (list-ref (list-ref bstr+info+suffix 0) 1)]
                                              [height (list-ref (list-ref bstr+info+suffix 0) 2)]
                                              [descent (and height
                                                            (+ (list-ref (list-ref bstr+info+suffix 0) 3)
                                                               (- (ceiling height) height)))]
                                              [fn (install-file (format "pict~a" suffix) bstr)])
                                         (if descent
                                             (printf "\\raisebox{-~apx}{\\makebox[~apx][l]{\\includegraphics{~a}}}" 
                                                     descent
                                                     width 
                                                     fn)
                                             (printf "\\includegraphics{~a}" fn))))]
                                 [else
                                  (parameterize ([rendering-tt (or tt? (rendering-tt))])
                                    (super render-content e part ri))]))]
                 [wrap (lambda (e s tt?)
                         (printf "\\~a{" s)
                         (core-render e tt?)
                         (printf "}"))])
            (define (finish tt?)
              (cond
               [(symbol? style-name)
                (case style-name
                  [(italic) (wrap e "textit" tt?)]
                  [(bold) (wrap e "textbf" tt?)]
                  [(tt) (wrap e "Scribtexttt" #t)]
                  [(url) (wrap e "nolinkurl" 'exact)]
                  [(no-break) (wrap e "mbox" tt?)]
                  [(sf) (wrap e "textsf" #f)]
                  [(subscript) (wrap e "textsub" #f)]
                  [(superscript) (wrap e "textsuper" #f)]
                  [(smaller) (wrap e "Smaller" #f)]
                  [(larger) (wrap e "Larger" #f)]
                  [(hspace)
                   (check-render)
                   (let ([s (content->string e)])
                     (case (string-length s)
                       [(0) (void)]
                       [else
                        (printf "\\mbox{\\hphantom{\\Scribtexttt{~a}}}"
                                (regexp-replace* #rx"." s "x"))]))]
                  [(newline) 
                   (check-render)
                   (unless (suppress-newline-content)
                     (printf "\\\\"))]
                  [else (error 'latex-render
                               "unrecognzied style symbol: ~s" style)])]
               [(string? style-name)
                (let* ([v (if style (style-properties style) null)]
                       [tt? (cond
                             [(memq 'tt-chars v) #t]
                             [(memq 'exact-chars v) 'exact]
                             [else tt?])])
                  (cond
                   [(multiarg-element? e)
                    (check-render)
                    (printf "\\~a" style-name)
                    (if (null? (multiarg-element-contents e))
                        (printf "{}")
                        (for ([i (in-list (multiarg-element-contents e))])
                          (printf "{")
                          (parameterize ([rendering-tt (or tt? (rendering-tt))])
                            (render-content i part ri))
                          (printf "}")))]
                   [else
                    (wrap e style-name tt?)]))]
               [else
                (core-render e tt?)]))
            (let loop ([l (if style (style-properties style) null)] [tt? #f])
              (if (null? l)
                  (finish tt?)
                  (let ([v (car l)])
                    (cond
                     [(target-url? v)
                      (printf "\\href{~a}{" (regexp-replace* #rx"%" (target-url-addr v) "\\\\%"))
                      (loop (cdr l) #t)
                      (printf "}")]
                     [(color-property? v)
                      (printf "\\intext~acolor{~a}{"
                              (if (string? (color-property-color v)) "" "rgb")
                              (color->string (color-property-color v)))
                      (loop (cdr l) tt?)
                      (printf "}")]
                     [(background-color-property? v)
                      (printf "\\in~acolorbox{~a}{"
                              (if (string? (background-color-property-color v)) "" "rgb")
                              (color->string (background-color-property-color v)))
                      (loop (cdr l) tt?)
                      (printf "}")]
                     [else (loop (cdr l) tt?)]))))))
        (when part-label?
          (printf "}"))
        (when (and (link-element? e)
                   (show-link-page-numbers)
                   (not (done-link-page-numbers)))
          (printf ", \\pageref{t:~a}"
                  (t-encode 
                   (let ([v (resolve-get part ri (link-element-tag e))])
                     (and v (vector-ref v 1))))))
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

    (define/override (render-flow p part ri starting-item? [wrap-each? #f])
      (if (null? p)
          null
          (begin
            (when wrap-each? (printf "{"))
            (render-block (car p) part ri starting-item?)
            (when wrap-each? (printf "}"))
            (for ([b (in-list (cdr p))])
              (if wrap-each?
                  (printf "%\n{")
                  (printf "\n\n"))
              (render-block b part ri #f)
              (when wrap-each? (printf "}")))
            null)))

    (define/override (render-table t part ri starting-item?)
      (render-table* t part ri starting-item? "[t]"))

    (define/private (render-table* t part ri starting-item? alignment)
      (let* ([s-name (style-name (table-style t))]
             [boxed? (eq? 'boxed s-name)]
             [index? (eq? 'index s-name)]
             [tableform
              (cond [index? "list"]
                    [(eq? 'block s-name) "tabular"]
                    [(not (current-table-mode)) "bigtabular"]
                    [else "tabular"])]
             [opt (cond [(equal? tableform "bigtabular") ""]
                        [(equal? tableform "tabular") alignment]
                        [else ""])]
             [blockss (if index? (cddr (table-blockss t)) (table-blockss t))]
             [cell-styless (extract-table-cell-styles t)]
             [twidth (if (null? (table-blockss t))
                         1
                         (length (car (table-blockss t))))]
             [single-column? (and (= 1 twidth)
                                  (or (not s-name) (string? s-name))
                                  (not (ormap (lambda (cell-styles)
                                                (ormap (lambda (s) 
                                                         (or (string? (style-name s))
                                                             (let ([l (style-properties s)])
                                                               (or (memq 'right l)
                                                                   (memq 'center l)))))
                                                       cell-styles))
                                              cell-styless))
                                  (not (current-table-mode)))]
             [inline?
              (and (not single-column?)
                   (not boxed?) 
                   (not index?)
                   (ormap (lambda (rs) 
                            (ormap (lambda (cs) (style-name cs)) rs))
                          cell-styless)
                   (= 1 twidth)
                   (let ([m (current-table-mode)])
                     (and m
                          (equal? "bigtabular" (car m))
                          (= 1 (length (car (table-blockss (cadr m))))))))])
        (if single-column?
            (begin
              (when (string? s-name)
                (printf "\\begin{~a}" s-name))
              (do-render-nested-flow 
               (make-nested-flow (make-style "SingleColumn" null) (map car (table-blockss t)))
               part 
               ri
               #t
               #f)
              (when (string? s-name)
                (printf "\\end{~a}" s-name)))
            (unless (or (null? blockss) (null? (car blockss)))
              (parameterize ([current-table-mode
                              (if inline? (current-table-mode) (list tableform t))]
                             [show-link-page-numbers
                              (or index? (show-link-page-numbers))])
                (cond
                 [index? (printf "\\begin{list}{}{\\parsep=0pt \\itemsep=1pt \\leftmargin=2ex \\itemindent=-2ex}\n")]
                 [inline? (void)]
                 [single-column? (printf "\\begin{tabbing}\n")]
                 [else
                  (printf "~a~a\\begin{~a}~a{@{~a}~a}\n~a"
                          (if (and starting-item? (equal? tableform "bigtabular"))
                              "\\bigtableinlinecorrect"
                              "")
                          (if (string? s-name)
                              (format "\\begin{~a}" s-name)
                              "")
                          tableform
                          opt
                          (if (equal? tableform "bigtabular")
                              "\\bigtableleftpad"
                              "")
                          (string-append*
                           (let ([l
                                  (map (lambda (i cell-style)
                                         (format "~a@{}"
                                                 (cond
                                                  [(memq 'center (style-properties cell-style)) "c"]
                                                  [(memq 'right (style-properties cell-style)) "r"]
                                                  [else "l"])))
                                       (car blockss)
                                       (car cell-styless))])
                             (if boxed? (cons "@{\\SBoxedLeft}" l) l)))
                          "")])
                (let loop ([blockss blockss]
                           [cell-styless cell-styless])
                  (let ([flows (car blockss)]
                        [cell-styles (car cell-styless)])
                    (let loop ([flows flows]
                               [cell-styles cell-styles])
                      (unless (null? flows)
                        (when index? (printf "\n\\item "))
                        (unless (eq? 'cont (car flows))
                          (let ([cnt (let loop ([flows (cdr flows)][n 1])
                                       (cond [(null? flows) n]
                                             [(eq? (car flows) 'cont)
                                              (loop (cdr flows) (add1 n))]
                                             [else n]))])
                            (unless (= cnt 1) (printf "\\multicolumn{~a}{l}{" cnt))
                            (render-table-cell (car flows) part ri (/ twidth cnt) (car cell-styles) (not index?))
                            (unless (= cnt 1) (printf "}"))
                            (unless (null? (list-tail flows cnt)) (printf " &\n"))))
                        (unless (null? (cdr flows)) (loop (cdr flows)
                                                          (cdr cell-styles)))))
                    (unless (or index? (null? (cdr blockss)))
                      (printf " \\\\\n"))
                    (unless (null? (cdr blockss))
                      (loop (cdr blockss) (cdr cell-styless)))))
                (unless inline?
                  (printf "\\end{~a}~a"
                          tableform
                          (if (string? s-name)
                              (format "\\end{~a}" s-name)
                              "")))))))
      null)

    (define/private (render-table-cell p part ri twidth vstyle can-box?)
      (let* ([top? (or (memq 'top (style-properties vstyle))
                       (memq 'baseline (style-properties vstyle)))]
             [bottom? (and (not top?)
                           (memq 'bottom (style-properties vstyle)))]
             [center? (and (not bottom?)
                           (not top?))]
             [as-box? (and can-box? (boxable? p))])
        (when (string? (style-name vstyle))
          (printf "\\~a{" (style-name vstyle)))
        (let ([minipage? (and can-box? (not as-box?))])
          (when minipage?
            (printf "\\begin{minipage}~a{~a\\linewidth}\n"
                    (cond
                     [top? "[t]"]
                     [center? "[c]"]
                     [else ""])
                    (/ 1.0 twidth)))
          (cond
           [(table? p)
            (render-table* p part ri #f (cond
                                         [top? "[t]"]
                                         [center? "[c]"]
                                         [else "[b]"]))]
           [as-box?
            (render-boxable-block p part ri (cond
                                             [top? 't]
                                             [center? 'c]
                                             [else 'b]))]
           [else
            (render-block p part ri #f)])
          (when minipage?
            (printf " \\end{minipage}\n")))
        (when (string? (style-name vstyle))
          (printf "}"))
        null))

    (define/private (boxable? p)
      (or (and (table? p)
               (for* ([l (in-list (table-blockss p))]
                      [p (in-list l)])
                 (boxable? p)))
          (and (nested-flow? p)
               (or (and (= 1 (length (nested-flow-blocks p)))
                        (memq (style-name (nested-flow-style p))
                              '(code-inset vertical-inset)))
                   (and
                    (ormap box-mode? (style-properties (nested-flow-style p)))
                    (andmap (lambda (p) (boxable? p)) (nested-flow-blocks p)))))
          (and (paragraph? p)
               (or (not (style-name (paragraph-style p)))
                   (ormap box-mode? (style-properties (paragraph-style p)))))))

    (define/private (render-boxable-block p part ri mode)
      (cond
       [(table? p)
        (render-table* p part ri #f (format "[~a]" mode))]
       [(nested-flow? p)
        (do-render-nested-flow p part ri #f mode)]
       [(paragraph? p)
        (do-render-paragraph p part ri #f mode)]))

    (define/private (box-mode-selector as-box-mode)
      (case as-box-mode
        [(t) box-mode-top-name]
        [(c) box-mode-center-name]
        [(b) box-mode-bottom-name]))

    (define/override (render-itemization t part ri)
      (let* ([style-str (let ([s (style-name (itemization-style t))])
                          (if (eq? s 'compact)
                              "compact"
                              s))]
             [mode (or (and (string? style-str)
                            style-str)
                       (if (eq? 'ordered style-str)
                           "enumerate"
                           "itemize"))])
        (printf "\\begin{~a}\\atItemizeStart" mode)
        (for ([flow (in-list (itemization-blockss t))])
          (printf "\n\n\\~a" (if (string? style-str)
                                  (format "~aItem{" style-str)
                                  "item "))
          (render-flow flow part ri #t)
          (when (string? style-str)
            (printf "}")))
        (printf "\\end{~a}" mode)
        null))

    (define/private (do-render-nested-flow t part ri single-column? as-box-mode)
      (let* ([props (style-properties (nested-flow-style t))]
             [kind (or (and as-box-mode
                            (or
                             (ormap (lambda (a)
                                      (and (box-mode? a)
                                           ((box-mode-selector as-box-mode) a)))
                                    props)
                             (case (style-name (nested-flow-style t))
                               [(code-inset) "SCodeInsetBox"]
                               [(vertical-inset) "SVInsetBox"]
                               [else (error "unexpected style for box mode")])))
                       (let ([s (style-name (nested-flow-style t))])
                         (or (and (string? s) s)
                             (and (eq? s 'inset) "quote")
                             (and (eq? s 'code-inset) "SCodeFlow")
                             (and (eq? s 'vertical-inset) "SVInsetFlow")))
                       "Subflow")]
             [multicommand? (memq 'multicommand props)]
             [command? (or (and as-box-mode (not multicommand?))
                           (memq 'command props))])
        (cond
         [command? (printf "\\~a{" kind)]
         [multicommand? (printf "\\~a" kind)]
         [else (printf "\\begin{~a}" kind)])
        (parameterize ([current-table-mode (if (or single-column?
                                                   (not (current-table-mode)))
                                               (current-table-mode)
                                               (list "nested-flow" t))])
          (if as-box-mode
              (for-each (lambda (p) 
                          (when multicommand? (printf "{"))
                          (render-boxable-block p part ri as-box-mode)
                          (when multicommand? (printf "}")))
                        (nested-flow-blocks t))
              (render-flow (nested-flow-blocks t) part ri #f multicommand?)))
        (cond
         [command? (printf "}")]
         [multicommand? (void)]
         [else (printf "\\end{~a}" kind)])
        null))

    (define/override (render-nested-flow t part ri starting-item?)
      (do-render-nested-flow t part ri #f #f))

    (define/override (render-compound-paragraph t part ri starting-item?)
      (let ([kind (style-name (compound-paragraph-style t))]
            [command? (memq 'command (style-properties (compound-paragraph-style t)))])
        (when kind
          (if command?
              (printf "\\~a{" kind)
              (printf "\\begin{~a}" kind)))
        (super render-compound-paragraph t part ri starting-item?)
        (when kind
          (if command?
              (printf "}")
              (printf "\\end{~a}" kind)))
        null))

    (define/override (render-other i part ri)
      (cond
        [(string? i) (display-protected i)]
        [(symbol? i)
         (display (case i
                    [(nbsp) "~"]
                    [(mdash) "{---}"]
                    [(ndash) "{--}"]
                    [(ldquo) "{``}"]
                    [(rdquo) "{''}"]
                    [(rsquo) "{'}"]
                    [(prime) "$'$"]
                    [(rarr) "$\\rightarrow$"]
                    [(larr) "$\\leftarrow$"]
                    [(alpha) "$\\alpha$"]
                    [(infin) "$\\infty$"]
                    [(lang) "$\\langle$"]
                    [(rang) "$\\rangle$"]
                    [else (error 'render "unknown symbol element: ~e" i)]))]
        [else (display-protected (format "~s" i))])
      null)

    (define/override (string-to-implicit-styles e)
      (for/fold ([ses null]) ([ch (in-string e)])
        (case ch
          [(#\☠) (cons skull-style ses)]
          [else ses])))

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
                     [(#\>) (if (rendering-tt) "{\\Stttextmore}" "$>$")]
                     [(#\<) (if (rendering-tt) "{\\Stttextless}" "$<$")]
                     [(#\|) (if (rendering-tt) "{\\Stttextbar}" "$|$")]
                     [(#\-) "{-}"] ;; avoid en- or em-dash
                     [(#\`) "{`}"] ;; avoid double-quotes
                     [(#\') "{'}"] ;; avoid double-quotes
                     [(#\? #\! #\. #\:)
                      (if (rendering-tt) (format "{\\hbox{\\texttt{~a}}}" c) c)]
                     [(#\~) "$\\sim$"]
                     [(#\{ #\}) (if (rendering-tt)
                                    (format "{\\char`\\~a}" c)
                                    (format "\\~a" c))]
                     [(#\[ #\]) (if (escape-brackets)
                                    (if (eq? c #\[)
                                        "{\\SOpenSq}"
                                        "{\\SCloseSq}")
                                    c)]
                     [(#\# #\% #\& #\$) (format "\\~a" c)]
                     [(#\uA0) "~"] ; non-breaking space
                     [(#\uAD) "\\-"] ; soft hyphen; unfortunately, also disables auto-hyphen
                     [(#\uDF) "{\\ss}"]
                     [else
                      (if ((char->integer c) . > . 127)
                          (case c
                            [(#\u2011) "\\mbox{-}"] ; non-breaking hyphen
                            [(#\uB0) "$^{\\circ}$"] ; degree
                            [(#\uB2) "$^2$"]
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
                            [(#\‘) "{`}"]
                            [(#\’) "{'}"]
                            [(#\“) "{``}"]
                            [(#\”) "{''}"]
                            [(#\u2013) "{--}"]
                            [(#\u2014) "{---}"]
                            [(#\∞) "$\\infty$"]
                            [(#\⇓) "$\\Downarrow$"]
                            [(#\↖) "$\\nwarrow$"]
                            [(#\↓) "$\\downarrow$"]
                            [(#\⇒) "$\\Rightarrow$"]
                            [(#\→) "$\\rightarrow$"]
                            [(#\↘) "$\\searrow$"]
                            [(#\↙) "$\\swarrow$"]
                            [(#\←) "$\\leftarrow$"]
                            [(#\↑) "$\\uparrow$"]
                            [(#\⇐) "$\\Leftarrow$"]
                            [(#\−) "$\\longrightarrow$"]
                            [(#\⇑) "$\\Uparrow$"]
                            [(#\⇔) "$\\Leftrightarrow$"]
                            [(#\↕) "$\\updownarrow$"]
                            [(#\↔) "$\\leftrightarrow$"]
                            [(#\↗) "$\\nearrow$"]
                            [(#\⇕) "$\\Updownarrow$"]
                            [(#\א) "$\\aleph$"]
                            [(#\′) "$\\prime$"]
                            [(#\∅) "$\\emptyset$"]
                            [(#\∇) "$\\nabla$"]
                            [(#\♦) "$\\diamondsuit$"]
                            [(#\♠) "$\\spadesuit$"]
                            [(#\♣) "$\\clubsuit$"]
                            [(#\♥) "$\\heartsuit$"]
                            [(#\♯) "$\\sharp$"]
                            [(#\♭) "$\\flat$"]
                            [(#\♮) "$\\natural$"]
                            [(#\√) "$\\surd$"]
                            [(#\¬) "$\\neg$"]
                            [(#\△) "$\\triangle$"]
                            [(#\∀) "$\\forall$"]
                            [(#\∃) "$\\exists$"]
                            [(#\∘) "$\\circ$"]
                            [(#\θ) "$\\theta$"]
                            [(#\τ) "$\\tau$"]
                            [(#\υ) "$\\upsilon$"]
                            [(#\φ) "$\\phi$"]
                            [(#\δ) "$\\delta$"]
                            [(#\ρ) "$\\rho$"]
                            [(#\ε) "$\\epsilon$"]
                            [(#\χ) "$\\chi$"]
                            [(#\ψ) "$\\psi$"]
                            [(#\ζ) "$\\zeta$"]
                            [(#\ν) "$\\nu$"]
                            [(#\ω) "$\\omega$"]
                            [(#\η) "$\\eta$"]
                            [(#\ι) "$\\iota$"]
                            [(#\ξ) "$\\xi$"]
                            [(#\Γ) "$\\Gamma$"]
                            [(#\Ψ) "$\\Psi$"]
                            [(#\∆) "$\\Delta$"]
                            [(#\Ξ) "$\\Xi$"]
                            [(#\Υ) "$\\Upsilon$"]
                            [(#\Ω) "$\\Omega$"]
                            [(#\Θ) "$\\Theta$"]
                            [(#\Π) "$\\Pi$"]
                            [(#\Φ) "$\\Phi$"]
                            [(#\±) "$\\pm$"]
                            [(#\∩) "$\\cap$"]
                            [(#\◇) "$\\diamond$"]
                            [(#\⊕) "$\\oplus$"]
                            [(#\∓) "$\\mp$"]
                            [(#\∪) "$\\cup$"]
                            [(#\△) "$\\bigtriangleup$"]
                            [(#\⊖) "$\\ominus$"]
                            [(#\×) "$\\times$"]
                            [(#\⊎) "$\\uplus$"]
                            [(#\▽) "$\\bigtriangledown$"]
                            [(#\⊗) "$\\otimes$"]
                            [(#\÷) "$\\div$"]
                            [(#\⊓) "$\\sqcap$"]
                            [(#\▹) "$\\triangleleft$"]
                            [(#\⊘) "$\\oslash$"]
                            [(#\∗) "$\\ast$"]
                            [(#\⊔) "$\\sqcup$"]
                            [(#\∨) "$\\vee$"]
                            [(#\∧) "$\\wedge$"]
                            [(#\◃) "$\\triangleright$"]
                            [(#\⊙) "$\\odot$"]
                            [(#\★) "$\\star$"]
                            [(#\†) "$\\dagger$"]
                            [(#\•) "$\\bullet$"]
                            [(#\‡) "$\\ddagger$"]
                            [(#\≀) "$\\wr$"]
                            [(#\⨿) "$\\amalg$"]
                            [(#\≤) "$\\leq$"]
                            [(#\≥) "$\\geq$"]
                            [(#\≡) "$\\equiv$"]
                            [(#\⊨) "$\\models$"]
                            [(#\≺) "$\\prec$"]
                            [(#\≻) "$\\succ$"]
                            [(#\∼) "$\\sim$"]
                            [(#\⊥) "$\\perp$"]
                            [(#\≼) "$\\preceq$"]
                            [(#\≽) "$\\succeq$"]
                            [(#\≃) "$\\simeq$"]
                            [(#\≪) "$\\ll$"]
                            [(#\≫) "$\\gg$"]
                            [(#\≍) "$\\asymp$"]
                            [(#\∥) "$\\parallel$"]
                            [(#\⊂) "$\\subset$"]
                            [(#\⊃) "$\\supset$"]
                            [(#\≈) "$\\approx$"]
                            [(#\⋈) "$\\bowtie$"]
                            [(#\⊆) "$\\subseteq$"]
                            [(#\⊇) "$\\supseteq$"]
                            [(#\≌) "$\\cong$"]
                            [(#\⊏) "$\\sqsubset$"]
                            [(#\⊐) "$\\sqsupset$"]
                            [(#\≠) "$\\neq$"]
                            [(#\⌣) "$\\smile$"]
                            [(#\⊑) "$\\sqsubseteq$"]
                            [(#\⊒) "$\\sqsupseteq$"]
                            [(#\≐) "$\\doteq$"]
                            [(#\⌢) "$\\frown$"]
                            [(#\∈) "$\\in$"]
                            [(#\∋) "$\\ni$"]
                            [(#\∝) "$\\propto$"]
                            [(#\⊢) "$\\vdash$"]
                            [(#\⊣) "$\\dashv$"]    
                            [(#\☠) "$\\skull$"]
                            [(#\☺) "$\\smiley$"]
                            [(#\☻) "$\\blacksmiley$"]
                            [(#\☹) "$\\frownie$"]
                            [(#\à) "\\`{a}"]
                            [(#\À) "\\`{A}"]
                            [(#\á) "\\'{a}"]
                            [(#\â) "\\^{a}"]
                            [(#\Â) "\\^{A}"]
                            [(#\Á) "\\'{A}"]
                            [(#\ç) "\\c{c}"]
                            [(#\Ç) "\\c{C}"]
                            [(#\è) "\\`{e}"]
                            [(#\È) "\\`{E}"]
                            [(#\é) "\\'{e}"]
                            [(#\É) "\\'{E}"]
                            [(#\ê) "\\^{e}"]
                            [(#\Ê) "\\^{E}"]
                            [(#\í) "\\'{i}"]
                            [(#\Í) "\\'{I}"]
                            [(#\î) "\\^{i}"]
                            [(#\Î) "\\^{I}"]
                            [(#\ô) "\\^{o}"]
                            [(#\Ô) "\\^{O}"]
                            [(#\û) "\\^{u}"]
                            [(#\Û) "\\^{U}"]
                            [(#\ä) "\\\"a"]
                            [(#\Ä) "\\\"A"]
                            [(#\ü) "\\\"u"]
                            [(#\Ü) "\\\"U"]
                            [(#\ö) "\\\"o"]
                            [(#\Ö) "\\\"O"]
                            [(#\ø) "{\\o}"]
                            [(#\Ø) "{\\O}"]
                            [(#\ł) "{\\l}"]
                            [(#\Ł) "{\\L}"]
                            [(#\š) "{\\v s}"]
                            [(#\uA7) "{\\S}"]
                            [(#\〚) "$[\\![$"]
                            [(#\〛) "$]\\!]$"]
                            [(#\↦) "$\\mapsto$"]
                            [(#\⊤) "$\\top$"]
                            [(#\¥) "{\\textyen}"]
                            [(#\™) "{\\texttrademark}"]
                            [(#\u2070) "$^0$"]
                            [(#\u00b9) "$^1$"]
                            [(#\u00b2) "$^2$"]
                            [(#\u00b3) "$^3$"]
                            [(#\u2074) "$^4$"]
                            [(#\u2075) "$^5$"]
                            [(#\u2076) "$^6$"]
                            [(#\u2077) "$^7$"]
                            [(#\u2078) "$^8$"]
                            [(#\u2079) "$^9$"]
                            [(#\u207a) "$^+$"]
                            [(#\u207b) "$^-$"]
                            [else c])
                          c)])))
                (loop (add1 i)))))))

    ;; ----------------------------------------

    (define/override (table-of-contents sec ri)
      ;; FIXME: isn't local to the section
      (make-toc-paragraph plain null))

    (define/override (local-table-of-contents part ri style)
      (make-paragraph plain null))

    ;; ----------------------------------------

    (super-new)))

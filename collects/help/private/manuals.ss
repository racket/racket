
(module manuals mzscheme
  (require (lib "list.ss")
           (lib "date.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "xml.ss" "xml")
           (lib "contract.ss")
           (lib "getinfo.ss" "setup")
           (lib "uri-codec.ss" "net")
	   (lib "dirs.ss" "setup")
	   "finddoc.ss"
           "colldocs.ss"
           "docpos.ss"
           "path.ss"
           "standard-urls.ss"
           "get-help-url.ss"
           "../servlets/private/util.ss"
           "../servlets/private/headelts.ss")

  ;; type sec = (make-sec name regexp (listof regexp))
  (define-struct sec (name reg seps))
  
  ;; sections : (listof sec)
  ;; determines the section breakdown for the manuals
  ;; elements in the outer list:
  ;;   string : name of section
  ;;   predicate : determines if a manual is in the section (based on its title)
  ;;   breaks -- where to insert newlines
  (define sections
    (list (make-sec "Getting started" 
                    #rx"(Tour)|(Teach Yourself)"
                    '())
          (make-sec "Languages"
                    #rx"Language|MrEd"
                    '(#rx"Beginning Student" #rx"ProfessorJ Beginner"))
          (make-sec "Tools" #rx"PLT DrScheme|PLT mzc|TeX2page|Web Server|PLoT" '())
          (make-sec "Libraries" #rx"SRFI|MzLib|Framework|PLT Miscellaneous|Teachpack|Swindle" '())
          (make-sec "Writing extensions" #rx"Tools|Inside|Foreign" '())
          (make-sec "Other" #rx"" '())))

  ; manual is doc collection subdirectory, e.g. "mred"
  (define (main-manual-page manual)
    (let* ([entry (assoc (string->path manual) known-docs)]
	   [name (or (and entry (cdr entry))
                     manual)]
           [doc-dir (find-doc-directory manual)])
      (if doc-dir
          (let ([href (get-help-url doc-dir)])
            `(A ((HREF ,href)) ,name))
          name)))

  ; string string string -> xexpr
  ; man is manual name
  ; ndx is index into the manual
  ; txt is the link text
  (define (manual-entry man ndx txt)
    (with-handlers ([exn:fail?
                     ;; warning: if the index file isn't present, this page
                     (lambda (x)
                       `(font ((color "red")) ,txt " [" ,(exn-message x) "]"))])
      `(A ((HREF ,(finddoc-page man ndx))) ,txt)))

  (define (basename path)
    (let-values ([(dir name dir?) (split-path path)]) name))

  (define (find-doc-names)
    (let* ([dirs        (find-doc-directories)]
           [installed   (map basename dirs)]
           [uninstalled (filter (lambda (x) (not (member (car x) installed)))
                                known-docs)])
      (append (map (lambda (short-name long-name)
                     (cons short-name (get-doc-name long-name)))
                   installed dirs)
              uninstalled)))

  ;; find-doc-directories : -> (listof path)
  ;; constructs a sorted list of directories where documentation may reside.
  (define (find-doc-directories)
    (let ([unsorted (append (find-info.ss-doc-directories)
                            (find-doc-directories-in-toplevel-docs))])
      (sort unsorted compare-docs)))

  (define (find-info.ss-doc-directories)
    (let ([dirs (find-relevant-directories '(html-docs) 'all-available)])
      (let loop ([dirs dirs])
        (cond
          [(null? dirs) null]
          [else (let* ([dir (car dirs)]
                       [info (get-info/full dir)])
                  (cond
                    [info
                     (let ([html-doc-paths (info 'html-docs (lambda () #f))])
                       (cond
                         [(and (list? html-doc-paths)
                               (andmap path-string? html-doc-paths))
                          (append (map (lambda (x) (build-path dir x)) html-doc-paths)
                                  (loop (cdr dirs)))]
                         [else
                          (loop (cdr dirs))]))]
                    [else (loop (cdr dirs))]))]))))

  (define (find-doc-directories-in-toplevel-docs)
    (apply append
           (map (lambda (docs-path)
                  (filter directory-exists?
                          (map (lambda (doc-path)
                                 (build-path docs-path doc-path))
                               (if (directory-exists? docs-path)
                                 (filter (lambda (x)
                                           (not (member (path->string x)
                                                        '(".svn" "CVS"))))
                                         (directory-list docs-path))
                                 '()))))
                (get-doc-search-dirs))))
  
  (define (find-manuals)
    (let* ([docs (sort (filter get-index-file (find-doc-directories))
                       compare-docs)]
           [names (map get-doc-name docs)]
           [names+paths (map cons names docs)])
      (let-values ([(collections-doc-files collection-names) (colldocs)])
        (apply
         string-append
         "<html>"
         (xexpr->string `(HEAD ,hd-css ,@hd-links (TITLE "PLT Manuals")))
         "<body>"

         (append

          (list "<H1>Installed Manuals</H1>")
          (if (repos-or-nightly-build?)
              (list
               "<b>Subversion:</b> <a mzscheme=\""
               (to-string/escape-quotes
                `((dynamic-require '(lib "refresh-manuals.ss" "help") 'refresh-manuals)))
               "\">"
               (string-constant plt:hd:refresh-all-manuals)
               "</a> &nbsp; &nbsp;"
               (format "<a href=\"~a\">flush index and keyword cache</a><br>" flush-manuals-url))
              '())
          (build-known-manuals names+paths)

          (list "<h3>Doc.txt</h3><ul>")
          (map
           (lambda (collection-doc-file name)
             (let ([path (build-path (car collection-doc-file)
                                     (cadr collection-doc-file))])
               (format "<LI> ~a"
                       (if (file-exists? path)
                         (format "<A HREF=\"/servlets/doc-anchor.ss?file=~a&name=~a&caption=Documentation for the ~a collection\">~a collection</A>"
                                 ;; escape colons and other junk
                                 (uri-encode (path->string path))
                                 (uri-encode name)
                                 (uri-encode name)
                                 name)
                         (format "<FONT COLOR=\"RED\">~a collection: specified doc.txt file (~a) not found</FONT>"
                                 name path)))))
           collections-doc-files
           collection-names)
          (list "</UL>")

          (let ([uninstalled (get-uninstalled docs)])
            (if (null? uninstalled)
              `("")
              `("<H3>Uninstalled Manuals</H3>"
                "<UL>"
                ,@(map
                   (lambda (doc-pair)
                     (let* ([manual (car doc-pair)]
                            [name (cdr doc-pair)]
                            [manual-path (find-doc-directory manual)])
                       (string-append
                        "<LI> Download and install <A mzscheme=\""
                        (to-string/escape-quotes 
                         `((dynamic-require '(lib "refresh-manuals.ss" "help") 'refresh-manuals)
                           (list (cons (bytes->path ,(path->bytes manual))
                                       ,name))))
                        (format "\">~a</A>~a"
                                name
                                (if (and manual-path
                                         (or (file-exists? (build-path manual-path "hdindex"))
                                             (file-exists? (build-path manual-path "keywords"))))
                                  " (index installed)"
                                  "")))))
                   uninstalled)
                "</UL>")))
          (list "</body></html>"))))))
  
  ;; break-between : regexp
  ;;                (listof (union string (cons string string))) 
  ;;             -> (listof (union string (cons string string)))
  ;; adds the para-mark string into the list at the first place
  ;; that the regexp fails to match (not counting other para-marks
  ;; in the list)
  (define (break-between re l)
    (let ([para-mark "<p>"])
      (let loop ([l l])
        (cond
          [(null? l) null]
          [else 
           (let ([fst (car l)])
             (cond
               [(pair? fst)
                (let ([name (car fst)])
                  (if (regexp-match re name)
                      (cons para-mark l)
                      (cons fst (loop (cdr l)))))]
               [else (cons fst (loop (cdr l)))]))]))))

  ;; build-known-manuals : (listof (cons string[title] string[path])) -> (listof string)
  (define (build-known-manuals names+paths)
    (let loop ([sections sections]
               [manuals names+paths])
      (cond
        [(null? sections) null]
        [else 
         (let* ([section (car sections)]
                [in (filter (lambda (x) (regexp-match (sec-reg section) 
                                                      (car x)))
                            manuals)]
                [out (filter (lambda (x) (not (regexp-match (sec-reg section) 
                                                       (car x))))
                             manuals)])
           (cons (build-known-section section in)
                 (loop (cdr sections) out)))])))

  ;; build-known-section : sec (listof (cons string[title] string[path]))) -> string
  (define (build-known-section sec names+paths)
    (if (null? names+paths)
        ""
        (string-append
         "<h3>" (sec-name sec) "</h3>"
         "<ul>"
         (apply 
          string-append
          (map (lambda (x) 
                 (if (string? x)
                     x
                     (mk-link (cdr x) (car x))))
               (let loop ([breaks (sec-seps sec)]
                          [names+paths names+paths])
                 (cond
                   [(null? breaks) names+paths]
                   [else
                    (let ([break (car breaks)])
                      (loop (cdr breaks)
                            (break-between (car breaks) names+paths)))]))))
         "</ul>")))

  ;; mk-link : string string -> string
  (define (mk-link doc-path name)
    (let* ([manual-name (basename doc-path)]
           [index-file (get-index-file doc-path)])
      (format "<LI> <A HREF=\"~a\">~a</A>~a"
              (get-help-url (build-path doc-path index-file))
              name
              (if (and (repos-or-nightly-build?)
                       (file-exists? (build-path doc-path index-file)))
                  (string-append 
                   "<BR>&nbsp;&nbsp;"
                   "<FONT SIZE=\"-1\">"
                   (if (is-known-doc? doc-path)
                       (string-append
                        (format
                         "[<A mzscheme=\"~a\">~a</a>]"
                         (to-string/escape-quotes 
                          `((dynamic-require '(lib "refresh-manuals.ss" "help") 'refresh-manuals)
                            (list (cons (bytes->path ,(path->bytes manual-name))
                                        ,name))))
                         (string-constant plt:hd:refresh))
                        "&nbsp;")
                       "")
                   (format (string-constant plt:hd:manual-installed-date)
                           (date->string
                            (seconds->date
                             (file-or-directory-modify-seconds
                              (build-path doc-path index-file)))))
                   "</FONT>")
                  ""))))

  (define (to-string/escape-quotes exp)
    (regexp-replace* #rx"\"" (format "~s" exp) "|"))

  ;; get-doc-name : path -> string
  (define cached-doc-names (make-hash-table 'equal))
  (define (get-doc-name doc-dir)
    (hash-table-get cached-doc-names doc-dir
      (lambda ()
        (let ([res (compute-doc-name doc-dir)])
          (hash-table-put! cached-doc-names doc-dir res)
          res))))

  ;; compute-doc-name : path -> string[title of manual]
  ;; gets the title either from the known docs list, by parsing the
  ;; html, or if both those fail, by using the name of the directory
  ;; Special-cases the help collection. It's not a known doc directory
  ;; per se, so it won't appear in known-docs, but its name is always
  ;; the same.
  (define (compute-doc-name doc-dir)
    (let ([doc-short-dir-name (basename doc-dir)])
      (cond
       [(equal? (string->path "help") doc-short-dir-name) "PLT Help Desk"]
       [(get-known-doc-name doc-dir) => values]
       [else (let* ([main-file (get-index-file doc-dir)]
                    [m (and main-file
                            (call-with-input-file (build-path doc-dir main-file)
                              (lambda (inp) (regexp-match re:title inp))))])
               (if m
                 (bytes->string/utf-8 (cadr m))
                 (path->string doc-short-dir-name)))])))
  (define re:title
    #rx"<[tT][iI][tT][lL][eE]>[ \t\r\n]*(.*?)[ \t\r\n]*</[tT][iI][tT][lL][eE]>")

  ;; is-known-doc? : string[path] -> boolean
  (define (is-known-doc? doc-path)
    (and (assoc (basename doc-path) known-docs) #t))

  ;; get-known-doc-name : string[full-path] -> (union string #f)
  (define (get-known-doc-name doc-path)
    (cond [(assoc (basename doc-path) known-docs) => cdr] [else #f]))

  ;; get-uninstalled : (listof path) -> (listof (cons path string[docs-name]))
  (define (get-uninstalled docs)
    (let ([ht (make-hash-table 'equal)])
      (for-each (lambda (known-doc)
                  (hash-table-put! ht 
                                   (car known-doc)
                                   (cdr known-doc)))
                known-docs)
      (for-each (lambda (doc) (hash-table-remove! ht (basename doc))) docs)
      (sort (hash-table-map ht cons)
            (λ (a b) (compare-docs (car a) (car b))))))

  (define (compare-docs a b)
    (let ([ap (standard-html-doc-position (basename a))]
          [bp (standard-html-doc-position (basename b))])
      (cond [(= ap bp) (string<? (path->string a) (path->string b))]
            [else (< ap bp)])))

  ;; get-manual-index : string -> html
  (define (get-manual-index manual-dirname) (get-help-url (build-path (find-doc-dir) manual-dirname)))
  
  ;; get-index-file : path -> (union #f path)
  ;; returns the name of the main file, if one can be found
  (define (get-index-file doc-dir)
    (cond
      [(file-exists? (build-path doc-dir "index.htm"))
       (build-path "index.htm")]
      [(file-exists? (build-path doc-dir "index.html"))
       (build-path "index.html")]
      [(tex2page-detected doc-dir)
       =>
       (lambda (x) x)]
      [else #f]))
  
  ;; tex2page-detected : string -> (union #f string)
  (define (tex2page-detected dir)
    (let loop ([contents (directory-list dir)])
      (cond
        [(null? contents) #f]
        [else (let* ([file (car contents)]
                     [m (regexp-match #rx#"(.*)-Z-H-1.html" (path->bytes file))])
                (or (and m
                         (file-exists? (build-path dir file))
                         (let ([index-file (bytes->path (bytes-append (cadr m) #".html"))])
                           (if (file-exists? (build-path dir index-file))
                               index-file
                               #f)))
                    (loop (cdr contents))))])))
  
  
  (provide main-manual-page)
  (provide finddoc
	   finddoc-page-anchor)
  
  (provide/contract [manual-entry (string? string? xexpr? . -> . xexpr?)]
                    [finddoc-page (string? string? . -> . string?)]
                    [get-doc-name (path? . -> . string?)]
                    [find-doc-directories (-> (listof path?))]
                    [find-doc-directory (path? . -> . (or/c false/c path?))]
                    [find-doc-names (-> (listof (cons/c path? string?)))]
                    [get-manual-index (-> string? string?)]
                    [get-index-file (path? . -> . (or/c false/c path?))])
  
  (provide find-manuals))

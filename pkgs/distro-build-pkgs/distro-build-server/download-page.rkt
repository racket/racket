#lang at-exp racket/base
(require racket/format
         racket/path
         racket/system
         racket/list
         net/url
         openssl/sha1
         xml)

(provide make-download-page
         get-installers-table
         (struct-out past-success))

(module+ main
  (require racket/cmdline)

  (define args null)
  (define (arg! kw val)
    (set! args (cons (cons kw val) args)))
  
  (define table-file
    (command-line
     #:once-each
     [("--at") url "URL for installaters reletaive to download page"
      (arg! '#:installers-url url)]
     [("--dest") file "Write to <dest>"
      (arg! '#:dest file)]
     [("--git") dir "Report information from git clone <dir>"
      (arg! '#:git-clone dir)]
     #:args
     (table-file)
     table-file))

  (let ([args (sort args keyword<? #:key car)])
    (keyword-apply make-download-page
                   (map car args)
                   (map cdr args)
                   (list table-file))))

(define (get-installers-table table-file)
  (define table (call-with-input-file table-file read))
  (unless (hash? table) 
    (raise-user-error
     'make-download-page
     (~a "given file does not contain a hash table\n"
         "  file: ~a")
     table-file))
  table)

(struct past-success (name relative-url file) #:prefab)

(define (make-download-page table-file
                            #:past-successes [past-successes (hash)]
                            #:dest [dest "index.html"]
                            #:installers-url [installers-url "./"]
                            #:log-dir [log-dir #f]
                            #:log-dir-url [log-dir-url #f]
                            #:docs-url [docs-url #f]
                            #:pdf-docs-url [pdf-docs-url #f]
                            #:title [title "Racket Downloads"]
                            #:current-rx [current-rx #f]
                            #:git-clone [git-clone #f]
                            #:help-table [site-help (hash)]
                            #:post-content [post-content null])

  (define base-table (get-installers-table table-file))

  (define table (for/fold ([table base-table]) ([(k v) (in-hash past-successes)])
                  (if (hash-ref table k #f)
                      table
                      (hash-set table k v))))

  (define (system*/string . args)
    (define s (open-output-string))
    (parameterize ([current-output-port s])
      (apply system* args))
    (get-output-string s))
  
  (define log-link
    (and log-dir-url
         `((div (a ([class "detail"] [href ,log-dir-url]) "Build Logs")))))

  (define sorted
    (sort (hash-keys table) string<?))
  (define sorted-and-split
    (map (lambda (s)
           (map (lambda (e)
                  (regexp-replace* #rx" *{[^}]*} *"
                                   e
                                   ""))
                (regexp-split #rx" *[|] *" s)))
         sorted))
  
  (define elems 
    (let loop ([l sorted-and-split]
               [keys sorted]
               [prev null]
               [started? #f])
      (define len (length prev))
      (define (add-sep l)
        (if (and started?
                 (null? prev))
            (cons '(#f) l)
            l))
      (cond
       [(null? l) '((#f) (#f nbsp))]
       [(not (equal? prev (take (car l) len)))
        ;; move out a layer:
        (loop l keys (drop-right prev 1) #t)]
       [(= (add1 len) (length (car l)))
        ;; a leaf entry:
        (add-sep
         (cons (cons (car keys)
                     (append (make-list len 'nbsp)
                             (list (list-ref (car l) len))))
               (loop (cdr l) (cdr keys) prev #t)))]
       [else
        ;; add a heder
        (define section (list-ref (car l) len))
        (add-sep
         (cons (cons #f
                     (append (make-list len 'nbsp) 
                             (list section)))
               (loop l keys (append prev (list section)) #t)))])))

  (define (get-site-help last-col)
    (let ([h (hash-ref site-help last-col #f)])
      (if h
          (let* ([id (~a "help" (gensym))]
                 [toggle (let ([elem (~a "document.getElementById" "('" id "')")])
                           (~a elem ".style.display = ((" elem ".style.display == 'inline') ? 'none' : 'inline');"
                               " return false;"))])
            `(" "
              (div ([class "helpbutton"])
                   (a ([href "#"]
                       [class "helpbuttonlabel"]
                       [onclick ,toggle]
                       [title "explain"])
                      nbsp "?" nbsp))
              (div ([class "hiddenhelp"]
                    [id ,id]
                    [onclick ,toggle]
                    [style "display: none"])
                   (div ([class "helpcontent"])
                        (div ([class "helptext"])
                             ,h)))))
          null)))

  (call-with-output-file* 
   dest
   #:exists 'truncate/replace
   (lambda (o)
     (parameterize ([empty-tag-shorthand html-empty-tags])
       (write-xexpr
        `(html
          (head (title ,title)
                (style @,~a|{ 
                             .detail { font-size: small; }
                             .checksum, .path { font-family: monospace; }
                             .group { background-color : #ccccff; padding-left: 0.5ex; }
                             .major { font-weight : bold; font-size : large; left-border: 1ex; }
                             .minor { font-weight : bold; }
                             .helpbutton {
                                 display: inline;
                                 font-family: sans-serif;
                                 font-size : x-small;
                                 background-color: #ffffee;
                                 border: 1px solid black;
                                 vertical-align: top;
                              }
                              .helpbuttonlabel{ vertical-align: top; }
                             .hiddenhelp {
                                 width: 0em;
                                 position: absolute;
                             }
                             .helpcontent {
                                 width: 20em;
                                 font-family: serif;
                                 font-size : small;
                                 font-weight : normal;
                                 background-color: #ffffee;
                                 padding: 10px;
                                 border: 1px solid black;
                              }
                            a { text-decoration: none; }
                           }|))
          (body
           (h2 ,title)
           (table
            ,@(for/list ([elem (in-list elems)])
                (define key (car elem))
                (define inst (and key (hash-ref table key)))
                (define mid-cols (if (null? (cdr elem))
                                     #f
                                     (drop-right (cdr elem) 1)))
                (define last-col (last elem))
                (define level-class
                  (case (length elem)
                    [(2) (~a "major" (if key "" " group"))]
                    [(3) "minor"]
                    [else "subminor"]))
                (define num-cols (if current-rx
                                     "7"
                                     "5"))
                (cond
                 [(not mid-cols)
                  `(tr (td ((colspan ,num-cols)) nbsp))]
                 [inst
                  `(tr (td
                        ,@(for/list ([col (in-list mid-cols)])
                            `(span nbsp nbsp nbsp))
                        ,(if (past-success? inst)
                             ;; Show missing installer
                             `(span ((class ,(string-append "no-installer " level-class)))
                                    ,last-col)
                             ;; Link to installer
                             `(a ((class ,(string-append "installer " level-class))
                                  (href ,(url->string
                                          (combine-url/relative
                                           (string->url installers-url)
                                           inst))))
                                 ,last-col))
                        ,@(get-site-help last-col))
                       (td nbsp)
                       (td ,(if (past-success? inst)
                                `(span ([class "detail"]) "")
                                `(span ([class "detail"])
                                       ,(~r (/ (file-size (build-path (path-only table-file)
                                                                      inst))
                                               (* 1024 1024))
                                            #:precision 1)
                                       " MB")))
                       (td nbsp)
                       (td ,(if (past-success? inst)
                                `(span ([class "detail"])
                                       ,@(if (and log-dir 
                                                  (file-exists? (build-path log-dir key)))
                                             `((a ([href ,(url->string
                                                           (combine-url/relative
                                                            (string->url log-dir-url)
                                                            key))])
                                                  "build failed")
                                               "; ")
                                             null)
                                       "last success: "
                                       (a ((href ,(~a (past-success-relative-url inst))))
                                          ,(past-success-name inst)))
                                `(span ([class "detail"])
                                       "SHA1: "
                                       (span ([class "checksum"])
                                             ,(call-with-input-file*
                                               (build-path (path-only table-file)
                                                           inst)
                                               sha1)))))
                       ,@(if current-rx
                             `((td nbsp)
                               (td (span ([class "detail"])
                                         ,(let ([inst-path (if (past-success? inst)
                                                               (past-success-file inst)
                                                               inst)])
                                            (if (regexp-match? current-rx inst-path)
                                                `(a ([href ,(url->string
                                                             (combine-url/relative
                                                              (string->url installers-url)
                                                              (bytes->string/utf-8
                                                               (regexp-replace current-rx
                                                                               (string->bytes/utf-8 inst-path)
                                                                               #"current"))))])
                                                    "as " ldquo "current" rdquo)
                                                'nbsp)))))
                             null))]
                 [else
                  `(tr (td ((class ,level-class)
                            (colspan ,num-cols))
                           ,@(for/list ([col (in-list mid-cols)])
                               `(span nbsp nbsp nbsp))
                           ,last-col
                           ,@(get-site-help last-col)))])))
           ,@(if docs-url
                 `((p (a ((href ,docs-url)) "Documentation")
                      ,@(if pdf-docs-url
                            `(nbsp
                              nbsp
                              (span ([class "detail"])
                                    (a ((href ,pdf-docs-url)) "[also available as PDF]")))
                            null)))
                 null)
           ,@(if git-clone
                 (let ([git (find-executable-path "git")])
                   (define origin (let ([s (system*/string git "remote" "show" "origin")])
                                    (define m (regexp-match #rx"(?m:Fetch URL: (.*)$)" s))
                                    (if m
                                        (cadr m)
                                        "???")))
                   (define stamp (system*/string git "log" "-1" "--format=%H"))
                   `((p
                      (div (span ([class "detail"]) "Repository: " (span ([class "path"]) ,origin)))
                      (div (span ([class "detail"]) "Commit: " (span ([class "checksum"]) ,stamp)))
                      ,@(or log-link null))))
                 null)
           ,@(if (and log-link (not git-clone))
                 `((p ,@log-link))
                 null)
           ,@post-content))
        o)
       (void)))))

#lang racket/base
(require racket/format
         racket/path
         racket/system
         net/url
         openssl/sha1
         xml)

(provide make-download-page)

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

(define (make-download-page table-file
                            #:dest [dest "index.html"]
                            #:installers-url [installers-url "./"]
                            #:docs-url [docs-url #f]
                            #:pdf-docs-url [pdf-docs-url #f]
                            #:title [title "Racket Downloads"]
                            #:git-clone [git-clone #f]
                            #:post-content [post-content null])

  (define table (call-with-input-file table-file read))

  (unless (hash? table) 
    (raise-user-error
     'make-download-page
     (~a "given file does not contain a hash table\n"
         "  file: ~a")
     table-file))

  (define (system*/string . args)
    (define s (open-output-string))
    (parameterize ([current-output-port s])
      (apply system* args))
    (get-output-string s))

  (call-with-output-file* 
   dest
   #:exists 'truncate/replace
   (lambda (o)
     (parameterize ([empty-tag-shorthand html-empty-tags])
       (write-xexpr
        `(html
          (head (title ,title)
                (style ,(~a " .detail { font-size: small; }"
                            " .checksum, .path { font-family: monospace; }"
                            " a { text-decoration: none; }")))
          (body
           (h2 ,title)
           (table
            ,@(for/list ([key (in-list (sort (hash-keys table) string<?))])
                (define inst (hash-ref table key))
                `(tr (td (a ((class "installer")
                             (href ,(url->string
                                     (combine-url/relative
                                      (string->url installers-url)
                                      inst))))
                            ,key))
                     (td nbsp)
                     (td (span ([class "detail"])
                               ,(~r (/ (file-size (build-path (path-only table-file)
                                                              inst))
                                       (* 1024 1024))
                                    #:precision 1)
                               " MB"))
                     (td nbsp)
                     (td (span ([class "detail"])
                               "SHA1: "
                               (span ([class "checksum"])
                                     ,(call-with-input-file*
                                       (build-path (path-only table-file)
                                                   inst)
                                       sha1)))))))
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
                      (div (span ([class "detail"]) "Commit: " (span ([class "checksum"]) ,stamp))))))
                 null)
           ,@post-content))
        o)
       (void)))))

(module util mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "xml.ss" "xml")
           (lib "uri-codec.ss" "net")
           (lib "string-constant.ss" "string-constants")
           (lib "contract.ss"))

  ;; would be nice if this could use version:version from the framework.
  (define (plt-version)
    (let ([mz-version (version)]
          [stamp-collection
           (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
             (collection-path "repos-time-stamp"))])
      (if (and stamp-collection
               (file-exists? (build-path stamp-collection "stamp.ss")))
          (format "~a-svn~a" mz-version
                  (dynamic-require '(lib "stamp.ss" "repos-time-stamp") 'stamp))
          mz-version)))

  (define home-page
    `(a ([href "/servlets/home.ss"] [target "_top"])
        ,(string-constant plt:hd:home)))

  (define (get-pref/default pref default)
    (get-preference pref (lambda () default)))

  (define (get-bool-pref/default pref default)
    (let ([raw-pref (get-pref/default pref default)])
      (if (string=? raw-pref "false") #f #t)))

  (define (put-prefs names vals)
    (put-preferences names vals))

  (define search-height-default "85")
  (define search-bg-default     "lightsteelblue")
  (define search-text-default   "black")
  (define search-link-default   "darkblue")

  (define *the-highlight-color* "forestgreen")

  ;; string xexpr ... -> xexpr
  (define (with-color color . s)
    `(font ([color ,color]) ,@s))

  ;; xexpr ... -> xexpr
  (define (color-highlight . s)
    (apply with-color *the-highlight-color* s))

  (define repos-or-nightly-build?
    (let ([helpdir (collection-path "help")])
      (lambda ()
        (or (directory-exists? (build-path helpdir ".svn"))
            (directory-exists? (build-path helpdir "CVS"))
            (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
              (collection-path "repos-time-stamp"))))))

  ; string string -> xexpr
  (define (collection-doc-link coll txt)
    (let ([coll-file (build-path (collection-path coll) "doc.txt")])
      (if (file-exists? coll-file)
        `(a ((href
              ,(format
                "~a?file=~a&name=~a&caption=Documentation for the ~a collection"
                "/servlets/doc-anchor.ss"
                (uri-encode (path->string coll-file))
                coll
                coll)))
            ,txt)
        "")))

  ;; (listof string) -> string
  ;; result is forward-slashed web path
  ;;  e.g. ("foo" "bar") -> "foo/bar"
  (define (fold-into-web-path lst)
    (foldr (lambda (s a) (if a (string-append s "/" a) s)) #f lst))

  (define (format-collection-message s)
    `(b ((style "color:green")) ,s))

  (define (make-javascript . ss)
    `(script ([language "Javascript"])
       ,(make-comment (apply string-append "\n"
                             (map (lambda (s) (string-append s "\n")) ss)))))

  (define (redir-javascript k-url)
    (make-javascript "function redir() {"
                     (string-append "  document.location.href=\"" k-url "\"")
                     "}"))

  (define (onload-redir secs)
    (string-append "setTimeout(\"redir()\","
                   (number->string (* secs 1000)) ")"))

  (provide/contract
   [fold-into-web-path ((listof string?) . -> . string?)])

  (provide get-pref/default
           get-bool-pref/default
           put-prefs
           repos-or-nightly-build?
           search-height-default
           search-bg-default
           search-text-default
           search-link-default
           color-highlight
           with-color
           collection-doc-link
           home-page
           format-collection-message
           plt-version
           make-javascript
           redir-javascript
           onload-redir))

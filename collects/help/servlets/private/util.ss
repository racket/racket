(module util mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "xml.ss" "xml")
           (lib "string-constant.ss" "string-constants")
           (lib "contract.ss"))

  (provide/contract
   [hexify-string (string? . -> . string?)]
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
           color-with
           collection-doc-link
           home-page
           format-collection-message
           nl
           plt-version
           make-javascript
           redir-javascript
           onload-redir)

  ;; would be nice if this could use version:version from the framework.
  (define (plt-version)
    (let ([mz-version (version)]
          [stamp-collection
           (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
             (collection-path "repos-time-stamp"))])
      (if (and stamp-collection (file-exists? (build-path stamp-collection "stamp.ss")))
          (format "~a-svn~a" mz-version (dynamic-require '(lib "stamp.ss" "repos-time-stamp") 'stamp))
          mz-version)))
  
  
  (define home-page 
    `(A ((HREF "/servlets/home.ss") (TARGET "_top"))
	,(string-constant plt:hd:home)))

  (define (get-pref/default pref default)
    (get-preference pref (lambda () default)))

  (define (get-bool-pref/default pref default)
    (let ([raw-pref (get-pref/default pref default)])
      (if (string=? raw-pref "false") #f #t)))

  (define (put-prefs names vals)
    (put-preferences names vals)) 

  (define search-height-default "85")
  (define search-bg-default "lightsteelblue")
  (define search-text-default "black")
  (define search-link-default "darkblue")

  (define *the-highlight-color* "forestgreen")

  ; string xexpr ... -> xexpr
  (define (color-with color . s)
    `(FONT ((COLOR ,color)) ,@s))

  ; xexpr ... -> xexpr
  (define (color-highlight . s)
    (apply color-with *the-highlight-color* s))

  (define repos-or-nightly-build?
    (let ([helpdir (collection-path "help")])
      (lambda ()
        (or (directory-exists? (build-path helpdir ".svn"))
            (directory-exists? (build-path helpdir "CVS"))
            (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
              (collection-path "repos-time-stamp"))))))
  
  (define hexifiable '(#\: #\; #\? #\& #\% #\# #\< #\> #\+))
  
  ;; hexify-string : string -> string
  ;; exploits good properties of utf-8 encoding
  ;; that if can-keep? returns true that the byte is
  ;; the character index
  (define (hexify-string s)
    (apply string-append 
	   (map (λ (b) 
		  (cond
                    [(can-keep? b) (string (integer->char b))]
                    [else (format "%~X" b)]))
		(bytes->list (string->bytes/utf-8 s)))))

  ;; can-keep? : byte -> boolean
  ;; source rfc 2396
  (define (can-keep? i)
    (or (<= (char->integer #\a) i (char->integer #\z))
        (<= (char->integer #\A) i (char->integer #\Z))
        (<= (char->integer #\0) i (char->integer #\9))
        (memq i (map char->integer 
                     '(#\- #\_ #\; #\. #\! #\~ #\* #\' #\( #\))))))
  
  ; string string -> xexpr
  (define (collection-doc-link coll txt)
    (let ([coll-file (build-path 
		      (collection-path coll) "doc.txt")])
      (if (file-exists? coll-file)
	  `(A ((HREF 
		,(format 
		  "/servlets/doc-anchor.ss?file=~a&name=~a&caption=Documentation for the ~a collection"
		  (hexify-string (path->string coll-file))
		  coll
		  coll)))
	      ,txt)
	  "")))

  ; (listof string) -> string
  ; result is forward-slashed web path
  ;  e.g. ("foo" "bar") -> "foo/bar"
  (define (fold-into-web-path lst)
      (foldr (lambda (s a)
	       (if a
		   (string-append s "/" a)
		   s))
	     #f
	     lst))
  
  ;; ??
  ;(define (text-frame) "_top")

  (define (format-collection-message s)
    `(B ((STYLE "color:green")) ,s))

  (define nl (string #\newline))

  (define (make-javascript . ss)
    `(SCRIPT ((LANGUAGE "Javascript"))
	     ,(make-comment
	       (apply string-append 
		      nl
		      (map (lambda (s)
			     (string-append s nl))
			   ss)))))

  (define (redir-javascript k-url)
    (make-javascript
     "function redir() {"
     (string-append
       "  document.location.href=\"" k-url "\"") 
     "}"))

  (define (onload-redir secs)
    (string-append 
     "setTimeout(\"redir()\","
     (number->string (* secs 1000))
     ")")))






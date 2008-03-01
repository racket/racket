(module search mzscheme
  (provide (all-defined))
  
  (require scheme/path
           setup/dirs
           ; (prefix help: help/search)
           (prefix help: "private/search.ss")
           (lib "servlet.ss" "web-server")
           (all-except (lib "xml.ss" "xml") 
                       document document? make-document struct:document)
           (prefix home: "home.scm")
           "private/html.scm"
           "private/request.scm"
           "../../indexer/query.scm"
           "../../indexer/indexer.scm"
           "../../indexer/planet/intersperse.scm"
           "../../indexer/snippet.scm"
           "../../indexer/documentation-indices.scm")
  
  (define interface-version 'v1)
  (define timeout 6000)
  
  (define (start request)
    (current-request request)
    (with-errors-to-browser send/finish 
      (lambda () (do-search-results-page))))
  
  
  (define (do-search-results-page)
    (make-response/full 200               ; code
                        "Okay"            ; message
                        (current-seconds) ; seconds
                        TEXT/HTML-MIME-TYPE
                        ;                 ; headers  
                        (list (make-header #"Pragma" #"No-cache")
                              (make-header #"Cache-Control" #"no-cache")
                              (make-header #"Expires" #"Thu, 01 Jan 1970 00:00:00 GMT"))
                        (list doctype-HTML-4.01-Transitional
                              (string-append
                               (xexpr->string
                                (html-page 
                                 #:title (title "Search Results")
                                 #:body "")))
                              (cond [(get-binding 'q #f)
                                     => (λ (q)
                                          (string-append 
                                           ; uncomment to see the search results
                                           ; from the Scribble generated index
                                           ; (help:search-results-page (list q))
                                           (format "<h1>Search results for '~a'</h1>" q)
                                           (xexpr->string 
                                            (query->xexpr q 
                                                          (get-sensitivity)
                                                          (get-contain-all)
                                                          (get-type-normal)))))]
                                    [else                                      
                                     (home:do-home-page)]))))
  
  ;;
  ;; PRESENTATION
  ;; 
  
  (define repository-url-root "/servlets/view.scm/")
  
  (define (query->xexpr q sensitive contain-all type-normal)
    (let* ([the-index (if sensitive the-sensitive-index the-insensitive-index)])
      (let-values ([(hits terms)
                    (query the-index q sensitive contain-all type-normal)])
        ; hits is a list of (cons d score)
        `(div ([class "results"])
              (div ([class "info"])
                   "Total number of hits: " ,(number->string (length hits)))
              (br)
              ,(hits->xml the-index hits terms)))))
  
  (define (hits->xml index hs terms)
    `(div ([class "hits"])
          ,@(map (λ (h) (hit->xml index h terms)) hs)))
  
  (define hit-document car)
  (define hit-score cdr)
  
  (define (hit->xml index h terms)
    (let* ([d       (hit-document h)]
           [score   (hit-score h)]
           [snippet (document->snippet 
                     (document-number->source-path index d) terms)])
      (let ([url (document-number->url index d)])
        `(div ([class "hit"])
              (p (a ([href ,url]) 
                    ,(path->link-text
                      (document-number->source-path index d))) 
                 (br)
                 ,(number->string score) (br)
                 ,@(if (not snippet)
                       '()
                       (list `(tt ,@(intersperse '(br) snippet)))))))))
  
  (define (document->snippet file terms)
    ; Find snippet with the first term occuring in the document.
    ; TODO: An improvement would be to find a snippet containing
    ;       multiple terms, rather than just the first.
    (let ([file (if (equal? (filename-extension file) #"html")
                    (path-add-suffix file ".txt")
                    file)])
      (define (get-snippet-for-term term)
        (let ([ls (occurs-at-lines file term)])
          (cond [(null? ls) #f]
                 [else (snippet-at-line file (car ls) 1)])))
      (define (get-snippet-from-a-term terms)
        (ormap get-snippet-for-term terms))
      (get-snippet-from-a-term terms)))
  
  
  (define (document-number->source-path index d)
    (lookup-document-path index d))
  
  (define (remove-common-root short-path long-path)
    (let loop ([ss (explode-path short-path)]
               [ls (explode-path long-path)])
      (cond [(null? ss)                 (apply build-path ls)]
            [(equal? (car ss) (car ls)) (loop (cdr ss) (cdr ls))]
            [else                       (apply build-path ls)])))
      
  
  (define (document-number->url index d)
    (cond
      [(document-number->source-path index d)
       => (λ (full-path)
            (apply string-append
                   repository-url-root
                   (intersperse "/"
                                (map path->string
                                     (explode-path 
                                      (remove-common-root (find-doc-dir) full-path))))))]
      [else #f]))
  
  (define (path->link-text full-path)
    (apply string-append
           (intersperse "/ "
                        (map path->string
                             (explode-path
                              (remove-common-root (find-doc-dir) full-path))))))
  
  )

(module stuff-url mzscheme
  (require (lib "url.ss" "net")
           (lib "list.ss")
           (lib "plt-match.ss")
           "utils.ss")
  
  ; XXX url: first try continuation, then turn into hash
  
  ; XXX different ways to hash, different ways to store (maybe cookie?)

  ;; before reading this, familiarize yourself with serializable values
  ;; covered in ch 36 in the MzScheme manual.
  
  ;; To keep things small, only serialize closure structures. Furthermore,
  ;; any such structures must come from the current servlet. One exception is
  ;; the kont structure.
  
  ;; The structure-type list (module map) will be replaced by a list of numbers. For example,
  ;; if the third element of the list is 19, then the third structure type is
  ;; the same as the 19th closure type defined in the servlet module.
  
  ;; The list described above may also contain the symbol 'k, if kont is *not*
  ;; at position 0 in the original module map.
  
  ;; The labeling code is the symbol prefix that is created by labels.ss. If the
  ;; servlet is changed in some non-trivial way (i.e. not whitespace or comment),
  ;; then a new labeling code will be created for the servlet. Thus the labeling code
  ;; must be kept as part of the URL. URLs with old labeling codes will simply not
  ;; work since the refactored module will not export any identifiers based off the
  ;; old labeling.
  
  ;; ****************************************
  ;; FUTURE DESIGN
  
  ;; To eliminate the single module requirement, create a global module map at compile time.
  ;; The global map must handle all struct types from any required modules. Then re-write
  ;; the serialized value (+ any graph and fixups) substituting the global numbers for the
  ;; local numbers.
  
  ;; Once the local value (+ any graph and fixups) have been translated to use the global map
  ;; then the local map can be eliminated. The labeling code must still be preserved in the
  ;; URL. Now the labeling code should identify the global map. Hmm... in this model the labeling
  ;; code should somehow reflect any changes to the global map.
  
  ;; ****************************************
  ;; URL LAYOUT
  
  ;; The mod-map will be encoded in the URL path. The graph, fixups and serial will be
  ;; encoded in the query.
  
  ;; The first path element following the servlet file-name will be the labeling code.
  
  ;; The remaining path elements will encode the mod-map, now represented as a list of
  ;; numbers.
  
  ;; The query will contain bindings for at least one and as many as three keys:
  
  ;; g -- the graph
  ;; f -- the fixups
  ;; v -- the main serial value.
  
  ;; If the graph and fixups are trivial, then they will be omitted from the query.
  
  (provide stuff-url
           extend-url-query
           unstuff-url
           find-binding)
  
  (define (read/string str)
    (read (open-input-string str)))
  (define (write/string v)
    (define str (open-output-string))
    (write v str)
    (get-output-string str))
  
  ;; compress-mod-map : (listof (cons mod-spec symbol)) -> (listof (cons (or mod-spec number) symbol))
  (define (compress-mod-map mm)
    (compress-mod-map/seen empty mm))
  
  (define (lookup-seen ms seen)
    (match seen
      [(list)
       (values #f (list ms))]
      [(list-rest ms+ seen+)
       (if (equal? ms ms+)
           (values 0 (list* ms+ seen+))
           (let-values ([(i seen++) (lookup-seen ms seen+)])
             (values (if i (add1 i) #f) (list* ms+ seen++))))]))
  
  (define (compress-mod-map/seen seen mm)
    (match mm
      [(list) 
       (list)]
      [(list-rest (list-rest mod-spec sym) mm)
       (define-values (i seen+) (lookup-seen mod-spec seen))
       (if i
           (list* (cons i sym) (compress-mod-map/seen seen+ mm))
           (list* (cons mod-spec sym) (compress-mod-map/seen seen+ mm)))]))
  
  ;; decompress-mod-map : (listof (cons (or mod-spec number) symbol)) -> (listof (cons mod-spec symbol))
  (define (decompress-mod-map cmm)
    (decompress-mod-map/seen empty cmm))
    
  (define (decompress-mod-map/seen seen cmm)
    (match cmm
      [(list)
       (list)]
      [(list-rest (list-rest mod-spec-or-n sym) cmm)
       (if (number? mod-spec-or-n)
           (list* (cons (list-ref seen mod-spec-or-n) sym)
                  (decompress-mod-map/seen seen cmm))
           (list* (cons mod-spec-or-n sym)
                  (decompress-mod-map/seen (append seen (list mod-spec-or-n)) cmm)))]))
  
  ; compress-serial : serial -> serial (with compressed mod-map)
  (define compress-serial
    (match-lambda
      [(list e0 mm e2 e3 e4 e5)
       (list e0 (compress-mod-map mm) e2 e3 e4 e5)]))
  
  ; decompress-serial : serial (with compressed mod-map) -> serial
  (define decompress-serial
    (match-lambda
      [(list e0 cmm e2 e3 e4 e5)
       (list e0 (decompress-mod-map cmm) e2 e3 e4 e5)]))
  
  ;; url-parts: module-path serial -> string (listof (union number 'k)) s-expr s-expr s-expr
  ;; compute the parts for the url:
  ;;   labeling code
  ;;   simplified mod-map encoding
  ;;   graph
  ;;   fixups
  ;;   main serial
  (define (url-parts mod-path sv)
    (let* ([mod-map (cadr sv)]
           [lab-code (get-labeling-code mod-path mod-map)]
           [simple-map (simplify-module-map mod-path lab-code mod-map)])
      (values lab-code simple-map
              (list-ref sv 3)
              (list-ref sv 4)
              (list-ref sv 5))))
  
  ;; recover-serial: module-path (listof (union number 'k)) s-expr s-expr s-expr -> serial
  ;; recover the serializable value from parts
  (define (recover-serial mod-path label-code simple-map graph fixups main-serial)
    (list (length simple-map)
          (reconstruct-mod-map mod-path label-code simple-map)
          (length graph)
          graph fixups main-serial))
  
  ;; reconstruct-mod-map: module-path string (listof (union number 'k)) -> module-map
  ;; reconstruct the module map from the simple map
  (define (reconstruct-mod-map mod-path label-code simple-map)
    (map
     (lambda (n-or-k)
       (if (symbol? n-or-k)
           `((lib "abort-resume.ss" "prototype-web-server") . ,n-or-k)
           (cons
            mod-path
            (string->symbol
             (format "web-deserialize-info:~a~a"
                     label-code
                     n-or-k)))))
     simple-map))
  
  ;; get-labeling-code: module-path module-map -> string
  ;; pull the labeling code out of the module map
  (define (get-labeling-code pth mod-map)
    (let loop ([mod-map mod-map])
      (cond
        [(null? mod-map)
         (error "couldn't find labeling code")]
        [(and (same-module? pth (caar mod-map))
              (match-labeling-code (cdar mod-map)))
         => (lambda (lcode) lcode)]
        [else (loop (cdr mod-map))])))
  
  (define WEB-DESERIALIZE-INFO-REGEXP (regexp "web-deserialize-info:([a-zA-Z]*)(.*)"))
  ;; match-labeling-code: symbol -> string
  ;; pull the labeling code out of the symbol
  (define (match-labeling-code sym)
    (let ([match? (regexp-match WEB-DESERIALIZE-INFO-REGEXP (symbol->string sym))])
      (and match? (cadr match?))))
  
  ;; match-label: symbol -> number
  ;; pull the closure number out of the symbol
  (define (match-label sym)
    (let ([match? (regexp-match WEB-DESERIALIZE-INFO-REGEXP (symbol->string sym))])
      (and match? (string->number (caddr match?)))))
  
  ;; simplify-module-map: module-path string module-map -> (listof (union number symbol))
  ;; convert the module-map into a simple list
  (define (simplify-module-map pth labeling-code mod-map)
    (let loop ([mm mod-map])
      (cond
        [(null? mm) '()]
        [(and (same-module? pth (caar mm))
              (match-label (cdar mm)))
         => (lambda (lab) (cons lab (loop (cdr mm))))]
        [(same-module? '(lib "abort-resume.ss" "prototype-web-server") (caar mm))
         (cons (cdar mm) (loop (cdr mm)))]
        [else
         (error "cannot construct abreviated module map" mod-map)])))
  
  ;; same-module?: module-path module-path -> boolean
  ;; do the module paths specify the same module?
  (define (same-module? path-str mod-path)
    (eqv? ((current-module-name-resolver) path-str #f #f)
          ((current-module-name-resolver) mod-path #f #f)))
  
  ;; stuff-url: serial url path -> url
  ;; encode in the url
  #;(define (stuff-url svl uri pth)
    (let-values ([(l-code simple-mod-map graph fixups sv)
                  (url-parts pth svl)])
      (let ([new-query
             `(,(cons 'c l-code)
                ,@(if (null? graph) '()
                      (list (cons 'g (format "~s" graph))))
                ,@(if (null? fixups) '()
                      (list (cons 'f (format "~s" fixups))))
                ,(cons 'v (format "~s" sv)))])
        (let ([result-uri
               (make-url
                (url-scheme uri)
                (url-user uri)
                (url-host uri)
                (url-port uri)
                #t
                (append (url-path uri)
                        (map
                         (lambda (n-or-sym) (make-path/param (format "~a" n-or-sym) empty))
                         simple-mod-map))
                new-query
                (url-fragment uri))])
          (begin0
            result-uri
            (when (> (string-length (url->string result-uri))
                     1024)
              (error "the url is too big: " (url->string result-uri))))))))
  
  (require (lib "md5.ss"))
  (define (md5-store str)
    (define hash (md5 (string->bytes/utf-8 str)))
    (with-output-to-file
        (format "/Users/jay/Development/plt/urls/~a" hash)
      (lambda ()
        (write str))
      'replace)
    (bytes->string/utf-8 hash))
  (define (md5-lookup hash)
    (with-input-from-file
        (format "/Users/jay/Development/plt/urls/~a" hash)
      (lambda () (read))))
  
  (define (stuff-url svl uri pth)
    #;(printf "stuff: ~s~n" svl)
    (let ([result-uri
           (make-url
            (url-scheme uri)
            (url-user uri)
            (url-host uri)
            (url-port uri)
            #t
            (url-path uri)
            (list (cons 'c (md5-store (write/string (compress-serial svl)))))
            (url-fragment uri))])
      (begin0
        result-uri
        (when (> (string-length (url->string result-uri))
                 1024)
          (error "the url is too big: " (url->string result-uri))))))
  
  (define (extend-url-query uri key val)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     #t
     (url-path uri)
     (list* (cons key val)
            (url-query uri))
     (url-fragment uri)))
    
  ;; unstuff-url: url url path -> serial
  ;; decode from the url and reconstruct the serial
  #;(define (unstuff-url req-url ses-url mod-path)
    (let ([suff (split-url-path ses-url req-url)]
          [qry (url-query req-url)])      
      (recover-serial
       mod-path
       (find-binding 'c qry)
       (map
        (lambda (elt)
          (define nelt (string->number elt))
          (if (not nelt) (string->symbol elt)
              nelt))
        suff)
       (or (find-binding 'g qry) '())
       (or (find-binding 'f qry) '())
       (find-binding 'v qry))))
  (define (unstuff-url req-url ses-url mod-path)
    (decompress-serial (read/string (md5-lookup (find-binding 'c (url-query req-url))))))
  
  ;; find-binding: symbol (list (cons symbol string)) -> (union string #f)
  ;; find the binding in the query or return false
  (define (find-binding key qry)
    (cond
      [(null? qry) #f]
      [(eqv? key (caar qry))
       (read (open-input-string (cdar qry)))]
      [else (find-binding key (cdr qry))])))

(module stuff-url mzscheme
  (require (lib "url.ss" "net")
           "utils.ss")

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
           unstuff-url
           find-binding)
  
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
       (if (eqv? n-or-k 'k)
           '((lib "abort-resume.ss" "prototype-web-server") . web-deserialize-info:kont)
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
  
  ;; simplify-module-map: module-path string module-map -> (listof (union number 'k))
  ;; convert the module-map into a simple list
  (define (simplify-module-map pth labeling-code mod-map)
    (let loop ([mm mod-map])
      (cond
        [(null? mm) '()]
        [(and (same-module? pth (caar mm))
              (match-label (cdar mm)))
         => (lambda (lab) (cons lab (loop (cdr mm))))]
        [(same-module? '(lib "abort-resume.ss" "prototype-web-server") (caar mm))
         (cons 'k (loop (cdr mm)))]
        [else
         (error "cannot construct abreviated module map" mod-map)])))
  
  ;; same-module?: module-path module-path -> boolean
  ;; do the module paths specify the same module?
  (define (same-module? path-str mod-path)
    (eqv? ((current-module-name-resolver) path-str #f #f)
          ((current-module-name-resolver) mod-path #f #f)))
  
  ;; stuff-url: serial url path -> url
  ;; encode in the url
  (define (stuff-url svl uri pth)
    (let-values ([(l-code simple-mod-map graph fixups sv)
                  (url-parts pth svl)])
      (let ([new-query
             `(,(cons 'c l-code)
                ,@(if (null? graph) '()
                      (cons 'g (format "~s" graph)))
                ,@(if (null? fixups) '()
                      (cons 'f (format "~s" fixups)))
                ,(cons 'v (format "~s" sv)))])
        (let ([result-uri
               (make-url
                (url-scheme uri)
                (url-user uri)
                (url-host uri)
                (url-port uri)
                (append (url-path uri)
                        (map
                         (lambda (n-or-sym) (format "~a" n-or-sym))
                         simple-mod-map))
                new-query
                (url-fragment uri))])
          (begin0
            result-uri
            (when (> (string-length (url->string result-uri))
                     1024)
              (error "the url is too big: " (url->string result-uri))))))))
    
  ;; unstuff-url: url url path -> serial
  ;; decode from the url and reconstruct the serial
  (define (unstuff-url req-url ses-url mod-path)
    (let ([suff (split-url-path ses-url req-url)]
          [qry (url-query req-url)])
      (recover-serial
       mod-path
       (find-binding 'c qry)
       (map
        (lambda (elt)
          (if (string=? elt "k") 'k
              (string->number elt)))
        suff)
       (or (find-binding 'g qry) '())
       (or (find-binding 'f qry) '())
       (find-binding 'v qry))))
  
  ;; find-binding: symbol (list (cons symbol string)) -> (union string #f)
  ;; find the binding in the query or return false
  (define (find-binding key qry)
    (cond
      [(null? qry) #f]
      [(eqv? key (caar qry))
       (read (open-input-string (cdar qry)))]
      [else (find-binding key (cdr qry))]))
  )
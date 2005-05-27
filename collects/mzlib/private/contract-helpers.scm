(module contract-helpers mzscheme

  (provide module-source-as-symbol build-src-loc-string mangle-id)

  ;; mangle-id : syntax string syntax ... -> syntax
  ;; constructs a mangled name of an identifier from an identifier
  ;; the name isn't fresh, so `id' combined with `ids' must already be unique.
  (define (mangle-id main-stx prefix id . ids)
    (datum->syntax-object
     #f
     (string->symbol
      (string-append
       prefix
       (format 
        "-~a~a"
        (syntax-object->datum id)
        (apply 
         string-append 
         (map 
          (lambda (id)
            (format "-~a" (syntax-object->datum id)))
          ids)))))))
  
  ;; build-src-loc-string : syntax -> (union #f string)
  (define (build-src-loc-string stx)
    (let ([source (syntax-source stx)]
          [line (syntax-line stx)]
          [col (syntax-column stx)]
          [pos (syntax-position stx)])
      (cond
        [(and (path? source) line col)
         (format "~a:~a:~a" (path->string source) line col)]
        [(and (string? source) line col)
         (format "~a:~a:~a" source line col)]
        [(and line col)
         (format "~a:~a" line col)]
        [(and (string? source) pos)
         (format "~a:~a" source pos)]
        [(and (path? source) pos)
         (format "~a:~a" (path->string source) pos)]
        [pos
         (format "~a" pos)]
        [else #f])))
  
  (define o (current-output-port))
  
  ;; module-source-as-symbol : syntax -> symbol
  ;; constructs a symbol for use in the blame error messages
  ;; when blaming the module where stx's occurs.
  (define (module-source-as-symbol stx)
    (let ([src-module (syntax-source-module stx)])
      (cond
        [(symbol? src-module) src-module]
        [(module-path-index? src-module) 
         (let-values ([(path base) (module-path-index-split src-module)])
           ;; we dont' normalize here, because we don't
           ;; want to assume that the collection paths
           ;; are set or the file system can be accessed.
           (if path
               (string->symbol (format "~s" path))
               'top-level))]
        [else 'top-level]))))

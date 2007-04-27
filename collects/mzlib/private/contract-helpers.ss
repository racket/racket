(module contract-helpers mzscheme

  (provide module-source-as-symbol build-src-loc-string 
           mangle-id mangle-id-for-maker
           build-struct-names
           nums-up-to
           add-name-prop
           all-but-last)

  (define (add-name-prop name stx)
    (cond
      [(identifier? name)
       (syntax-property stx 'inferred-name (syntax-e name))]
      [(symbol? name)
       (syntax-property stx 'inferred-name name)]
      [else stx]))
  
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
  
  (define (mangle-id-for-maker main-stx prefix id . ids)
    (let ([id-w/out-make (regexp-replace #rx"^make-" (format "~a" (syntax-object->datum id)) "")])
      (datum->syntax-object
       #f
       (string->symbol
        (string-append
         "make-"
         prefix
         (format 
          "-~a~a"
          id-w/out-make
          (apply 
           string-append 
           (map 
            (lambda (id)
              (format "-~a" (syntax-object->datum id)))
            ids))))))))
  
  ;; (cons X (listof X)) -> (listof X)
  ;; returns the elements of `l', minus the last element
  ;; special case: if l is an improper list, it leaves off
  ;; the contents of the last cdr (ie, making a proper list
  ;; out of the input), so (all-but-last '(1 2 . 3)) = '(1 2)
  (define (all-but-last l)
    (cond
      [(null? l) (error 'all-but-last "bad input")]
      [(not (pair? l)) '()]
      [(null? (cdr l)) null]
      [(pair? (cdr l)) (cons (car l) (all-but-last (cdr l)))]
      [else (list (car l))]))
  
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
        [else 'top-level])))
  
  
  (define build-struct-names
    (lambda (name-stx fields omit-sel? omit-set? srcloc-stx)
      (let ([name (symbol->string (syntax-e name-stx))]
            [fields (map symbol->string (map syntax-e fields))]
            [+ string-append])
        (map (lambda (s)
               (datum->syntax-object name-stx (string->symbol s) srcloc-stx))
             (append
              (list 
               (+ "struct:" name)
               (+ "make-" name)
               (+ name "?"))
              (let loop ([l fields])
                (if (null? l)
                    null
                    (append
                     (if omit-sel?
                         null
                         (list (+ name "-" (car l))))
                     (if omit-set?
                         null
                         (list (+ "set-" name "-" (car l) "!")))
                     (loop (cdr l))))))))))
  
  (define (nums-up-to n)
    (let loop ([i 0])
      (cond
        [(= i n) '()]
        [else (cons i (loop (+ i 1)))]))))

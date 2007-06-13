(module util mzscheme
  (require (lib "list.ss")
           (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "serialize.ss")
           (lib "pretty.ss")
           (lib "xml.ss" "xml")
           (lib "url.ss" "net"))
  (define path-element?
    (or/c string? path? (symbols 'up 'same)))
  
  (define port-number? (between/c 1 65535))
  
  (provide/contract
   [path-element? contract?]
   [port-number? contract?]
   [pretty-print-invalid-xexpr (exn:invalid-xexpr? any/c . -> . void)]
   [url-replace-path (((listof path/param?) . -> . (listof path/param?)) url? . -> . url?)]
   [explode-path* (path? . -> . (listof path-element?))]
   [path-without-base (path? path? . -> . (listof path-element?))]
   [list-prefix? (list? list? . -> . boolean?)]
   [strip-prefix-ups ((listof path-element?) . -> . (listof path-element?))] 
   [url-path->string ((listof path/param?) . -> . string?)]
   [network-error ((symbol? string?) (listof any/c) . ->* . (void))]
   [directory-part (path? . -> . path?)]
   ; XXX Eliminate use of this
   [lowercase-symbol! ((or/c string? bytes?) . -> . symbol?)]
   [exn->string ((or/c exn? any/c) . -> . string?)]
   [build-path-unless-absolute (path-string? path-string? . -> . path?)]
   [read/string (string? . -> . serializable?)]
   [write/string (serializable? . -> . string?)])
  
  (define (pretty-print-invalid-xexpr exn xexpr)
    (define code (exn:invalid-xexpr-code exn))
    (parameterize ([pretty-print-size-hook (lambda (v display? out)
                                             (and (equal? v code)
                                                  (string-length (format (if display? "~a" "~v") v))))]
                   [pretty-print-print-hook (lambda (v display? out)
                                              (fprintf out
                                                       (string-append
                                                        "<font color=\"red\">"
                                                        (if display? "~a" "~v")
                                                        "</font>")
                                                       v))])
      (pretty-print xexpr)))
  
  (define (read/string str)
    (read (open-input-string str)))
  (define (write/string v)
    (define str (open-output-string))
    (write v str)
    (get-output-string str))
  
  ; explode-path* : path? -> (listof path?)
  (define (explode-path* p)
    (let loop ([p p] [r empty])
      (cond 
        [(eq? 'relative p) r]
        [(not p) r]
        [else
         (let-values ([(base name dir?) (split-path p)])
           (loop base (list* name r)))])))
  
  ; strip-prefix-ups : (listof path-element?) -> (listof path-element?)
  (define (strip-prefix-ups l)
    (define prefix? (box #t))
    (filter (lambda (p)
              (if (unbox prefix?)
                  (if (eq? 'up p)
                      #f
                      (begin #t
                             (set-box! prefix? #f)))
                  #t))
            l))
  
  ; list-prefix : list? list? -> (or/c list? false/c)
  ; Is l a prefix or r?, and what is that prefix?
  (define (list-prefix? ls rs)
    (match ls
      [(list)
       #t]
      [(list-rest l0 ls)
       (match rs
         [(list)
          #f]
         [(list-rest r0 rs)
          (if (equal? l0 r0)
              (list-prefix? ls rs)
              #f)])]))
  
  ; path-without-base : path? path? -> (listof path-element?)
  (define (path-without-base base path)
    (define b (explode-path* base))
    (define p (explode-path* path))
    (if (list-prefix? b p)
        (list-tail p (length b))
        (error 'path-without-base "~a is not a prefix of ~a" base path)))
  
  ;; replace-path: (url-path -> url-path) url -> url
  ;; make a new url by replacing the path part of a url with a function
  ;; of the url's old path
  ;; also remove the query
  (define (url-replace-path proc in-url)
    (let ([new-path (proc (url-path in-url))])
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       (url-path-absolute? in-url)
       new-path
       empty
       (url-fragment in-url))))
  
  ;; ripped this off from url-unit.ss
  (define (url-path->string strs)
    (apply string-append
           (apply append
                  (map (lambda (s) (list "/" (maybe-join-params s)))
                       strs))))
  
  ;; needs to unquote things!
  (define (maybe-join-params s)
    (if (string? s)
        s
        (let ([s (path/param-path s)])
          (if (string? s)
              s
              (case s
                [(same) "."]
                [(up)   ".."]
                [else (error 'maybe-join-params
                             "bad value from path/param-path: ~e" s)])))))
  
  ;; network-error: symbol string . values -> void
  ;; throws a formatted exn:fail:network
  (define (network-error src fmt . args)
    (raise (make-exn:fail:network (format "~a: ~a" src (apply format fmt args))
                                  (current-continuation-marks))))
  
  ;; build-path-unless-absolute : path-string? path-string? -> path?
  (define (build-path-unless-absolute base path)
    (if (absolute-path? path)
        (build-path path)
        (build-path base path)))
  
  ;; exn->string : (or/c exn any) -> string
  (define (exn->string exn)
    (if (exn? exn)
        (parameterize ([current-error-port (open-output-string)])
          ((error-display-handler) (exn-message exn) exn)
          (get-output-string (current-error-port)))
        (format "~s\n" exn)))
  
  ; lowercase-symbol! : (or/c string bytes) -> symbol
  (define (lowercase-symbol! s)
    (string->symbol
     (string-downcase
      (if (bytes? s)
          (bytes->string/utf-8 s)
          (string-copy s)))))
  
  (define (directory-part path)
    (let-values ([(base name must-be-dir) (split-path path)])
      (cond
        [(eq? 'relative base) (current-directory)]
        [(not base) (error 'directory-part "~a is a top-level directory" path)]
        [(path? base) base]))))
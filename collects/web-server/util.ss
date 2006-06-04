(module util mzscheme
  (require (lib "contract.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net")
           (lib "errortrace-lib.ss" "errortrace")
           (lib "uri-codec.ss" "net"))
  (require "request-structs.ss")
  
  (provide provide-define-struct
           extract-flag
           translate-escapes
           hash-table-empty?
           url-path->string)
  
  (provide/contract
   [valid-port? (any/c . -> . boolean?)]
   [decompose-request ((request?) . ->* . (url? symbol? string?))]
   [network-error ((symbol? string?) (listof any/c) . ->* . (void))]
   [path->list  (path? . -> . (cons/c (or/c path? (symbols 'up 'same))
                                      (listof (or/c path? (symbols 'up 'same)))))]
   [url-path->path ((or/c (symbols 'up 'same) path?) string? . -> . path?)]
   [directory-part (path? . -> . path?)]
   [lowercase-symbol! ((or/c string? bytes?) . -> . symbol?)]
   [exn->string ((or/c exn? any/c) . -> . string?)]
   [build-path-unless-absolute (path? (or/c string? path?) . -> . path?)])
  
  ;; valid-port? : any/c -> boolean?
  (define (valid-port? p)
    (and (number? p) (integer? p) (exact? p) (<= 1 p 65535)))
  
  ;; ripped this off from url-unit.ss
  (define (url-path->string strs)
    (apply
     string-append
     (let loop ([strs strs])
       (cond
         [(null? strs) (list)]
         [else (list* "/"
                      (maybe-join-params (car strs))
                      (loop (cdr strs)))]))))
  
  ;; needs to unquote things!
  (define (maybe-join-params s)
    (cond
      [(string? s) s]
      [else (path/param-path s)]))
  
  ;; decompse-request : request -> uri * symbol * string
  (define (decompose-request req)
    (let* ([uri (request-uri req)]
           [method (request-method req)]
           [path (translate-escapes (url-path->string (url-path uri)))])
      (values uri method path)))
  
  ;; network-error: symbol string . values -> void
  ;; throws a formatted exn:fail:network
  (define (network-error src fmt . args)
    (raise (make-exn:fail:network
            (string->immutable-string
             (apply format (format "~a: ~a" src fmt) args))
            (current-continuation-marks))))
  
  ;; build-path-unless-absolute : path (or/c string? path?) -> path?
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
    (let ([s (if (bytes? s)
                 (bytes->string/utf-8 s)
                 s)])
      (string-lowercase! s)
      (string->symbol s)))
  
  ; prefix? : str -> str -> bool
  ; more here - consider moving this to mzlib's string.ss
  ;; Notes: (GregP)
  ;; 1. What's the significance of char # 255 ???
  ;; 2. 255 isn't an ascii character. ascii is 7-bit
  ;; 3. OK fuck this. It is only used in three places, some of them
  ;;    will involve bytes while the others may involve strings. So
  ;;    I will just use regular expressions and get on with life.
  (define (prefix?-old prefix)
    (let* ([len (string-length prefix)]
           [last (string-ref prefix (sub1 len))]
           [ascii (char->integer last)])
      (if (= 255 ascii)
          ; something could be done about this - ab255 -> ac
          ; and all 255's eliminates upper range check
          (error 'prefix? "prefix can't end in the largest character")
          (let ([next (string-append (substring prefix 0 (sub1 len))
                                     (string (integer->char (add1 ascii))))])
            (lambda (x)
              (and (string<=? prefix x) (string<? x next)))))))      
  
  (define (directory-part path)
    (let-values ([(base name must-be-dir) (split-path path)])
      (cond
        [(eq? 'relative base) (current-directory)]
        [(not base) (error 'directory-part "~a is a top-level directory" path)]
        [(path? base) base])))
  
  ; more here - ".." should probably raise an error instead of disappearing.
  ; XXX: This is terrible. should re-write.
  (define (url-path->path base p)
    (let ([path-elems (regexp-split #rx"/" p)])
      ;;; Hardcoded, bad, and wrong
      (if (or (string=? (car path-elems) "servlets")
              (and (string=? (car path-elems) "")
                   (string=? (cadr path-elems) "servlets")))
          ;; Servlets can have extra stuff after them
          (let ([build-path
                 (lambda (b p)
                   (if (string=? p "")
                       b
                       (build-path b p)))])
            (let loop ([p-e (if (string=? (car path-elems) "")
                                (cddr path-elems)
                                (cdr path-elems))]
                       [f (build-path base 
                                      (if (string=? (car path-elems) "")
                                          (cadr path-elems)
                                          (car path-elems)))])
              (cond
                [(null? p-e)
                 f]
                [(directory-exists? f)
                 (loop (cdr p-e) (build-path f (car p-e)))]
                [(file-exists? f)
                 f]
                [else
                 f]))) ;; Don't worry about e.g. links for now
          (apply build-path base
                 (reverse!
                  (foldl (lambda (x acc)
                           (cond
                             [(string=? x "") acc]
                             [(string=? x ".") acc]
                             [(string=? x "..") (if (pair? acc) (cdr acc) acc)]
                             [else (cons x acc)]))
                         null
                         (regexp-split #rx"/" p)))))))

  ; update-params : Url (U #f String) -> String
  ; to create a new url just like the old one, but with a different parameter part
  ;; GREGP: this is broken! replace with the version from new-kernel
  ;  (define (update-params uri params)
  ;    (url->string
  ;     (make-url (url-scheme uri)
  ;               (url-user uri)
  ;               (url-host uri)
  ;               (url-port uri)
  ;               (url-path uri)
  ;               params
  ;               (url-query uri)
  ;               (url-fragment uri))))
  
  ; to convert a platform dependent path into a listof path parts such that
  ; (forall x (equal? (path->list x) (path->list (apply build-path (path->list x)))))
  (define (path->list p)
    (let loop ([p p] [acc null])
      (let-values ([(base name must-be-dir?) (split-path p)])
        (let ([new-acc (cons name acc)])
          (cond
            [(string? base) (loop base new-acc)]
            [else ; conflate 'relative and #f
             new-acc])))))
  
  ; this should go somewhere that other collections can use it too
  (define-syntax provide-define-struct
    (lambda (stx)
      (syntax-case stx ()
        [(_ (struct-name parent-name) (field ...))
         (syntax (begin (define-struct (struct-name parent-name) (field ...))
                        (provide (struct struct-name (field ...)))))]
        [(_ struct-name (field ...))
         (syntax (begin (define-struct struct-name (field ...))
                        (provide (struct struct-name (field ...)))))])))
  
  ; this is used by launchers
  ; extract-flag : sym (listof (cons sym alpha)) alpha -> alpha
  (define (extract-flag name flags default)
    (let ([x (assq name flags)])
      (if x
          (cdr x)
          default)))
  
  ; hash-table-empty? : hash-table -> bool
  (define (hash-table-empty? table)
    (let/ec out
      (hash-table-for-each table (lambda (k v) (out #f)))
      #t))
  
  ; This comes from Shriram's collection, and should be exported form there.
  ; translate-escapes : String -> String
  (define-struct servlet-error ())
  (define-struct (invalid-%-suffix servlet-error) (chars))
  (define-struct (incomplete-%-suffix invalid-%-suffix) ())
  (define (translate-escapes init)
    (define raw (uri-decode init))
    (list->string
     (let loop ([chars (string->list raw)])
       (match chars
         [(list)
          (list)]
         [(list-rest ic cs)
          (define c
            (cond
              [(char=? ic #\+) #\space]
              [else ic]))
          (list* c (loop cs))])))))
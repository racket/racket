(module util mzscheme
  (require (lib "contract.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "plt-match.ss")
           (lib "errortrace-lib.ss" "errortrace"))
  (require "response-structs.ss"
           "request-structs.ss")
  
  (provide provide-define-struct
           extract-flag
           translate-escapes
           hash-table-empty?
           url-path->string)
  
  (provide/contract
   [decompose-request ((request?) . ->* . (url? symbol? string?))]
   [network-error ((symbol? string?) (listof any/c) . ->* . (void))]
   [path->list  (path? . -> . (cons/c (union path? (symbols 'up 'same))
                                      (listof (union path? (symbols 'up 'same)))))]
   [url-path->path ((union (symbols 'up 'same) path?) string? . -> . path?)]
   [directory-part (path? . -> . path?)]
   [lowercase-symbol! ((union string? bytes?) . -> . symbol?)]
   [exn->string ((union exn? any/c) . -> . string?)]
   [get-mime-type (path? . -> . bytes?)]
   [build-path-unless-absolute (path? (union string? path?) . -> . path?)])
    
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
  
  ;; build-path-unless-absolute : path (union string? path?) -> path?
  (define (build-path-unless-absolute base path)
    (if (absolute-path? path)
        (build-path path)
        (build-path base path)))
  
  ;; exn->string : (union exn any) -> string
  ;; builds an error message, including errortrace annotations (if present)
  ;; TODO: do we really need this? Ask Robby about better ways to print exceptions.
  (define (exn->string exn)
    (if (exn? exn)
        (let ([op (open-output-string)])
          (display (exn-message exn) op)
          (newline op)
          (print-error-trace op exn)
          (get-output-string op))
        (format "~s\n" exn)))
  
  ; lowercase-symbol! : (union string bytes) -> symbol
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
  
  
  ;; get-mime-type: path -> bytes
  ;; determine the mime type based on the filename's suffix
  ;;
  ;; Notes (GregP):
  ;; 1. Can we determine the mime type based on file contents?
  ;; 2. Assuming that 7-bit ASCII is correct for mime-type
  (define get-mime-type
    (let ([file-suffix-regexp (byte-regexp #".*\\.([^\\.]*$)")])
      (lambda (path)
        (match (regexp-match file-suffix-regexp (path->bytes path))
          [(list path-bytes sffx)
           (hash-table-get MIME-TYPE-TABLE
                           (lowercase-symbol! sffx)
                           (lambda () DEFAULT-MIME-TYPE))]
          [_ DEFAULT-MIME-TYPE]))))
  
  
  (define DEFAULT-MIME-TYPE #"text/plain")
  
  (define MIME-TYPE-TABLE
    (let ([table (make-hash-table)])
      (for-each (lambda (x) (hash-table-put! table (car x) (cdr x)))
                '((htm  . #"text/html")
                  (html . #"text/html")
                  (css  . #"text/css")
                  (txt  . #"text/plain")
                  (hqx  . #"application/mac-binhex40")
                  (doc  . #"application/msword")
                  (plt  . #"application/octet-stream")
                  (w02  . #"application/octet-stream")
                  (w03  . #"application/octet-stream")
                  (exe  . #"application/octet-stream")
                  (bin  . #"application/octet-stream")
                  (pdf  . #"application/pdf")
                  (ps   . #"application/postscript")
                  (rtf  . #"application/rtf")
                  (dvi  . #"application/x-dvi")
                  (tar  . #"application/x-tar")
                  (tex  . #"application/x-tex")
                  (zip  . #"application/zip")
                  (xls  . #"application/msexcel")
                  (ppt  . #"application/powerpoint")
                  (pot  . #"application/powerpoint")
                  (ppf  . #"application/persuasion")
                  (fm   . #"application/filemaker")
                  (pm6  . #"application/pagemaker")
                  (psd  . #"application/x-photoshop")
                  (pdd  . #"application/x-photoshop")
                  (ram  . #"audio/x-pn-realaudio")
                  (ra   . #"audio/x-realaudio")
                  (swf  . #"application/x-shockwave-flash")
                  (aif  . #"audio/aiff")
                  (au   . #"audio/basic")
                  (voc  . #"audio/voice")
                  (wav  . #"audio/wave")
                  (mov  . #"video/quicktime")
                  (mpg  . #"video/mpeg")
                  (png  . #"image/png")
                  (bmp  . #"image/bmp")
                  (gif  . #"image/gif")
                  (jpg  . #"image/jpeg")
                  (tif  . #"image/tiff")
                  (pic  . #"image/x-pict")))
      table))
  
  (define (directory-part path)
    (let-values ([(base name must-be-dir) (split-path path)])
      (cond
        [(eq? 'relative base) (current-directory)]
        [(not base) (error 'directory-part "~a is a top-level directory" path)]
        [(path? base) base])))

  ; more here - ".." should probably raise an error instead of disappearing.
  (define (url-path->path base p)
    (let ((path-elems (chop-string #\/ p)))
      ;;; Hardcoded, bad, and wrong
      (if (or (string=? (car path-elems) "servlets")
              (and (string=? (car path-elems) "")
                   (string=? (cadr path-elems) "servlets")))
          ;; Servlets can have extra stuff after them
          (let loop ((p-e (if (string=? (car path-elems) "")
                              (cddr path-elems)
                              (cdr path-elems)))
                     (f (build-path base 
                                    (if (string=? (car path-elems) "")
                                        (cadr path-elems)
                                        (car path-elems)))))
            (cond
              ((null? p-e) f)
              ((directory-exists? f) (loop (cdr p-e) (build-path f (car p-e))))
              ((file-exists? f) f)
              (else f))) ;; Don't worry about e.g. links for now
          ; spidey can't check build-path's use of only certain symbols
          (apply build-path base
                 (foldr (lambda (x acc)
                          (cond
                            [(string=? x "") acc]
                            [(string=? x ".") acc]
                            [(string=? x "..") acc] ; ignore ".." (cons 'up acc)]
                            [else (cons x acc)]))
                        null
                        (chop-string #\/ p))))))
  
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
  
  ; chop-string : Char String -> (listof String)
  (define (chop-string separator s)
    (let ([p (open-input-string s)])
      (let extract-parts ()
        (cons (list->string
               (let part ()
                 (let ([char (peek-char p)])
                   (cond
                     [(eof-object? char) null]
                     [else (cond
                             [(eq? separator char) null]
                             [else (read-char p) (cons char (part))])]))))
              (cond
                [(eof-object? (read-char p)) null]
                [else (extract-parts)])))))
  
  
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
  (define (translate-escapes raw)
    (list->string
     (let loop ((chars (string->list raw)))
       (if (null? chars) null
           (let ((first (car chars))
                 (rest (cdr chars)))
             (let-values (((this rest)
                           (cond
                             ((char=? first #\+)
                              (values #\space rest))
                             ((char=? first #\%)
                              ; MF: I rewrote this code so that Spidey could eliminate all checks.
                              ; I am more confident this way that this hairy expression doesn't barf.
                              (if (pair? rest)
                                  (let ([rest-rest (cdr rest)])
                                    (if (pair? rest-rest)
                                        (values (integer->char
                                                 (or (string->number (string (car rest) (car rest-rest)) 16)
                                                     (raise (make-invalid-%-suffix
                                                             (if (string->number (string (car rest)) 16)
                                                                 (car rest-rest)
                                                                 (car rest))))))
                                                (cdr rest-rest))
                                        (raise (make-incomplete-%-suffix rest))))
                                  (raise (make-incomplete-%-suffix rest))))
                             (else (values first rest)))))
               (cons this (loop rest))))))))
  )

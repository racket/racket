#lang racket/base
(require net/url
         racket/match
         racket/contract)

(require web-server/dispatchers/dispatch
         web-server/private/util
         web-server/http
         web-server/http/response
         web-server/dispatchers/filesystem-map)

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [read-range-header (-> (listof header?) (or/c (listof pair?) false/c))]
 [make
  (->* (#:url->path url->path/c)
       (#:path->mime-type (path-string? . -> . (or/c false/c bytes?))
                          #:indices (listof path-string?))
       dispatcher/c)])

;; looks-like-directory : str -> bool
;; to determine if is url style path looks like it refers to a directory
(define (looks-like-directory? path)
  (and (regexp-match #rx"/$" path)
       #t))

(define interface-version 'v1)

(define (make #:url->path url->path
              #:path->mime-type [path->mime-type (lambda (path) #f)]
              #:indices [indices (list "index.html" "index.htm")])
  (lambda (conn req)
    (define uri (request-uri req))
    (define method (request-method req))
    (define-values (path _) (url->path uri))
    (cond [(file-exists? path)
           (output-file conn path method (path->mime-type path)
                        (read-range-header (request-headers/raw req)))]
          [(directory-exists? path)
           (if (looks-like-directory? (url-path->string (url-path uri)))
               (let/ec esc
                 (for-each (lambda (dir-default)
                             (define full-name (build-path path dir-default))
                             (when (file-exists? full-name)
                               (esc (output-file conn full-name method (path->mime-type full-name)
                                                 (read-range-header (request-headers/raw req))))))
                           indices)
                 (next-dispatcher))
               (output-response 
                conn
                (redirect-to (string-append (url-path->string (url-path uri)) "/"))))]
          [else (next-dispatcher)])))

;; read-range-header : (listof header) -> (U (alist-of (U integer #f) (U integer #f)) #f)
;;
;; Returns a list of pairs of the byte offsets specified in an HTTP Range
;; header, or #f if the header is missing or malformed.
;;
;; The HTTP spec for the Range header can be found here:
;;
;;     http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35
;;
;; More specifically, the (non-#f form of the) result is:
;;
;;     (list-of (U byte-range-spec suffix-byte-range-spec))
;;
;; where:
;;
;;     byte-range-spec : (cons integer (U integer #f))
;;     suffix-byte-range-spec : (cons #f integer)
;;
;; All offsets are inclusive: the integers are precisely those that appear
;; in the header.
(define read-range-header
  (let ([range-header-regexp    #px#"^bytes=(.*)$"]
        [range-delimiter-regexp #px#","]
        [range-regexp           #px#"^([0-9]*)-([0-9]*)$"]
        [range-error            (lambda (header)
                                  (display (format "Bad Range header: ~s. File a Racket bug report!\n"
                                                   (header-value header))
                                           (current-error-port))
                                  #f)])
    (lambda (headers)
      (let ([header (headers-assq* #"Range" headers)])
        (if header
            (let/ec escape
              (match (regexp-match range-header-regexp (header-value header))
                [(list _ ranges-string)
                 (let ([ranges (regexp-split range-delimiter-regexp ranges-string)])
                   (map (lambda (range-string)
                          (match (regexp-match range-regexp range-string)
                            [(list _ start-offset end-offset)
                             (cons (string->number (bytes->string/utf-8 start-offset))
                                   (string->number (bytes->string/utf-8 end-offset)))]
                            [#f (escape (range-error header))]))
                        ranges))]
                [#f (escape (range-error header))]))
            #f)))))

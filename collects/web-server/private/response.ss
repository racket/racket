(module response mzscheme
  (require (lib "contract.ss")
           (lib "md5.ss")
           (lib "port.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (only (lib "list.ss" "srfi" "1") fold filter-map)
           (only (lib "string.ss" "srfi" "13") string-join)
           (lib "xml.ss" "xml")
           "connection-manager.ss"
           "../private/request-structs.ss"
           "../private/response-structs.ss"
           "util.ss")
  
  (provide/contract
   [rename ext:output-response output-response (connection? response? . -> . void)]
   [rename ext:output-response/method output-response/method (connection? response? symbol? . -> . void)]
   [rename ext:output-file output-file (connection? path? symbol? bytes? (or/c pair? false/c) . -> . void)])
  
  ;; Table 1. head responses:
  ; ------------------------------------------------------------------------------
  ; |method | close? | x-fer coding || response actions
  ; |-----------------------------------------------------------------------------
  ; |-----------------------------------------------------------------------------
  ; |head   |   #t   | chunked      || 1. Output the headers only.
  ; |-------------------------------|| 2. Add the special connection-close header.
  ; |head   |   #t   | not-chunked  ||
  ; |-----------------------------------------------------------------------------
  ; |head   |   #f   | chunked      || 1. Output the headers only.
  ; |-------------------------------|| 2. Don't add the connection-close header.
  ; |head   |   #f   | not-chunked  ||
  ; |-----------------------------------------------------------------------------
  
  ;; Table 2. get responses:
  ; ------------------------------------------------------------------------------
  ; |method | x-fer-coding | close? || response actions
  ; |-----------------------------------------------------------------------------
  ; |-----------------------------------------------------------------------------
  ; | get   | chunked      |   #t   || 1. Output headers as above.
  ; |       |              |        || 2. Generate trivial chunked response.
  ; |-----------------------------------------------------------------------------
  ; | get   | chunked      |   #f   || 1. Output headers as above.
  ; |       |              |        || 2. Generate chunks as per RFC 2616 sec. 3.6
  ; |-----------------------------------------------------------------------------
  ; | get   | not chunked  |   #t   || 1. Output headers as above.
  ; |-------------------------------|| 2. Generate usual non-chunked response.
  ; | get   | not chunked  |   #f   ||
  ; |-----------------------------------------------------------------------------
  
  ;; Notes:
  ;; 1. close? is a boolean which corresponds roughly to the protocol version.
  ;;    #t |-> 1.0 and #f |-> 1.1. See function close-connection? in
  ;;    private/request.ss
  ;;
  ;; 2. In the case of a chunked response when close? = #f, then the response
  ;;    must be compliant with http 1.0. In this case the chunked response is
  ;;    simply turned into a non-chunked one.
  
  (define (output-response conn resp)
    (output-response/method conn resp 'get))
  
  ; XXX Check method in response
  (define (output-response/method conn resp meth)
    (define bresp (response->response/basic (connection-close? conn) resp))
    (output-headers+response/basic conn bresp)
    (unless (eq? meth 'head)
      (output-response/basic conn bresp)))
  
  (define (response->response/basic close? resp)
    (cond
      [(response/full? resp)
       (make-response/full 
        (response/basic-code resp)
        (response/basic-message resp)
        (response/basic-seconds resp)
        (response/basic-mime resp)
        (list* (make-header #"Content-Length" (string->bytes/utf-8 (number->string (response/full->size resp))))
               (response/basic-headers resp))
        (response/full-body resp))]
      [(response/incremental? resp)
       (if close?
           resp
           (make-response/incremental 
            (response/basic-code resp)
            (response/basic-message resp)
            (response/basic-seconds resp)
            (response/basic-mime resp)
            (list* (make-header #"Transfer-Encoding" #"chunked")
                   (response/basic-headers resp))
            (response/incremental-generator resp)))]
      [(and (pair? resp) (bytes? (car resp)))
       (response->response/basic
        close?
        (make-response/full 200 "Okay" (current-seconds) (car resp) empty
                            (cdr resp)))]
      [else
       (response->response/basic
        close?
        (make-response/full 200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE empty
                            (list (xexpr->string resp))))]))
  
  ;; Write the headers portion of a response to an output port.
  ;; NOTE: According to RFC 2145 the server should write HTTP/1.1
  ;;       header for *all* clients.
  (define (output-headers+response/basic conn bresp)
    (fprintf (connection-o-port conn)
             "HTTP/1.1 ~a ~a\r\n"
             (response/basic-code bresp)
             (response/basic-message bresp))
    (output-headers
     conn 
     (list* (make-header #"Date" (string->bytes/utf-8 (seconds->gmt-string (current-seconds))))
            (make-header #"Last-Modified" (string->bytes/utf-8 (seconds->gmt-string (response/basic-seconds bresp))))
            (make-header #"Server" #"PLT Scheme")
            (make-header #"Content-Type" (response/basic-mime bresp))
            (append (if (connection-close? conn)
                        (list (make-header #"Connection" #"close"))
                        empty)
                    (response/basic-headers bresp)))))
  
  ;; output-headers : connection (list-of header) -> void
  (define (output-headers conn headers)
    (print-headers (connection-o-port conn) headers))
  
  ;; print-headers : output-port (list-of header) -> void
  (define (print-headers out headers)
    (for-each (match-lambda
                [(struct header (field value))
                 (fprintf out "~a: ~a\r\n" field value)])
              headers)
    (fprintf out "\r\n"))
  
  (define (output-response/basic conn bresp)
    (define o-port (connection-o-port conn))
    (match bresp
      [(? response/full?)
       (for-each
        (lambda (str) (display str o-port))
        (response/full-body bresp))]
      [(? response/incremental?)
       (if (connection-close? conn)
           ((response/incremental-generator bresp)
            (lambda chunks
              (for-each (lambda (chunk) (display chunk o-port)) chunks)))
           (begin
             ((response/incremental-generator bresp)
              (lambda chunks
                (fprintf o-port "~x\r\n"
                         (apply + 0 (map data-length chunks)))                     
                (for-each (lambda (chunk) (display chunk o-port)) chunks)
                (fprintf o-port "\r\n")))
             ; one \r\n ends the last (empty) chunk and the second \r\n ends the (non-existant) trailers
             (fprintf o-port "0\r\n\r\n")))]))
  
  (define (data-length x)
    (if (string? x)
        (data-length (string->bytes/utf-8 x))
        (bytes-length x)))
  
  ; seconds->gmt-string : Nat -> String
  ; format is rfc1123 compliant according to rfc2068 (http/1.1)
  (define (seconds->gmt-string s)
    (let* ([local-date (seconds->date s)]
           [date (seconds->date (- s
                                   (date-time-zone-offset local-date)
                                   (if (date-dst? local-date) 3600 0)))])
      (format "~a, ~a ~a ~a ~a:~a:~a GMT"
              (vector-ref DAYS (date-week-day date))
              (two-digits (date-day date))
              (vector-ref MONTHS (sub1 (date-month date)))
              (date-year date)
              (two-digits (date-hour date))
              (two-digits (date-minute date))
              (two-digits (date-second date)))))
  
  ; two-digits : num -> str
  (define (two-digits n)
    (let ([str (number->string n)])
      (if (< n 10) (string-append "0" str) str)))
  
  (define MONTHS
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  
  (define DAYS
    #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  
  (define (ext:wrap f)
    (lambda (conn . args)
      (with-handlers ([exn? (lambda (exn)
                              (kill-connection! conn)
                              (raise exn))])
        (apply f conn args)
        (flush-output (connection-o-port conn)))))
  
  (define ext:output-response
    (ext:wrap output-response))
  
  ;; response/full->size: response/full -> number
  (define (response/full->size resp)
    (apply + (map data-length (response/full-body resp))))
  
  ;; output-file: connection
  ;;              path
  ;;              symbol
  ;;              bytes
  ;;              (U (listof (U byte-range-spec suffix-byte-range-spec)) #f)
  ;;           -> void
  ;;
  ;; Ranges is #f if the client did not specify a Range header, or:
  ;;
  ;;     (list-of (U byte-range-spec suffix-byte-range-spec))
  ;;
  ;; where:
  ;;
  ;;     byte-range-spec : (cons integer (U integer #f))
  ;;     suffix-byte-range-spec : (cons #f integer)
  ;;
  ;; as described in the comments in dispatchers/dispatch-files.ss.
  ;;
  ;; A boundary is generated only if a multipart/byteranges response needs
  ;; to be generated (i.e. if a Ranges header was specified with more than
  ;; one range in it).
  (define (output-file conn file-path method mime-type ranges)
    (output-file/boundary
     conn
     file-path
     method
     mime-type
     ranges
     (if (and ranges (> (length ranges) 1))
         (md5 (string->bytes/utf-8 (number->string (current-inexact-milliseconds))))
         #f)))
  
  ;; output-file: connection
  ;;              path
  ;;              symbol
  ;;              bytes
  ;;              (U (listof (U byte-range-spec suffix-byte-range-spec)) #f)
  ;;              (U bytes #f)
  ;;           -> void
  (define (output-file/boundary conn file-path method mime-type ranges boundary)
    ; total-file-length : integer
    (define total-file-length
      (file-size file-path))
    ; modified-seconds : integer
    (define modified-seconds
      (file-or-directory-modify-seconds file-path))
    ; boundary-length : (U integer #f)
    (define boundary-length
      (if boundary
          (bytes-length boundary)
          #f))
    ; If convert-http-ranges fails, send a 416 bad range resposne:
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (fprintf (current-error-port)
                                (format "~a File a PLT bug report if this is on a live server!~n" (exn-message exn)))
                       (output-headers+response/basic
                        conn
                        (make-416-response modified-seconds mime-type)))])
      (let* (; converted-ranges : (alist-of integer integer)
             ; This is a list of actual start and end offsets in the file. 
             ; See the comments for convert-http-ranges for more information.
             [converted-ranges
              (if ranges
                  (convert-http-ranges ranges total-file-length)
                  (list (cons 0 total-file-length)))]
             ; multipart-headers : (list-of bytes)
             ; This is a list of header blocks to prepend to each range being sent.
             ; The idea is so we can calculate an overall content-length for the
             ; response. This *must be* the same length as converted-ranges.
             [multipart-headers
              (if (> (length converted-ranges) 1)
                  (prerender-multipart/byteranges-headers mime-type converted-ranges total-file-length)
                  (list #""))]
             ; total-content-length : integer
             [total-content-length
              (if (= (length converted-ranges) 1)
                  ; One range: content-length is the length of the range being sent:
                  (- (cdar converted-ranges) (caar converted-ranges))
                  ; Multiple ranges: content-length is the length of the multipart,
                  ; including content, headers and boundaries:
                  (fold (lambda (range headers accum)
                          (+ accum                       ; length so far
                             (bytes-length headers)      ; length of the headers and header newlinw
                             (- (cdr range) (car range)) ; length of the content
                             2))                         ; length of the content newline
                        (+ (* (+ boundary-length 4) 
                              (length converted-ranges)) ; length of the intermediate boundaries
                           (+ boundary-length 6))        ; length of the final boundary
                        converted-ranges
                        multipart-headers))])
        ; Send a 206 iff ranges were specified in the request:
        (output-headers+response/basic
         conn
         (if ranges
             (make-206-response modified-seconds mime-type total-content-length total-file-length converted-ranges boundary)
             (make-200-response modified-seconds mime-type total-content-length)))
        ; Send the appropriate file content:
        (when (eq? method 'get)
          ; XXX Move out?
          (adjust-connection-timeout! ; Give it one second per byte.
           conn
           (apply + (map (lambda (range)
                           (- (cdr range) (car range)))
                         converted-ranges)))
          (with-handlers ([exn? (lambda (exn) (network-error 'output-file (exn-message exn)))])
            (call-with-input-file file-path
              (lambda (input)
                (if (= (length converted-ranges) 1)
                    ; Single ranges (in 200 or 206 responses) are sent straight out
                    ; in their simplest form:
                    (output-file-range conn input (caar converted-ranges) (cdar converted-ranges))
                    ; Multiple ranges are encoded as multipart/byteranges:
                    (let loop ([ranges converted-ranges] [multipart-headers multipart-headers])
                      (match ranges
                        [(list) 
                         ; Final boundary (must start on new line; ends with a new line)
                         (fprintf (connection-o-port conn) "--~a--\r\n" boundary)
                         (void)]
                        [(list-rest (list-rest start end) rest)
                         ; Intermediate boundary (must start on new line; ends with a new line)
                         (fprintf (connection-o-port conn) "--~a\r\n" boundary)
                         ; Headers and new line
                         (display (car multipart-headers) (connection-o-port conn))
                         ; Content
                         (output-file-range conn input start end)
                         ; Newline before next field
                         (fprintf (connection-o-port conn) "\r\n")
                         (loop rest (cdr multipart-headers))]))))))))))
  
  ;; prerender-multipart/byteranges-headers : bytes (alist-of integer integer) integer -> (list-of bytes)
  (define (prerender-multipart/byteranges-headers mime-type converted-ranges total-file-length)
    (map (lambda (range)
           (match range
             [(list-rest start end)
              (let ([out (open-output-bytes)])
                (print-headers out (list (make-header #"Content-Type" mime-type)
                                         (make-content-range-header start end total-file-length)))
                (begin0 (get-output-bytes out)
                        (close-output-port out)))]))
         converted-ranges))

  ;; output-file-range : connection file-input-port integer integer -> void
  ;;
  ;; start must be inclusive; end must be exclusive.
  (define (output-file-range conn input start end)
    (file-position input start)
    (let ([limited-input (make-limited-input-port input (- end start) #f)])
      (copy-port limited-input (connection-o-port conn))
      (close-input-port limited-input)))
  
  ;; convert-http-ranges : (alist-of (U integer #f) (U integer #f))
  ;;                       integer
  ;;                    -> (alist-of integer integer)
  ;;
  ;; Converts a list of HTTP ranges:
  ;;
  ;; into pairs of offsets we can use to read from a file:
  ;; 
  ;;   - suffix-byte-range-specs are converted to pairs of absolute offsets;
  ;;   - missing end offsets in byte-range-specs ranges are filled in;
  ;;   - end offsets are exclusive (as opposed to the inclusive offsets in ranges and the HTTP spec).
  ;;
  ;; The HTTP spec recommends that ranges are sent in the order they are specified in the request.
  (define (convert-http-ranges ranges total-file-length)
    (define converted
      (filter-map (lambda (range)
                    ; a : (U integer #f)
                    ; b : (U integer #f)
                    ; The original values quoted in the Range header:
                    (define-values (a b)
                      (values (car range)
                              (cdr range)))
                    ; a* : integer
                    ; b* : integer
                    ; Convert inclusive end offsets and suffix ranges:
                    (define-values (a* b*)
                      (cond [(not a) (values (- total-file-length b) total-file-length)]
                            [(not b) (values a total-file-length)]
                            [else    (values a (add1 b))]))
                    ; a** : integer
                    ; b** : integer
                    ; Trim to the size of the file:
                    (define-values (a** b**)
                      (values (max 0 (min total-file-length a*))
                              (max 0 (min total-file-length b*))))
                    ; Get rid of zero-length ranges (including ones that are outside the file length):
                    (if (< a** b**)
                        (cons a** b**)
                        #f))
                  ranges))
    (if (null? converted)
        (error (format "No satisfiable ranges in ~a/~a." ranges total-file-length))
        converted))
  
  ;; make-206-response : integer bytes integer integer (alist-of integer integer) bytes -> basic-response
  (define (make-206-response modified-seconds mime-type total-content-length total-file-length converted-ranges boundary)
    (if (= (length converted-ranges) 1)
        (let ([start (caar converted-ranges)]
              [end   (cdar converted-ranges)])
          (make-response/basic
           206 "Partial content"
           modified-seconds
           mime-type 
           (list (make-header #"Accept-Ranges" #"bytes")
                 (make-content-length-header total-content-length)
                 (make-content-range-header start end total-file-length))))
        (make-response/basic
         206 "Partial content"
         modified-seconds
         (bytes-append #"multipart/byteranges; boundary=" boundary)
         (list (make-header #"Accept-Ranges" #"bytes")
               (make-content-length-header total-content-length)))))
  
  ;; make-200-response : integer bytes integer -> basic-response
  (define (make-200-response modified-seconds mime-type total-content-length)
    (make-response/basic
     200 "OK"
     modified-seconds
     mime-type 
     (list (make-header #"Accept-Ranges" #"bytes")
           (make-content-length-header total-content-length))))
  
  ;; make-416-response : integer bytes -> basic-response
  (define (make-416-response modified-seconds mime-type)
    (make-response/basic
     416 "Invalid range request"
     modified-seconds
     mime-type 
     null))
  
  ;; make-content-length-header : integer -> header
  (define (make-content-length-header total-content-length)
    (make-header #"Content-Length" (string->bytes/utf-8 (number->string total-content-length))))
  
  ;; make-content-range-header : integer integer integer -> header
  ;; start must be inclusive; end must be exclusive.
  (define (make-content-range-header start end total-file-length)
    (make-header #"Content-Range" 
                 (string->bytes/utf-8
                  (format "bytes ~a-~a/~a" start (sub1 end) total-file-length))))
  
  (define ext:output-file
    (ext:wrap output-file))
  
  (define ext:output-response/method
    (ext:wrap output-response/method))
  
  )